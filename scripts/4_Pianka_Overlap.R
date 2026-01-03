#
#                   R SCRIPT FOR CREATING DIFFERENTIATION MATRICES 
#                    FOR OBSERVATIONS USING PIANKA OVERLAP METRIC 
#                              FOR INSECTIVORE FORAGING
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Kimberly Navarro Velez, Dec 12 2025
# Last edited by Matthew Gilbert, Jan 2 2026
#                                
#______________________________________________________________________________

# Set working directory to source folder

# load libraries
library(ggplot2)
library(dplyr)
library(tibble)
library(tidyr)
library(reshape2)
library(cowplot)
library(ggpattern)
library(ggtext)
library(ggrepel)
library(vegan)
library(tidyverse)
library(janitor)
library(viridis)


data <- read.csv("0_Full_Dataset.csv", header = T)
set.seed(444)

#____________________________________________________________________________

#              Process data 

df <- data %>%
  rename(
    row = X,
    individual_id = ID..,
    species = Species,
    bird_height = Bird.Height,
    foraging_location = Foraging.Location,
    foraging_technique = Foraging.Behavior,
    substrate_type = Substrate.type,
    canopy_height = Canopy.height,
    prop_canopy_height = X..Height.in.Canopy
  )

df_reduced <- df %>%
  mutate(
    foraging_technique = case_when(
      bird_height == 0 & foraging_technique == "G" ~ "K",
      TRUE ~ foraging_technique
    )
  )

#____________________________________________________________________________
#____________________________________________________________________________
# Construct working dataset (wd)

wd <- df_reduced %>%
  transmute(
    species            = species,
    bird_height        = prop_canopy_height,
    foraging_location  = foraging_location,
    foraging_behavior  = foraging_technique,
    canopy_height      = canopy_height
    
  ) %>%
  drop_na() %>%
  arrange(species)

#____________________________________________________________________________
# height binning
wd <- wd %>%
  mutate(
    height_bin = cut(
      bird_height,
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = c("Understory", "Lower Mid", "Higher Mid", "Upperstory"),
      include.lowest = TRUE
    )
  )

hist(wd$canopy_height)

# canopy binning
wd <- wd %>%
  mutate(
    canopy_bin = cut(
      canopy_height,
      breaks = c(0, 5, 7.5, 10, 12.5, 25),
      labels = c("0-5", "5-7.5", "7.5-10", "10-12.5", "12.5-25"),
      include.lowest = TRUE
    )
  )

#____________________________________________________________________________
# Pianka index + matrix

pianka_index <- function(p1, p2) {
  sum(p1 * p2) / sqrt(sum(p1^2) * sum(p2^2))
}

compute_pianka_matrix <- function(df, species_col, niche_col) {
  
  # -----------------------------
  # 1. Drop invalid niche values
  # -----------------------------
  df_clean <- df %>%
    filter(
      !is.na({{ niche_col }}),
      {{ niche_col }} != "",
      toupper(as.character({{ niche_col }})) != "NONE"
    )
  
  # -----------------------------
  # 2. Build frequency table
  # -----------------------------
  freq_table <- df_clean %>%
    count({{ species_col }}, {{ niche_col }}) %>%
    pivot_wider(
      names_from  = {{ niche_col }},
      values_from = n,
      values_fill = 0
    )
  
  # -----------------------------
  # 3. Drop species with zero rows
  # -----------------------------
  freq_table <- freq_table %>%
    filter(rowSums(across(-{{ species_col }})) > 0)
  
  # -----------------------------
  # 4. Proportional use matrix
  # -----------------------------
  prop_matrix <- freq_table %>%
    column_to_rownames(var = as_label(enquo(species_col))) %>%
    as.matrix() %>%
    prop.table(1)
  
  spp <- rownames(prop_matrix)
  n   <- length(spp)
  
  # -----------------------------
  # 5. Pianka overlap
  # -----------------------------
  pianka_mat <- matrix(NA, n, n, dimnames = list(spp, spp))
  
  for (i in 1:n) {
    for (j in 1:n) {
      pianka_mat[i, j] <- pianka_index(prop_matrix[i, ], prop_matrix[j, ])
    }
  }
  
  list(
    freq_table    = freq_table,
    prop_matrix   = prop_matrix,
    pianka_matrix = pianka_mat
  )
}


#____________________________________________________________________________

pianka_behavior <- compute_pianka_matrix(
  wd,
  species_col = species,
  niche_col   = foraging_behavior
)

pianka_location <- compute_pianka_matrix(
  wd,
  species_col = species,
  niche_col   = foraging_location
)

pianka_height <- compute_pianka_matrix(
  wd,
  species_col = species,
  niche_col   = height_bin
)

pianka_canopy <- compute_pianka_matrix(
  wd,
  species_col = species,
  niche_col   = canopy_bin
)

#____________________________________________________________________________
# Heatmap plotting function
plot_pianka_heatmap <- function(pianka_matrix, title) {
  
  species_order <- c(
    "BBTO", "BWVI", "OVEN", "PAWA", "YRWA",
    "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"
  )
  
  pianka_df <- as.data.frame(pianka_matrix) %>%
    rownames_to_column("species1") %>%
    pivot_longer(-species1, names_to = "species2", values_to = "overlap") %>%
    mutate(
      species1 = factor(species1, levels = species_order),
      species2 = factor(species2, levels = species_order),
      is_diag  = species1 == species2,
      overlap_plot = ifelse(is_diag, NA, overlap),
      label = ifelse(is_diag, "", sprintf("%.2f", overlap))
    )
  
  ggplot(pianka_df, aes(species1, species2)) +
    geom_tile(aes(fill = overlap_plot), color = "white") +
    geom_tile(
      data = subset(pianka_df, is_diag),
      fill = "grey80",
      color = "white"
    ) +
    geom_text(aes(label = label),
              color = "white", size = 3.5) +
    scale_fill_viridis(limits = c(0, 1), na.value = "grey80") +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(
        angle = 45, hjust = 1,
        face = ifelse(levels(pianka_df$species1) %in% c("BBTO", "BWVI"),
                      "bold", "plain")
      ),
      axis.text.y = element_text(
        face = ifelse(levels(pianka_df$species2) %in% c("BBTO", "BWVI"),
                      "bold", "plain")
      ),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(fill = "Overlap", title = title)
}


heat_behavior <- plot_pianka_heatmap(
  pianka_behavior$pianka_matrix,
  "Pianka Overlap — Foraging Technique"
)

heat_location <- plot_pianka_heatmap(
  pianka_location$pianka_matrix,
  "Pianka Overlap — Foraging Substrate"
)

heat_height <- plot_pianka_heatmap(
  pianka_height$pianka_matrix,
  "Pianka Overlap — Height Bin"
)

heat_canopy <- plot_pianka_heatmap(
  pianka_canopy$pianka_matrix,
  "Pianka Overlap — Canopy Height Bin"
)

ggsave(filename = "plots/FigureS3.png", plot = heat_behavior, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS5.png", plot = heat_location, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS7.png", plot = heat_height, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS9.png", plot = heat_canopy, width = 8, height = 6, dpi = 300)

#____________________________________________________________________________
#                       PLOT MINIMUM ACROSS 4 VARIABLES
#___________________________________________________________________________

# element-wise minimum
pianka_min_matrix <- pmin(
  pianka_behavior$pianka_matrix,
  pianka_location$pianka_matrix,
  pianka_height$pianka_matrix,
  pianka_canopy$pianka_matrix,
  na.rm = TRUE
)

heat_min <- plot_pianka_heatmap(
  pianka_min_matrix,
  " "
)

ggsave(
  filename = "plots/Figure4.png",
  plot = heat_min,
  width = 8,
  height = 6,
  dpi = 300
)

#____________________________________________________________________________
# Null model (RA3) + bootstrap

compute_pianka_matrix_chr <- function(df, species_col, niche_col_chr) {
  
  df_clean <- df %>%
    filter(
      !is.na(.data[[niche_col_chr]]),
      .data[[niche_col_chr]] != "",
      toupper(as.character(.data[[niche_col_chr]])) != "NONE"
    )
  
  freq_table <- df_clean %>%
    count({{ species_col }}, .data[[niche_col_chr]]) %>%
    pivot_wider(
      names_from  = .data[[niche_col_chr]],
      values_from = n,
      values_fill = 0
    )
  
  freq_table <- freq_table %>%
    filter(rowSums(across(-{{ species_col }})) > 0)
  
  prop_matrix <- freq_table %>%
    column_to_rownames(var = as_label(enquo(species_col))) %>%
    as.matrix() %>%
    prop.table(1)
  
  spp <- rownames(prop_matrix)
  n   <- length(spp)
  
  pianka_mat <- matrix(NA, n, n, dimnames = list(spp, spp))
  
  for (i in 1:n) {
    for (j in 1:n) {
      pianka_mat[i, j] <- pianka_index(prop_matrix[i, ], prop_matrix[j, ])
    }
  }
  
  pianka_mat
}


randomize_resources_RA2 <- function(df, niche_col) {
  df %>%
    mutate(rand_resource = sample({{ niche_col }}))
}

bootstrap_pianka <- function(df, species_col, niche_col, n_iter = 1000) {
  
  obs <- compute_pianka_matrix_chr(
    df,
    species_col = {{ species_col }},
    niche_col_chr = as_label(enquo(niche_col))
  )
  
  S <- nrow(obs)
  null_mats <- array(NA, dim = c(S, S, n_iter))
  
  for (i in 1:n_iter) {
    df_rand <- randomize_resources_RA2(df, {{ niche_col }})
    null_mats[,,i] <- compute_pianka_matrix_chr(
      df_rand,
      species_col = {{ species_col }},
      niche_col_chr = "rand_resource"
    )
  }
  
  pvals <- ses <- matrix(NA, S, S, dimnames = dimnames(obs))
  
  for (i in 1:S) {
    for (j in 1:S) {
      null_vals <- null_mats[i, j, ]
      pvals[i, j] <- mean(null_vals >= obs[i, j])
      ses[i, j] <- ifelse(
        sd(null_vals) == 0,
        NA,
        (obs[i, j] - mean(null_vals)) / sd(null_vals)
      )
    }
  }
  
  list(
    observed = obs,
    null     = null_mats,
    pvals    = pvals,
    ses      = ses
  )
}

#____________________________________________________________________________
# Bootstrap results

boot_behavior <- bootstrap_pianka(
  wd,
  species_col = species,
  niche_col   = foraging_behavior,
  n_iter = 1000
)

boot_location <- bootstrap_pianka(
  wd,
  species_col = species,
  niche_col   = foraging_location,
  n_iter = 1000
)

boot_height <- bootstrap_pianka(
  wd,
  species_col = species,
  niche_col   = height_bin,
  n_iter = 1000
)

boot_canopy <- bootstrap_pianka(
  wd,
  species_col = species,
  niche_col   = canopy_bin,
  n_iter = 1000
)

#____________________________________________________________________________
# SES HEATMAPS
#____________________________________________________________________________
# SES HEATMAPS (CLIPPED AT ±2)
plot_ses_heatmap <- function(ses_matrix, title) {
  
  species_order <- c(
    "BBTO", "BWVI", "OVEN", "PAWA", "YRWA",
    "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"
  )
  
  bold_species <- c("BBTO", "BWVI")
  
  ses_df <- as.data.frame(ses_matrix) %>%
    rownames_to_column("species1") %>%
    pivot_longer(
      -species1,
      names_to  = "species2",
      values_to = "SES"
    ) %>%
    mutate(
      species1 = factor(species1, levels = species_order),
      species2 = factor(species2, levels = species_order),
      SES_clipped = pmax(pmin(SES, 2), -2)
    )
  
  ggplot(ses_df, aes(x = species1, y = species2, fill = SES_clipped)) +
    geom_tile(color = "white") +
    geom_text(
      aes(
        label = ifelse(
          is.na(SES),
          "NA",
          sprintf("%.1f", SES)
        )
      ),
      color = "black",
      size = 3
    ) +
    scale_fill_gradient2(
      low       = "#2166AC",
      mid       = "white",
      high      = "#B2182B",
      midpoint  = 0,
      limits    = c(-2, 2),
      oob       = scales::squish,
      na.value  = "grey85"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        face  = ifelse(levels(ses_df$species1) %in% bold_species,
                       "bold", "plain")
      ),
      axis.text.y = element_text(
        face = ifelse(levels(ses_df$species2) %in% bold_species,
                      "bold", "plain")
      ),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(
      fill  = "SES",
      title = title
    )
}



#____________________________________________________________________________
# PLOT SES MATRICES

ses_behavior <- plot_ses_heatmap(
  boot_behavior$ses,
  "Standardized Effect Size — Foraging Technique"
)

ses_location <- plot_ses_heatmap(
  boot_location$ses,
  "Standardized Effect Size — Foraging Substrate"
)

ses_height <- plot_ses_heatmap(
  boot_height$ses,
  "Standardized Effect Size — Proportional Height Bin"
)

ses_canopy <- plot_ses_heatmap(
  boot_canopy$ses,
  "Standardized Effect Size — Canopy Height Bin"
)

# Display
ses_behavior #ggsave as figure S4
ses_location #ggsave as figure S6
ses_height #ggsave as figure S8
ses_canopy #ggsave as figure S10

ggsave(filename = "plots/FigureS4.png", plot = ses_behavior, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS6.png", plot = ses_location, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS8.png", plot = ses_height, width = 8, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS10.png", plot = ses_canopy, width = 8, height = 6, dpi = 300)











