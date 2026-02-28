#
#                   R SCRIPT FOR CREATING DIFFERENTIATION MATRICES 
#                    FOR OBSERVATIONS USING PIANKA OVERLAP METRIC 
#                              FOR INSECTIVORE FORAGING
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Kimberly Navarro Velez, Dec 12 2025
# Last edited by Matthew Gilbert, Feb 1 2026
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
data <- data[is.na(data$Canopy.height) | data$Canopy.height <= 20, ]
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

df_reduced <- df_reduced %>%
  mutate(
    species = recode(
      species,
      "BBTO" = "TODSUB",
      "BWVI" = "VIRALT",
      "OVEN" = "SEIAUR",
      "PAWA" = "SETPAL",
      "YRWA" = "SETCOR",
      "AMRE" = "SETRUT",
      "PRAW" = "SETDIS",
      "BAWW" = "MNIVAR",
      "NOPA" = "SETAME",
      "CMWA" = "SETTIG"
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
# height binning #BREAKS CHOSEN IN SCRIPT 5
wd <- wd %>%
  mutate(
    height_bin = cut(
      bird_height,
      breaks = c(0, 0.20, 0.46, 0.71, 1),
      labels = c("Understory", "Lower Mid", "Higher Mid", "Upperstory"),
      include.lowest = TRUE
    )
  )

hist(wd$canopy_height)

# canopy binning #BREAKS CHOSEN IN SCRIPT 5
wd <- wd %>%
  mutate(
    canopy_bin = cut(
      canopy_height,
      breaks = c(0, 3, 6, 10, 13, 25),
      labels = c("0-3", "3-6", "6-10", "10-13", "13-17"),
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
  
  species_order <- c("TODSUB", "VIRALT", "SEIAUR", "SETPAL", "SETCOR", "SETRUT", "SETDIS", "MNIVAR", "SETAME", "SETTIG")
  
  
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
              color = "white", size = 5) +
    scale_fill_viridis(limits = c(0, 1), na.value = "grey80") +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(size = 20, color = "black"),
      legend.title = element_text(size = 20, color = "black"),
      legend.text = element_text(size = 15, color = "black"), 
      axis.text.x = element_text(
        angle = 30, hjust = 1, size = 16, color = "black", 
        face = ifelse(levels(pianka_df$species1) %in% c("TODSUB", "VIRALT"),
                      "bold", "plain")
      ),
      axis.text.y = element_text(size = 16, color = "black", 
        face = ifelse(levels(pianka_df$species2) %in% c("TODSUB", "VIRALT"),
                      "bold", "plain")
      ),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    labs(fill = "Pairwise Overlap \n")
}


heat_behavior <- plot_pianka_heatmap(
  pianka_behavior$pianka_matrix,
  ""
)

heat_behavior <- ggdraw(heat_behavior) +
  draw_plot_label(label = "B", x = 0.90, y = 0.98, size = 22, fontface = "bold")

heat_location <- plot_pianka_heatmap(
  pianka_location$pianka_matrix,
  ""
)
heat_location <- ggdraw(heat_location) +
  draw_plot_label(label = "B", x = 0.90, y = 0.98, size = 22, fontface = "bold")


heat_height <- plot_pianka_heatmap(
  pianka_height$pianka_matrix,
  ""
)

heat_canopy <- plot_pianka_heatmap(
  pianka_canopy$pianka_matrix,
  ""
)

ggsave(filename = "plots/Figure3B.png", plot = heat_behavior, width = 9, height = 5, dpi = 300)
ggsave(filename = "plots/Figure4B.png", plot = heat_location, width = 9, height = 5, dpi = 300)
ggsave(filename = "plots/FigureS4.png", plot = heat_height, width = 9, height = 6, dpi = 300)
ggsave(filename = "plots/FigureS5.png", plot = heat_canopy, width = 9, height = 6, dpi = 300)

#____________________________________________________________________________
#                       PLOT PRODUCT ACROSS 4 VARIABLES
#___________________________________________________________________________

# element-wise product
pianka_prod_matrix <- 
  pianka_behavior$pianka_matrix *
  pianka_location$pianka_matrix *
  pianka_height$pianka_matrix *
  pianka_canopy$pianka_matrix


heat_prod <- plot_pianka_heatmap(
  pianka_prod_matrix,
  " "
)

ggsave(
  filename = "plots/Figure1.png",
  plot = heat_prod,
  width = 10,
  height = 6,
  dpi = 300
)








