#
#                R SCRIPT FOR TESTING PATTERNS BY FLOCKING AND PLAYBACK 
#                    FOR OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Kimberly Navarro-Velez, Dec 9 2025
# Last edited by Matthew Gilbert, Jan 1 2026
#                                
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________

## Analysis of flock influence in variables.

# libraries
library(tidyverse)
library(readr)
library(janitor)

# Set working directory to source file location

raw_data <- read.csv("raw_data.csv", header = T)

names(raw_data)


raw_data <- raw_data %>%
  rename(
    individual_id = ADJUSTED.NUMBER,
    species = Species,
    bird_height = Bird.Height,
    foraging_location = Foraging.Location,
    foraging_technique = Foraging.Behavior,
    substrate_type = Substrate.type,
    canopy_height = Canopy.height,
    flock = Flock., 
    flock_type = Con.hetero
  )

raw_data$prop_canopy_height = raw_data$bird_height / raw_data$canopy_height

wd <- raw_data %>%
  select(
    individual_id,
    species,
    bird_height,
    canopy_height,
    prop_canopy_height,
    foraging_location,
    foraging_technique,
    substrate_type,
    flock,
    flock_type
  )


wd <- wd %>%
  filter(species %in% c(
    "BBTO", "BWVI", "OVEN", "PAWA", "YRWA",
    "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"
  ))

wd <- wd %>%
  filter(
    !is.na(foraging_technique),
    !is.na(foraging_location),
    !is.na(canopy_height)
  )

head(wd)

# species that flock
species_that_flock <- wd %>% 
  filter(flock == "Y") %>% 
  distinct(species) %>% 
  pull(species)

# clean dataset
df <- wd %>% 
  filter(species %in% species_that_flock) %>% 
  filter(!is.na(flock)) %>%           
  mutate(flock = factor(flock))


# FOR BEHAVIOR:
# function to test behavior diff within species
test_behavior_by_species <- function(df, fisher_B = 1e5) {
  
  df %>%
    filter(!is.na(foraging_technique), !is.na(flock)) %>%
    group_by(species) %>%
    group_map(
      ~{
        sp  <- .y$species
        tbl <- table(.x$flock, .x$foraging_technique)
        
        # Degenerate table
        if (nrow(tbl) < 2 || ncol(tbl) < 2) {
          return(tibble(
            species   = sp,
            method    = "Not tested (degenerate table)",
            statistic = NA_real_,
            p_value   = NA_real_
          ))
        }
        
        chi <- suppressWarnings(chisq.test(tbl))
        
        if (any(chi$expected < 5)) {
          test <- fisher.test(
            tbl,
            simulate.p.value = TRUE,
            B = fisher_B
          )
          
          tibble(
            species   = sp,
            method    = "Fisher Exact Test (simulated)",
            statistic = NA_real_,   # Fisher has no statistic
            p_value   = test$p.value
          )
          
        } else {
          tibble(
            species   = sp,
            method    = "Chi-square Test",
            statistic = unname(chi$statistic),
            p_value   = chi$p.value
          )
        }
      }
    ) %>%
    bind_rows()
}


behavior_results <- test_behavior_by_species(df)
behavior_results


# FOR SUBSTRATE

test_substrate_by_species <- function(df) {
  
  df %>% 
    filter(!is.na(foraging_location)) %>%
    group_by(species) %>%
    group_modify(~{
      
      tbl <- table(.x$flock, .x$foraging_location)
      
      # If only one flock state present → cannot test
      if(nrow(tbl) < 2) {
        return(tibble(
          species = unique(.x$species),
          method = "Only one flock category present",
          statistic = NA,
          p_value = NA
        ))
      }
      
      # If chi-square assumptions are met
      if(!any(chisq.test(tbl)$expected < 5)) {
        test <- chisq.test(tbl)
        method <- "Chi-square Test"
      } else {
        # Run Fisher test using Monte Carlo simulation
        test <- fisher.test(tbl, simulate.p.value = TRUE, B = 5000)
        method <- "Fisher Exact Test (Simulated)"
      }
      
      tibble(
        species = unique(.x$species),
        method = method,
        statistic = unname(test$statistic),
        p_value = test$p.value
      )
    })
}


substrate_results <- test_substrate_by_species(df)
substrate_results


# foraging height

test_height_by_species <- function(df) {
  
  df %>%
    filter(!is.na(bird_height)) %>%
    group_by(species) %>%
    group_modify(~{
      
      # Need at least 2 groups present
      if(length(unique(.x$flock)) < 2) {
        return(tibble(
          species = unique(.x$species),
          method = "Not enough data (only 1 flock group)",
          statistic = NA,
          p_value = NA
        ))
      }
      
      test <- suppressWarnings(
        wilcox.test(bird_height ~ flock, data = .x)
      )
      
      tibble(
        species = unique(.x$species),
        method = "Wilcoxon rank-sum test",
        statistic = unname(test$statistic),
        p_value = test$p.value
      )
    })
}


height_results <- test_height_by_species(df)
height_results


# results in one table

summary_stats <- behavior_results %>%
  rename(p_behavior = p_value) %>%
  left_join(substrate_results %>% rename(p_substrate = p_value),
            by = "species") %>%
  left_join(height_results %>% rename(p_height = p_value),
            by = "species")

summary_stats



#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________

# 0. data
library(tidyverse)
library(readr)
library(janitor)

raw_data <- read_csv("data/raw_data.csv")
# NOTE: This has to be replaced with the working file.
# NOTE 2: The working file should not have NA's, this will affect the calculations.

# working data

wd <- raw_data %>% 
  clean_names() %>%
  select(species, bird_height, foraging_location, foraging_behavior,flock, con_hetero, canopy_height) %>% 
  #filter(species %in% c("AMRE", "XXX")) %>%  ## Select the species we have kept.
  arrange(species)

head(wd)  


# Optional: bin heights (you can change bins)
wd <- wd %>%
  mutate(height_bin = cut(bird_height,
                          breaks = c(0, 5, 10, 15),
                          labels = c("0–5", "6–10", "11–15"), include.lowest = TRUE))

# cHECK RESULTS:
table(wd$height_bin, useNA = "ifany")


#1.pianka function
pianka_index <- function(p1, p2) {
  sum(p1 * p2) / sqrt(sum(p1^2) * sum(p2^2))
}

#2. apply function to each variable
compute_pianka_matrix <- function(df, species_col, niche_col) {
  
  freq_table <- df %>%
    count({{ species_col }}, {{ niche_col }}) %>%
    pivot_wider(names_from = {{ niche_col }}, 
                values_from = n,
                values_fill = 0)
  
  # Convert to proportional matrix
  prop_matrix <- freq_table %>%
    column_to_rownames(var = as_label(enquo(species_col))) %>%
    as.matrix() %>%
    prop.table(1)
  
  # Prepare output matrix
  species_names <- rownames(prop_matrix)
  n <- length(species_names)
  
  pianka_mat <- matrix(NA, n, n, dimnames = list(species_names, species_names))
  
  # Compute pairwise Pianka overlaps
  for(i in 1:n){
    for(j in 1:n){
      pianka_mat[i, j] <- pianka_index(prop_matrix[i,], prop_matrix[j,])
    }
  }
  
  return(list(freq_table = freq_table,
              prop_matrix = prop_matrix,
              pianka_matrix = pianka_mat))
}

# 3. RESULTS
# pianka - behavior/technique
pianka_behavior <- compute_pianka_matrix(wd,
                                         species_col = species,
                                         niche_col = foraging_behavior)

pianka_behavior$freq_table        # raw frequencies
pianka_behavior$prop_matrix       # proportional use
pianka_behavior$pianka_matrix     # Pianka overlap

#Pianka - Substrate
pianka_substrate <- compute_pianka_matrix(wd,
                                          species_col = species,
                                          niche_col = foraging_location)

pianka_substrate$pianka_matrix
pianka_substrate$prop_matrix       # proportional use
pianka_substrate$pianka_matrix     # Pianka overlap


#Pianka - Height
pianka_height <- compute_pianka_matrix(wd,
                                       species_col = species,
                                       niche_col = height_bin)

pianka_height$pianka_matrix
pianka_height$prop_matrix       # proportional use
pianka_height$pianka_matrix 


#####NOTE: Interpretation OF pIANKA values:

#  1.0 = perfect overlap (same species)

# 0.8–1.0 = high overlap

# 0.4–0.6 = moderate overlap

# 0–0.3 = low overlap (differentiated niches)


#4. PLOTS - heatmaps

library(ggplot2)
library(viridis) #Colorblind friendly palette but feel free to change it.

plot_pianka_heatmap <- function(pianka_matrix, title = "Pianka Overlap") {
  
  pianka_df <- as.data.frame(pianka_matrix) %>%
    rownames_to_column("species1") %>%
    pivot_longer(-species1, names_to = "species2", values_to = "overlap")
  
  ggplot(pianka_df, aes(x = species1, y = species2, fill = overlap)) +
    geom_tile(color = "white") +
    scale_fill_viridis(option = "C", limits = c(0,1), na.value = "grey90") +
    geom_text(aes(label = sprintf("%.2f", overlap)), color = "white", size = 3.5) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank(),
          panel.grid = element_blank()) +
    labs(fill = "Pianka", title = title)
}


#plot behaviot
heat_behavior <- plot_pianka_heatmap(pianka_behavior$pianka_matrix,
                                     title = "Pianka Overlap — Foraging Behavior")

heat_behavior

# plot foraging substrate
heat_substrate <- plot_pianka_heatmap(pianka_substrate$pianka_matrix,
                                      title = "Pianka Overlap — Foraging Substrate")

heat_substrate

# plot height
heat_height <- plot_pianka_heatmap(pianka_height$pianka_matrix,
                                   title = "Pianka Overlap — Height Bins")

heat_height


# EXPORT PLOTS
# NOTE: use ggsave to obtain consistent plots ready for publication. You can also change the resolution if JCO requires a higher one.

ggsave("heat_behavior_300dpi.png",
       plot = heat_behavior,
       dpi = 300,
       width = 8, height = 8, units = "in")


# Pianka's Significance

compute_pianka_matrix_chr <- function(df, species_col, niche_col_chr) {
  
  freq_table <- df %>%
    count({{ species_col }}, .data[[niche_col_chr]]) %>%
    pivot_wider(names_from = {{ niche_col_chr }},
                values_from = n,
                values_fill = 0)
  
  prop_matrix <- freq_table %>%
    column_to_rownames(var = as_label(enquo(species_col))) %>%
    as.matrix() %>%
    prop.table(1)
  
  n <- nrow(prop_matrix)
  species_names <- rownames(prop_matrix)
  pianka_mat <- matrix(NA, n, n, dimnames = list(species_names, species_names))
  
  for (i in 1:n){
    for (j in 1:n){
      pianka_mat[i, j] <- pianka_index(prop_matrix[i,], prop_matrix[j,])
    }
  }
  
  return(pianka_mat)
}


randomize_resources_RA3 <- function(df, species_col, niche_col) {
  df %>%
    group_by({{ species_col }}) %>%
    mutate(rand_resource = sample({{ niche_col }})) %>%
    ungroup()
}

bootstrap_pianka <- function(df, species_col, niche_col, n_iter = 1000) {
  
  # 1. Observed matrix
  obs <- compute_pianka_matrix_chr(
    df,
    species_col = {{ species_col }},
    niche_col_chr = as_label(enquo(niche_col))
  )
  
  S <- nrow(obs)
  null_mats <- array(NA, dim = c(S, S, n_iter),
                     dimnames = list(rownames(obs), colnames(obs), NULL))
  
  # 2. Generate null matrices
  for(i in 1:n_iter){
    df_rand <- randomize_resources_RA3(df, {{ species_col }}, {{ niche_col }})
    
    null_mats[,,i] <- compute_pianka_matrix_chr(
      df_rand,
      species_col = {{ species_col }},
      niche_col_chr = "rand_resource"
    )
  }
  
  # 3. Initialize outputs
  pvals <- matrix(NA, S, S, dimnames = dimnames(obs))
  ses   <- matrix(NA, S, S, dimnames = dimnames(obs))
  
  # 4. Compare observed vs null
  for(i in 1:S){
    for(j in 1:S){
      null_vals <- null_mats[i,j,]
      
      pvals[i,j] <- mean(null_vals >= obs[i,j])
      
      sd_null <- sd(null_vals)
      if(sd_null == 0){
        ses[i,j] <- NA
      } else {
        ses[i,j] <- (obs[i,j] - mean(null_vals)) / sd_null
      }
    }
  }
  
  return(list(
    observed = obs,
    null = null_mats,
    pvals = pvals,
    ses = ses
  ))
}


# bootstrap behavior
boot_behavior <- bootstrap_pianka(wd,
                                  species_col = species,
                                  niche_col = foraging_behavior,
                                  n_iter = 1000)
#bootstrap substrate
boot_substrate <- bootstrap_pianka(wd,
                                   species_col = species,
                                   niche_col = foraging_location,
                                   n_iter = 1000)
# height bins
boot_height <- bootstrap_pianka(wd,
                                species_col = species,
                                niche_col = height_bin,
                                n_iter = 1000)
#Observed Pianka
boot_behavior$observed
boot_substrate$observed
boot_height$observed

#p-values (proportion of null ≥ observed)
boot_behavior$pvals
boot_substrate$pvals
boot_height$pvals

#Standardized Effect Sizes (SES)
boot_behavior$ses
boot_substrate$ses
boot_height$ses

# Standardized effect size interp.
# >+2 means significantly higher overlap (convergnce)
# <-2 means significantly lower overlap (niche part.)
# -2 to +2 No significant.















