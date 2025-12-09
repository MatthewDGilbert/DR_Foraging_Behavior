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


##################### MULTIVARIATE APPROACH ##################

#1. JOINT NICHE STATE
wd2 <- wd %>%
  mutate(joint_state = paste(foraging_behavior,
                             foraging_location,
                             height_bin,
                             sep = "_"))

#2. COMPUTE PIANKA
pianka_joint <- compute_pianka_matrix(wd2,
                                      species_col = species,
                                      niche_col = joint_state)

#3. HEAT MAP
heat_joint <- plot_pianka_heatmap(pianka_joint$pianka_matrix,
                                  title = "Multivariate Pianka — Behavior × Location × Height")

heat_joint

# SIGNIFICANCE OF MULTIVARIATE PIANKA

# FUNCTION TO RANDOMIZE RESOURCE USE 
randomize_resources_RA3 <- function(df, species_col, niche_col) {
  df %>%
    group_by({{ species_col }}) %>%
    mutate(rand_resource = sample({{ niche_col }})) %>%
    ungroup()
}

# pianks funtction for multivariate
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

# BOOTSTRAPPING PIANKA FOR EACH VARIAVLE
bootstrap_pianka <- function(df, species_col, niche_col, n_iter = 999) {
  
  obs <- compute_pianka_matrix_chr(df, 
                                   species_col = {{ species_col }},
                                   niche_col_chr = as_label(enquo(niche_col)))
  
  null_mats <- array(NA, dim = c(nrow(obs), ncol(obs), n_iter),
                     dimnames = list(rownames(obs), colnames(obs), NULL))
  
  for(i in 1:n_iter){
    df_rand <- randomize_resources_RA3(df, {{ species_col }}, {{ niche_col }})
    
    null_mats[,,i] <- compute_pianka_matrix_chr(df_rand,
                                                species_col = {{ species_col }},
                                                niche_col_chr = "rand_resource")
  }
  
  # p-values and SES
  pvals <- ses <- matrix(NA, nrow(obs), ncol(obs), dimnames = dimnames(obs))
  
  for(i in 1:nrow(obs)){
    for(j in 1:ncol(obs)){
      
      null_vals <- null_mats[i,j,]
      
      pvals[i,j] <- mean(null_vals >= obs[i,j])
      
      # Prevent division by zero
      if(sd(null_vals) == 0){
        ses[i,j] <- NA
      } else {
        ses[i,j] <- (obs[i,j] - mean(null_vals)) / sd(null_vals)
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


# RUN FUNCTION FOR EACH VARIABLE.

boot_behavior <- bootstrap_pianka(wd,
                                  species_col = species,
                                  niche_col = foraging_behavior,
                                  n_iter = 1000)

boot_behavior$observed
boot_behavior$pvals
boot_behavior$ses # Standardized effect size
# >+2 means significantly higher overlap (convergnce)
# <-2 means significantly lower overlap (niche part.)
# -2 to +2 No significant.

hist(boot_behavior$null["AMRE","COYE",], main="Null distribution")
abline(v = boot_behavior$observed["AMRE","COYE"], col="red")


