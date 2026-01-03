#
#                   R SCRIPT FOR CREATING DIFFERENTIATION MATRICES 
#                    FOR OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Nov 12 2024
# Last edited by Matthew Gilbert, Dec 13 2025
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


data <- read.csv("0_Full_Dataset.csv", header = T)
set.seed(444)

#____________________________________________________________________________

#              Process data (including for 1 point per individual)

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

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

get_random <- function(x) {
  sample(na.omit(x), 1)
}

df_individual <- df_reduced %>%
  select(-row) %>%
  group_by(individual_id) %>%
  summarise(
    species = first(species), 
    bird_height = mean(bird_height, na.rm = TRUE),
    canopy_height = mean(canopy_height, na.rm = TRUE),
    prop_canopy_height = mean(prop_canopy_height, na.rm = TRUE),
    foraging_location = get_random(foraging_location),
    foraging_technique = get_random(foraging_technique),
    substrate_type = get_random(substrate_type),
    .groups = "drop"
  )



#______________________________________________________________________________

#                        FISHERS EXACT TEST for TECHNIQUE

# Ensure correct data is used
cleaned_data <- df_individual[!is.na(df_individual$foraging_technique) & !is.null(df_individual$foraging_technique), ]

# Get unique species names
species <- unique(cleaned_data$species)

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- cleaned_data[cleaned_data$species %in% c(species[i], species[j]), ]
    
    # Create a contingency table
    contingency_table <- table(subset_data$foraging_technique, subset_data$species)

    # Perform Fisher's Exact Test with simulated p-values
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      test_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
      # Store the p-value in the matrix
      pairwise_pvals[species[i], species[j]] <- test_result$p.value
      pairwise_pvals[species[j], species[i]] <- test_result$p.value # Mirror the result
    }
  }
}

# Flatten the upper triangle of the matrix into a data frame
pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# Adjust p-values for multiple comparisons (Benjamini-Hochberg FDR)
pairwise_df$Adjusted_P <- pairwise_df$P_Value * 1

# Create a table of adjusted p-values rounded to 3 decimal places
adjusted_pval_table <- pairwise_df[, c("Species1", "Species2", "Adjusted_P")]
adjusted_pval_table$Adjusted_P <- round(adjusted_pval_table$Adjusted_P, 3)

# Print the table
print(adjusted_pval_table)

# Reorder species to place BBTO and BWVI first
species_order <- c("BBTO", "BWVI", setdiff(unique(adjusted_pval_table$Species1), c("BBTO", "BWVI")))
adjusted_pval_table$Species1 <- factor(adjusted_pval_table$Species1, levels = species_order)
adjusted_pval_table$Species2 <- factor(adjusted_pval_table$Species2, levels = species_order)

# Add a significance level category
adjusted_pval_table$Significance <- cut(
  adjusted_pval_table$Adjusted_P,
  breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
  labels = c("p < 0.01", "p < 0.05", "p < 0.1", "Less Significant"))


figure2B <- ggplot(adjusted_pval_table, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Less Significant" = "gray80",
      "p < 0.1" = "gray65",
      "p < 0.05" = "#5DADE2",
      "p < 0.01" = "royalblue3"),
    name = "Significance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16, color = "black", face = ifelse(levels(adjusted_pval_table$Species1) %in% c("BBTO", "BWVI"), "bold", "plain")),  # Larger x-axis labels
    axis.text.y = element_text(size = 16, color = "black", face = ifelse(levels(adjusted_pval_table$Species1) %in% c("BBTO", "BWVI"), "bold", "plain")),                        # Larger y-axis labels
    #plot.title = element_text(size = 18, color = "black"),          # Larger title
    axis.title.x = element_text(size = 18, color = "black"),                      # Larger x-axis title
    axis.title.y = element_text(size = 18, color = "black"),                      # Larger y-axis title
    legend.title = element_text(size = 20, color = "black"),                       # Larger legend title
    legend.text = element_text(size = 15, color = "black"))+                        # Larger legend text
  labs(
    #title = "Adjusted Pairwise Fisher's Test for Foraging Technique",
    x = "\nSpecies",
    y = "Species\n")
figure2B <- ggdraw(figure2B) + draw_plot_label(label = "B", x = 0.005, y = 0.98, size = 22, fontface = "bold")  # adjust x, y as needed
figure2B

ggsave(filename = "plots/Figure2B.png", plot = figure2B, width = 10, height = 6, dpi = 1000, units = "in")


#______________________________________________________________________________
#               PERMANOVA TECHNIQUE

# Step 1: Create one-hot encoded matrix of foraging technique per individual
technique_matrix <- df_individual %>%
  filter(!is.na(foraging_technique), foraging_technique != "") %>%
  mutate(dummy = 1) %>%
  pivot_wider(
    id_cols = individual_id,
    names_from = foraging_technique,
    values_from = dummy,
    values_fill = list(dummy = 0)
  )

# Step 2: Add species info and prepare for adonis
adonis_input <- technique_matrix %>%
  left_join(df_individual %>% select(individual_id, species), by = "individual_id") %>%
  column_to_rownames("individual_id")

# Step 3: Run PERMANOVA on binary technique presence across species
adonis_result <- adonis2(
  as.matrix(adonis_input[, !names(adonis_input) %in% "species"]) ~ species,
  data = adonis_input,
  permutations = 999,
  method = "bray"
)

print(adonis_result)

#______________________________________________________________________________

#                        FISHERS EXACT TEST for SUBSTRATE

# Define substrate mapping and desired orderings
substrate_map <- c(
  "G" = "Ground", "D" = "Dead Material", "T" = "Trunk",
  "S" = "Branch", "B" = "Branch",
  "F" = "Twig", "OF" = "Twig", "UF" = "Twig",
  "L" = "Leaf", "OL" = "Leaf", "UL" = "Leaf",
  "Y" = "Flower/Berry", "W" = "Flower/Berry",
  "SW" = "Other"
)

species_order <- c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", 
                   "AMRE", "PRAW", "BAWW", "NOPA", "CMWA")

substrate_order <- c("Ground", "Dead Material", "Trunk", 
                     "Branch", "Twig", "Leaf", 
                     "Flower/Berry", "Other")

# Clean and prepare data from df_individual
substrate_counts <- df_individual %>%
  filter(foraging_location %in% names(substrate_map)) %>%
  mutate(
    Foraging_substrate = substrate_map[foraging_location]
  ) %>%
  count(Species = species, Foraging_substrate) %>%
  complete(Species = species_order,
           Foraging_substrate = substrate_order,
           fill = list(n = 0)) %>%
  group_by(Species) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(
    Species = factor(Species, levels = species_order),
    Foraging_substrate = factor(Foraging_substrate, levels = substrate_order)
  )

# Filter out BBTO and BWVI species and remove missing data
filtered_data <- substrate_counts %>%
  filter(!Species %in% c("BBTO", "BWVI")) %>%
  drop_na(Species, Foraging_substrate, n) 

# Get unique species names
species <- as.character(unique(filtered_data$Species))

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- filtered_data %>%
      filter(Species %in% c(species[i], species[j])) %>%
      select(Species, Foraging_substrate, n)
    
    # Create a contingency table
    contingency_table <- xtabs(n ~ Foraging_substrate + Species, data = subset_data)
    
    # Perform Fisher's Exact Test with simulated p-values
    if (nrow(contingency_table) > 1 && ncol(contingency_table) > 1) {
      test_result <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
      # Store the p-value in the matrix
      pairwise_pvals[species[i], species[j]] <- test_result$p.value
      pairwise_pvals[species[j], species[i]] <- test_result$p.value # Mirror the result
    }
  }
}

pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# DO NOT Adjust p-values using Bonferroni correction
# Adjusted p-values = p-values because ANOVA showed strong global significance
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pairwise_df$P_Value * 1

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
  labels = c("p < 0.01", "p < 0.05", "p < 0.1", "Less Significant"))

# Plot the heatmap of adjusted p-values
figure3B <- ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Less Significant" = "gray80",
      "p < 0.1" = "gray65",
      "p < 0.05" = "#5DADE2",
      "p < 0.01" = "royalblue3"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),                        # Larger y-axis labels
    #plot.title = element_text(size = 18, color = "black"),          # Larger title
    axis.title.x = element_text(size = 18, color = "black"),                      # Larger x-axis title
    axis.title.y = element_text(size = 18, color = "black"),                      # Larger y-axis title
    legend.title = element_text(size = 20, color = "black"),                       # Larger legend title
    legend.text = element_text(size = 15, color = "black"))+                        # Larger legend text
  labs(
    #title = "Adjusted Pairwise Fisher's Test for Foraging Technique",
    x = "\nSpecies",
    y = "Species\n")

figure3B <- ggdraw(figure3B) + draw_plot_label(label = "B", x = 0.005, y = 0.98, size = 22, fontface = "bold")  # adjust x, y as needed
figure3B

ggsave(filename = "plots/Figure3B.png", plot = figure3B, width = 10, height = 5.3, dpi = 1000, units = "in")

#______________________________________________________________________________
#               PERMANOVA SUBSTRATE

# Step 1: Create one-hot encoded matrix of foraging technique per individual
substrate_matrix <- df_individual %>%
  filter(!is.na(foraging_location), foraging_location != "") %>%
  mutate(dummy = 1) %>%
  pivot_wider(
    id_cols = individual_id,
    names_from = foraging_location,
    values_from = dummy,
    values_fill = list(dummy = 0)
  )

# Step 2: Add species info and prepare for adonis
adonis_input <- substrate_matrix %>%
  left_join(df_individual %>% select(individual_id, species), by = "individual_id") %>%
  column_to_rownames("individual_id")

# Step 3: Run PERMANOVA on binary technique presence across species
adonis_result <- adonis2(
  as.matrix(adonis_input[, !names(adonis_input) %in% "species"]) ~ species,
  data = adonis_input,
  permutations = 999,
  method = "bray"
)

print(adonis_result)



#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________