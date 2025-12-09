#
#                   R SCRIPT FOR CREATING DIFFERENTIATION MATRICES 
#                    FOR OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Nov 12 2024
# Last edited by Matthew Gilbert, Apr 1 2025
# Validated by K.Navarro-Velez May 5th 2025                               
#______________________________________________________________________________

library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(cowplot)

# Set working directory to project directory.

raw_data <- read.csv("data/Unsorted_Full_Dataset.csv", header = T, na.strings = " ")
Combined_Location = read.csv("data/Sorted_Foraging_Substrate_Combined.csv", header=F)

set.seed(444)


#______________________________________________________________________________

#                        FISHERS EXACT TEST for FORAGING TECHNIQUE

# Step 1. Tidy up database ----

# Remove empty records.
cleaned_data <- raw_data[!is.na(raw_data$Foraging.Behavior) & !is.null(raw_data$Foraging.Behavior), ]

# Get unique species names
species <- unique(cleaned_data$Species)


# Step 2. Create distance matrix and pairwise comparisons ----

# Create an empty matrix to store raw p-values
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- cleaned_data[cleaned_data$Species %in% c(species[i], species[j]), ]
    
    # Create a contingency table
    contingency_table <- table(subset_data$Foraging.Behavior, subset_data$Species)

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


# Step 3: Generate p-values for multiple comparisons ----

# method = fdr (Benjamini-Hochberg)
pairwise_df$Adjusted_P <- p.adjust(pairwise_df$P_Value, method = "fdr")
pairwise_df$Adjusted_P <- pairwise_df$P_Value * nrow(pairwise_df)

# Create a table of adjusted p-values rounded to 3 decimal places
adjusted_pval_table <- pairwise_df[, c("Species1", "Species2", "Adjusted_P")]
adjusted_pval_table$Adjusted_P <- round(adjusted_pval_table$Adjusted_P, 3)

# Print the table
print(adjusted_pval_table)


# Step 4: Generate table for figures and figures ----

# Reorder species to place BBTO and BWVI first
species_order <- c("BBTO", "BWVI", setdiff(unique(adjusted_pval_table$Species1), c("BBTO", "BWVI")))
adjusted_pval_table$Species1 <- factor(adjusted_pval_table$Species1, levels = species_order)
adjusted_pval_table$Species2 <- factor(adjusted_pval_table$Species2, levels = species_order)

# Add a significance level category
adjusted_pval_table$Significance <- cut(
  adjusted_pval_table$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("p < 0.001", "p < 0.01", "p < 0.05", "Not Significant"))

figure2B <- ggplot(adjusted_pval_table, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "p < 0.05" = "#5DADE2",
      "p < 0.01" = "royalblue3",
      "p < 0.001" = "darkblue"),
    name = "Significance") +
  theme_classic() +
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

ggsave(filename = "Figure2B.jpeg", plot = figure2B, width = 10, height = 6, dpi = 1000, units = "in")





#______________________________________________________________________________

#                        FISHERS EXACT TEST for SUBSTRATE ----


# Step 1: Filter out BBTO and BWVI species and remove missing data ----
filtered_data <- Combined_Location %>%
  filter(!V1 %in% c("BBTO", "BWVI")) %>%
  drop_na(V1, V2, V3)  # Ensure no missing values in Species (V1), Foraging_Type (V2), or Count (V3)

# Get unique species names
species <- unique(filtered_data$V1)
#species <- c("AMRE", "NOPA", "BAWW")

# Step 2: Create an empty matrix to store raw p-values ----
pairwise_pvals <- matrix(NA, nrow = length(species), ncol = length(species), 
                         dimnames = list(species, species))

# Perform pairwise Fisher's Exact Tests with simulated p-values
for (i in 1:(length(species) - 1)) {
  for (j in (i + 1):length(species)) {
    # Subset data for the two species being compared
    subset_data <- filtered_data %>%
      filter(V1 %in% c(species[i], species[j])) %>%
      select(V1, V2, V3)
    
    # Create a contingency table
    contingency_table <- xtabs(V3 ~ V2 + V1, data = subset_data)
    
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

# Step 3: Adjust p-values using Bonferoni correction (multiply by number of comparisons)----
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pairwise_df$P_Value * n_comparisons

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("p < 0.001", "p < 0.01", "p < 0.05", "Not Significant"))

# Step 4: Plot the heatmap of adjusted p-values ----
figure3B <- ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "p < 0.05" = "#5DADE2",
      "p < 0.01" = "royalblue3",
      "p < 0.001" = "darkblue"
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

ggsave(filename = "Figure3B.jpeg", plot = figure3B, width = 10, height = 5.3, dpi = 1000, units = "in")

#_______________________________________________________________________________________________________
# End of script.
#_______________________________________________________________________________________________________