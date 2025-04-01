#
#                   R SCRIPT FOR CREATING DIFFERENTIATION MATRICES 
#                    FOR OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                    BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Nov 12 2024
# Last edited by Matthew Gilbert, Apr 1 2025
#                                
#______________________________________________________________________________

Mar_3_data <- read.csv("Unsorted_Full_Dataset.csv", header = T, na.strings = " ")
Combined_Location = read.csv("Sorted_Foraging_Substrate_Combined.csv", header=F)

#______________________________________________________________________________

#                        FISHERS EXACT TEST for TECHNIQUE

# Ensure Mar_3_data is used
cleaned_data <- Mar_3_data[!is.na(Mar_3_data$Foraging.Behavior) & !is.null(Mar_3_data$Foraging.Behavior), ]

# Get unique species names
species <- unique(cleaned_data$Species)

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

# Adjust p-values for multiple comparisons (Benjamini-Hochberg FDR)
pairwise_df$Adjusted_P <- p.adjust(pairwise_df$P_Value, method = "fdr")
pairwise_df$Adjusted_P <- pairwise_df$P_Value * nrow(pairwise_df)

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
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("P < 0.001", "P < 0.01", "P < 0.05", "Not Significant")
)

# Plot the data
figure3 <- ggplot(adjusted_pval_table, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "P < 0.05" = "#5DADE2",
      "P < 0.01" = "royalblue3",
      "P < 0.001" = "darkblue"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16, color = "black"),  # Larger x-axis labels
    axis.text.y = element_text(size = 16, color = "black"),                        # Larger y-axis labels
    #plot.title = element_text(size = 18, color = "black"),          # Larger title
    axis.title.x = element_text(size = 18, color = "black"),                      # Larger x-axis title
    axis.title.y = element_text(size = 18, color = "black"),                      # Larger y-axis title
    legend.title = element_text(size = 20, color = "black"),                       # Larger legend title
    legend.text = element_text(size = 15, color = "black")                        # Larger legend text
  ) +
  labs(
    #title = "Adjusted Pairwise Fisher's Test for Foraging Technique",
    x = "\nSpecies",
    y = "Species\n"
  )
figure3
ggsave(filename = "Figure3.png", plot = figure3, width = 9, height = 6, dpi = 1000, units = "in")



#______________________________________________________________________________

#                        FISHERS EXACT TEST for SUBSTRATE

# Filter out BBTO and BWVI species and remove missing data
filtered_data <- Combined_Location %>%
  filter(!V1 %in% c("BBTO", "BWVI")) %>%
  drop_na(V1, V2, V3)  # Ensure no missing values in Species (V1), Foraging_Type (V2), or Count (V3)

# Get unique species names
species <- unique(filtered_data$V1)
#species <- c("AMRE", "NOPA", "BAWW")

# Create an empty matrix to store raw p-values
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

# Flatten the upper triangle of the matrix into a data frame
library(reshape2)
pairwise_df <- melt(pairwise_pvals, varnames = c("Species1", "Species2"), value.name = "P_Value")
pairwise_df <- pairwise_df[!is.na(pairwise_df$P_Value), ]

# Adjust p-values using Bonferroni correction (multiply by number of comparisons)
n_comparisons <- nrow(pairwise_df)
pairwise_df$Adjusted_P <- pairwise_df$P_Value * n_comparisons

# Add a significance level category
pairwise_df$Significance <- cut(
  pairwise_df$Adjusted_P,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("P < 0.001", "P < 0.01", "P < 0.05", "Not Significant")
)

# Plot the heatmap of adjusted p-values
figure5 <- ggplot(pairwise_df, aes(Species1, Species2, fill = Significance)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c(
      "Not Significant" = "gray80",
      "P < 0.05" = "#5DADE2",
      "P < 0.01" = "royalblue3",
      "P < 0.001" = "darkblue"
    ),
    name = "Significance"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 15, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 20, ),
    legend.text = element_text(size = 15)
  ) +
  labs(
    x = "\nSpecies",
    y = "Species\n")

figure5
ggsave(filename = "Figure5.png", plot = figure5, width = 9, height = 6, dpi = 1000, units = "in")

#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________