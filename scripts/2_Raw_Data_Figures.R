#
#                   R SCRIPT FOR CREATING STACKED BAR PLOTS FOR 
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Feb 15 2023
# Last edited by Matthew Gilbert, Dec 13 2025
#                                
#______________________________________________________________________________

#                                   SETUP

# Set working directory to source folder

# load libraries
library(dplyr)
library(ggpattern)
library(ggtext)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(cowplot)


# Load data frame from folder
data <- read.csv("0_Full_Dataset.csv", header = T)

#______________________________________________________________________________

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

df_individual <- df_reduced %>%
  select(-row) %>%
  group_by(individual_id) %>%
  summarise(
    species = first(species), 
    bird_height = mean(bird_height, na.rm = TRUE),
    canopy_height = mean(canopy_height, na.rm = TRUE),
    prop_canopy_height = mean(prop_canopy_height, na.rm = TRUE),
    foraging_location = get_mode(foraging_location),
    foraging_technique = get_mode(foraging_technique),
    substrate_type = get_mode(substrate_type),
    .groups = "drop"
  )

BWVI <- subset(df_individual, df_individual$species=="AMRE")
mean(BWVI$prop_canopy_height, na.rm = T)



#______________________________________________________________________________

#                          FORAGING BEHAVIOR BAR PLOT

behavior_map <- c(
  C = "Consuming Plant",
  D = "Drinking Nectar",
  F = "Flycatching",
  G = "Gleaning",
  H = "Hover-Gleaning",
  K = "Poking Ground",
  P = "Pouncing",
  S = "Snatching"
)
# Full behavior names (in order)
behavior_levels <- names(behavior_map)

# Unique species list
species_list <- unique(df_reduced$species)

# Create full grid (Species × short Behavior codes)
full_grid <- expand.grid(Species = species_list,
                         Behavior_code = behavior_levels,
                         stringsAsFactors = FALSE)

# Count species × behavior from df_reduced
behavior_counts <- df_reduced %>%
  filter(foraging_technique %in% behavior_levels) %>%
  count(Species = species, Behavior_code = foraging_technique)

# Join, replace NAs, and calculate proportions
final_table <- full_grid %>%
  left_join(behavior_counts, by = c("Species", "Behavior_code")) %>%
  mutate(n = replace_na(n, 0)) %>%
  group_by(Species) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup() %>%
  mutate(Behavior = behavior_map[Behavior_code]) %>%
  select(Species, Behavior, n, Proportion) %>%
  arrange(Species, Behavior)

# sort data for standardized
sorted_data <- final_table

# Restructure the species order: BBTO and BWVI first, followed by the others
sorted_data$Species <- factor(sorted_data$Species,
                              c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"))

behavior_patterns <- c(
  "Consuming Plant" = "stripe",
  "Drinking Nectar" = "none",
  "Flycatching" = "circle",
  "Gleaning" = "none",
  "Hover-Gleaning" = "stripe",
  "Poking Ground" = "crosshatch",
  "Pouncing" = "circle",
  "Snatching" = "none")

behavior_colors <- c(
  "Consuming Plant" = "#D55E00",        
  "Drinking Nectar" = "gold",        
  "Flycatching" = "#0072B2",           
  "Gleaning" = "#009E73",              
  "Hover-Gleaning" = "#56B4E9",      
  "Poking Ground" = "#C47F17",         
  "Pouncing" = "#CC79A7",             
  "Snatching" = "#999999")

# Plot with manually set colors
figure2A <- ggplot(sorted_data, aes(x = Species, y = Proportion, fill = Behavior, pattern = Behavior)) +
  geom_bar_pattern(stat = "identity", position = "fill", pattern_density = 0.15, pattern_fill = "black", pattern_spacing = 0.02, pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = behavior_colors) +
  scale_pattern_manual(values = behavior_patterns) +
  labs(fill = "Foraging Behavior", pattern = "Foraging Behavior", x = "Species", y = "Proportion of Total Foraging Attempts\n") +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 20, color = "black"),
    legend.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16, color = "black",
                                   face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain")))
figure2A <- ggdraw(figure2A) + draw_plot_label(label = "A", x = 0.005, y = 0.98, size = 22, fontface = "bold")  # adjust x, y as needed
figure2A

ggsave(filename = "plots/Figure2A.png", plot = figure2A, width = 10, height = 6, dpi = 1000, units = "in")


#______________________________________________________________________________

#                     FORAGING SUBSTRATE BAR PLOT 

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

# Clean and prepare data from df_reduced
substrate_counts <- df_reduced %>%
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

# Define color and pattern mappings
foraging_colors <- c(
  "Ground" = "#8B4513",
  "Dead Material" = "#D55E00",
  "Trunk" = "royalblue",
  "Branch" = "#56B4E9",
  "Twig" = "#9FD3E9",
  "Leaf" = "#009E73",
  "Flower/Berry" = "#CC79A7",
  "Other" = "#999999"
)

substrate_patterns <- c(
  "Ground" = "none",
  "Dead Material" = "crosshatch",
  "Trunk" = "circle",
  "Branch" = "stripe",
  "Twig" = "circle",
  "Leaf" = "none",
  "Flower/Berry" = "stripe",
  "Other" = "crosshatch"
)

# Create the plot
figure3A <- ggplot(substrate_counts, aes(
  fill = Foraging_substrate,
  y = Proportion,
  x = Species,
  pattern = Foraging_substrate
)) + 
  geom_bar_pattern(
    stat = "identity",
    position = "fill",
    pattern_density = 0.15,
    pattern_fill = "black",
    pattern_spacing = 0.02,
    pattern_key_scale_factor = 0.6
  ) +
  scale_fill_manual(values = foraging_colors) +
  scale_pattern_manual(values = substrate_patterns) +
  theme_minimal() +
  labs(
    fill = "Foraging Substrate",
    pattern = "Foraging Substrate",
    x = "Species",
    y = "Proportion of Total Foraging Attempts\n"
  ) +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 20, color = "black"),
    legend.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(
      angle = 30,
      hjust = 1,
      size = 16,
      color = "black",
      face = ifelse(levels(substrate_counts$Species) %in% c("BBTO", "BWVI"), "bold", "plain")
    )
  )

# Add subplot label "A"
figure3A <- ggdraw(figure3A) +
  draw_plot_label(label = "A", x = 0.005, y = 0.98, size = 22, fontface = "bold")

# Display the plot
figure3A

# Save the plot
ggsave(
  filename = "plots/Figure3A.png",
  plot = figure3A,
  width = 10,
  height = 6,
  dpi = 1000,
  units = "in"
)

#______________________________________________________________________________

#                   DOT AND WHISKER PLOT FOR BIRD/CANOPY HEIGHT
library(dplyr)
library(purrr)
library(ggplot2)

# -----------------------------
# Bootstrap function
# -----------------------------
bootstrap_ci <- function(x, B = 5000) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(c(mean = NA, lower = NA, upper = NA))
  
  boot_means <- replicate(B, mean(sample(x, n, replace = TRUE)))
  
  ci <- quantile(boot_means, c(0.05, 0.95))
  
  c(
    mean  = mean(x),
    lower = unname(ci[1]),
    upper = unname(ci[2]) 
  )
}

# -----------------------------
# Data preparation
# -----------------------------

df_individual <- df_individual %>%
  mutate(
    canopy_height = as.numeric(canopy_height),
    prop_canopy_height = as.numeric(prop_canopy_height))


data_filtered <- df_individual

data_filtered <- data_filtered %>%
  mutate(Species = factor(
    species,
    levels = c("BBTO", "BWVI", setdiff(unique(species), c("BBTO", "BWVI")))
  ))

# -----------------------------
# Bootstrap summary per species
# -----------------------------
species_summary <- data_filtered %>%
  group_by(Species) %>%
  reframe(
    n_x = sum(!is.na(canopy_height)),
    n_y = sum(!is.na(prop_canopy_height)),
    
    # canopy height bootstrap
    {
      bx <- bootstrap_ci(as.numeric(canopy_height))
      tibble(
        mean_x  = bx["mean"],
        lower_x = bx["lower"],
        upper_x = bx["upper"]
      )
    },
    
    # bird height bootstrap
    {
      by <- bootstrap_ci(as.numeric(prop_canopy_height))
      tibble(
        mean_y  = by["mean"],
        lower_y = by["lower"],
        upper_y = by["upper"]
      )
    }
  )

# -----------------------------
# Label offsets
# -----------------------------
species_summary$label_dx <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, -0.5, 0.5, 0.5)
species_summary$label_dy <- c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, -0.03, 0.03, 0.03)

# -----------------------------
# Point shapes
# -----------------------------
shape_values <- c(
  "BBTO" = 21, "BWVI" = 21, "PAWA" = 21, "OVEN" = 21, "YRWA" = 21,
  "AMRE" = 21, "PRAW" = 21, "BAWW" = 21, "NOPA" = 21, "CMWA" = 21
)

# -----------------------------
# Plot
# -----------------------------
figure1 <- ggplot(
  species_summary,
  aes(x = mean_x, y = mean_y, color = Species, shape = Species, fill = Species)
) +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y),
                width = 0.1, size = 1.2, na.rm = TRUE) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x),
                 height = 0.01, size = 1.2, na.rm = TRUE) +
  geom_point(size = 4, stroke = 1, na.rm = TRUE) +
  geom_text(
    aes(x = mean_x + label_dx, y = mean_y + label_dy, label = Species),
    size = 5.5, show.legend = FALSE, na.rm = TRUE
  ) +
  labs(x = "\nCanopy Height (m)", y = "Proportional Height within Canopy\n") +
  theme_minimal() +
  coord_cartesian(xlim = c(6, 14)) +
  scale_shape_manual(values = shape_values) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

figure1

# -----------------------------
# Save figure
# -----------------------------
ggsave(
  filename = "plots/Figure1.png",
  plot = figure1,
  width = 8, height = 6, dpi = 1000, units = "in"
)

#______________________________________________________________________________

#                        BOXPLOT OF HEIGHT

species_order <- c("OVEN", "PAWA", "YRWA", "AMRE", "PRAW", 
                   "BBTO", "BAWW", "NOPA", "CMWA", "BWVI")

# Apply the order using factor()
unsorted_data <- df_reduced %>%
  mutate(
    species = factor(species, levels = species_order),
    y_jitter = prop_canopy_height + runif(n(), -0.02, 0.02),
    y_jitter = pmin(pmax(y_jitter, 0), 1)
  )

# sort it by mean height
figure_box <- ggplot(unsorted_data, aes(x = species, y = y_jitter, fill = species)) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0, size = 2, alpha = 0.5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Species", y = "Foraging Height in Canopy (Proportion)") +
  theme_minimal() +
  theme(
    text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 14, color = "black", angle = 30, hjust = 1),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.position = "none"
  )

figure_box

# Save the plot
ggsave(filename = "plots/FigureS2.png", plot = figure_box,
       width = 8, height = 6, dpi = 1000, units = "in")



#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________

