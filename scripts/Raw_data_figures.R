#
#                   R SCRIPT FOR CREATING STACKED BAR PLOTS FOR 
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Feb 15 2023
# Last edited by Matthew Gilbert, Apr 1 2025
#                                
#______________________________________________________________________________

#                                   SETUP

# Set Working directory to folder containing data and R scripts
setwd("~/Desktop/dominican_republic")

# load libraries
library(dplyr)
library(ggpattern)
library(ggtext)
library(ggplot2)
library(tidyr)

# Load data frame from folder
Stacked = read.csv("Sorted_Foraging_Technique.csv", header=F)
Combined_Location = read.csv("Sorted_Foraging_Substrate.csv", header=F)
unsorted_data = read.csv("Unsorted_Reduced_Dataset.csv", header=T)


#______________________________________________________________________________

#                          FORAGING BEHAVIOR BAR PLOT

# create a dataset
Species <- c(rep("AMRE" , 8) , rep("BAWW" , 8) , rep("BBTO" , 8) , rep("BWVI" , 8) , rep("CMWA" , 8) , rep("NOPA" , 8) , rep("OVEN" , 8) , rep("PAWA" , 8) , rep("PRAW" , 8) , rep("YRWA" , 8) )
Behavior <- rep(c("Consuming Plant" , "Drinking Nectar" , "Flycatching", "Gleaning", "Hover-Gleaning", "Poking Ground", "Pounching", "Snatching") , 10)
Proportion <- abs(Stacked$V3)
data <- data.frame(Species,Behavior,Proportion)

# sort data for standardized
sorted_data <- data

# Restructure the species order: BBTO and BWVI first, followed by the others
sorted_data$Species <- factor(sorted_data$Species,
                              c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"))

behavior_patterns <- c(
  "Consuming Plant" = "stripe",
  "Drinking Nectar" = "crosshatch",
  "Flycatching" = "circle",
  "Gleaning" = "none",
  "Hover-Gleaning" = "stripe",
  "Poking Ground" = "crosshatch",
  "Pounching" = "circle",
  "Snatching" = "none")

behavior_colors <- c(
  "Consuming Plant" = "#D55E00",        
  "Drinking Nectar" = "gold",        
  "Flycatching" = "#0072B2",           
  "Gleaning" = "#009E73",              
  "Hover-Gleaning" = "#56B4E9",      
  "Poking Ground" = "#C47F17",         
  "Pounching" = "#CC79A7",             
  "Snatching" = "#999999")

# Plot with manually set colors
figure2 <- ggplot(sorted_data, aes(x = Species, y = Proportion, fill = Behavior, pattern = Behavior)) +
  geom_bar_pattern(stat = "identity", position = "fill", pattern_density = 0.15, pattern_fill = "black", pattern_spacing = 0.02, pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = behavior_colors) +
  scale_pattern_manual(values = behavior_patterns) +
  labs(fill = "Foraging Behavior", pattern = "Foraging Behavior", x = "Species", y = "Proportion") +
  theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_markdown(size = 15, color = "black",
                                   face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain")),
    axis.title.y = element_text(margin = margin(t = -50)))
figure2
ggsave(filename = "Figure2.png", plot = figure2, width = 11, height = 7, dpi = 1000, units = "in")


#______________________________________________________________________________

#                     FORAGING SUBSTRATE BAR PLOT 

# throughout the code, this is referred to as Foraging Location

# Define substrate mapping and desired factor levels. Group similar substrate types
substrate_map <- c(
  "G" = "Ground", "D" = "Dead Material", "T" = "Trunk",
  "S" = "Branch", "B" = "Branch",
  "F" = "Twig", "OF" = "Twig", "UF" = "Twig",
  "L" = "Leaf", "OL" = "Leaf", "UL" = "Leaf",
  "Y" = "Flower/Berry", "W" = "Flower/Berry",
  "SW" = "Other")
species_order <- c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", 
                   "AMRE", "PRAW", "BAWW", "NOPA", "CMWA")
location_order <- c("Ground", "Dead Material", "Trunk", 
                    "Branch", "Twig", "Leaf", 
                    "Flower/Berry", "Other")

# Aggregate and align data
sorted_data <- Combined_Location %>%
  rename(Species = V1, Code = V2, Value = V3) %>%
  mutate(Category = substrate_map[Code]) %>%
  group_by(Species, Category) %>%
  summarise(Proportion = sum(Value), .groups = "drop") %>%
  complete(Species = species_order, Category = location_order, fill = list(Proportion = 0)) %>%
  mutate(
    Species = factor(Species, levels = species_order),
    Foraging_Location = factor(Category, levels = location_order)
  ) %>%
  select(Species, Foraging_Location, Proportion)

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
  "Ground" = "stripe",
  "Dead Material" = "crosshatch",
  "Trunk" = "circle",
  "Branch" = "none",
  "Twig" = "stripe",
  "Leaf" = "crosshatch",
  "Flower/Berry" = "circle",
  "Other" = "none")

substrate_patterns <- c(
  "Ground" = "none",
  "Dead Material" = "crosshatch",
  "Trunk" = "circle",
  "Branch" = "stripe",
  "Twig" = "circle",
  "Leaf" = "none",
  "Flower/Berry" = "stripe",
  "Other" = "crosshatch")

# Create the plot
figure4 <- ggplot(sorted_data,aes(fill = Foraging_Location,y = Proportion,x = Species,pattern = Foraging_Location)) + 
  geom_bar_pattern(stat = "identity",position = "fill",pattern_density = 0.15,pattern_fill = "black",pattern_spacing = 0.02,pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = foraging_colors) +
  scale_pattern_manual(values = substrate_patterns) +
  theme_minimal() +
  labs(fill = "Foraging Substrate",pattern = "Foraging Substrate",x = "Species",y = "Proportion") +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_markdown(
      size = 15,
      color = "black",
      face = ifelse(
        levels(sorted_data$Species) %in% c("BBTO", "BWVI"),
        "bold", "plain")),
    axis.title.y = element_text(margin = margin(t = -50)))

figure4

# Save the plot
ggsave(filename = "Figure4.png", plot = figure4, width = 11, height = 7, dpi = 1000, units = "in")



#______________________________________________________________________________

#                   DOT AND WHISKER PLOT FOR BIRD/CANOPY HEIGHT

data_filtered <- unsorted_data

# Reorder species so BBTO and BWVI are first
data_filtered <- data_filtered %>%
  mutate(Species = factor(Species, levels = c("BBTO", "BWVI", setdiff(unique(Species), c("BBTO", "BWVI")))))

# Compute means and confidence intervals for each species
species_summary <- data_filtered %>%
  group_by(Species) %>%
  summarize(
    mean_x = mean(Canopy.height),
    mean_y = mean(Height.in.Canopy),
    se_x = sd(Canopy.height) / sqrt(n()),
    se_y = sd(Height.in.Canopy) / sqrt(n()),
    lower_x = mean_x - qt(0.975, df = n() - 1) * se_x,
    upper_x = mean_x + qt(0.975, df = n() - 1) * se_x,
    lower_y = mean_y - qt(0.975, df = n() - 1) * se_y,
    upper_y = mean_y + qt(0.975, df = n() - 1) * se_y)

shape_values <- c(
  "BBTO" = 21, "BWVI" = 22, "PAWA" = 23, "OVEN" = 25, "YRWA" = 24,
  "AMRE" = 2, "PRAW" = 1, "BAWW" = 5, "NOPA" = 0, "CMWA" = 6)

# Plot with color-coded error bars and dots for the means
figure1 <- ggplot(species_summary, aes(x = mean_x, y = mean_y, color = Species, shape=Species, fill = Species)) +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y), width = 0.1, size = 1.2) +  # Vertical error bars
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x), height = 0.01, size = 1.2) +  # Horizontal error bars
  geom_point(size = 4, stroke = 1) +  # Dots with shape and border
  labs(x = "\n Canopy Height (m)", y = "Proportional Height within Canopy \n") +
  theme_minimal() +
  scale_shape_manual(values = shape_values)+
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16))

figure1

ggsave(filename = "Figure1.png", plot = figure1, width = 8, height = 6, dpi = 1000, units = "in")

#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________