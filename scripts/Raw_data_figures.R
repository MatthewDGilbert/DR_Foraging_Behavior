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
library(ggtext)
library(ggplot2)
library(tidyr)

# Load data frame from folder
Stacked = read.csv("Sorted_Foraging_Technique.csv", header=F)
Combined_Location = read.csv("Sorted_Foraging_Substrate.csv", header=F)
unsorted_data = read.csv("Unsorted_Full_Dataset.csv", header=T)


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
figure2 <- ggplot(sorted_data, aes(fill = Behavior, y = Proportion, x = Species)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = behavior_colors) +  # Custom colors for behaviors
  labs(fill = "Foraging Behavior", x = "Species", y = "Proportion") + 
  theme(text = element_text(size = 20, color = "black")) +
  theme(axis.text.y = element_text(size = 15, color = "black")) +
  theme(axis.text.x = element_text(size = 13, color = "black")) +
  theme(axis.text.x = element_markdown(size = 15, color = "black", 
                                       face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain"))) + 
  theme(axis.title.y = element_text(margin = margin(t = -50)))

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

# Define color-blind-friendly palette
foraging_colors <- c(
  "Ground" = "#8B4513",         # Saddle brown for ground substrate
  "Dead Material" = "#D55E00",  # Rich orange-brown for dead material
  "Trunk" = "darkblue",          # Golden orange for trunk-level foraging
  "Branch" = "#0072B2",         # Deep blue for larger branches
  "Twig" = "#56B4E9",           # Light blue for smaller branches
  "Leaf" = "#009E73",           # Teal green for foliage
  "Flower/Berry" = "#CC79A7",   # Magenta for flowers and berries
  "Other" = "#999999")           # Neutral gray for unspecified substrate)

# Create the plot
figure4 <- ggplot(sorted_data, aes(fill = Foraging_Location, y = Proportion, x = Species)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = foraging_colors) +  # Apply manual color palette
  labs(fill = "Foraging Substrate", 
       #title = "Foraging Substrate Type by Species", 
       x = "Species", 
       y = "Proportion") + 
  #theme_minimal() +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 15, color = "black"),
    axis.text.x = element_markdown(size = 15, color = "black", 
                                   face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain")),
    axis.title.y = element_text(margin = margin(t = -50))
  )
figure4
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
    upper_y = mean_y + qt(0.975, df = n() - 1) * se_y
  )

# Plot with color-coded error bars and dots for the means
figure1 <- ggplot(species_summary, aes(x = mean_x, y = mean_y, color = Species)) +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y), width = 0.1, size = 1.2) +  # Vertical error bars
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x), height = 0.01, size = 1.2) +  # Horizontal error bars
  geom_point(size = 3, shape = 19) +  # Add dots for means
  labs(x = "\n Canopy Height (m)", y = "Proportional Height within Canopy \n") +  # Removed title
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 18),  # Larger font size for axis titles
    axis.text = element_text(size = 16),   # Larger font size for axis tick labels
    legend.text = element_text(size = 14), # Larger font size for legend text
    legend.title = element_text(size = 16)) # Larger font size for legend title

figure1

ggsave(filename = "Figure1.png", plot = figure1, width = 8, height = 6, dpi = 1000, units = "in")

#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________
#______________________________________________________________________________