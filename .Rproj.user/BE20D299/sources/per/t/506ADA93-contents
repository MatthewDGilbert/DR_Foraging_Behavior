#
#                   R SCRIPT FOR CREATING STACKED BAR PLOTS FOR 
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Feb 15 2023
# Last edited by Matthew Gilbert, Apr 2 2025
# Validated by K.Navarro-Velez May 5th 2025                                
#______________________________________________________________________________


# Set working directory to project directory.

# load libraries
library(dplyr)
library(ggpattern)
library(ggtext)
library(ggplot2)
library(tidyr)
library(ggrepel)


# Load data frame from folder
Stacked = read.csv("data/Sorted_Foraging_Technique.csv", header=F)
Combined_Location = read.csv("data/Sorted_Foraging_Substrate.csv", header=F)
unsorted_data = read.csv("data/Unsorted_Reduced_Dataset.csv", header=T)


#______________________________________________________________________________

#                          FORAGING TECHNIQUE BAR PLOT ----

# Step 1: create the dataframe ---
Species <- c(rep("AMRE" , 8) , rep("BAWW" , 8) , rep("BBTO" , 8) , rep("BWVI" , 8) , rep("CMWA" , 8) , rep("NOPA" , 8) , rep("OVEN" , 8) , rep("PAWA" , 8) , rep("PRAW" , 8) , rep("YRWA" , 8) )
Technique <- rep(c("Consuming Plant" , "Drinking Nectar" , "Flycatching", "Gleaning", "Hover-Gleaning", "Poking Ground", "Pounching", "Snatching") , 10)
Proportion <- abs(Stacked$V3)
data <- data.frame(Species,Technique,Proportion)

# Step 2: Assign order, colors and patterns ---- 
sorted_data <- data

# Restructure the species order: BBTO and BWVI first, followed by the others
sorted_data$Species <- factor(sorted_data$Species,
                   c("BBTO", "BWVI", "OVEN", "PAWA", "YRWA", "AMRE", "PRAW", "BAWW", "NOPA", "CMWA"))

Technique_patterns <- c(
  "Consuming Plant" = "stripe",
  "Drinking Nectar" = "crosshatch",
  "Flycatching" = "circle",
  "Gleaning" = "none",
  "Hover-Gleaning" = "stripe",
  "Poking Ground" = "crosshatch",
  "Pounching" = "circle",
  "Snatching" = "none")

Technique_colors <- c(
  "Consuming Plant" = "#D55E00",        
  "Drinking Nectar" = "gold",        
  "Flycatching" = "#0072B2",           
  "Gleaning" = "#009E73",              
  "Hover-Gleaning" = "#56B4E9",      
  "Poking Ground" = "#C47F17",         
  "Pounching" = "#CC79A7",             
  "Snatching" = "#999999")

# Step 3: Plot ----
figure2A <- ggplot(sorted_data, aes(x = Species, y = Proportion, fill = Technique, pattern = Technique)) +
  geom_bar_pattern(stat = "identity", position = "fill", pattern_density = 0.15, pattern_fill = "black", pattern_spacing = 0.02, pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = Technique_colors) +
  scale_pattern_manual(values = Technique_patterns) +
  labs(fill = "Foraging Technique", pattern = "Foraging Technique", x = "Species", y = "Proportion\n") +
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

ggsave(filename = "Figure2A.jpeg", plot = figure2A, width = 10, height = 6, dpi = 1000, units = "in")



#______________________________________________________________________________

#                     FORAGING SUBSTRATE BAR PLOT ----

# Note: throughout the code, this is referred to as Foraging Location

# Step 1: Define substrate mapping and desired factor levels. ----
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

# Step 2: Aggregate and align data ----
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
  "Ground" = "none",
  "Dead Material" = "crosshatch",
  "Trunk" = "circle",
  "Branch" = "stripe",
  "Twig" = "circle",
  "Leaf" = "none",
  "Flower/Berry" = "stripe",
  "Other" = "crosshatch")

# Create the plot
figure3A <- ggplot(sorted_data,aes(fill = Foraging_Location,y = Proportion,x = Species,pattern = Foraging_Location)) + 
  geom_bar_pattern(stat = "identity",position = "fill",pattern_density = 0.15,pattern_fill = "black",pattern_spacing = 0.02,pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = foraging_colors) +
  scale_pattern_manual(values = substrate_patterns) +
  theme_minimal() +
  labs(fill = "Foraging Substrate",pattern = "Foraging Substrate",x = "Species",y = "Proportion\n") +
  theme(
    text = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 20, color = "black"),
    legend.text = element_text(size = 15, color = "black"), 
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16, color = "black",
                               face = ifelse(levels(sorted_data$Species) %in% c("BBTO", "BWVI"), "bold", "plain")))

figure3A <- ggdraw(figure3A) + draw_plot_label(label = "A", x = 0.005, y = 0.98, size = 22, fontface = "bold")  # adjust x, y as needed

ggsave(filename = "Figure3A.jpeg", plot = figure3A, width = 10, height = 6, dpi = 1000, units = "in")



#______________________________________________________________________________

#                   DOT AND WHISKER PLOT FOR BIRD/CANOPY HEIGHT ----

# Step 1: select and sort data
data_filtered <- unsorted_data

data_filtered <- data_filtered %>%
  mutate(Species = factor(Species, levels = c("BBTO", "BWVI", setdiff(unique(Species), c("BBTO", "BWVI")))))

# Step2: Compute means and confidence intervals for each species ----
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
  "BBTO" = 21, "BWVI" = 21, "PAWA" = 21, "OVEN" = 21, "YRWA" = 21,
  "AMRE" = 21, "PRAW" = 21, "BAWW" = 21, "NOPA" = 21, "CMWA" = 21)


species_summary$label_dx <- c(0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, -0.35, 0.35, 0.35)  # example: n = 10
species_summary$label_dy <- c(-0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, -0.03, 0.03, 0.03)


figure1 <- ggplot(species_summary, aes(x = mean_x, y = mean_y, color = Species, shape = Species, fill = Species)) +
  geom_errorbar(aes(ymin = lower_y, ymax = upper_y), width = 0.1, size = 1.2) +
  geom_errorbarh(aes(xmin = lower_x, xmax = upper_x), height = 0.01, size = 1.2) +
  geom_point(size = 4, stroke = 1) +
  geom_text(
    aes(x = mean_x + label_dx, y = mean_y + label_dy, label = Species),
    size = 5.5,
    show.legend = FALSE) +
  labs(x = "\n Canopy Height (m)", y = "Proportional Height within Canopy \n") +
  theme_minimal() +
  scale_shape_manual(values = shape_values) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16))

ggsave(filename = "Figure1.jpeg", plot = figure1, width = 8, height = 6, dpi = 1000, units = "in")



#______________________________________________________________________________

#                        BOXPLOT OF FORAGING HEIGHT ----

# Step 1: sort it by mean height
unsorted_data <- unsorted_data %>%
  mutate(Species = reorder(Species, Height.in.Canopy, FUN = mean))

#Step 2: Plot
figure_box <- ggplot(unsorted_data, aes(x = Species, y = Height.in.Canopy, fill = Species)) +
  geom_boxplot(alpha = 0.65, outlier.shape = NA) +
  geom_jitter(width = 0.15, height = 0.02, size = 2, alpha = 0.5) +  # Adjust jitter width here
  labs(x = "Species", y = "Foraging Height in Canopy (Proportion)") +
  theme_minimal() +
  ylim(0, 1) + 
  theme(
    text = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    legend.position = "none")


# Save the plot
ggsave(filename = "FigureS2.jpeg", plot = figure_box,
       width = 8, height = 6, dpi = 1000, units = "in")



#_______________________________________________________________________________________________________
# End of script.
#_______________________________________________________________________________________________________