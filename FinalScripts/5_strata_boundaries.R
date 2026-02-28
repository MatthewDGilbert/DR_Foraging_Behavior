
#
#                  R SCRIPT FOR CALCULATING STRATA DELINEATIONS FOR 
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Kimberly Navarro Velez, Feb 5 2026
# Last edited by Matthew Gilbert, Feb 10 2026
#                                
#______________________________________________________________________________


# load libraries
library(dplyr)
library(ggpattern)
library(ggtext)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(cowplot)
library(classInt)



# Load data frame from folder
data <- read.csv("0_Full_Dataset.csv", header = T)
data <- data[is.na(data$Canopy.height) | data$Canopy.height <= 20, ]

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

#______________________________________________________________________________
#
#                     TESTING IDEAL NUMBER OF BINS (proportional_height)


gvf_jenks <- function(x, brks) {
  x <- x[is.finite(x)]
  # Total sum of squared deviations from mean
  sdam <- sum((x - mean(x))^2)
  
  # Class assignment
  g <- cut(x, breaks = brks, include.lowest = TRUE, right = TRUE)
  
  # Sum of squared deviations within classes
  sdcM <- sum(tapply(x, g, function(v) sum((v - mean(v))^2)), na.rm = TRUE)
  
  # Goodness of Variance Fit
  (sdam - sdcM) / sdam
}


x <- df_reduced$prop_canopy_height
x <- x[is.finite(x)] # avoids NA issues

for (k in 2:6) {
  ci <- classIntervals(x, n = k, style = "jenks")
  gvf <- gvf_jenks(x, ci$brks)
  
  cat(
    "Bins:", k,
    "| GVF:", round(gvf, 3),
    "| Breaks:", paste(round(ci$brks, 2), collapse = ", "),
    "\n"
  )
}

# GVF had greatest gains between 3 and 4 bins, and seemed to taper off afterwards, so going with 4. 

#______________________________________________________________________________
#
#             CALCULATE DATA-DRIVEN NATURAL BREAKS (Proportional_height)

breaks <- classIntervals(
  df_reduced$prop_canopy_height,
  n = 4,
  style = "jenks"
)

breaks$brks 

#then apply these bins to the vble
labs <- c("Bin1", "Bin2", "Bin3", "Bin4")

df_reduced <- df_reduced %>% 
  mutate(
    bird_height_nat_breaks = cut(
      prop_canopy_height,
      breaks = breaks$brks,
      include.lowest = TRUE,
      labels = labs
    )
  )

#______________________________________________________________________________
#
#                     TESTING IDEAL NUMBER OF BINS (canopy heights)


gvf_jenks <- function(x, brks) {
  x <- x[is.finite(x)]
  # Total sum of squared deviations from mean
  sdam <- sum((x - mean(x))^2)
  
  # Class assignment
  g <- cut(x, breaks = brks, include.lowest = TRUE, right = TRUE)
  
  # Sum of squared deviations within classes
  sdcM <- sum(tapply(x, g, function(v) sum((v - mean(v))^2)), na.rm = TRUE)
  
  # Goodness of Variance Fit
  (sdam - sdcM) / sdam
}

x <- df_reduced$canopy_height
x <- x[is.finite(x)] # avoids NA issues

for (k in 2:6) {
  ci <- classIntervals(x, n = k, style = "jenks")
  gvf <- gvf_jenks(x, ci$brks)
  
  cat(
    "Bins:", k,
    "| GVF:", round(gvf, 3),
    "| Breaks:", paste(round(ci$brks, 2), collapse = ", "),
    "\n"
  )
}

# GVF had stopped having strong gains after 5 so going with 5. 

#______________________________________________________________________________
#
#             CALCULATE DATA-DRIVEN NATURAL BREAKS (canopy heights)

library(classInt)

breaks <- classIntervals(
  df_reduced$canopy_height,
  n = 5,
  style = "jenks"
)

breaks$brks 

#then apply these bins to the vble
labs <- c("Bin1", "Bin2", "Bin3", "Bin4", "Bin 5")

df_reduced <- df_reduced %>% 
  mutate(
    bird_height_nat_breaks = cut(
      canopy_height,
      breaks = breaks$brks,
      include.lowest = TRUE,
      labels = labs
    )
  )

