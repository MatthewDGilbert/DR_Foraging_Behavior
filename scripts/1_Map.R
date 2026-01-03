#
#                   R SCRIPT FOR CREATING A MAP OF THE STUDY AREA FOR  
#                     OBSERVATIONS OF INSECTIVORE BIRD FEEDING   
#                   BEHAVIOR IN THE DOMINICAN REPUBLIC, JAN 2023
#
# Created by Matthew Gilbert, Nov 10 2023
# Last edited by Matthew Gilbert, Jan 1 2026
#                                
#______________________________________________________________________________

# Packages
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)

# File path for the KML file
kml_file <- "FPC.kml"

# Load the KML shapefile
fpc_shape <- st_read(kml_file)

# Download high-resolution basemap for the Dominican Republic with administrative divisions
dom_rep_states <- ne_states(country = "Dominican Republic", returnclass = "sf")

# Highlight La Altagracia province
la_altagracia <- dom_rep_states[dom_rep_states$name == "La Altagracia", ]

# Coordinates for Labels and Points
annotations <- data.frame(
  label = c("La Altagracia \nProvince", "Study Area"),
  x = c(-68.5, -68.15),
  y = c(19, 18.3)
)

# Coordinates for Punta Cana (point and label)
punta_cana <- data.frame(
  label = "Punta Cana",
  x = -68.35,
  y = 18.6
)

santo_domingo <- data.frame(
  label = "Santo\nDomingo",
  x = -69.93,
  y = 18.47
)


# Create the map
map <- ggplot() +
  # Plot all provinces in the Dominican Republic
  geom_sf(data = dom_rep_states, fill = "gray95", color = "black", size = 0.5) +
  # Highlight La Altagracia province
  geom_sf(data = la_altagracia, fill = "gray85", color = "black", size = 0.7) +
  # Overlay the KML shapefile
  geom_sf(data = fpc_shape, fill = "gray45", color = "black", alpha = 0.8) +
  # Add a filled circle for Punta Cana
  geom_point(data = punta_cana, aes(x = x, y = y), color = "black", size = 3, shape = 21, fill = "black") +
  geom_point(data = santo_domingo, aes(x = x, y = y), color = "black", size = 3, shape = 21, fill = "black") +
  # Add label for Punta Cana
  geom_text(data = punta_cana, aes(x = x, y = y, label = label), nudge_y = 0.1, size = 4, fontface = "bold") +
  geom_text(data = santo_domingo, aes(x = -69.86, y = 18.25, label = label), nudge_y = 0.1, size = 4, fontface = "bold") +
  # Add line pointing to the Study Area
  geom_segment(aes(x = -68.15, y = 18.35, xend = -68.35, yend = 18.515), color = "black", size = 1) + 
# Add labels for La Altagracia and Study Area
  geom_text(data = annotations, aes(x = x, y = y, label = label), size = 4, fontface = "bold") +
  # Adjust limits to focus on La Altagracia
  coord_sf(xlim = c(-70.8, -68), ylim = c(18.0, 19.8)) +
  # Theme adjustments for black-and-white styling
  #theme_void() +
theme(
  panel.grid.major = element_line(color = "gray80", size = 0.2), # Black major grid lines
  panel.grid.minor = element_line(color = "gray80", size = 0.1), # Black minor grid lines
  panel.background = element_rect(fill = "white"), # White background
  #axis.text = element_blank(), # Remove axis text
  #axis.ticks = element_blank(), # Remove axis ticks
  axis.title = element_blank()) # Remove axis titles

# Save plot
ggsave(filename = "plots/FigureS1.png", plot = map, width = 7, height = 5, dpi = 1000, units = "in")


