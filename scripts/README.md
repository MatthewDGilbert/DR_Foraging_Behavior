# SCRIPTS
ASSESSING VULNERABILITY OF EASTERN NORTH AMERICAN LANDBIRDS TO CLIMATE AND LAND USE CHANGE

### Raw_data_figures.R

This script generates both the stacked bar plots of the foraging substrate and technique for each species as well as the 2-D box-and-whisker plot showing the confidence intervals around the mean values for bird height and canopy height (i.e. habitat maturity)

        INPUT: 
        Sorted_Foraging_Technique.csv
        Sorted_Foraging_Substrate.csv
        Unsorted_Reduced_Dataset.csv
        
        OUTPUT: 
        Figure 1
        Figure 2
        Figure 4
        
### Pairwise_differences.R

This script computes a series of Fisher's Exact Tests between each species pairing for technique and substrate use, corrects them for multiple comparisons, and then plots them to a dissimilarity matrix showing which species have significant overlaps and which don't. 

        INPUT: 
        Unsorted_Full_Dataset.csv
        Sorted_Foraging_Substrate_Combined.csv
        
        OUTPUT: 
        Figure 3
        Figure 5

### map.R

This script takes the .kml file in the plots folder and uses boundaries defined in the rnaturalearth to map out the provinces of the Dominican Republic as well as the boundary of our study area. 

        INPUT: 
        FPC.kml
  
        OUTPUT: 
        Figure S1
        
