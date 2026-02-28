# SCRIPTS
FORAGING BEHAVIOR OF CO-OCCURRING NEOTROPICAL RESIDENTS AND NON-BREEDING MIGRANTS, A PILOT STUDY IN PUNTA CANA

### 1_Map.R

This script takes the .kml file in the plots folder and uses boundaries defined in the rnaturalearth to map out the provinces of the Dominican Republic as well as the boundary of our study area. 

        INPUT: 
        FPC.kml
  
        OUTPUT: 
        Figure S1

### 2_Raw_Data_Figures.R

This script generates both the stacked bar plots of the foraging substrate and technique for each species as well as the 2-D box-and-whisker plot showing the confidence intervals around the mean values for bird height and canopy height (i.e. habitat maturity). It also plots a figure showing the height stratification across species. 

        INPUT: 
        0_Full_Dataset.csv
        
        OUTPUT: 
        Figure 1
        Figure 2A
        Figure 3A
        Figure S2
        
### 3_Pairwise_Differences.R

This script computes a series of Fisher's Exact Tests between each species pairing for technique and substrate use, corrects them for multiple comparisons, and then plots them to a dissimilarity matrix showing which species have significant overlaps and which don't. 

        INPUT: 
        0_Full_Dataset.csv
        
        OUTPUT: 
        Figure 2B
        Figure 3B

### 4_Pianka_Overlap.R

This script conducts analysis of Pianka Overlap Indices between all species pairs for all 4 dimensions, as well as the minimum index for each of the species pairings across the dimensions. It also conducts analyses of standardized effect sizes using the RA2 null model of resource partitioning. 

        INPUT: 
        0_Full_Dataset.csv
  
        OUTPUT: 
        Figure 4
        Figures S3-S10


