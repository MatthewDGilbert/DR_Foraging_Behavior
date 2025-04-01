# DATA
WINTER FORAGING BEHAVIOR OF NEOTROPICAL MIGRANTS AND RESIDENTS MAY REVEAL NICHE DIFFERENTIATION

### raw_data.csv
This data frame consists of our raw field data, including notes, information about flocking behavior, coordinates, time, observers, playback information, etc. Since much of this data was not used in our personal analysis, other files have more condensed data which is organized from this original file. 

### Unsorted_Full_Dataset.csv
This data frame contains all the rows from raw_data.csv, and keeps the columns which are relevant to data analysis including: Species, Individual #, Foraging Location, substrate, and technique, Canopy height, and bird height. This is used in the pairwise dissimilarity matrix for Figure 3. 

### Unsorted_Reduced_Dataset.csv
This data frame contains all the columns from Unsorted_Full_Dataset.csv, but any observation without data on canopy height or bird height was removed. This data frame was used to create Figure 1. 

### Sorted_Foraging_Technique.csv
This data frame counts the number of each foraging technique type exhibited by each species (using the information from raw_data.csv). It is used to create Figure 2, which shows the proportion of each technique type for each species. 

### Sorted_Foraging_Substrate.csv
This data frame counts the number of each foraging substrate type used by each species (using the information from raw_data.csv). It is used to create Figure 4, which shows the proportion of each technique type for each species. 

### Sorted_Foraging_Substrate_Combined.csv
This data frame counts the number of each foraging substrate type used by each species (using the information from raw_data.csv). However, foraging substrate types are grouped (i.e. Underneath-leaf and Over-leaf are joined into one group: Leaf). It is used to create Figure 5. This same grouping is used in figure 4, but is executed within the script body rather than pre-sorted like on this file. 
