## Analysis of flock influence in variables.

# 0. data
library(tidyverse)
library(readr)
library(janitor)

raw_data <- read_csv("data/raw_data.csv")
# NOTE: This has to be replaced with the working file.
# NOTE 2: The working file should not have NA's, this will affect the calculations.

# working data

wd <- raw_data %>% 
  clean_names() %>%
  select(species, bird_height, foraging_location, foraging_behavior,flock, con_hetero, canopy_height) %>% 
  #filter(species %in% c("AMRE", "XXX")) %>%  ## Select the species we have kept.
  arrange(species)

head(wd)


# species that flock

species_that_flock <- wd %>% 
  filter(flock == "Y") %>% 
  distinct(species) %>% 
  pull(species)

# clean dataset
 df <- wd %>% 
   filter(species %in% species_that_flock) %>% 
   filter(!is.na(flock)) %>%           
   mutate(flock = factor(flock))

 
 # FOR BEHAVIOR:
 # function to test behavior diff within species
 test_behavior_by_species <- function(df) {
   
   df %>% 
     filter(!is.na(foraging_behavior)) %>%
     group_by(species) %>%
     group_modify(~{
       tbl <- table(.x$flock, .x$foraging_behavior)
       
       # Decide between chi-square or Fisher test
       if(any(chisq.test(tbl)$expected < 5)) {
         test <- fisher.test(tbl)
         method <- "Fisher Exact Test"
       } else {
         test <- chisq.test(tbl)
         method <- "Chi-square Test"
       }
       
       tibble(
         species = unique(.x$species),
         method = method,
         statistic = unname(test$statistic),
         p_value = test$p.value
       )
     })
 }
 

 behavior_results <- test_behavior_by_species(df)
 behavior_results
 
 
 # FOR SUBSTRATE
 
 test_substrate_by_species <- function(df) {
   
   df %>% 
     filter(!is.na(foraging_location)) %>%
     group_by(species) %>%
     group_modify(~{
       
       tbl <- table(.x$flock, .x$foraging_location)
       
       # If only one flock state present → cannot test
       if(nrow(tbl) < 2) {
         return(tibble(
           species = unique(.x$species),
           method = "Only one flock category present",
           statistic = NA,
           p_value = NA
         ))
       }
       
       # If chi-square assumptions are met
       if(!any(chisq.test(tbl)$expected < 5)) {
         test <- chisq.test(tbl)
         method <- "Chi-square Test"
       } else {
         # Run Fisher test using Monte Carlo simulation
         test <- fisher.test(tbl, simulate.p.value = TRUE, B = 5000)
         method <- "Fisher Exact Test (Simulated)"
       }
       
       tibble(
         species = unique(.x$species),
         method = method,
         statistic = unname(test$statistic),
         p_value = test$p.value
       )
     })
 }
 
 
 substrate_results <- test_substrate_by_species(df)
 substrate_results
 

# foraging height
 
 test_height_by_species <- function(df) {
   
   df %>%
     filter(!is.na(bird_height)) %>%
     group_by(species) %>%
     group_modify(~{
       
       # Need at least 2 groups present
       if(length(unique(.x$flock)) < 2) {
         return(tibble(
           species = unique(.x$species),
           method = "Not enough data (only 1 flock group)",
           statistic = NA,
           p_value = NA
         ))
       }
       
       test <- suppressWarnings(
         wilcox.test(bird_height ~ flock, data = .x)
       )
       
       tibble(
         species = unique(.x$species),
         method = "Wilcoxon rank-sum test",
         statistic = unname(test$statistic),
         p_value = test$p.value
       )
     })
 }
 
 
 height_results <- test_height_by_species(df)
 height_results
 

# results in one table
 
 summary_stats <- behavior_results %>%
   rename(p_behavior = p_value) %>%
   left_join(substrate_results %>% rename(p_substrate = p_value),
             by = "species") %>%
   left_join(height_results %>% rename(p_height = p_value),
             by = "species")
 
 summary_stats
 
 












