---
title: "Data Cleaning and Standards Assignment"
output: html_document
date: "2025-09-03"
autor: "Sandra Jaskowiak - with Andie Siemens and Katie Chettle"
---

### Decription ------------------------------------------------------------------

##The Problem:
#We want to perform some statistical analyses on bromeliad data which we recently obtained. Before we can get to our statistical analysis, we must first ensure that the data is input and formatted correctly to ensure our analysis is accurate. This is called the "quality control" step. In this initial data cleaning stage, we must check for spelling errors, outliers, or improbable values in the data set. Spelling, punctuation, and formatting errors are common in character columns that we can see in the species and habitat names. Numeric columns may contain values that have been measured or input incorrectly causing impossible values such as negatives and outliers. Some variables within this data set, particularly numeric variables, are correlated due to biological context and this must be considered during quality control. 


##The Solution:
# To complete this quality control problem and move on to data analysis stages, we must address possible errors one by one. In Part 1 of the code below, we first identify spelling errors in the character columns "species" and "habitat". We provide a list of correctly spelled names that will be matched and replace the incorrect original entries. In Part 2, we identify negative values, outliers, and multivariate outliers in the numeric columns max_water, longest_leaf, and total_detritus. Once these errors are identified and flagged, they are converted to "NA" values so they do not skew results of future statistical analysis.Throughout the coding process, checks are implemented before and after cleaning steps to confirm that the problems were resolved without changing correct data.

#### Set Up -------------------------------------------------------------------

#Install Packages that may be needed
install.packages("tidyverse")
install.packages("palmerpenguins")
install.packages("assertr")
install.packages("lubridate")
install.packages("stringdist")
install.packages("GGally")
install.packages("sf")
install.packages("taxize")

library(readr)
library(tidyverse)
library(palmerpenguins)
library(assertr)
library(lubridate)
library(stringdist)
library(GGally)
library(sf)
library(taxize)

#Set working directory

setwd("~/Simon Fraser University/Classes/Scientific Data Management for Ecology and Evolution/CIEE_Scientific-Data-Management")

#Import data files

bromeliads_messy <- read_csv("BWG_database_messy/bwgv1_bromeliads_messy.csv")

#Explore the data
dim(bromeliads_messy) #76 rows and 18 columns
head(bromeliads_messy, 10) # view first 10 rows of data
str(bromeliads_messy) # check the structure
summary(bromeliads_messy) # summarize the dataset

### Part 1. Clean Character Columns: species and habitat ------------------------------

# Check the number of species and habitat entries and look at list of unique entries to identify possible spelling mistakes
sum(!is.na(bromeliads_messy$species))#76 entries
table(bromeliads_messy$species) #spelling mistakes identified

sum(!is.na(bromeliads_messy$habitat)) #18 habitat entries
table(bromeliads_messy$habitat) #spelling mistakes identified

#List unique names for species and habitat with correct spelling
species.list <- c("Guzmania_sp",
                  "Vriesea_gladioliflora",
                  "Vriesea_kupperiana",
                  "Vriesea_sanguinolenta",
                  "Vriesea_sp")

habitat.list <- c("pasture", "secondary")

#create a new dataframe that will contain the cleaned data version to avoid altering original data. Make two new columns (species_clean and habitat_clean) that will contain corrected entries populated by amatch() based on closest match to names in the species and habitat lists.

bromeliads_clean <- bromeliads_messy %>%
  mutate(                                                   
    species_clean = species.list[amatch(species,        
                                        species.list,   
                                        maxDist = 5)],  #max 5 edits to make the match
    habitat_clean = habitat.list[amatch(habitat,        
                                        habitat.list,
                                        maxDist = 5)]  
  )

#Verify that number of entries in the new columns didn't change, and that all unique entries are spelled correctly
sum(!is.na(bromeliads_clean$species_clean)) #still 76 entries
table(bromeliads_clean$species_clean) #no spelling mistakes identified

sum(!is.na(bromeliads_clean$habitat_clean)) #still 18 entries
table(bromeliads_clean$habitat_clean) #no spelling mistakes identified 


## Move new "clean" columns beside where the original columns are located in the dataframe.To avoid redundancy, delete original species and habitat columns.
bromeliads_clean <- bromeliads_clean %>%
  relocate(species_clean, .after = species) %>%
  relocate(habitat_clean, .after = habitat) %>%
  select(-c(species, habitat))

#Verify that the dataframe's structure and properties are the same as before cleaning
head(bromeliads_clean) # Looks good 
dim(bromeliads_messy) # Looks good (still 76 rows and 18 columns)
str(bromeliads_messy) # Looks good
summary(bromeliads_messy) # Looks good


### Part 2. Clean Numeric Columns: max_water, longest_leaf, total_detritus -----------------


##Determine the number of observations in max_water
bromeliads_clean %>%
  summarise(
    n_non_na = sum(!is.na(max_water)), 
    n_na     = sum(is.na(max_water))   
  ) #Total of 76 observations (71 non-NA values and 5 NA values)


##Create histogram to visually see the data in max_water, longest_leaf, and total_detritus. Look for clear outliers and impossible values. 
hist(bromeliads_clean$max_water, nclass = 30) 
  #clear outlier >6000 and some possible outliers >1000
hist(bromeliads_clean$longest_leaf, nclass = 30) 
  #an impossible negative vlaue is present
hist(bromeliads_clean$total_detritus, nclass = 30) 
  #clear outlier >800 and an impossible negative value`


## Use assert() and insist() to identify outliers and impossible values in max_water, longest_leaf, total_detritus
bromeliads_clean %>%
  chain_start %>%
  verify(nrow(.) == 76) %>% 
  # Are there 76 rows as expected?
  assert(within_bounds(0, Inf), 
         c(max_water, longest_leaf, total_detritus)) %>% 
  # are all column values positive? 
  insist(within_n_mads(2), c(max_water, longest_leaf, total_detritus))  %>% 
  # are there outliers outside of 2 median absolute deviations(MADS)? 
  insist_rows(maha_dist, within_n_mads(4), c(max_water, longest_leaf, total_detritus)) %>% 
  # are there multivariate outliers?
  chain_end %>% 
  group_by(species) %>%
  #if no errors, group by species
  summarise(mean_max_water = mean(max_water, na.rm = TRUE)) 
  #if no errors, calculate mean max_water per species

#There are 37 errors across 6 verbs: 2 impossible negative values,30 outliers outside of 2 MADs, and 5 values with large maha distances




