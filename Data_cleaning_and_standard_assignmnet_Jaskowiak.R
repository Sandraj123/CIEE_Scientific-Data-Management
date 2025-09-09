---
title: "Data Cleaning and Standards Assignment"
output: html_document
date: "2025-09-03"
autor: "Sandra Jaskowiak - with Andie Siemens and Katie Chettle"
---

#-------------------------------------------------------------------------------------
### Description
#-------------------------------------------------------------------------------------

##The Problem:
#We want to perform some statistical analyses on bromeliad data which we recently obtained. Before we can get to our statistical analysis, we must first ensure that the data is input and formatted correctly to ensure our analysis is accurate. This is called the "quality control" step. In this initial data cleaning stage, we must check for spelling errors, outliers, or improbable values in the data set. Spelling, punctuation, and formatting errors are common in character columns that we can see in the species and habitat names. Numeric columns may contain values that have been measured or input incorrectly causing impossible values such as negatives and outliers. Some variables within this data set, particularly numeric variables, are correlated due to biological context and this must be considered during quality control. 

##The Solution:
# To complete this quality control problem and move on to data analysis stages, we must address possible errors one by one. In Part 1 of the code below, we first identify spelling errors in the character columns "species" and "habitat". We provide a list of correctly spelled names that will be matched and replace the incorrect original entries. In Part 2, we identify negative values, outliers, and multivariate outliers in the numeric columns max_water, longest_leaf, and total_detritus. Once these errors are identified and flagged, they are converted to "NA" values so they do not skew results of future statistical analysis.Throughout the coding process, checks are implemented before and after cleaning steps to confirm that the problems were resolved without changing correct data.


#-------------------------------------------------------------------------------------
#### Set Up 
#-------------------------------------------------------------------------------------


#Install Packages that may be needed
install.packages("tidyverse")
install.packages("assertr")
install.packages("stringdist")
install.packages("GGally")

library(tidyverse)
library(assertr)
library(stringdist)
library(GGally)

#Set working directory

setwd("~/Simon Fraser University/Classes/Scientific Data Management for Ecology and Evolution/CIEE_Scientific-Data-Management")

#Import data file

bromeliads_messy <- read_csv("BWG_database_messy/bwgv1_bromeliads_messy.csv")

#Explore the data
dim(bromeliads_messy) # 76 rows and 18 columns- Verify that the number of rows reflects number of observations in your metadata
head(bromeliads_messy, 10) # view first 10 rows of data
str(bromeliads_messy) # check the structure
summary(bromeliads_messy) # summarize the data set

#make "n_rows" the number of rows in the data that you can plug into more complex operations later. Change as needed based on the metadata.
n_rows <- 76 #change this number based on how many observations you know exist in the metadata


#-------------------------------------------------------------------------------------
### Part 1. Clean Character Columns: species and habitat 
#-------------------------------------------------------------------------------------


#Check the number of species and habitat entries and look at list of unique entries to identify possible spelling mistakes
sum(!is.na(bromeliads_messy$species))#76 entries
table(bromeliads_messy$species) #spelling/format/punctuation mistakes identified

sum(!is.na(bromeliads_messy$habitat)) #18 habitat entries
table(bromeliads_messy$habitat) #spelling/format/punctuation mistakes identified

#List all unique names for species and habitat with correct spelling
species.list <- c("Guzmania_sp",
                  "Vriesea_gladioliflora",
                  "Vriesea_kupperiana",
                  "Vriesea_sanguinolenta",
                  "Vriesea_sp")

habitat.list <- c("pasture", "secondary")

#create a new dataframe that will contain the cleaned data version to avoid altering original data. Make two new columns "species_clean" and "habitat_clean". These will contain the corrected entries populated by amatch() based on closest match to names in the species and habitat lists.

bromeliads_clean <- bromeliads_messy %>%
  mutate(                                                   
    species_clean = species.list[amatch(species,        
                                        species.list,   
                                        maxDist = 5)],  #max 5 edits to make the match
    habitat_clean = habitat.list[amatch(habitat,        
                                        habitat.list,
                                        maxDist = 5)]  #max 5 edits to make the match
  )

#Verify that number of entries in the new columns didn't change, and that all unique entries are spelled correctly
sum(!is.na(bromeliads_clean$species_clean)) #still 76 entries - looks good
table(bromeliads_clean$species_clean) #no mistakes identified - looks good

sum(!is.na(bromeliads_clean$habitat_clean)) #still 18 entries - looks good
table(bromeliads_clean$habitat_clean) #no mistakes identified - looks good


##Move new "clean" columns beside where the original columns are located in the dataframe.To avoid redundancy, delete original "species" and "habitat" columns.
bromeliads_clean <- bromeliads_clean %>%
  relocate(species_clean, .after = species) %>%
  relocate(habitat_clean, .after = habitat) %>%
  select(-c(species, habitat))

#Verify that the dataframe's structure and properties are the same as before cleaning
head(bromeliads_clean) # Looks good 
dim(bromeliads_messy) # Looks good (still 76 rows and 18 columns)
str(bromeliads_messy) # Looks good
summary(bromeliads_messy) # Looks good


#-------------------------------------------------------------------------------------
### Part 2. Clean Numeric Columns: max_water, longest_leaf, total_detritus 
#-------------------------------------------------------------------------------------

##Determine the number of observations in max_water
bromeliads_clean %>%
  summarise(
    n_non_na = sum(!is.na(max_water)), 
    n_na     = sum(is.na(max_water))   
  ) 
#Total of 76 observations (71 non-NA values and 5 NA values). This matches our data frame dimensions as previously confirmed.


##Create histogram to visually see the data in max_water, longest_leaf, and total_detritus. Look for clear outliers and impossible values. 
hist(bromeliads_clean$max_water, nclass = 30) 
  #clear outlier >6000 and some possible outliers >1000
hist(bromeliads_clean$longest_leaf, nclass = 30) 
  #an impossible negative value is present
hist(bromeliads_clean$total_detritus, nclass = 30) 
  #clear outlier >800 and an impossible negative value`


##Use assert() and insist() to identify outliers and impossible values in max_water, longest_leaf, total_detritus
bromeliads_clean %>%
  chain_start %>%
  verify(nrow(.) == n_rows) %>% #"n_rows" represents number of rows that you should have based on the metadata
  #are there 76 rows as expected?
  assert(within_bounds(0, Inf), 
         c(max_water, longest_leaf, total_detritus)) %>% 
  #are all column values positive? 
  insist(within_n_mads(2), c(max_water, longest_leaf, total_detritus))  %>% 
  #are there outliers outside of 2 median absolute deviations(MADS)? 
  insist_rows(maha_dist, within_n_mads(4), c(max_water, longest_leaf, total_detritus)) %>% 
  #are there multivariate outliers?
  chain_end %>% 
  group_by(species) %>%
  #if no errors, group by species
  summarise(mean_max_water = mean(max_water, na.rm = TRUE)) 
  #if no errors, calculate mean max_water per species

#Output: There are 37 errors across 6 verbs
  # 2 impossible negative values
  # 30 outliers outside of 2 MADs
  # 5 values with large maha distances
#NOTE: The errors above are identified based on steps that were suggested in the tutorial activity. Only the extreme and biologically impossible outliers will be removed for the purposes of this assignment. All other other identified errors are currently left in because I would not remove such values without consulting the metadata or an expert in the study.

##Explore the identified errors further. Check the upper tails of the three numeric variables: 
#Observe max_water upper tails. Use "mw" to denote "max water".          
mw_outlier <- bromeliads_clean %>%
  arrange(desc(max_water)) %>% # arrange values in descending order 
  pull(max_water) %>%# transform column it into a vector
  head(10) # display first 10 values 
mw_outlier #view values
#the largest value (6116) appears to be unrealistic compared to the next highest values all below 2000. This is a likely outlier based on this data set despite my limited knowledge of this study organism. 

#Observe longest_leaf upper tails. Use "ll" to denote "longest leaf".
ll_outlier <- bromeliads_clean %>%
  arrange(desc(longest_leaf)) %>% # arrange values in descending order
  pull(longest_leaf) %>% # transform column it into a vector
  head(10) # display the first 10 values 
ll_outlier#view values
#There are no obvious and extreme outliers in the upper tails of longest_leaf

#Observe total_detritus upper tails. Use "td" to denote "total detritus".    
td_outlier <- bromeliads_clean %>%
  arrange(desc(total_detritus)) %>% # arrange values in descending order 
  pull(total_detritus) %>% # transform column it into a vector
  head(10) # display first 10 values  
td_outlier #view values
#the largest value (1000) appears to be unrealistic compared to the next highest values all below 200. This is a likely outlier based on this data set despite my limited knowledge of this study organism. 


##Choose a user-defined maximum threshold for max water, longest leaf, and total detritus variables. Any value above of this thresholds will be considered outliers. 
#NOTE: Change this value as required. Arbitrary threshold values based on visual observation of histogram distribution are selected for the purposes of this assignment. To define real and plausible thresholds, consult expert on the data set. 

mw_maxthreshold <- 2000
ll_maxthreshold <- 100
td_maxthreshold <- 200


#Create new columns for max water, longest leaf, and total detritus variables that populate "NA" when original values are negative or fall outside of maximum threshold. When original values are within threshold range, the same original value will appear in new column.
bromeliads_clean <- bromeliads_clean %>% 
  mutate(max_water_clean = case_when(max_water > mw_maxthreshold ~ NA, 
                                     # NA if value > threshold
                                     max_water < 0 ~ NA, 
                                     # NA if value < 0 (negative)
                                     TRUE ~ max_water)) %>% 
                                     # keep all other original values 
  mutate(longest_leaf_clean = case_when(longest_leaf > ll_maxthreshold ~ NA,
                                        longest_leaf < 0 ~ NA, 
                                        TRUE ~ longest_leaf))  %>% 
  mutate(total_detritus_clean = case_when(total_detritus > td_maxthreshold ~ NA, 
                                        total_detritus < 0 ~ NA, 
                                          TRUE ~ total_detritus))

#Verify that values (within range) are the same between new and old columns.
bromeliads_clean %>%
  select(max_water, max_water_clean, longest_leaf, longest_leaf_clean, total_detritus, total_detritus_clean) %>%
  head(20) #looks good


#Check if outliers were removed
hist(bromeliads_clean$max_water_clean) #looks better - still some possible outliers that I would consider removing if I knew biological context for these values
hist(bromeliads_clean$longest_leaf_clean) #looks good
hist(bromeliads_clean$total_detritus_clean) #looks better- still some possible outliers that I would consider removing if I knew biological context for these values


#Move new "clean" columns beside where the original columns are located in the dataframe.To avoid redundancy, delete original "max_water", "total_detritus", and "longest_leaf" columns.
bromeliads_clean <- bromeliads_clean %>%
  relocate(max_water_clean, .after = max_water) %>%
  relocate(total_detritus_clean, .after = total_detritus) %>%
  relocate(longest_leaf_clean, .after = longest_leaf) %>%
  select(-c(max_water, total_detritus, longest_leaf))

# Check dataframe to ensure that original columns have been replaced with clean columns. Verify that the data frame structure and properties are the same as before cleaning
head(bromeliads_clean) #Looks good
dim(bromeliads_messy) # Looks good (still 76 rows and 18 columns)
str(bromeliads_messy) # Looks good
summary(bromeliads_messy) # Looks good


## END
#-------------------------------------------------------------------------------------
