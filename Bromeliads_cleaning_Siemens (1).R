################################################################################
############## Data Cleaning Assignment ########################################
############## Data Quality Control ############################################ 
############## Authors: Siemens, Katie Chettle, Sandra Jaskowiak ###############
################################################################################

############## Code Description ################################################ 
## This code if for quality control of the bromeliads dataframe.
## The first part of this code will clean 2 character columns of bromeliads, 
## species and habitat, using amatch() to correct spelling mistakes by finding 
## the closes match to a given list of correct species names. The second part
## of the code cleans 3 numeric columns, max_water, longest_leaf, and 
## total_detritus, using innsist() to check for impossible negative values, 
## outliers outside of 2 median absolute deviation (MAD), and multivariate 
## outliers using Mahalanobis distance
################################################################################

## Load in libraries ##
library(tidyverse)
library(stringr)
library(assertr)
library(GGally)
library(stringdist)

## Clear environment
rm(list=ls())

## Check working directory is correct ##
getwd()

## Load in data ##
#strip.white = TRUE to remove spaces
bromeliads_messy <- read.csv("BWG database messy/bwgv1_bromeliads_messy.csv", strip.white = TRUE)

## Inspect dataframe ##
head(bromeliads_messy)

############## Part 1: Character columns (species and habitat) ################# 

##Visually inspect species and habitat
sum(!is.na(bromeliads_messy$species)) #[1] 76 entries for species
table(bromeliads_messy$species) #lots of spelling mistakes

sum(!is.na(bromeliads_messy$habitat)) #[1] 18 entries for habitat
table(bromeliads_messy$habitat) #also spelling mistakes

##List correct spelling for species and habitat
species.list <- c("Guzmania_sp",
                  "Vriesea_gladioliflora",
                  "Vriesea_kupperiana",
                  "Vriesea_sanguinolenta",
                  "Vriesea_sp")

habitat.list <- c("pasture", "secondary")

##Correct spelling mistakes with amatch()
bromeliads_clean <- bromeliads_messy %>% #create a "clean" dataframe
  mutate(                                                   
    species_clean = species.list[amatch(species,        #new column for clean species
                                            species.list,   #find closest match of species in species list
                                            maxDist = 5)],  #max 5 edits to make the match
    habitat_clean = habitat.list[amatch(habitat,        #repeat for habitat
                                            habitat.list,
                                            maxDist = 5)]  
  )

##Check that the renaming worked
sum(!is.na(bromeliads_clean$species_clean)) #still 76 entries for species (didn't lose any)
table(bromeliads_clean$species_clean) #no spelling mistakes

sum(!is.na(bromeliads_clean$habitat_clean)) #still 18 entries for species (didn't lose any)
table(bromeliads_clean$habitat_clean) #no spelling mistakes

##Remove original species and habitat columns (no redundancy in dataframe)
bromeliads_clean <- bromeliads_clean %>%
  relocate(species_clean, .after = species) %>% # move clean columns to original columns position
  relocate(habitat_clean, .after = habitat) %>%
  select(-c(species, habitat)) # remove the original columns

head(bromeliads_clean) #check dataframe

############## Part 2: Numeric columns (max_water, longest_leaf, total_detritus) ####### 

##Get number of observations in max_water
bromeliads_clean %>%
  summarise(
    n_non_na = sum(!is.na(max_water)), #number of non-NA values
    n_na     = sum(is.na(max_water))   #number of NA values
  )
#Returns 71 non-NA values and 5 NA values, total of 76 observations

##Visually inspect histograms of the max_water, longest_leaf, total_detritus
hist(bromeliads_clean$max_water, nclass = 30) #can see at least 1 large outlier >6000, maybe others >1000
hist(bromeliads_clean$longest_leaf, nclass = 30) #can see at least 1 small outlier -50 (impossible value)
hist(bromeliads_clean$total_detritus, nclass = 30) #can see at least 1 large outlier 1000 and a small outlier <0 (impossible value)

## Find impossible values and outliers in max_water, longest_leaf, total_detritus using assert() and insist()
bromeliads_clean %>%
  chain_start %>%
  verify(nrow(.) == 76) %>% # checks for 76 observations (rows)
  assert(within_bounds(0, Inf), c(max_water, longest_leaf, total_detritus)) %>% # checks all values in the 3 columns are positive
  insist(within_n_mads(2), c(max_water, longest_leaf, total_detritus))  %>% # identifies values outside of 2 MADs (outliers)
  insist_rows(maha_dist, within_n_mads(4), c(max_water, longest_leaf, total_detritus)) %>% # identifies potential multivariate outliers by large maha_distance
  chain_end %>% 
  group_by(species) %>% #if no errors, group by species
  summarise(mean_max_water = mean(max_water, na.rm = TRUE)) #if no errors, calculate mean max_water per species

#Returned 37 errors:
# 2 negative values (longest-leaf = -49, and total_detritus = -55)
# 30 outliers outside 2 MADs
# 5 values with large maha distances (potential multivariate outliers)

##List index of potential outliers / errors per variable
index_LL_negative <- c(30) #impossible negative value for longest_leaf (LL)
index_TD_negative <- c(49) #impossible negative value for total_detritus (TD)

index_MW_outlier <- c(11, 17, 18, 19, 39, 41, 45, 49, 50, 57, 58, 59, 60, 64)    #max_water outlier
index_LL_outlier <- c(30)                                                        #longest_leaf outlier
index_TD_outlier <- c(3, 13, 17, 18, 22, 25, 27, 31, 39, 41, 48, 49, 50, 57, 58) #total_detritus outlier

index_MVoutlier <- c(25, 27, 30, 48, 58) #multivariate outlier (maha_distance)

##Add a column for outlier status (good, negative, outlier, MVoutlier)
bromeliads_clean <-  bromeliads_clean %>%
  mutate(outlier_status = case_when(row_number() %in% c(index_LL_negative, index_TD_negative) ~ "negative",                   #index in the negative list will be labelled "negative"
                                    row_number() %in% c(index_MW_outlier, index_LL_outlier, index_TD_outlier)  ~ "outlier",   #index in the negative list will be labelled "outlier"
                                    row_number() %in% index_MVoutlier ~ "MVoutlier",                                          #index in the negative list will be labelled "MVoutlier"
                                    TRUE ~ "good"))                                                                           #all other rows not found in the index lists will be "good"
#note that case_when works top-down, so a row that is flagged
#as both an outlier and a MVoutlier will only be labeled as an outlier

table(bromeliads_clean$outlier_status)
#    good negative  outlier 
#     54        2       20 
# No MVoutlier because all rows flagged for MVoutliers were already flagged for having outliers being outside 2 MADs
# Some rows flagged as "outlier" may have more than 1 outlier variable (ex. row 17 has an outlier max_water and total_detritus)

##Visualize outliers as variable x variable
ggpairs(
  bromeliads_clean,
  columns = c("max_water", "longest_leaf", "total_detritus"), #only plot the 3 variables
  aes(colour = outlier_status, alpha = 0.8) #colour points by outlier status
) +
  scale_colour_manual( #manually assign the colours
    values = c(
      "negative" = "red",
      "outlier"  = "blue",
      "MVoutlier" = "purple",
      "good"         = "green"   
    ),
  ) +
  theme_classic()+
  theme(legend.position= "right")

## Visualize outliers as raw points per variable
bromeliads_clean %>%
  pivot_longer(
    cols = c(max_water, longest_leaf, total_detritus),#pivot 3 variable columns into 2 columns (variable and value)
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = variable, y = value, colour = outlier_status)) + #colour points by outlier status
  geom_point(
    position = position_jitter(width = 0.15), #jitter the points so they don't overlap as much
    size = 2, alpha = 0.6
  ) +
  labs(x = "Variable", y = "Value") +
  scale_color_manual(values = c( #manually assign colouors
    "negative" = "red",
    "outlier"  = "blue",
    "MVoutlier" = "purple",
    "good"     = "green"
  )) +
  theme_classic() +
  theme(legend.position = "right")

## Turn negative and outlier values to NA (only for the specific variable, not all variables of the observation)
bromeliads_clean <-  bromeliads_clean %>%
  mutate(max_water_clean = ifelse(row_number() %in% index_MW_outlier, NA, max_water)) %>%
  mutate(longest_leaf_clean = ifelse(row_number() %in% c(index_LL_negative, index_LL_outlier), NA, longest_leaf)) %>%
  mutate(total_detritus_clean = ifelse(row_number() %in% c(index_TD_negative, index_TD_outlier), NA, total_detritus))

## Check that outliers were properly cleaned with histograms
hist(bromeliads_clean$max_water, nclass = 15) #original max_water with outliers
hist(bromeliads_clean$max_water_clean, nclass = 15) #cleaned max_water without outlier

hist(bromeliads_clean$longest_leaf, nclass = 15) #original longest_leaf
hist(bromeliads_clean$longest_leaf_clean, nclass = 15) #cleaned longest_leaf

hist(bromeliads_clean$total_detritus, nclass = 15) #original total_detritus
hist(bromeliads_clean$total_detritus_clean, nclass = 15) #cleaned total_detritus

## Remove original columns (no redundancy in dataframe)
bromeliads_clean <- bromeliads_clean %>%
  relocate(max_water_clean, .after = max_water) %>% # move clean columns to original columns position
  relocate(longest_leaf_clean, .after = longest_leaf) %>%
  relocate(total_detritus_clean, .after = total_detritus) %>%
  select(-c(max_water, longest_leaf, total_detritus)) # remove the original columns

head(bromeliads_clean) #check dataframe
##################################
