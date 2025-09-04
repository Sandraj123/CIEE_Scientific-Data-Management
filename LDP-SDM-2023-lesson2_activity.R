# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   Living Data Project -- Data Management Module           # - # - #
# - # - #   Lesson 2: Data Manipulation and data wrangling          # - # - #
# - # - #   Introduction to some useful packages and function       # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   IN-CLASS EXERCISES -- work through in small groups      # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

## Instructions ##
## With your group, try and answer the questions below using what you've learned 
## in the tutorial. Questions 1 and 2 can be answered by adapting the code from 
## the answers to Questions 1 and 2 in the tutorial. If you are already familiar
## with these concepts, start with question 3. Note that the questions will get 
## more challenging as you go along. If you finish Qs 3-5, try and come up with 
## your own questions to ask about the data.

## NOTE ##
## In order to import the data with this script, your working directory needs to
## the folder which has this script and the BWG_database folder in it.


# Set up ------------------------------------------------------------------

## check if working directory is set to correct folder
getwd()
# if it isn't, use setwd() to change it to the correct folder

### Packages ###

## import (or install) required packages
library(tidyverse)
library(lubridate)

### Import files ###

## what files are in the BWG data folder?
myfiles <- list.files(path = "BWG_database/", pattern = "*.csv", full.names = TRUE)

# import all tables as separate data frames, remove file path and file extensions (.csv)
list2env(
  lapply(
    setNames(myfiles, 
             make.names(gsub(".*1_", "", 
                             tools::file_path_sans_ext(myfiles)))), 
         read.csv), 
  envir = .GlobalEnv)


# Questions ---------------------------------------------------------------

## 1: Create a new table that includes only bromeliads from the genus Vriesea 
## that are > 100 mL. Arrange this table by bromeliad volume (max_water variable)

## 1a. How many Vriesea bromeliads have a volume > 100 mL

## 1b. What is the mean volume of this subset of Vriesea bromeliads?

## 1c. How many individual plants do we have for each species?


## 2: Use the "extended_diameter" variable to classify bromeliads by size.
## Categories: small (< 50 cm), medium (50 - 100 cm), and large (> 100 cm)
## Create a summary table that lists how many bromeliads of each Genus 
## (Guzmania vs. Vriesea) are in each size class


## 3. What owner is associated with the most bromeliads, and who is associated 
## with the least?


## 4. What is the mean invertebrate morphospecies richness for each bromeliad species?
## HINT: first calculate the morphospecies richness (i.e., total # of species)
## inside each individual bromeliad. Then, summarize by bromeliad genus to get 
## the mean total invertebrate abundance for each species. Show this in a figure,
## if you want to.


## 5. What is the mean total invertebrate abundance for each bromeliad species?
## HINT: first calculate the total abundance of invertebrates in each bromeliad,
## then summarize by bromeliad species to get the mean total invertebrate 
## abundance for each species. Show this in a figure if you want to.


## Finished the questions above? You have a couple options!
## 1) Feel free to try your hand at the next two questions using strings and 
##    writing your own functions 
## 2) Review your code for the questions above and try to make your code as 
##    efficient as possible.
## 3) Continue exploring the datasets on your own!


## 6. Create a dataframe with the mean and standard deviation of the relative
## abundance (per individual bromeliad) of each invertebrate morphospecies. Turn
## it into a function.


## 7. Rank-Abundance Plot of Invertebrate Morphospecies in Guzmania.
## Let's make a "rank-abundance" plot for the morphospecies found in Guzmania 
## bromeliads. Which morphospecies is/are present in the most bromeliads and 
## which in the fewest? 
## Next, plot the data from highest presence (rank 1) to lowest. In order for 
## the plot to be more readable, let's adjust the morphospecies names to 
## something shorter. Select the first two letters, followed by the "." 
## and number code (e.g., Di.136)


# Answers -----------------------------------------------------------------

## Question 1 ##

# 1a. 35

# 1b. 706 mL

# 1c.
#                species  n
#1 Vriesea_gladioliflora 11
#2    Vriesea_kupperiana  3
#3 Vriesea_sanguinolenta 12
#4            Vriesea_sp  9


## Question 2 ##

#  Genus    bromeliad_diameter n
#1 Guzmania large              6
#2 Guzmania medium            11
#3 Guzmania small              6
#4 Vriesea  large              9
#5 Vriesea  medium            11
#6 Vriesea  small             32
#7 Vriesea  NA                 1


## Question 3 ##

#         owner_name  n
#1  Diane Srivastava 76
#2 Michael Melnychuk 20
#3    Jana Petermann 18


## Question 4 ##

#1 Guzmania   8.43
#2 Vriesea    9.75


## Question 5 ##

#1 Guzmania_sp                      162.
#2 Vriesea_gladioliflora            133 
#3 Vriesea_kupperiana               369.
#4 Vriesea_sanguinolenta            260.
#5 Vriesea_sp                       293.


## Question 6 ##

# Answer (first 6 rows)
# bwg_name         mean       sd
# 1 Coleoptera.13 0.00418 0.000610
# 2 Coleoptera.31 0.311   0.199   
# 3 Diptera.11    0.0131  0.0166  
# 4 Diptera.118   0.0164  0.0334  
# 5 Diptera.119   0.00945 0.00708 
# 6 Diptera.12    0.0118  0.0169 


## Question 7 ##

# most frequent: Di.269, Co.31
# least frequent: Diptera.119, Diptera.120, Diptera.124, Diptera.55, Diptera.57,
#                 Unknown.1


# (first 6 rows)
# bwg_name       plotting_id count
# 1 Coleoptera.31  Co.31          21
# 2 Diptera.269    Di.269         21
# 3 Diptera.188    Di.188         18
# 4 Diptera.203    Di.203         17
# 5 Oligochaeta.13 Ol.13          16
# 6 Diptera.298    Di.298         13


## -- ## -- ## -- ## -- ## END OF SCRIPT ## -- ## -- ## -- ## -- ##
