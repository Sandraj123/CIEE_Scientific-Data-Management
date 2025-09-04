# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   Living Data Project -- Data Management Module           # - # - #
# - # - #   Lesson 2: Data Manipulation and data wrangling          # - # - #
# - # - #   Introduction to some useful packages and function       # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   SELF-DIRECTED TUTORIAL -- DO BEFORE CLASS               # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

## NOTE 1 ##
## In order to import the data with this script, your working directory needs to
## be set to the correct folder!
## 1. Create a new folder (or use the zip folder you downloaded)
## 2. Make sure this script into the folder you are using.
## 3. Also make sure the BWG_database folder is in the same folder.
## 4. Close and reopen R by opening the this script with RStudio (right-click and
##    select "Open with" -> RStudio. Alternatively, you can also navigate to the 
##    folder in the "Files" tab then clicking "More" (with the gear icon) and 
##    selecting "Set As Working Directory"
## Your working directory should now be set to the correct folder.

## NOTE 2 ##
## This is NOT an assignment for you to turn in! 
## It's just an exercise to get you comfortable with the BWG database.


# Set up ------------------------------------------------------------------

### Working Directory ###

## check if working directory is set to current folder
getwd()   

## if it is not the correct folder, use setwd() or here::here() to set your 
## working directory to the correct folder.
## Alternatively, in the panel with the "Files" tab, navigate to the correct folder,
## then click "More" (with the gear next to it), and select "Set As Working Directory"


### Packages ###
install.packages("tidyverse")
install.packages("palmerpenguins")
install.packages("assertr")
install.packages("lubridate")
install.packages("stringdist")
install.packages("GGally")
install.packages("sf")
install.packages("taxize")
install.packages("myTAI")

### import (or install) required packages
library(tidyverse)
library(lubridate)
library(sf)
library(taxize)
library(myTAI)

## to install missing packages, go to "Tools" -> "Install Packages..."  ~or~
## run install.packages() with the package name in quotes inside the parentheses
## after installing via either option, run the code above again (library())


### Import Files ###

## what files are in the BWG database
myfiles <- list.files(path = "BWG_database/", pattern = "*.csv", full.names = TRUE)
myfiles

# import all tables as separate data frames, remove file path and file extensions (.csv)
list2env(
  lapply(
    setNames(myfiles, 
             make.names(
               gsub(".*1_", "", 
                    tools::file_path_sans_ext(myfiles)))), 
         read_csv), 
  envir = .GlobalEnv)


# Part 1: data manipulation -----------------------------------------------


## some of this may be review for you. Feel free to just skip over the sections 
## you are already comfortable with (but still run the code).


### The pipe: %>% ###
## keyboard shortcut: Ctrl+shift+m / cmd+shift+m

# the pipe %>% allows for "chaining", which means that you can invoke multiple 
# method calls without needing variables to store the intermediate results.

## Example
# check out the mtcars dataframe that was loaded with tidyverse
head(mtcars)

## Let's calculate the average mpg for each cylinder (cyl)

## Option 1: Store each step in the process sequentially
result_int <- group_by(mtcars, cyl)
result_int

result <- summarise(result_int, meanMPG = mean(mpg))
result

## Option 2: use the pipe %>% to chain the functions together
result <- group_by(mtcars, cyl) %>% 
  summarise(meanMPG = mean(mpg))
result

## using the pipe, we did not have to ceate an intermediate object (result_int)

## remove the two objects from global environment
rm(result, result_int)


### DPLYR::SELECT ###

## select the columns you want to work with

## first, let's take a look at the structure of the bromeliads data frame
str(bromeliads)
# or 
glimpse(bromeliads)

## Comment: A LOT of columns!

## to select columns by name:
bromeliads %>%
  select(bromeliad_id, species)

## to select columns column number:
bromeliads %>%
  select(1:3, 5)

## Create a new dataframe called bromeliads_selected
## include the columns: bromeliad_id, species, num_leaf, extended_diameter, 
## max_water, and total_detritus
bromeliads_selected <- bromeliads %>%
  select(bromeliad_id, species, num_leaf, 
         extended_diameter, max_water, total_detritus)

head(bromeliads_selected)


### DPLYR::RENAME ###

## we can rename a variable within the select command
bromeliads_selected <- bromeliads %>%
  select(visit_id, bromeliad_id, species, num_leaf, 
         diameter = extended_diameter, 
         volume = max_water, 
         total_detritus)

head(bromeliads_selected)

## or by using DPLYR::RENAME
bromeliads_selected <- bromeliads_selected %>%
  dplyr::rename(detritus = total_detritus)

head(bromeliads_selected)


### DPLYR::ARRANGE, ascending order (default) ###
## We can sort this data frame by the values for bromeliad volume
bromeliads_selected %>%
  arrange(volume)


### DPLYR:ARRANGE, descending order ###
## We can also reverse the order, sorting from largest to smallest
bromeliads_selected %>%
  arrange(desc(volume))


### GGPLOT2 ###

## plot relationship of volume and detritus
ggplot(data = bromeliads_selected, 
       mapping = aes(x = detritus, y = volume, color = species)) +
  geom_point()

## maybe log-scale is better, and add diameter as size
ggplot(data = bromeliads_selected, 
       aes(x = detritus, y = volume, 
           color = species, size = diameter)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

## facet wrap
ggplot(data = bromeliads_selected, 
       mapping = aes(x = detritus, y = volume, size = diameter)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ species)


### DPLYR::FILTER ###

## let's subset our data to include only Guzmania_sp
bromeliads_selected %>%
  arrange(volume) %>%
  filter(species == "Guzmania_sp")

## we can also filter for multiple species using the %in% operator
bromeliads_selected %>%
  arrange(volume) %>%
  filter(species %in% c("Guzmania_sp", "Vriesea_sp"))

## or filter for every species BUT the two listed by using an !
bromeliads_selected %>%
  arrange(volume) %>%
  filter(!species %in% c("Guzmania_sp", "Vriesea_sp"))

## we may also want all bromeliads in the Vriesea genus for that we can use 
## stringr::str_detect() 
bromeliads_selected %>%
  arrange(volume) %>%
  filter(str_detect(species, "Vriesea"))

## some species are only found in bromeliads with a maximum volume > 100 ml
## (let's use filter to subset for Guzmania bromeliads > 100 ml only
bromeliads_selected %>%
  arrange(volume) %>%
  filter(species == "Guzmania_sp",
         volume > 100)


### DPLYR::COUNT ###
## Count the number of bromeliads for each species in our dataset
bromeliads_selected %>%
  count(species)

## sort from most to least common
bromeliads_selected %>%
  count(species) %>%
  arrange(desc(n))

## you can also use
bromeliads_selected %>%
  count(species, sort = TRUE)


### DPLYR::MUTATE ###
## mutate will always return the same number of rows as the original dataset

## Bromeliads contain little wells that are formed in the axils of their leaves.
## Let's create a new column (av_well_volume) that represents the average volume 
## of a bromeliad leaf well
bromeliads_selected %>%
  mutate(av_well_volume = volume / num_leaf)


### MUTATE in combination with ifelse() ###
## let's categorize bromeliads based on their water holding capacity
## small: < 50 mL
## medium: 50 - 100 mL
## large: > 100 mL
bromeliads_selected %>%
  mutate(bromeliad_size = ifelse(volume < 50, "small",
                                 ifelse(volume <= 100, "medium",
                                        ifelse(volume > 100, "large", NA))))

## We could also use DPLYR::CASE_WHEN
bromeliads_selected %>% 
  mutate(bromeliad_size = case_when(volume < 50 ~ "small", 
                                    volume >= 50 & volume <= 100 ~ "medium", 
                                    volume > 100 ~ "large")) 


## let's visualize how many bromeliads we have in each size category by 
## piping this into count and ggplot
bromeliads_selected %>% 
  mutate(bromeliad_size = case_when(volume < 50 ~ "small", 
                                    volume >= 50 & volume <= 100 ~ "medium", 
                                    volume > 100 ~ "large")) %>% 
  count(bromeliad_size) %>%
  ggplot(aes(x = bromeliad_size, y = n)) +
  geom_bar(stat = "identity")


# Exercise 1 --------------------------------------------------------------

## SOLUTIONS TO ALL EXERCISE CAN BE FOUND AT THE END OF THE SCRIPT

## Combine some of the functions you have learned so far.
## Create a new dataframe (Guzmania_selected) from the bromeliads_selected 
## table only include bromeliads of the genus Guzmania that are > 100 ml. 
## Add a column for the average well-volume (av_well_volume), 
## and sort/arrange by this column from largest to smallest.

Guzmania_selected <- bromeliads_selected %>% 
  filter(species == "Guzmania_sp")%>%
  filter(volume > 100)%>%
  mutate(av_well_volume = volume / num_leaf)%>%
  arrange(desc(av_well_volume)) 

head(Guzmania_selected)

## Answer
Guzmania_selected <- bromeliads_selected %>%
  filter(species == "Guzmania_sp",
         volume > 100) %>%
  mutate(av_well_volume = volume / num_leaf) %>%
  arrange(desc(av_well_volume))

head(Guzmania_selected)

# Part 1 continued --------------------------------------------------------

### DPLYR::TRANSMUTE ###
## maybe we are only interested in the average well volume, without wanting the 
## volume column and the num_leaf column
bromeliads_selected %>%
  dplyr::transmute(bromeliad_id, species, av_well_volume = volume / num_leaf)

## Note: transmute is a 'superseded' function, which means that it has been replaced by 
## a different function that is thought to perform better. But because they are widely used, 
## they will remain in use

## alternative to the above code using transmute: 
bromeliads_selected %>%
  select(bromeliad_id, species, volume, num_leaf) %>% 
  mutate(av_well_volume = volume / num_leaf, .keep = "unused")


### DPLYR::SUMMARIZE/SUMMARISE ###
# summarise will return a tibble with fewer rows than the original data

## what is the mean volume across all bromeliads (use bromeliads_selected)
bromeliads_selected %>%
  summarize(mean_volume = mean(volume))

## oh no! This gives us NA
bromeliads_selected$volume
# that is because there are NAs in our data

## let's try again
bromeliads_selected %>%
  summarize(mean_volume = mean(volume, na.rm = TRUE))

## we can also summarize several columns at once
bromeliads_selected %>%
  summarize(mean_leaf = mean(num_leaf, na.rm = TRUE), 
            mean_diameter = mean(diameter, na.rm = TRUE), 
            mean_volume = mean(volume, na.rm = TRUE),
            n = n())

## we can do the same as above more succinctly by combing mutate() and across()
bromeliads_selected %>% 
  summarise(across(.cols = num_leaf:volume, ~ mean(., na.rm = TRUE)), 
            n = n())

## use the .names argument to change the name of the column according to the function used
bromeliads_selected %>% 
  summarise(across(.cols = num_leaf:volume, ~ mean(., na.rm = TRUE), .names = "mean_{.col}"), 
            n = n())


### DPLYR::GROUP_BY - AGGREGATE WITHIN GROUPS ###
## summarise is often used in conjunction with group_by, to perform operations on 
## groups of variables. For example, the mean leaf size of each species

# we can also summarize for each species
bromeliads_selected %>%
  group_by(species) %>%
  summarise(across(.cols = num_leaf:volume, ~ mean(., na.rm = TRUE)), 
            n = n()) %>%
  arrange(desc(n))


### TIDYR::SEPARATE ###

## we may also want to group by genus
## for that, we need to create a new column (Genus)
head(bromeliads_selected$species)

separate(data = bromeliads_selected, col = species,
                    into = c("Genus", "Species"),
                    sep = "_", remove = FALSE) %>% 
  head() 

## pipe into group_by and summarize to get summary statistics for the genera
separate(data = bromeliads_selected, col = species,
         into = c("Genus", "Species"),
         sep = "_", remove = FALSE) %>%
  group_by(Genus) %>%
  summarise(across(.cols = num_leaf:volume, ~ mean(., na.rm = TRUE)), 
            n = n()) %>%
  arrange(desc(n))

### TIDYR::UNITE ###
## the opposite of separate is the unite function, to join two or more columns together
bromeliads %>% 
  unite(col = "dataset_name_id", c(dataset_name, dataset_id), sep = "_") %>% 
  select(dataset_name_id)

# Exercise 2 --------------------------------------------------------------

## Which bromeliad genus has a higher fraction of large bromeliads (> 100 ml)?
## use what you have learned to create a table that lists the number of small 
## and large bromeliads for each genus. Use this table to calculate the fraction
## of large (>100 mL) bromeliads by hand. If you feel ambitions, calculate the 
## fractions in R (TIDYR:PIVOT_WIDER and DPLYR:REFRAME will come handy here)

Genus_sizes <- bromeliads_selected %>% ##...

# Part 2: joining tables --------------------------------------------------

## For more information on join_() functions, check out:
## https://dplyr.tidyverse.org/reference/join.html

## We may want to find out when these bromeliads were sampled.
## However, this information is not in the bromeliad table.
names(bromeliads)

# Having a look at our BWG database diagram, we can see that sampling dates are 
# stored in the "visits" table, which is connected to the "bromeliads" table via 
# the key "visit_id"

# lets check out visits (click on visits in your global environment)
str(visits)

## let's convert the dates to the right format so we can work with them
## the LUBRIDATE package covers all your date-handling needs!
visits$date
visits$date <- lubridate::as_date(visits$date) # convert to date-time format
# visits$date <- ymd(visits$date)


### DPLYR::LEFT_JOIN ###

# lets join visits to bromeliads to extract the sampling dates
bromeliads_selected %>%
  left_join(., visits, by = "visit_id")

# but we don't really want all these columns, as this get's way to busy
# let's use join in combination with select
names(visits)
select(visits, visit_id, dataset_id, date, latitude, longitude)

bromeliad_visits <- visits %>% 
  select(visit_id, dataset_id, date, latitude, longitude) %>% 
  right_join(., bromeliads_selected, by = "visit_id")
View(bromeliad_visits)

## NOTE: using right_join() (as opposed to inner_join) ensures that all rows in the 
## bromeliad table are preserved. However, in this case this does not make a 
## difference, as all visit_id keys are represented in both tables.


### JOINING MULTIPLE TABLES ###

# What countries were the bromeliads sampled in?
# the country data is in the "datasets" table (two tables away)

# join bromeliads_selected table with the countries column of the datasets table
bromeliads_selected %>%
  left_join(., select(visits, visit_id, dataset_id), by = "visit_id") %>%
  left_join(., select(datasets, dataset_id, country), by = "dataset_id")

# all bromeliads were sampled in Costa Rica!


# Exercise 3 --------------------------------------------------------------

## add two new columns to the bromeliad_selected table that lists the name 
## and affiliation of the dataset owner
## hint: did you get a warning message when trying to join tibbles? 
## update your code accordingly 

bromeliads_owners <- bromeliads_selected %>% # ...


## QUESTION: this increased the number of rows from 76 to 114. Why do you think 
## this happened?
dim(bromeliads_selected)
dim(bromeliads_owners)


# Part 3: a brief intro to more other useful packages ---------------------

## the purpose is not to give you a detailed tutorial, but to make you 
## aware that these packages exist and what you can use them for


### STRINGR: working with strings ###

## Let's look at the abundance table
head(abundance)
str(abundance)

## How many morphospecies are there in the species pool?
## Since R is case sensitive, it is advised to always convert everything to 
## lower case when working with strings -- we can do this using the 
## stringr::str_to_lower() function
morphospecies <- abundance %>% 
  select(bwg_name) %>% 
  mutate(bwg_name = str_to_lower(bwg_name)) %>% 
  distinct(bwg_name)
         
morphospecies # these are all the morphospecies
n_distinct(morphospecies) # there are 49 morphospecies


### How many (and which) morphospecies belong to the family Diptera?
# we can use the str_detect() or str_match() functions to extract all diptera
morphospecies %>% 
  filter(str_detect(bwg_name, "diptera"))

# we can use DPLYR::COUNT to find out how many Dipteran species there are
morphospecies %>% 
  filter(str_detect(bwg_name, "diptera")) %>% count()

## NOTE: try replacing count() with n_distinct() or tally()...
## although these three functions return the same thing in this instance, they are NOT
## necessarily interchangeable

# alternatively, use STR_COUNT and take the sum
sum(str_count(morphospecies$bwg_name, "diptera"))


### LUBRIDATE: working with dates and times ###
## Let's look at the visits table
head(visits)
str(visits)

# we already converted the dates column to the right format above
# visits$date <- as_date(visits$date) # convert to date-times format
class(visits$date) # this should be "Date"

# check out the date column
visits$date

# extract year / month / day
year(visits$date)
month(visits$date)
month(visits$date, label = TRUE, abbr = FALSE)
day(visits$date)

## find the first and last dates in the dataset
max(visits$date) # the oldest date
min(visits$date) # the newest date

# what was the maximum timespan?
max(visits$date) - min(visits$date)


### Dealing with spatial data ###

## Note: If you have previously worked with spatial data, 
## you may have used the RGDAL package, which is being 
## retired very soon. It's recommended to use the sf package instead
## This page is very helpful for this endeavour: 
## https://github.com/r-spatial/sf/wiki/migrating

## Let's look at the visits table again
head(visits)
str(visits)

# note that we have columns for longitude and latitude
xy <- visits[c("longitude", "latitude")]
xy

## we may want to convert those to UTM

## first, convert coordinates to "SpatialPoints"
xy <- st_as_sf(xy, 
               coords = c("longitude", "latitude"),
               crs = "+proj=longlat +datum=WGS84")

# transform to UTM coordinate system
xy_utm <- st_transform(xy, crs = "+proj=utm +zone=16 +datum=WGS84")
xy_utm

### TAXIZE and MYTAI: extracting taxonomic data ###

# extract the taxonomic classification for bromeliad-dwelling damselflies (M. modesta)
#taxize::classification
classification("Mecistogaster modesta", db = 'ncbi')

# what other species are in the Mecistogaster genus?
# myTAI::taxonomy
taxonomy(organism = "Mecistogaster" , db = "ncbi", output = "children" )



# Exercise solutions ------------------------------------------------------

### Solution to Exercise 1 ###
Guzmania_selected <- bromeliads_selected %>%
  filter(species == "Guzmania_sp",
         volume > 100) %>%
  mutate(av_well_volume = volume / num_leaf) %>%
  arrange(desc(av_well_volume))

head(Guzmania_selected)

## if we want to remove this last column from our table again, we can do this with
Guzmania_selected <- Guzmania_selected %>%
  select(-av_well_volume)

head(Guzmania_selected)


### Solution to Exercise 2 ###
genus_sizes <- bromeliads_selected %>%
  # create new column for small (<= 100mL) and large (> 100ml) bromeliads
  mutate(bromeliad_size = case_when(volume > 100 ~ "large",
                                      volume <= 100 ~ "small",
                                      is.na(volume) ~ NA)) %>% 
  # create new column for Genus using tidyr::seperate
  separate(col = species,
           into = c("genus", "species"),
           sep = "_", 
           remove = FALSE) %>%
  # group by Genus, then count by bromeliad_size
  count(genus, bromeliad_size) 

genus_sizes

# calculate fraction of large bromeliads by hand...
# or do it in R
pivot_wider(genus_sizes, names_from = bromeliad_size, values_from = n) %>%
  mutate(fraction_large = large / (large + small)) 
# Vriesea has almost twice the amount of large bromeliads than Guzmania

# there are always several different ways to perform the same task. 
# For example, this is similar to the previous solution but using 'reframe'
# instead of 'mutate'. How is the output different?
pivot_wider(genus_sizes, names_from = bromeliad_size, values_from = n) %>%
  reframe(fraction_large = large / (large + small)) 
# reframe is a dplyr function similar to summarise, without as many restrictions on row number 

# or without using pivot_wider...
genus_sizes %>% 
  filter(!is.na(bromeliad_size)) %>% 
  group_by(genus) %>% 
  reframe(total_bromeliads = sum(n)) %>% 
  right_join(., genus_sizes, by = "genus") %>% 
  mutate(prop_total = n / total_bromeliads) %>% 
  filter(bromeliad_size == "large")

### Solution to Exercise 3 ###
bromeliads_owners <- bromeliads_selected %>%
  right_join(select(visits, visit_id, dataset_id), .,  by = "visit_id") %>% 
  right_join(ownership, ., by = "dataset_id", relationship = "many-to-many") %>% 
  right_join(select(owners, owner_id, owner_name, institution), .,  by = "owner_id")

bromeliads_owners

## QUESTION: this increased the number of rows from 76 to 114. 
## Why do you think this happened?

### ANSWER ###
## The rows with bromeliads from datasets owned by multiple people are duplicated 
## (see "ownership" table)


## -- ## -- ## -- ## -- ## END OF SCRIPT ## -- ## -- ## -- ## -- ##
