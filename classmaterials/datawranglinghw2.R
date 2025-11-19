# FIW5584: Conservation Data Science
# Data_wrangling part 2 practice exercise ------

# Part 1: Get set up ----------

## 1.1) Load libraries ----------

library(tidyverse)



## 1.2) Read in data ----------
# From the tidy folder, read in the poll_long_partialtidy.csv file 

library(readr)
pollination2 <- read_csv("data/tidy/poll_long_partialtidy.csv")
View(pollination2)

## 1.3) Change name of columns -------
# "date traps out" should be "dateout" and and "date traps coll" sould be "datecoll"

names(pollination2)

pollination2 <- pollination |>
  rename(datecoll = `date traps coll`,
         dateout = `date traps out`)

## 1.4) Change the class of each variable as appropriate ------
# Make variables into factors, numeric, character, etc. Leave the dates as is for now. 

glimpse(pollination2)

pollination2 <- pollination2 |>
  mutate(across(c(...1, numinsects),as.numeric))

pollination2 <- pollination2 |>
  mutate(across(c(transect,topcolor,bowlcolor),as_factor))
         

## 1.5) What format are the dates in? Change to date format ----

#It appears they're already in <date> format

capture <- capture |>
mutate(dateout = ymd(dateout))

# Part 2: Fix errors within cells ------

## 2.1) Fix the levels of island and site ------
# Make sure all island and site names are in lowercase 
# Rename sites: forbigrid as forbig and racetrack as race

#island and site already in lowercase

view(pollination2)

pollination2 <- pollination2 |>
  mutate(site = fct_recode(site, 
                             "race" = "racetrack", 
                             "forbig" = "forbigrid"))

## 2.2) Do you see any other errors that should be cleaned up? -----
# Just good practice to do a final check on this. Insect orders should remain capitalized. 

#How can you change the uniqueID to reflect the change in site names? i.e. GuamRacetrackL3 to GuamRaceL3?


# Part 3: Create new columns ------

## 3.1: Create a new column for the duration of time traps were out. ------
# Make sure new column is in the numeric class. 

pollination2 <- pollination2 |>
  mutate(trapduration=as.numeric(datecoll-dateout))

glimpse(pollination2)

pollination2 <- pollination2 |>
  mutate(trapduration=as.numeric(trapduration))

pollination2 <- pollination2 |>
  select(-`as.numeric(datecoll - dateout)`)

capture <- capture |>
  mutate(across(c(factor, factor)), tolower),
across(c(3:4),as.factor)) |>

## 3.2: Create a new column with just the first 5 letters of the InsectOrder ------
# Name new column order_abbrev and make sure it is a factor 

pollination2 <- pollination2 |>
  mutate(order_abbrev=as_factor(substr(insectorder,1,5)))

# Part 4: Re-arrange levels of a variable and rearrange rows ------
## 4.1) Arrange levels of insectorder by average number of insects. ------ 
#this will let us create a graph later on of insect orders with the most common on one side and least common on the other side of the x-axis.

levels(pollination2$insectorder)
pollination2 <- pollination2 |>
  mutate(insectorder = fct_reorder(insectorder,numinsects, mean),.na_rm = FALSE, .desc = T)

## 4.2) Arrange entire dataset by the number of insects ------
# make these descending, so the greatest number is on row 1. 

pollination2 <- pollination |>
  arrange(desc(numinsects))

# Part 5: Print tidied, wrangled database ------
# name file "poll_long_tidy.csv" and put in tidy database

write.csv(pollination2, "poll_long_tidy.csv")

# Part 6: Subset & summarize --------
# Now that you have a tidy database, you can start summarizing the data in all sorts of ways! 

## 5.1) Make a new dataframe with just the data from Guam at the racetrack site and name accordingly. --------


tidypollination <- read_csv("data/tidy/poll_long_partialtidy.csv")
view(tidypollination)

Guam_racetrack <- tidypollination |>
  filter(site == "Racetrack", island== "Guam")

## 5.2) Make a new dataframe with just the uniqueID, island, site, transect, insectorder, numinsects, and duration columns. --------

Newdataframe <- pollination2 |>
  select(uniqueID, island, site, transect, insectorder, numinsects, trapduration)

## 5.3) With the full database (not the new ones you created in the two previous steps), summarize data, to get: --------

### 5.3.a) a table with the total number of insects at each site, and then arrange rows in descending order --------
view(table)

totalinsectnumbers <- pollination2 |>
  group_by(site) |>
  summarise(totalinsect=sum(numinsects, na.rm = T)) |>
  arrange(desc(totalinsect))

### 5.3.b) a table that shows the mean number of insects per island, arranged in ascending (smallest first) order --------

meaninsectisland <- pollination2 |>
  group_by(island) |>
  summarise(meaninsect=mean(numinsects)) |>
  arrange(meaninsect)

### 5.3.c) a table that shows the min and max number of insects per transect (note that the transects have the same name at each site) --------

minmaxtransect <- pollination2 |>
  group_by(transect) |>
  summarise(mininsect=min(numinsects),
            maxinsect=max(numinsects))

## 5.4) Figure out which insect order is found across the greatest number of sites and has the most total insects --------

bestinsect <- pollination2 |>
  group_by(insectorder) |>
  summarise(occurance=n_distinct(site),
            mostinsects=sum(numinsects)) |>
  arrange(desc(occurance), desc(mostinsects))

view(bestinsect)

## 5.5) For the insect order with the greatest total number of insects and found at the most sites, calculate the mean and sd by site. Include the island name in the final table. --------

Lepidoptera_rules <- pollination2 |>
  filter(insectorder == "Lepidoptera") |>
  group_by(island, site) |>
  summarise(meanlepid=mean(numinsects),
            sdlepid=sd(numinsects))
  .groups = "drop"
  
  view(Lepidoptera_rules)

## 5.6) Ask a question about the relationship between bowl color and insectorder, and then write the code to answer your question. ------
  
##What bowl color is most frequently observed across all insect orders?
    
bowlcolorfreqs <- pollination2 |>
    group_by(bowlcolor, insectorder) |>
    summarise(sumcolor=sum(numinsects)) |>
    arrange(desc(sumcolor))
  
  view(bowlcolorfreqs)
  