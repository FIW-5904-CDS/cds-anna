
#script contains raw data from my fieldwork in Fire Island summer 2025. 
#need to clean data and add in locations once I pull them off of my Garmin, add column for lat and long


#I will use the following dataframes to analyze day-roost tree characteristics

#capture surveys that has info on species captured
capture <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025capturedata.csv")

#the day roost tree and non roost tree measurements
tree <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/treedataFIIS2025.csv")

#emerengence count data
emergence <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025emergencecountdata.csv")

#where we found our MYSE day roosting
dayroost <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025batdayroostdata.csv")

#packages
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyr)
library(data.table)

glimpse(tree) 
colnames(tree) #need to make all the non-roost (direction) trees their own row to compare roost vs non roost trees
view(tree)
summary(tree)
class(tree)
dim(tree) #The dataframe has 8 rows and 62 columns

##Delete unnecessary columns

treeclean <- tree |>
  select(-gpsunit, -treeid, -date,-understorydescription,-midstorydescription,-comments,-roostheightdistanceft) 

view(treeclean)

##let's rename stuff
treeclean <- treeclean |>
  rename(treeid = gpspoint, treespecies = roostspecies, dbh = roostdbh, canopyclass = roostcanopyclass, decaystage = roostdecaystage, heightdistance = roostheightdistance, heightup = roostheightup, heightdown = roostheightdown)

#add distance to roost for roost trees

treeclean <- treeclean |>
  mutate(distancetoroost=0)

#add column for direction
treeclean <- treeclean |>
  mutate(direction = "roost")

print(treeclean)

#select all the non-roost trees by direction

northeasttrees <- select(treeclean, starts_with("Ne"))
southeasttrees <- select(treeclean, starts_with("se"))
southwesttrees <- select(treeclean, starts_with("sw"))
northwesttrees <- select(treeclean, starts_with("Nw"))

view(northeasttrees)
view(southeasttrees)

colnames(northeasttrees) <- c('treespecies', 'distancetoroost', 'dbh', 'heightdistance', 'heightup', 'heightdown', 'decaystage', 'canopyclass', 'roosttype') #rename cols to be same with other direction
northeasttrees$direction <- 'NE' # ID it as the direction tree
northeasttrees$treeid <- treeid ## add in the treeid columns
northeasttrees$bandnumber <- treeclean$bandnumber
northeasttrees$lat <- treeclean$lat
northeasttrees$long <- treeclean$long

colnames(southeasttrees) <- c('treespecies', 'distancetoroost', 'dbh', 'heightdistance', 'heightup', 'heightdown', 'decaystage', 'canopyclass', 'roosttype') #rename cols to be same with other direction
southeasttrees$direction <- 'SE' # ID it as the direction tree
southeasttrees$treeid <- treeid ## add in the treeid columns
southeasttrees$bandnumber <- treeclean$bandnumber
southeasttrees$lat <- treeclean$lat
southeasttrees$long <- treeclean$long

colnames(southwesttrees) <- c('treespecies', 'distancetoroost', 'dbh', 'heightdistance', 'heightup', 'heightdown', 'decaystage', 'canopyclass', 'roosttype') #rename cols to be same with other direction
southwesttrees$direction <- 'SW' # ID it as the direction tree
southwesttrees$treeid <- treeid ## add in the treeid columns
southwesttrees$bandnumber <- treeclean$bandnumber
southwesttrees$lat <- treeclean$lat
southwesttrees$long <- treeclean$long

colnames(northwesttrees) <- c('treespecies', 'distancetoroost', 'dbh', 'heightdistance', 'heightup', 'heightdown', 'decaystage', 'canopyclass', 'roosttype') #rename cols to be same with other direction
northwesttrees$direction <- 'NW' # ID it as the direction tree
northwesttrees$treeid <- treeid ## add in the treeid columns
northwesttrees$bandnumber <- treeclean$bandnumber
northwesttrees$lat <- treeclean$lat
northwesttrees$long <- treeclean$long


nonroosttrees <- rbind(northeasttrees, southeasttrees, southwesttrees, northwesttrees)
view(nonroosttrees)

treecleanroost <- treeclean |>
  select(-matches("^[NS][EW]tree"))

treecleanroost <- treecleanroost |> mutate(across(c(dbh, heightdistance, heightup, heightdown, distancetoroost), as.numeric)) #make sure they're all the same class so they can bind

nonroosttrees <- nonroosttrees |> mutate(across(c(dbh, heightdistance, heightup, heightdown, distancetoroost), as.numeric))


alltreescleantest <- bind_rows(treecleanroost, nonroosttrees) #bind my roost tree rows with non roost tree rows

alltreescleantest <- alltreescleantest |>
  mutate(batpresence = if_else(direction == "roost", "Y", "N"))

view(alltreescleantest)

#then take day roost info and join by treeID to add info on dates of bats and which bats