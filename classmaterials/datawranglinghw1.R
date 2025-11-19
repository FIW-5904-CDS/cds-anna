# FIW5594: Conservation Data Science
# 1 October 2025 
# Data wrangling part 1, practice script ----------

# We will be working with a real insect pan traps dataset that I've amended
# slightly in order to practice the skills from Monday.  
# The file is called "Data_wrangling_day1_pollination.xlsx" and it is 
# located in the data folder, and then in the raw folder within that. 

# 1) Load libraries -----
# you will need tidyverse and readxl

library(tidyverse) #this also loads ggplot2, lubridate, tidyr, forcats
library(readxl)


# 2) Read in data from the InsectData worksheet --------

pollination <- read_excel("data/raw/Data_wrangling_day1_pollination.xlsx")
View(pollination)

# 3) Rename columns using rename not clean_names --------
# Leave columns of insect orders with capital letters, but make all other 
# column names lowercase. 
# Remove any spaces in column names. Change "location" to "site". 
# Change "tract" to "transect". 

names(pollination)
#new name on left, old name on right
pollination <- pollination |> 
  rename(site = Location)

pollination <- pollination |>
  rename(transect = Tract)

pollination <- pollination |>
  rename(Topcolorbowl = `Top color - Bowl color`)

pollination <- pollination |>
  rename_with(tolower, c(Island,Partial,Topcolorbowl, Other))

#you can combine all these separate functions!

# 4) Add missing data --------
# The people who entered the data did not drag down the island or site
# column to fill every row. Use code to fill in this missing data. 
# Double check to make sure this worked correctly. 

pollination <- pollination |>
  complete(island, site)

# 5) Separate "Top color - Bowl color" into two different columns ------
# The first letter represents the top color and the second letter represents the
# bowl color. We do not need to save the original column. 

colnames(pollination)
pollination <- pollination |>
  separate(col = topcolorbowl, into = c("topcolor", "bowlcolor"), sep = "-", remove = TRUE) 

# 6) Use the complete function ----------
# Check if we have data for all 3 transects at each site 
# Do not overwrite the poll data frame when you do this. 

pollination_comp <- pollination |>
  complete(transect)



# Look at the resulting dataframe - can you figure out which transects appear to be missing, and why? 

pollination_com

# 7) Unite island, site, transect into a single column -----
# Do not add spaces or punctuation between each part. Call this column uniqueID. 
# Keep the original columns too. 

class(pollination$island)
class(pollination$site)
class(pollination$transect)

pollination <- pollination |>
unite(col = "uniqueID", island, site, transect, sep = "", remove = FALSE)

# 8) Now, make this "wide" dataset into a "long" dataset ---------
# one column should include the insect orders, and one column the number of insects. 

pollination_long <- pollination |>
  pivot_longer(cols = c(7:19), names_to = "insectorders", values_to = "count")

# 9) Just to test it out, make your "long" dataset into a "wide" one and see if anything is different. -------

pollinationtwide <- pollination_long |>
  pivot_wider(names_from = c(Coleoptera, Araneae, Formicidae, Isoptera, Apoidea, Crabronidae, Trichoptera, Diptera, Lepidoptera, Hemiptera, Blattodea, values_from = count)

# Are you getting a warning? Can you figure out why? 

#when I try to run the long to wide code, it turns the > symbol in the console to the + symbol. Not sure why.

# 10) Join the "InsectData" with the "CollectionDates" tab ---------
# Add a collection date for each row of InsectData. You'll need to read in the
# CollectionDates tab. Play around with the various types of 'mutating joins' 
# (i.e. inner_join, left_join, right_join, full_join), to see what each one does
# to the final dataframe, and note which one you think does the job correctly. 

#cannot find the collection dates tab

#when joinging, make a nose of the number of observations and variables 

# 11) Create a csv with the long dataframe -------
# dataframe should include collection dates
# put new csv in your data/tidy folder

write.csv(pollination_long, "data/tidy/pollination_tidy.csv", row.names=FALSE)
