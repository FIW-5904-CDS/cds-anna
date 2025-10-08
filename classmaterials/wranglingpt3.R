# FIW5584: Data wrangling Part 3 ----------

# Subsetting & summarizing 
# 1) Subset data (filter)
# 2) Select columns (select)
# 3) Summarize data (summarize, count)
# 4) Group data (group_by)

# Load libraries ----------
library("tidyverse") #loads dplyr, stringr, ggplot2, tidyr, forcats

# Load data, explore ----------
# Use tidy, wrangled data from Data Wrangling Part 2
transplant <- read_csv("data/tidy/transplant_tidy_clean.csv")

# Explore data
str(transplant)
glimpse(transplant)

# 1: Subset data - pick observations by their values (filter) ----------
#use filter to extract rows based on values of columns. 

# Create a table from transplant data with only data from the anao site.  
(transplant2 <- transplant |> 
   filter(site == "anao")) #note the use of  == not =

# Note - adding the parentheses around the whole thing makes it produce a variable and print the head of the tibble in the console. 

# Can use any standard operation: >, >=, <, <=, != (not equal), == (equal)

# Create a table with web area greater than 0.2
(transplant2 <- transplant |> 
    filter(webarea > 0.2)) 

# Can combine multiple variables using Boolean operators 
# & means “and”
# | means “or”
# ! means “not”

(transplant2 <- transplant |>
    filter(site == "anao" & webarea > 0.2))

# If you want to filter on multiple values of the same variable, need to enter each as a separate command
(transplant2 <- transplant |>
    filter(site == "anao" | site == "ladt")) 

# Another way to do this is to use the %in% function, which can be used to identify if an element belongs to a vector or dataframe. For example, this will select every row where the site is anao, ladt, or forb
(transplant2 <- transplant |>
    filter(site %in% c("anao", "ladt", "forb"))) 

# NA's: filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. If you want to preserve missing values, ask for them explicitly
(transplant2 <- transplant |>
    filter(is.na(webarea) | webarea > 0.2)) #note, there aren't any webarea values that are "NA", but this is the code to use if there were and you wanted to keep them as well as the web areas greater than 0.2

# Can use filter on dates too!
(transplant2 <- transplant |>
    filter(between(startdate, as.Date("2013-07-01"), as.Date("2013-07-30"))))

#Your turn: Use filter to keep only rows with "native" webs where the web is absent, and the duration is less than or equal to 4. How many rows and variables does this produce?

(transplant2 <- transplant |> 
    filter(native== yes & webpres=="no" and duration <=4)

#Your turn 2: keep rows where startdate is before 2013-07-30 and end date is before 2013-08-02 and webpres is yes. 

(transplant2 <- transplant |> 
    filter(before(startdate as.Date("2013-07-30"))),before  | )

# 2: Choose columns by their names (select) ----------
# Remember - Select only works on columns, so its arguments refer to column names, 
# NOT on the values within a given column/vector

# use select to choose columns. Note that the order of columns in the tibble it produces is determined by the order of variables in the select function. 
(transplant2 <- transplant |>
   select(island, site, websize, duration))

# can also use it to omit certain columns
(transplant2 <- transplant |>
    select(-island, -site, -websize, -duration))

# can select all columns between two columns
(transplant2 <- transplant |>
    select(web:webpres))

(transplant2 <- transplant |>
    select(1:4))

# useful helper functions for select (from the tidyverse)
# starts_with("abc"): matches names that begin with “abc”.
# ends_with("xyz"): matches names that end with “xyz”.
# contains("ijk"): matches names that contain “ijk”.
# num_range("x", 1:3): matches a numerical range like x1, x2 and x3.

(transplant2 <- transplant |>
    select(contains("eb"))) #7 variables have "eb" in their name

# Can use select() in conjunction with the everything() helper to change the order of columns. This is useful if you have a handful of variables you’d like to move to the start of the data frame, and then leave everything else after that. 
(transplant2 <- transplant |>
    select(native, netting, duration, everything()))

# Can select based on column type too, to pull out all factors or all numeric columns
(transplant2 <- transplant |>
    select(where(is.numeric)))

#Your Turn 3: Select columns web_a, web_b, webarea, native, netting, and duration, and arrange them so that native, netting, and duration are the first three columns. 
(transplant2 <-transplant |>
    select(native, netting, web_a, web_b, webarea))

# 3: Summarize data (summarize, count) ----------

## 3.1 Summarize function --------
# use summarize to compute a table using whatever summary function you want 
# handy functions: 
# finding the center or total: mean, median, sum
# finding the spread or range: max, min, sd, var 
# identifying by position: first(), last(), nth()
# counting: n, n_distinct
# logical: any(), all()

# Used alone, summarize will collapse a dataframe to a single row

(transplant2 <- transplant |>
   summarize(avgweb = mean(websize, na.rm = T))) # name the new column "avgweb"

# You can do multiple summary calculations at a time
(transplant2 <- transplant |>
    summarize(avgwebsize = mean(websize, na.rm = T), 
              medwebsize = median(websize, na.rm = T), 
              sdwebsize = sd(websize, na.rm = T), 
              maxarea = max(webarea, na.rm = T), 
              minarea = min(webarea, na.rm = TRUE)))

# note, if you have na's in a column, you'll get NA as the result if you try to calculate mean, median, sd, max, min, var, sum without including "na.rm = T" as an argument

# You can also do summary calculations on a subset of the data. 
# This calculates the average websize of all webs larger than 50 cm. 
(transplant2 <- transplant |>
    filter(site == "anao") |>
    summarize(avganao = mean(websize, na.rm = T))) ## look at this more 

# There are a bunch of ways to get the # of rows or # of levels of a variable
(transplant2 <- transplant |>
    filter(netting == "yes") |>
    summarize(numrows = n(), 
              nsites = n_distinct(site))) 

# n() takes no arguments, and returns the size of the group (number total rows)
# n_distinct(variablename) gives the number of unique levels of a factor

# To count the number of non-missing values, use sum(!is.na(x)) 

# Your turn 4: Calculate the mean, min, and max webarea for spiders on Saipan. 
  
  (transplant2 <- transplant |>
      filter(island == "saipan") |>
      summarize(avgweb = mean(webarea, na.rm =T)),
    minweb = min(webarea),
    maxweb=max(webarea))


## 3.2 Table & Count functions --------
# Table is Base R, and Count is dplyr (tidyverse)
# Both counts the number of rows in groups, and are functions on their own, not within summarize. 

# table is a handy base R function that counts number of rows based on 1 or more variables
table(transplant$site) #OR 
with(transplant, table(site))

# can count the number of rows based on two variables
with(transplant, table(native, netting))

# and even more! But ftable works better for 3 variables (ftable = "flat contingency table")
with(transplant, table(island, native, netting))
with(transplant, ftable(island, native, netting)) 

# Count is the tidyverse version of the same thing
# Sort = T sorts the output in descending order. 

transplant |>
  count(site, sort = T) 

transplant |>
  count(native, netting, sort = T)  

transplant |>
  count(island, native, netting, sort = T) 

#Can add continuous values, but not very useful unless the same values are repeated frequently. 
transplant |>
  count(websize, sort = T)  

# 4: Group data (group_by) ----------
#use group_by to split a dataframe into different groups, then do something to each group

(transplant2 <- transplant |>
   group_by(island) |>
   summarize (avg = mean(websize)))


(transplant2 <- transplant |>
    group_by(island, site, netting) |>
    summarize (avgweb = mean(websize, na.rm = T),
               avgduration = mean(duration, na.rm = T), 
               numobs = n()))

# Another way to group is using the .by within a function. The advantage of this is that the grouping is only temporary for that function, and does not get carried over to output. 
(transplant3 <- transplant |>
    summarize (avg = mean(websize), .by = island))

# Can use group_by with other functions too. 
# Here, we use filter to pull out the sites that have more than 2 rows for a combination of island/site/native (i.e. a site that has 3 or more spiders that were already in place (native) or moved (not native)), and then summarize the mean websize within each of those groups (e.g. 'native' spiders at anao)

transplant2 <- transplant |>
  group_by(island, site, native) |>
  filter(n() > 2) |>
  summarize (avgweb = mean(websize), .groups = "drop")  #.groups ungroups the output, which can be handy for future calculations using this tibble. 

# Your turn 5: Calculate the mean, median, min, and max duration for webs with and without netting on Guam and Saipan, and calculate the number of webs in each group

# Your turn 5, advanced version: 
# 1. Calculate mean duration of webs with and without netting at each site on each island. 
# 2. Add a column that calculates the difference between duration of each web and the mean_duration for all webs at that island/site/netting combo.
# 3. Select just the island, site, netting, mean_dur, web, websize, duration, and diff_mean_duration columns. 

