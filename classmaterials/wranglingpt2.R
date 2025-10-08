# FIW5594: Conservation Data Science ---------
# Data Wrangling part 2 ------

# Part 2: Finish getting data frame tidied and wrangled 
# 1) Create new columns or edit observations within column (mutate)
# 2) Change class of columns
# 3) Fix cells within columns - factors (rename factor levels, reorder factor levels, ghost factor levels) (forcats package)
# 4) Fix cells within columns - character strings (change levels tolower/toupper, combine strings, extract parts of a string, replace parts of a string) (stringr package)
# 5) Scale/center continuous variables
# 6) Work with dates (lubridate package)
# 7) Arrange data by the levels of a particular column (arrange)
# 8) Print tidied, wrangled database

# Load Libraries ----------
library("tidyverse") #loads dplyr, stringr, ggplot2, tidyr, forcats
library("lubridate")

# Load tidy data, explore ----------
transplant <- read_csv("data/tidy/transplant_tidy.csv")
str(transplant)
glimpse(transplant)

# 1: Create new columns or edit observations within existing columns (mutate) ----------

transplant <- transplant |>
  mutate(webarea = pi * ((websize/2)/100)^2)

glimpse(transplant) #check for webarea

#assume circle, divide in half to get radius, divide by 100 to get from cm to m, calculate area (pi * radius squared)

# 2: Change class of columns ----------
# note - the as.character and as.numeric lines are in here for teaching purposes since those columns are already in the right class. 
# We are using forcats version of as_factor (not base as.factor) because forcats preserves input order

transplant <- transplant |>
  mutate(websize = as.numeric(websize),  # to change class in one column
         across(c(island, site, web_number, native, netting, spid_pres, web_pres), as_factor), # use 'across' to change multiple columns at a time
         across(c(web_a, web_b), as.character)) 

glimpse(transplant)

# 3: Fix cells within columns (e.g. naming, capitalization) ----------

summary(transplant) #need to fix capitalization, spelling, whitespace (maybe)

## 3.1: Rename levels of a variable (forcats) ----------
# There are a lot of ways to do this. Here is the forcats approach. 

transplant <- transplant |>
  mutate(island = fct_recode(island, 
                             "saipan" = "siapan", 
                             "guam" = "gaum"))

levels(transplant$island)

#can also combine groups using fct_collapse. For each new variable, you provide a vector of old levels. Note - this is just to demonstrate, because the new variable is not useful, so I am saving to transplant2. 

transplant2 <- transplant |>
  mutate(siteisl = fct_collapse(site,
                                "saipan_site" = c("forbi", "ladt", "mtr"), 
                                "guam_site" = c("anao", "nblas")))

## 3.3: Re-order levels within a variable (forcats) ----------
# default order is alphabetical. When you graph or run analyses, you may want a different order. 

ggplot(transplant, aes(site, websize))+
  geom_boxplot()

str(transplant)
levels(transplant$site)

# tidyverse approach 

# Manually re-order
transplant <- transplant |>
  mutate(site = fct_relevel(site, c("nblas", "anao", "ladt", "forbi", "mtr")))

levels(transplant$island)

#base R approach for fct_relevel
transplant2$site <- factor(transplant$site, levels = c("nblas", "anao", "ladt", "forbi", "mtr"))

# Reorder by the levels of another variable
levels(transplant$site)
transplant <- transplant |>
  mutate(site = fct_reorder(site, websize, mean))  #order site levels by the mean websize for each level, with smallest websize first (ascending)

#view shows that the rows are still in the same order, but the levels have been reordered
levels(transplant$site)

# Can reverse order of levels
transplant <- transplant |>
  mutate(site = fct_rev(site)) #reverses order of levels

levels(transplant$site) #now shows order from larger to smaller mean websize 

# Can also go from larger to smaller by adding ".desc = T" to fct_reorder


# Reorder by the frequency of the variable

transplant <- transplant |>
  mutate(site = fct_infreq(site)) #by number of observations of each level, with most first

levels(transplant$site) 


## 3.3: Get rid of ghost levels ----------
# sometimes you get rid of a level and R still thinks it is there (i.e. there are 0 rows, but it still shows up when you use levels() on your variable)

#first create ghost level by adding level to factor
transplant <- transplant |>
  mutate(island = fct_expand(island, "hawaii"))
levels(transplant$island)

#tidyverse approach
transplant <- transplant |>
  mutate(island = fct_drop(island))

levels(transplant$island)

#base R approach
transplant$island <- droplevels(transplant$island) # or
transplant$island <- factor(transplant$island)
levels(transplant$island)

# 4: Deal with character data or complex strings (stringr) -----
# string functions work with regular expressions, patterns of text
# https://stringr.tidyverse.org/ shows the full extent of the package

## 4.1. Change levels of variable to lower case (mutate, tolower) ----------
# Note that str_to_lower creates a character vector, so may need to change back to factor.

transplant <- transplant |>
  mutate(across(c(web_a, web_b), str_to_lower),  #coerces to character
         across(c(web_a, web_b), as_factor)) |>  #change it to a factor 
  print() #shows first few lines of dataframe 

# across() allows you to apply the same function to multiple columns 

#base R code: transplant$island <- tolower(transplant$island)

## 4.2. Manipulate characters within each string -------

# str_c() allows you to combine the characters from two variables 
transplant2 <- transplant |>
  mutate(islandsite = str_c(island, site, sep = ", "))

# str_sub() allows you to extract parts of a string, say only the first 3 characters 

transplant2 <- transplant |>
  mutate(site = str_sub(site, 1,3)) #keep characters in the 1st to 3rd position

transplant2 <- transplant |>
  mutate(endsite = str_sub(site, -3, -1)) #to extract the last 3 characters (characters from the third from the last position through to the last position)

# str_replace() allows you to replace parts of a string. str_replace just replaces the first instance, str_replace_all replaces all instances. 
transplant2 <- transplant |>
  mutate(web_number = str_replace(web_number, "'", "flowers"), 
         island = str_replace_all(island, "[aeiou]", "-"))

levels(transplant2$island)

## 4.3: Remove trailing whitespace ----------
# sometimes excel will leave spaces in a factor, which makes "guam" and "guam ", for example, into different factor levels. trimws() gets rid of this trailing white space
transplant <- transplant |> 
  mutate(site = str_trim(site))

## 4.4: Count number of characters in a string
# str_length() tells you the # of characters in a string
str_length(transplant$web_a)


# 5: Center continuous predictors ----------
# Centering continuous predictors with large values is a useful practice for analysis - may help with convergence
# centering and scaling gives Z-scores (# of standard deviations a data point is from the mean); can just center the mean at 0 instead if desired
transplant <- transplant |>
  mutate(websize_c = as.numeric(scale(websize, center = TRUE, scale = FALSE)))

# 6: Deal with dates (lubridate) ----------
#Change date format to standard yyyymmdd format
#helpful site: https://www.r-bloggers.com/date-formats-in-r/
class(transplant$start_date2)

## 6.1: Tell R the format of the date

transplant <- transplant |>
  mutate(start_date2 = dmy(start_date2))

transplant <- transplant |>
  mutate(end_date2 = dmy(end_date2))

# can even fix the column that is missing a year
transplant2 <- transplant |>
  mutate(start_date = dmy(paste(start_date, "2013"))) 

## 6.2: Can do math on your dates!
transplant <- transplant |>
  mutate(duration = end_date2 - start_date2) 

summary(transplant$duration)

# 7: Re-arrange order of rows of data (arrange) ----------
# can re-arrange the data frame by the levels of a particular column. This can be helpful for visualization. Note that this does not change the order of levels. 

transplant <- transplant |>
  arrange(websize) 

#use desc inside the arrange fx to go from high to low
transplant <- transplant |>
  arrange(desc(websize))

# 8: Print tidy, wrangled database ----------

write.csv(transplant, "data/tidy/transplant_tidy_clean.csv", row.names = F)

