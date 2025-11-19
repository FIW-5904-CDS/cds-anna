#Data Exploration and Visualization ------
#FIW5584 Fall 2025

# Topics in this script --------
# 1. Identify research question, variables of interest
# 2. Load libraries and dataset
# 3. Get to know your dataset
## 3.1. Quick overview - Check structure and data quality 
## 3.2. identify missing values
## 3.3. duplicate or impossible values
## 3.4. Skim/Create Report
# 4. Systematically explore the dataset
## Side lesson: Graphs using ggplot
## 4.1: Explore continuous variables
## 4.2: Explore categorical variables
# 5. Examine relationships between variables
# 6. Summarize findings

# 1: Identify research question, variables of interest -------

# Potential Research Questions: 
# Q1: Do spiders build smaller webs when birds are present? 
# If so, then web size should be smaller on Saipan than on Guam. (note the N=1 problem here).
# Q2: Does websize vary depending on whether spider was transplanted or found in the area? 
# Q3: Does duration web persists depend on island or netting or a combination of the two? 

# 2: Load libraries and dataset --------
library(tidyverse)
library(skimr) # just to check out a function
library(DataExplorer) # just to check out a function

# Load dataset
transplant <- read_csv("data/tidy/transplant_tidy_clean.csv")
nrow(transplant) #91 rows

# 3: Get to know your dataset ---------

# First, identify your variables of interest
# Response: websize (continuous), duration (continuous)
# Predictor: island (categorical), native (categorical), netting (yes/no)
# Random effect: site (categorical)

## 3.1: Quick overview of the structure/class ---- 
# What does each row & column represent? Check data structure & quality

# Numeric variables stored as numeric?
# Factors vs. characters?
# Dates in Date format?

glimpse(transplant)
names(transplant)
summary(transplant)

## 3.2: Identify important missing values ----
# Missing response means you cannot use that row in analysis
# Missing value in one of your predictors will typically drop that entire row from analysis

# What’s missing, by column?
colSums(is.na(transplant))


# Rows missing just the analysis-critical fields
transplant |> 
  filter(if_any(c(websize, web_b, duration, island, native, netting, site), is.na()))

## 3.3: Any duplicated or imposible values? -----
anyDuplicated(transplant)

# Any impossible values? 
range(transplant$websize, na.rm = T)
range(transplant$duration, na.rm = T)


## 3.4: Skim and Create Report -----
# try these two functions, from the skimr and DataExplorer packages
skim(transplant) #runs quickly
create_report(transplant) #slow to run, not all of it is relevant

# 4: Systematically explore the dataset ---------------

## Side Lesson - Introduction to ggplot --------
# Resources
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

# General structure: ggplot2(dataset, aes(x, y)) + geom()
# Part 1: dataset: can subset using square brackets if needed
# Part 2: mapping: aes = aesthetic, means "something you can see"
# Part 3: geom's: http://sape.inf.usi.ch/quick-reference/ggplot2/geom. A plot must have at least one geom; there is no upper limit.

ggplot(data = transplant, mapping = aes(x = websize, y = duration)) +
  geom_point()

# barplot  - to count number of rows per category of a variable
ggplot(transplant, aes(site))+
  geom_bar()

## 4.1: Explore continuous variables --------

# What are the bounds (only positive? Between 0 and 1? Integers?)? # Are distributions roughly normal or skewed?
# Any outliers to investigate?  
# Are measurement units and ranges consistent?
# For response variable, do you have a lot (>25%) of 0’s? 
# Do any predictors need centering or scaling for analysis?

# Our two continuous variables: websize, duration
  
# histogram - to look at variation within continuous variables
ggplot(transplant, aes(websize))+
  geom_histogram()

ggplot(data = transplant, aes(duration))+
  geom_histogram()

# Distributions
ggplot(transplant, aes(x = websize)) + 
  geom_histogram(binwidth = 5)

ggplot(transplant, aes(x = duration)) + 
  geom_density()

# Identify and inspect outliers
summary(transplant$websize)

which(transplant$websize > 100)

ggplot(transplant, aes(x = websize, y= reorder(site, websize, median))) +
  geom_point(size = 2, alpha = 0.8) #ggplot version of a Cleveland dotplot

ggplot(transplant, aes(websize)) +
  geom_boxplot()

ggplot(transplant, aes(duration))+
  geom_boxplot()

#### no major outliers in Y -websize or duration,

# Center or scale continuous variables if needed
transplant <- transplant |>
  mutate(
    websize_c  = scale(websize, center = TRUE, scale = FALSE),  # center only
    duration_s = scale(duration)                                # center + scale
  )

# Examine Zero inflation Y 
# #For count data, mostly
# If >25% of response values are 0, may have zero-inflated data. Will need to use zero-inflated model approach. 

transplant |>
  summarise(
    num_zeros = sum(websize == 0, na.rm = TRUE),
    n_total   = sum(!is.na(websize)),
    prop_zeros = mean(websize == 0, na.rm = TRUE)
  )

#### duration doesn't have any zero's so not an issue here. 



## 4.2: Explore categorical variables --------

# Are category names consistent?
# Are any levels empty or rare?

# count()
count(transplant, island, sort = TRUE)
count(transplant, native, netting)

# group then count
transplant |>
  group_by(island, site) |>
  count(netting)

# plot with a barchart
ggplot(transplant, aes(x = island)) + 
  geom_bar()

# table() gives a count of number of rows with a combination of variables
with(transplant, table(site, netting)) 

# ftable() makes a table with three variables more visually appealing
with(transplant, ftable(island, site, netting))




# 5: Examine relationships between variables ----

## 5.1: Visualize how response changes with predictors ---

## Continuous X Categorical ----
# boxplot - to summarize variation in continuous variable across categorical variables
ggplot(transplant, aes(netting, duration))+
  geom_boxplot() 

# create different boxplots for each island
ggplot(transplant, aes(netting, duration, color=island))+
  geom_boxplot() 

# can use summarizing too
transplant |> 
  group_by(island, site, netting) |> 
  summarize (mean_web = mean(websize, na.rm = T)) 

## Continuous X Continuous ----
# Scatterplot - use geom_point for two continuous variables 
ggplot(transplant, aes(websize, duration)) +
  geom_point()

# add facet_grid to show other variables
ggplot(transplant, aes(websize, duration))+
  geom_point()+
  facet_grid(netting~island)

## Categorical X Categorical ----
ggplot(transplant, aes(island, site)) +
  geom_tile()

# 6. Summarize findings ------





