# FIW5584: Conservation Data Science
# Data Exploration and Visualization practice exercise ------

# Research Question ---------
# We will be working with a dataset from an experiment where we planted seedlings near and far from conspecific adults and monitored them for survival. 
# Does survival of seedlings depend on distance from nearest conspecific adult, and does that effect vary by species or canopy openness? 

##### Data dictionary ----------
# "species"- six plant species     
# "disp" - disperser present on island - yes/no          
# "island" - island (guam, saipan, tinian, rota)     
# "site"    - 5 sites on Guam, 3 each on Rota, Tinian, Saipan       
# "fence"   - fence name (based on forest plot grid names)       
# "numalive"  - number seedlings alive in fence 
# "date"       - date fence checked     
# "observer"   - person collecting data      
# "dataentry"   - person entering data     
# "dateenter"    - date data entered    
# "uniqueidsppfence" - unique id for each spp:fence combo
# "canopydate"    - date canopy cover data taken 
# "north"          - canopy measurement 1  
# "east"           - canopy measurement 2     
# "south"            - canopy measurement 3  
# "west"             - canopy measurement 4   
# "avgcover"        -average canopy measurement (% cover)    
# "avgopen"          -average canopy measurement (% open)   
# "doubleplant"     - was this fence double planted? 
# "plantdt"          - planting data
# "dist"             - near or far from conspecific? 
# "soil"             - soil type within the fence
# "numseedplant"    - number of seedlings planted
# "DDsurvival_notes"  - notes
# "bird"             - bird presence or absence on the island
# "age"             - age of seedlings (since planting)
# "centavgopen"      - centered average open
# "adultdens_wdisp"  - adult tree density on islands with disperser for that spp
# "adultdens_wodisp" - adult tree density on islands without disperser for that spp
# "seedsize"       - seed size 
# "numtrees"        - number of conspecific trees in the plot 
# "area"            - area of the plot
# "dens_100m"       - calculated density per 100 m
# "regdens"         - density across all plots
# "regdenswd"       - density just from plots with dispersers for that species
####

# Load Libraries -----------
library(tidyverse)
library(skimr)
library(DataExplorer)

# Load Dataset ------

# Start with a tidy dataset. Load data/tidy/fencesurv_tidy.csv from the tidy folder. NA values include c("", "NA", "na"). 

library(readr)
fencesurv_tidy <- read_csv("data/tidy/fencesurv_tidy.csv")
View(fencesurv_tidy)

# 1. Get dataset prepared for exploration ----------
# If necessary, you would subset to the dataset you will use for the analysis. However, we will use the whole dataset for now.

# 1. Get dataset prepared for exploration ----------
# If necessary, you would subset to the dataset you will use for the analysis. However, we will use the whole dataset for now.

# 1.1: Check structure. 

# 1.2: Decide which variables are your response variables and which are your predictors, and identify any other variables that might be responsible for additional patterns in your dataset. 
# Response: _number seedlings alive, age_____
# Continuous predictors: _north, east, south, west (densiometer measurements), density, age of seedlings_______
# Categorical predictors: _bird presence, soil type, distance, disp, number planted, doublefence, species___________
# Other factors that may be of interest: _____________

# 1.3: Make sure everything is in correct class. If not, put into correct class. 

glimpse(fencesurv_tidy)

# 1.4: Make a new column for proportion alive (propalive) by dividing numalive/numseedplant 

fencesurv_tidy <- fencesurv_tidy |>
  mutate("propalive" = (numalive)/(numseedplant))

colnames(fencesurv_tidy)

# 1.5: Any duplicated rows? How many NA's? 

sum(is.na(fencesurv_tidy)) #785
sum(duplicated(fencesurv_tidy)) #0

# 2. Data Exploration with comprehensive functions ---------
# Note anything that stands out to you from these two approaches below. 
# 2.1: Try the skim() functions from the skimr package 
# 2.2: Try the create_report() function from DataExplorer package. 

skim(fencesurv_tidy)
#when using the skim function, there are some values for the centeravgopen that are in red. They are all negative values which makes me think that the red indicates there is something wrong with the values. The average center open can't be less than 0? 

#When looking at the itsy little histogram, the values for the proportion alive appear to be extreme. That is, the proportion is either very high or very low. 

create_report(fencesurv_tidy) #did not work
help(DataExplorer)
??DataExplorer

# 3. Data Exploration: Individual Variables ---------
## 3.1: Continuous variables ---------

# 3.1a: How many NA's do you have in your continuous variables? 

#continuous variables: avgcover, avgopen, numseedplant, seedsize

colSums(is.na(fencesurv_tidy))

#avgcover: 40, avgopen: 40, numseedplant: 0, seedsize: 0

# 3.1b: Distribution and outliers - With your continuous response and predictor variables, use ggplot and geom_histogram to learn about the distribution of your data and to look for outliers.   

library(ggplot2)

ggplot(fencesurv_tidy, aes(avgcover))+ geom_histogram()

ggplot(fencesurv_tidy, aes(numseedplant)) + geom_histogram()

ggplot(fencesurv_tidy, aes(numalive)) + geom_histogram()

# 3.1c: Zero-inflation - With your continuous response variable, look for zero-inflation (count data only). Are there more than 25% zero's in the response? 

sum(fencesurv_tidy$numalive == 0, na.rm=TRUE) #90
522/90

#no there are not more than 25% zero's in the response

# 3.1d: Independence - With your continuous response variable, look for independence. 
# Are there patterns in the data that are unrelated to the fixed or random effects identified above?  Consider patterns over time, for example. 

#for the age of seedlings, this will increase over time. Is that the kind of answer you're looking for?


## 3.2: Categorical variables (predictors) --------
# 3.2a: Sample size - assess whether you have adequate sample size. How many observations per level of each of your categorical predictors? Are there any that have fewer than 15 observations?  Don't worry about combinations of variables at this point, just single variables. 

fencesurv_tidy |>
  count(dist) 
 
fencesurv_tidy |>
  count(bird)
  

# 4. Data Exploration - Relationships between variables ------------

## 4.1: Explore relationships between your predictor variables -------
# 4.1a: look for correlation/covariation between each of your predictors (fixed & random)
#If 2 continuous predictors, use ggplot, geom_point to plot against each other, or use pairs()
#If 1 continuous and 1 categorical predictor, use ggplot with geom_boxplot() 
#For two categorical predictors, use summarize or table (ftable for more than 2 categories), or geom_tile()

ggplot(fencesurv_tidy, aes(y=dist,x=avgcover))+ geom_boxplot() #not seeing anything crazy here

ggplot(fencesurv_tidy, aes(y=species,x=numseedplant)) +geom_boxplot() #very few neisosperma and morinda were planted so if the number of those species having survived is low, that makes sense 

ggplot(fencesurv_tidy, aes(avgcover, bird)) + geom_boxplot()

# 4.1b: Interactions: need to make sure you have adequate data for any 2-way or 3-way interactions in your model. 
## We are interested in a species * distance * centavgopen interaction. Do we have adequate sampling across this interaction? 

#I do not know how to check for this. 

## 4.2: Look at relationships of Y vs X’s ---------
#Plot each predictor and random effect against the response
#See if variances are similar for each X value, identify the type of relationship (linear, log, etc.)

#oof also do not know

# 5. Summary of data exploration ---------------
# Pull together all of your findings from above to summarize your general results here. This guides you on how to move forward with your analysis. 

## 5.1: Individual variables ---------
### 5.1.a: Continuous variables --------

# Outliers (response & predictors)

# Zero-inflation (response)

# Independence (response)


### 5.1.b: Categorical variables ---------

# Sufficient data across all levels? Any NA's?


## 5.2. Multiple  variables -----------
# What is the relationship between variables? 

### 5.2.a: Between predictor variables ----------

# 5.2.a.1: Collinearity:  

# 5.2.a 2: Interactions - do we have enough data? 


### 5.2.b: Between each predictor and response ----------
# Is the relationship of Y vs X's linear with homogeneous variance? 

