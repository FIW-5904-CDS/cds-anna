
#script contains raw data from my fieldwork in Fire Island summer 2025. 


#reading in data

#capture surveys that has info on species captured
capture <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025capturedata.csv")

#the day roost tree measurements
tree <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025treemeasurementdata.csv")

#emerengence count data
emergence <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025emergencecountdata.csv")

#where we found our MYSE day roosting
dayroost <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/2025batdayroostdata.csv")

#using this package for figures
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)

glimpse(capture)

class(capture)
dim(capture)
head(capture)
colnames(capture)
rownames(capture)
labels(capture)
summary(capture)
str(capture)
view(capture)
view(treemeasurements)

#need to change classes! date should be date format? starttime and endtime should be reformatted to be times. 
# Change class using as.factor(), as.numeric(), as.integer(), as.character()

capture$forearmlength <- as.numeric(capture$forearmlength)

##order of wrangling: Fix column names, Tidy (Re-shape) dataframe (wide, long), Fix cells (levels, spaces, case etc.)

#clean column names



