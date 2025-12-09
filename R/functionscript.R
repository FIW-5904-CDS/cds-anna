#' @title PCA Analysis of Day Roost Characteristics
#' @description Produces a PCA
#' @param tree containing day roost characteristics
#' @return a plot of PCA
#' @export
#'

library(tidyverse)## packages
library(vegan) ## to run the pca
library(ggplot2)
library(factoextra) ## pca visualization
library(FactoMineR) ## pca
library(corrplot) ## to visualize the quality of representation of the variables
library(ggpubr) ## better visualization
library(ggfortify) ## plot
library(RColorBrewer) ## get a better color palette
library(dplyr)
library(gridExtra) 

#reading in data
tree <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/FireIsland/Data/Tidy/alltreesclean.csv")

#cleaning data real quick
roosts <- tree |>
  filter(batpresence == "Y")

roosts <- roosts |>
  rename('Canopy Cover' = canopycover, 'DBH' = dbh, 'Percent Bark Remaining' = roostpercentbark, 'Canopy Class' = canopyclass, 'Decay Stage' = decaystage, 'Basal Area' = basalarea)

str(roosts)
t(names(roosts)) #checking out the df and seeing the different variables

roosts <- roosts |>
  mutate(treespecies = as.factor(treespecies)) 

## Run PCA

roostpca <- PCA(roosts[c(5, 6, 10, 13, 17, 18)], graph = FALSE) 

#Create plot 

PCAfinalfigure <- fviz_pca_biplot(roostpca, 
                                  label = "var",
                                  pointshape = 16,
                                  repel = TRUE,
                                  arrowsize = 1, #change weight of arrow vectors
                                  # habillage = as.factor(roosts$treespecies), # color by species
                                  legend.title = "Tree Species",
                                  
                                  col.var = c("black"),
                                  # gradient.cols = ,
                                  # fill.var = "white", alpha.var = 1,
                                  title = "Principal Components Analysis of Roost Characteristics") + geom_point(size= 8, aes(color = roosts$treespecies))+ scale_color_brewer(palette = "PuOr") + theme_classic() + theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold")) + xlab("Principal Component 1 (53%)") + ylab("Principal Component 2 (23%)")


