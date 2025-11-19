##########Creating figures
library(ggplot2)
library(maps)
library(mapdata)
# gridExtra for combining plots
library(gridExtra)
library(grid)

######### ------Panel 1: map---------------

# Get map data for New York state
ny_map <- map_data("county", region = "new york")

# Filter for the four Long Island counties
longisland <- subset(ny_map, subregion %in% c("nassau", "suffolk", "queens", "kings"))

p1 <- ggplot(longisland, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgreen", color = "black") +
  geom_point(aes(x = -72.829525, y = 40.767852), color = "red", size = 4.5) +
  geom_text(aes(x = -73.05, y = 40.79), label = "William Floyd Estate") +
coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Long Island, New York",
       x = "Longitude", y = "Latitude")

################-----------Panel 2: PCA to identify 2 most important characteristics----------

#pca necessary packages
library(vegan) ## to run the pca
library(dplyr)
library(tibble)
library(RColorBrewer) ## get a better color palette
library(lmom) ## extreme variable plot
library(ggrepel) ## have texts on plots repel

##reading in data
tree <- read.csv("C:/Users/annar/OneDrive/Documents/FIIS/FireIsland/Data/Tidy/alltreesclean.csv")

roostsites <- tree |>
  filter(batpresence == "Y") |>
  distinct(treeid, .keep_all = TRUE) #plotting only the roost trees and plotting just one each

roost <- roostsites |>
  select(treespecies, dbh, canopyclass, decaystage, treeheight)

print(roost)

### plotting pca

##### missing 5 of the characteristics I'll also be using in the PCA :(

pca <- rda(decostand(roost[,-c(1)], method = "hellinger"), scale = TRUE)


p2 <- biplot(pca, scaling = "symmetric")

screeplot(pca, bstick = TRUE, type = "l", main = NULL) #displays ordination and broken stick

### in order to combine I need to capture PCA figure as image apparently??

png("pca_biplot.png", width = 800, height = 600, res = 150)

biplot(pca, scaling = "symmetric")

dev.off()

# Add points for tree species
species <- factor(roostsites$treespecies)
palette <- RColorBrewer::brewer.pal(n = length(levels(species)), "Set2")
points(pca, display = "sites", scaling = "symmetric", pch = 19, col = palette[species])
legend("topright", legend = levels(species), col = palette, pch = 19, bty = "n")
dev.off()

#load image as grob idk what the hell a grob is 
library(png)
library(grid)

p2_grob <- rasterGrob(readPNG("pca_biplot.png"), interpolate = TRUE)


### adding points for tree species
species <- factor(roostsites$treespecies)
palette <- brewer.pal(n = length(levels(species)), "Set2")

points(pca, display = "sites", scaling = "symmetric",
       pch = 19, col = palette[species])

legend("topright", legend = levels(species),
       col = palette, pch = 19, bty = "n")



#panel 3: Probability of bat presence for dbh

treelogistic <- tree |>
  mutate(batpresence = if_else(batpresence == "Y", 1, 0))

view(treelogistic)

m_dbh <- glm(batpresence ~ dbh, data = treelogistic, family = binomial)

p3 <- ggplot(treelogistic, aes(x = dbh, y = batpresence)) +
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "blue", linewidth = 1.2) +
  labs(x = "DBH (cm)",
       y = "Probability of being a roost tree",
       title = "Effect of DBH on Probability of Roost") +
  theme_classic(base_size = 14)

#panel 4: probability of bat presence for decay stage 

m_decay <- glm(batpresence ~ decaystage, data = treelogistic, family = binomial)

p4 <- ggplot(treelogistic, aes(x = as.factor(decaystage), y = batpresence)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "darkgreen") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  labs(x = "Decay Stage",
       y = "Probability of being a roost tree",
       title = "Effect of Decay Stage on Roost Probability") +
  theme_classic(base_size = 14)

####### ---- Combine figures ----
#this was a whole thing because the PCA is not a ggplot thing so it wouldn't combine

library(gridExtra)

pdf("fourpanelfigures.pdf", width = 12, height = 10)

grid.arrange(
  p1,      # Map
  p2_grob, # PCA as image
  p3,      # DBH probability
  p4,      # Decay stage probability
  ncol = 2
)

#################### Close PDF device
dev.off()

