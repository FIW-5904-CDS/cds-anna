# =====================================================
# Data Simulation Assignment
# Anna Reachmack
# FIW5594: Conservation Data Science
# =====================================================

# Load packages ----
library(tidyverse)

# =====================================================
# 1. Simulate one variable
# =====================================================

# GOAL: Simulate 40 trees and measure DBH

set.seed(1)   # makes results reproducible #you get the same thing the next time you do a random draw

# Sample size and parameters
n <- 40
mean_dbh <- 10 #mean DBH, a characteristic of the trees that contributed the most to the variance found in PCA
sd_dbh <- 5

# Draw random DBH measurements from a Normal distribution
treedbh <- rnorm(n, mean = mean_dbh, sd = sd_dbh)


# Combine into a tibble
sim <- tibble(id = 1:n, treedbh = treedbh)

# Look at the data
print(sim)
summary(sim$treedbh) #mean from random draw is 11.177, a llittle higher than the sample parameter I set but similar

# Visualize with a histogram
ggplot(sim, aes(x = treedbh)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Simulated tree DBH measurements", x = "DBH", y = "Count") +
  theme_classic()

# =====================================================
# 2. Add a treatment effect
# =====================================================

# GOAL: Simulate a simple experimental effect on DBH.

set.seed(1)

# Create a treatment variable (half younger than 10 years or young, half older than 10 years)
n <- 40
treatment <- rep(c("young", "old"), each = n/2)


# set means for each group
mean_young <- 5
mean_old <- 15
sd_both <- 5

# Simulate body sizes with different means by treatment
treedbhexperiment <- rnorm(
  n = n,
  mean = if_else(treatment == "young", mean_young, mean_old), #if the treatment is bat yes, then use the mean_batyes. If not batyes, use the mean_batno for the mean
  sd = sd_both
)

# Combine into tibble
(sim2 <- tibble(treatment, treedbhexperiment))

# Visualize
ggplot(sim2, aes(x = treatment, y = treedbhexperiment)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Simulated DBH measurements by age",
       x = "Age", y = "DBH") +
  theme_classic()

# Quick summaries
sim2 |>
  group_by(treatment) |>
  summarise(mean_dbh = mean(treedbhexperiment), sd_size = sd(body_size), n = n())


# ======================================================
# 3. Add a random site effect (optional)
# =====================================================

# GOAL: Add realistic variation among sites.

set.seed(1)

# Define number of sites and individuals per site
n_site <- 4
n_per_site <- 10
n <- n_site * n_per_site

site <- rep(1:n_site, each = n_per_site)

# Each site gets its own random offset (site effect)
site_effect <- rnorm(n_site, mean = 0, sd = 0.5)[site] # the [site] part is indexing by the site vector here (e.g. for site = 1, it pulls out the first value from rnorm, site 2 has 2nd value and so on) #this is assigning a sd value to each site so show the slight difference with each site. To calculate the site effect

# baseline mean and residual sd
mean_overall <- 10
resid_sd <- 1

# Generate body sizes including site effects
treedbh <- rnorm(n, mean = mean_overall + site_effect, sd = resid_sd)

# Combine into tibble
(sim3 <- tibble(site = factor(site), treedbh))

# Visualize variation among sites
ggplot(sim3, aes(x = site, y = body_size)) +
  geom_boxplot(fill = "plum") +
  labs(title = "Simulated body sizes by site",
       x = "Site", y = "Body size (cm)") +
  theme_classic()

# Check site means
sim3 |>
  group_by(site) |>
  summarise(mean_size = mean(body_size), sd_size = sd(body_size))


