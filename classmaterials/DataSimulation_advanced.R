#  FWC 5584: Conservation Data Science
#  Advanced Data Simulation - linear models assuming various distributions
# =====================================================

library(tidyverse)
set.seed(1)  # make results reproducible

# ---- Choose sample size & structure ----
N <- 200                               # total rows
x1_levels <- c("control","treatment")  # levels for categorical X1
n_per <- N / length(x1_levels)
X1 <- rep(x1_levels, each = n_per)     # categorical predictor
X2 <- runif(N, 0, 100)                 # continuous predictor

# Optional grouping
use_group <- FALSE                      # set TRUE to run, FALSE to skip

if (use_group) {
  n_group <- 10
  group <- rep(1:n_group, length.out = N)
  group_effect <- rnorm(n = n_group, mean = 0, sd = 0.5)[group]
} else {
  group <- NA
  group_effect <- 0
}

# Helper: convert X1 to numeric effect (0/1)
x1_eff <- if_else(X1 == "treatment", 1, 0)

# =====================================================
#  PICK ONE RESPONSE FAMILY BELOW
# =====================================================

# ---- A) Continuous (Normal) ----
# parameters
beta0 <- 10     #intercept
beta1 <- 2      #coefficient for x1
beta2 <- 0.05   #coefficient for x2
sigma <- 1.5    #standard deviation

mu <- beta0 + beta1*x1_eff + beta2*X2 + group_effect

Y <- rnorm(n = N, mean = mu, sd = sigma)

# ---- B) Count (Poisson) ----
# parameters
beta0 <- 1.5 #intercept
beta1 <- 0.4 #coefficient for x1
beta2 <- 0.01 #coefficient for x2

eta <- beta0 + beta1*x1_eff + beta2*X2 + group_effect

Y <- rpois(n = N, lambda = exp(eta))

# ---- C) Binary (Bernoulli) ----
# parameters
beta0 <- -1
beta1 <- 1
beta2 <- 0.02

p <- plogis(beta0 + beta1*x1_eff + beta2*X2 + group_effect)

Y <- rbinom(n = N, size = 1, prob = p) #size = number of trials

# ---- D) Proportion (Beta) ----
# parameters
beta0 <- -1
beta1 <- 0.8
beta2 <- 0.01
phi <- 20

mu <- plogis(beta0 + beta1*x1_eff + beta2*X2 + group_effect)

a <- mu * phi
b <- (1 - mu) * phi

Y <- rbeta(n = N, shape1 = a, shape2 = b) #Beta distribution has two shape parameters

# =====================================================
#  Build tibble & explore
# =====================================================
sim <- tibble(X1, X2, group = factor(group), Y)

# Quick visual
ggplot(sim, aes(X1, Y)) + 
  geom_boxplot()

ggplot(sim, aes(X2, Y, color = X1)) + 
  geom_point(alpha = 0.6)

# Quick summaries
sim |> 
  summarise(mean_Y = mean(Y), sd_Y = sd(Y), n = n())

sim |> 
  group_by(X1) |> 
  summarise(mean_Y = mean(Y), n = n())

# =====================================================
#  Try changing beta values, sigma, or group_effect SD
# =====================================================
