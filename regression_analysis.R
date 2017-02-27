### Korean Christian Softball League
### Regression Analysis 2011-2016
library(tidyverse)
library(ggplot2)
rm(list = ls())

# load data
kcsa <- read.csv("~/Documents/r_directory/kcsa/kcsa.csv")
kcsa <- as_tibble(kcsa)

# Make adjusted wins and losses for ties
kcsa <- kcsa %>%
    mutate(
        adj.W = W + 0.5*T,
        adj.L = L + 0.5*T,
        adj.WPER = adj.W / GP,
        adj.DIFF = DIFF / GP
    ) %>%
    select(Season:GP, adj.W, adj.L, adj.WPER, adj.DIFF, W:DIFF,-D)

# Fit a linear regression model
lm <- with(kcsa, lm(adj.WPER ~ adj.DIFF))
summary(lm)

# Filter 2016 season
kcsa16 <- kcsa %>%
    filter(Season == 2016)

# Calculate expected win prediction from linear regression model
kcsa16$Expected.W <- predict(lm, kcsa16)*10

# Calculate column for over/under performing run differential
kcsa16_diff <- kcsa16 %>%
    mutate(Luck = adj.W - Expected.W) %>%
    select(Season:adj.W, Expected.W, Luck, everything()) %>%
    arrange(desc(Expected.W))

kcsa16_diff %>%
    print(n = 32)


