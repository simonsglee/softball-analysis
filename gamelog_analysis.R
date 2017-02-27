### Analysis performed using KCSA gamelogs from 2011-2016
### 1. Home field advantage
### 2. Park Factors
### 3. Runs scored time series analysis

library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)

# Load gamelog data
gamelog <- read.csv("~/Documents/r_directory/kcsa/kcsa_gamelogs.csv")
gamelog <- as_tibble(gamelog)

# Add total_run column and remove cancelled games
gamelog <- gamelog %>%
    mutate(total_runs = Home_Score + Away_Score) %>%
    filter(total_runs > 0)

### 1. Determine homefield advantage

# remove ties
no_ties <- gamelog %>%  mutate(
    tie = ifelse(Home_Score == Away_Score, TRUE, FALSE),
    home_win = ifelse(Home_Score > Away_Score, TRUE, FALSE)
    ) %>%
    filter(tie == FALSE)

# calculate winning percentage for home teams
mean(no_ties$home_win)

### 2. Park factors

# calculate average runs per game for each park
park_runs <- gamelog %>%
    group_by(Park) %>%
    summarize(
        n_games = n(),
        avg_total_runs = mean(total_runs)
    )

# move this section to plot script
park_runs %>% 
    filter(n_games > 10) %>%
    ggplot(aes(Park, avg_total_runs)) +
    geom_bar(stat = "identity") + coord_flip()

# average runs per game time series




