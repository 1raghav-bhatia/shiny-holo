#### Preamble ####
# Purpose: Downloads and cleans the regular season dataset from the nflverse library.
# Author: Raghav Bhatia
# Date: 2 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(dataverse)
library(janitor)
library(knitr)
library(readr)
library(nflverse)
library(arrow)

#### Load data ####

## Loading the regular season dataset from the nflverse library

qb_regular_season_stats <- 
  load_player_stats(seasons = TRUE) |> 
  filter(season_type == "REG" & position == "QB")

qb_regular_season_stats_data <- qb_regular_season_stats |>
  mutate(air_yards_per_attempt = passing_air_yards / attempts,
         sack_rate = ifelse(attempts != 0, sacks / attempts, 0),
         week_1_to_9 = ifelse(1 <= week & week <= 9, 1, 0),
         epa_1_to_9 = ifelse(week_1_to_9 == 1, passing_epa, 0),
         epa_10_to_18 = ifelse(week_1_to_9 == 0, passing_epa, 0)
  ) |> 
  group_by(recent_team, season) |>
  summarise(Air_Yards = mean(air_yards_per_attempt, na.rm = TRUE),
            Sack_Rate = mean(sack_rate, na.rm = TRUE),
            Dakota = mean(dakota, na.rm = TRUE),
            EPA_prior = sum(epa_1_to_9, na.rm = TRUE) / sum(week_1_to_9, na.rm = TRUE),
            EPA_post = sum(epa_10_to_18, na.rm = TRUE) / (n() - sum(week_1_to_9, na.rm = TRUE))
  )

qb_regular_season_stats_data_18_to_22 <- 
  qb_regular_season_stats_data |>
  filter(2018 <= season & season <= 2022)

qb_regular_season_stats_data_2023 <- 
  qb_regular_season_stats_data |>
  filter(season == 2023) |>
  select(-EPA_post)


#### Save data ####

write_csv(qb_regular_season_stats_data_18_to_22,
              "data/cleaned_data/qb_regular_season_data.csv")
write_csv(qb_regular_season_stats_data_2023,
          "data/cleaned_data/qb_regular_current_season_data.csv")

