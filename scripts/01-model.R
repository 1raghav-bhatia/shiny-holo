#### Preamble ####
# Purpose: To create the Multiple linear regression model.
# Author: Raghav Bhatia 
# Date: 02 April 2024
# Contact: raghav.bhatia@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the cleaned dataset.



#### Workspace setup ####
library(boot)
library(broom.mixed)
library(collapse)
library(dataverse)
library(janitor)
library(knitr)
library(marginaleffects)
library(modelsummary)
library(rstanarm)
library(tidybayes)
library(tidyverse)
library(arrow)
library(nflverse)
library(tidymodels)
library(nflreadr)
library(ggrepel)

#### Read data ####
nfl_data <- read_csv("data/cleaned_data/qb_regular_season_data.csv")
current_season_nfl_data <- read_csv("data/cleaned_data/qb_regular_current_season_data.csv")

### Mathematical Model ###

## \begin{align}
## \text{future EPA}_i|\mu_i &\sim \mbox{Normal}(\mu_i, \sigma_i) \\
## \mu_i &=  \beta_0 \, + \, \beta_1 \times \text{air yards per attempt}_i 
## \, + \, \beta_2 \times \text{sack rate}_i \, +  \, \beta_3 \times \text{dakota}_i \\
## &\quad \, + \, \beta_4 \times \text{EPA prior}_i \\
## \beta_0 &\sim \text{Normal}(0, 2.5) \\
## \beta_1 &\sim \text{Normal}(0, 2.5) \\
## \beta_2 &\sim \text{Normal}(0, 2.5) \\
## \beta_3 &\sim \text{Normal}(0, 2.5) \\
## \beta_4 &\sim \text{Normal}(0, 2.5)
## \end{align} 


### Model data ###

set.seed(853)

## EPA prediction model ##

# This glm regresses week 10-18 EPA on air_yards, sack_rate, Dakota, and weeks 1-9 EPA.

epa_prediction_model <-
  linear_reg() |>
  set_engine(engine  = "lm") |>
  fit(
    EPA_post ~ Air_Yards + Sack_Rate + Dakota + EPA_prior,
    data = nfl_data
  )
predictions <- epa_prediction_model |>
  predict(new_data = current_season_nfl_data)

predicted_dataset <- cbind(current_season_nfl_data, predictions) |>
  mutate(EPA_future = .pred) |>
  select(-.pred)


#### Save model ####

saveRDS(
  epa_prediction_model,
  file = "models/epa_prediction_model.rds"
)


