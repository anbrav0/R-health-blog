# Ana Bravo 
# Practice Tidy Models 
# 2022-07-25 


library(skimr)
library(PerformanceAnalytics)
library(splines)
library(tidyverse)
library(tidymodels)

skimr::skim(MASS::Boston)
PerformanceAnalytics::chart.Correlation(MASS::Boston, method = "spearman")
# The black proportion transformation is defined as the squared distance from
#   63%. I have no idea why. I want to "un-transform" this, but I can't find an
#   inverse transformation that uniquely defines the neighborhood proportion of
#   blacks from this scale variable (e.g., values between 0 and 136.9 map back
#   to two different proportions of black inhabitants).
plot(
  x = seq(0, 1, length.out = 50),
  y = 1000 * (seq(0, 1, length.out = 50) - 0.63)^2
)
# In the 1970 Census, Boston had 16.3% Black population. I am assuming there
#   are no homes from neighborhoods with more than 110% Black people (the other
#   solution to 1000 * {x - 0.63}^2 = 218.089. Therefore, an "average" amount of
#   Black people in a Boston neighbourhood would correspond to a score of ~218.
table(MASS::Boston$black > 218)
# https://en.wikipedia.org/wiki/Boston

bostonFull_df <- 
  MASS::Boston %>% 
  mutate(on_river = chas == 1) %>% 
  mutate(is_large_lot_zoned = zn > 0) %>% 
  # 96% of Boston was either white or black in 1970
  mutate(in_white_nbd = black > 218) %>% 
  # 1970s had much higher crime rates than today, but 1/1000 is reasonable for
  #   a "low crime" indicator
  mutate(has_low_crime = crim <= 1) %>% 
  mutate(rad_idx = factor(rad, ordered = TRUE)) %>% 
  rename(value = medv) %>% 
  mutate(log(value)) |> 
  select(-chas, -zn, -crim, -black, -rad) %>% 
  select(value, everything()) %>% 
  as_tibble()

PerformanceAnalytics::chart.Correlation(
  # bostonFull_df, 
  bostonFull_df[, -14],
  method = "spearman"
)



# split data 


set.seed(1995)

housing_split <- initial_split(bostonFull_df)

housing_train <- training(housing_split)
housing_test <- testing(housing_split)

housing_split


#  build the receipt 

housing_receipt <- 
  recipe(formula = value ~ ., data = bostonFull_df) |> 
  step_normalize(all_numeric_predictors()) |> 
  step_zv(all_predictors()) |> 
  step_dummy(all_nominal_predictors())


# do specifications 

kknn_spec <- 
  nearest_neighbor(neighbors = 5) |> 
  set_mode("regression") |> 
  set_engine("kknn")


# do workflow for kknn 

kknn_workflow <- 
  workflow() |> 
  add_recipe(housing_receipt) |> 
  add_model(kknn_spec)

kknn_workflow 


# fit the model in the training set 

housing_fit <- fit(kknn_workflow, housing_train)

housing_fit

# check predictions 

housing_pred <- 
  augment(housing_fit, housing_train) |> 
  select(value, .pred)

housing_pred

