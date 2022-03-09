library(tidyverse)
library(readxl)
library(rsample)
library(recipes)
library(h2o)
product_backorders_tbl <- read.csv("C:/Bhavana/LECTURES/Sem 3/ds_basics-BhavanaSirumalla/product_backorders.csv")

# Data split

set.seed(seed = 1113)

split_obj <- rsample::initial_split(product_backorders_tbl, prop = 0.85)

train_product_backorders_tbl <- training(split_obj)

test_product_backorders_readable_tbl  <- testing(split_obj)





#2. Specify the response and predictor variables ---

recipe_obj <- recipe(went_on_backorder ~., data = product_backorders_tbl) %>%
  
  step_zv(all_predictors()) %>%
  
  #step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>%
  
  prep()



train_tbl <- bake(recipe_obj, new_data = train_product_backorders_tbl)

test_tbl  <- bake(recipe_obj, new_data = test_product_backorders_readable_tbl)





#3. Run AutoML specifying the stopping criterion ---

h2o.init() #Modelling

split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]

valid_h2o <- split_h2o[[2]]

test_h2o  <- as.h2o(test_tbl)



y <- "went_on_backorder" #target

x <- setdiff(names(train_h2o), y) #preductors

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

#4. View the leader board
typeof(automl_models_h2o)
slotNames(automl_models_h2o)
automl_models_h2o@leaderboard
automl_models_h2o@leader






