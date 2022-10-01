rm(list = ls())
source("../scripts/train_test_impute.R")
library(tidymodels)
install.packages("ranger")
install.packages("xgboost")


class_xgb_spec <- boost_tree(
  trees = 500, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),stop_iter = 5                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

class_xgb_spec


class_xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30
)

class_xgb_grid



##########################1.Classification Model################################


class_xgb_wf <- workflow() %>%
  add_recipe(rec_clas) %>%
  add_model(class_xgb_spec)



doParallel::registerDoParallel()

set.seed(234)

class_xgb_res <- class_xgb_wf %>% 
  tune_grid(
    grid = class_xgb_grid,
    resamples = validation_split,
    control = control_grid(save_pred = TRUE)
  )
  

###Collecting metrics

collect_metrics(class_xgb_res)


library(vip)

final_xgb %>%
  fit(data = vb_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")