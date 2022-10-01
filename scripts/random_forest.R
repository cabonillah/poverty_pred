rm(list = ls())
source("../scripts/train_test_impute.R")
library(tidymodels)
install.packages("ranger")
#devtools::install_github("imbs-hl/ranger")

###############################Random Forest####################################

train %>% 
  count(Pobre) %>% 
  mutate(prop = n/sum(n))

class_rf_grid <- expand.grid(
  mtry = c(1,5), min_n = 1:3
)


reg_rf_grid <- expand.grid(
  mtry = c(1,5), min_n = 1:3
)

##########################1.Classification Model################################

class_rf_tuner <- 
  rand_forest(trees = 300,engine = "ranger", 
              mtry = tune(),
              min_n = tune(),
              ) %>% 
  set_mode("classification")

#rf_mod <- 
#  rand_forest(engine = "ranger") %>% 
#  set_mode("classification")

class_rf_wf <- workflow() %>%
  add_recipe(rec_clas) %>%
  add_model(class_rf_tuner)
  
  set.seed(456)
  
  class_rf_results<- 
    class_rf_wf  %>% 
    tune_grid(
      grid = class_rf_grid,
      resamples = validation_split
    )
  
  collect_metrics(class_rf_results)

  class_rf_results %>% 
    show_best(metric = "roc_auc", n = 5)
  
  class_rf_results %>% autoplot()
  
  alz_best <-
    class_rf_results %>% 
    select_best(metric = "roc_auc")
  alz_best
  
  last_rf_workflow <- 
    class_rf_wf %>%
    finalize_workflow(alz_best)
  
  
##########################2.Regression Model####################################
  
  reg_rf_tuner <- 
    rand_forest(trees = 300,engine = "ranger", 
                mtry = tune(),
                min_n = tune(),
               ) %>% 
    set_mode("regression")
  
  reg_rf_wf <- workflow() %>%
    add_recipe(rec_reg) %>%
    add_model(reg_rf_tuner)
  
  set.seed(456)
  
  reg_rf_results<- 
    reg_rf_wf  %>% 
    tune_grid(
      grid = reg_rf_grid,
      resamples = validation_split
    )
  
  collect_metrics(reg_rf_results)
  
  reg_rf_results %>% 
    show_best(metric = "rmse", n = 5)
  
  reg_rf_results %>% autoplot()
  
  alz_best <-
    reg_rf_results %>% 
    select_best(metric = "rmse")
  alz_best
  
  last_rf_workflow <- 
    class_rf_wf %>%
    finalize_workflow(alz_best)
  