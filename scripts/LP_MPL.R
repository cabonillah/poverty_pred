rm(list = ls())
source("../scripts/train_test_impute.R")

### LM 
tune_specLM <- linear_reg() %>%
  set_engine("lm")

workflowLM <- workflow() %>%
  add_recipe(rec_reg) %>%
  add_model(tune_specLM)

# doParallel::registerDoParallel(7)

tune_resultLM <- workflowLM %>%
  tune_grid(
    metrics = metric_set(rmse),
    resamples = validation_split
  )

tune_resultLM%>% collect_metrics()
tune_resultLM %>% show_best()
tune_bestLM <- tune_resultLM %>% select_best()
tune_bestLM

###### LOGIT ######

tune_specLOG <- logistic_reg() %>%
  set_engine("glm")

workflowLOG <- workflow() %>%
  add_recipe(rec_clas) %>%
  add_model(tune_specLOG)

# doParallel::registerDoParallel(7)

tune_resultLOG <- workflowLOG %>%
  tune_grid(
    metrics = metric_set(f_score),
    resamples = validation_split
  )

tune_resultLOG %>% collect_metrics()
tune_resultLOG %>% show_best()
tune_bestLOG <- tune_resultLOG %>% select_best()
tune_bestLOG
