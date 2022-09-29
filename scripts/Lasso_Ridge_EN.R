rm(list = ls())
source("../scripts/train_test_impute.R")

############################0.TUNE PARAMS#######################################
class_tune_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

clas_workflow <- workflow() %>%
  add_recipe(rec_clas) %>%
  add_model(class_tune_spec)
reg_tune_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

reg_workflow <- workflow() %>%
  add_recipe(rec_reg) %>%
  add_model(reg_tune_spec)
###Create grid search for lasso, ridge and elastic net
lasso_param_grid <- expand.grid(
  penalty = seq(0.001, 0.005, length.out = 2),
  mixture = 1
)
ridge_param_grid <- expand.grid(
  penalty = seq(0.001, 0.005, length.out = 2),
  mixture = 0
)
elastic_param_grid <- expand.grid(
  penalty = seq(0.001, 0.005, length.out = 2),
  mixture = seq(0.1, 0.9, length.out = 2)
)
############################1.Classification Model##############################
###Lasso
class_lasso_tune_result <- clas_workflow %>%
  tune_grid(
    grid = lasso_param_grid,
    metrics = metric_set(f_score),
    resamples = validation_split
  )
class_lasso_tune_result %>% collect_metrics()
class_lasso_tune_result %>% show_best()
class_lasso_tune_result_best <- class_lasso_tune_result %>% select_best()
class_lasso_tune_result_best
###Ridge
class_ridge_tune_result <- clas_workflow %>%
  tune_grid(
    grid = ridge_param_grid,
    metrics = metric_set(f_score),
    resamples = validation_split
  )
class_ridge_tune_result %>% collect_metrics()
class_ridge_tune_result %>% show_best()
class_ridge_tune_result_best <- class_ridge_tune_result %>% select_best()
class_ridge_tune_result_best
###Elastic_Net
class_elastic_tune_result <- clas_workflow %>%
  tune_grid(
    grid = elastic_param_grid,
    metrics = metric_set(f_score),
    resamples = validation_split
  )
class_elastic_tune_result %>% collect_metrics()
class_elastic_tune_result %>% show_best()
class_elastic_tune_result_best <- class_elastic_tune_result %>% select_best()
class_elastic_tune_result_best
############################2.Regression Model##################################
###Lasso
reg_lasso_tune_result <- reg_workflow %>%
  tune_grid(
    grid = lasso_param_grid,
    metrics = metric_set(rmse),
    resamples = validation_split
  )
reg_lasso_tune_result %>% collect_metrics()
reg_lasso_tune_result %>% show_best()
reg_lasso_tune_result_best <- reg_lasso_tune_result %>% select_best()
reg_lasso_tune_result_best
###Ridge
reg_ridge_tune_result <- reg_workflow %>%
  tune_grid(
    grid = ridge_param_grid,
    metrics = metric_set(rmse),
    resamples = validation_split
  )
reg_ridge_tune_result %>% collect_metrics()
reg_ridge_tune_result %>% show_best()
reg_ridge_tune_result_best <- reg_ridge_tune_result %>% select_best()
reg_ridge_tune_result_best
###Elastic_Net
reg_elastic_tune_result <- reg_workflow %>%
  tune_grid(
    grid = elastic_param_grid,
    metrics = metric_set(rmse),
    resamples = validation_split
  )
reg_elastic_tune_result %>% collect_metrics()
reg_elastic_tune_result %>% show_best()
reg_elastic_tune_result_best <- reg_elastic_tune_result %>% select_best()
reg_elastic_tune_result_best


