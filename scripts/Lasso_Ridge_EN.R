rm(list = ls())
source("../scripts/train_test_impute.R")
library(knitr)
library(kableExtra)

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
tableLasso <- data.frame(Problema= "Clasificación", Modelo = "Lasso", 
                      class_lasso_tune_result %>% show_best())
tableLasso <- tableLasso %>% slice_min(mean)
saveRDS(tableLasso, file = "../stores/BEST_Lasso_Class.rds")
colnames(tableLasso) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableLasso[1:9], digits = 4) %>%
  kable_styling()

###Ridge
class_ridge_tune_result <- clas_workflow %>%
  tune_grid(
    grid = ridge_param_grid,
    metrics = metric_set(f_score),
    resamples = validation_split
  )
class_ridge_tune_result %>% collect_metrics()
class_ridge_tune_result %>% show_best()
tableRidge <- data.frame(Problema= "Clasificación", Modelo = "Ridge", 
                         class_ridge_tune_result %>% show_best())
tableRidge <- tableRidge %>% slice_min(mean)
saveRDS(tableRidge, file = "../stores/BEST_Ridge_Class.rds")
colnames(tableRidge) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableRidge[1:9], digits = 4) %>%
  kable_styling()

###Elastic_Net
class_elastic_tune_result <- clas_workflow %>%
  tune_grid(
    grid = elastic_param_grid,
    metrics = metric_set(f_score),
    resamples = validation_split
  )
class_elastic_tune_result %>% collect_metrics()
class_elastic_tune_result %>% show_best()
tableElasticNet <- data.frame(Problema= "Clasificación", Modelo = "Elastic Net", 
                              class_elastic_tune_result %>% show_best())
tableElasticNet <- tableElasticNet %>% slice_min(mean)
saveRDS(tableElasticNet, file = "../stores/BEST_ElasticNet_Class.rds")
colnames(tableElasticNet) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableElasticNet[1:9], digits = 4) %>%
  kable_styling()
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
tableLassoReg <- data.frame(Problema= "Regresión", Modelo = "Lasso", 
                            reg_lasso_tune_result %>% show_best())
tableLassoReg <- tableLassoReg %>% slice_min(mean)
saveRDS(tableLassoReg, file = "../stores/BEST_Lasso_Res.rds")
colnames(tableLassoReg) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableLassoReg[1:9], digits = 4) %>%
  kable_styling()

###Ridge
reg_ridge_tune_result <- reg_workflow %>%
  tune_grid(
    grid = ridge_param_grid,
    metrics = metric_set(rmse),
    resamples = validation_split
  )
reg_ridge_tune_result %>% collect_metrics()
reg_ridge_tune_result %>% show_best()
tableRidgeReg <- data.frame(Problema= "Regresión", Modelo = "Ridge", 
                            reg_ridge_tune_result %>% show_best())
tableRidgeReg <- tableRidgeReg %>% slice_min(mean)
saveRDS(tableRidgeReg, file = "../stores/BEST_Ridge_Res.rds")
colnames(tableRidgeReg) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableRidgeReg[1:9], digits = 4) %>%
  kable_styling()


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
tableElasticNetReg <- data.frame(Problema= "Regresión", Modelo = "Elastic Net", 
                                 reg_elastic_tune_result %>% show_best())
tableElasticNetReg <- tableElasticNetReg %>% slice_min(mean)
saveRDS(tableElasticNetReg, file = "../stores/BEST_ElasticNet_Res.rds")
colnames(tableElasticNetReg) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableElasticNetReg[1:9], digits = 4) %>%
  kable_styling()
