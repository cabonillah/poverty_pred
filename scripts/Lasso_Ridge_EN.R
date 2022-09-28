rm(list=ls())
############Downloading libraries
library("tidyverse")

p_load(dplyr, tidyverse, caret, MLmetrics, tidymodels, themis, glmnet)

######Splitting data between train and test#######
auto_split <- initial_split(data, prop =0.7)
auto_train <- training(auto_split)

##### Setting Cross Validation

Auto_cv <- vfold_cv(auto_train, v=10)

##### Creating the recipe for Poor model and Ingcug model

rec_Pobre <- recipe(Pobre  ~ . -Ingpcug , data = auto_train) %>%
  step_scale(all_numeric())

rec_Ingreso <- recipe(Ingpcug  ~ . -Pobre , data = auto_train) %>%
  step_scale(all_numeric())


############################1.Categorical Model#################################
####Creating Recipe for Ridge mixture =0

ridge_grid <- expand_grid(penalty = seq (0,100, by=10))

ridge_spec <- logistic_reg(penalty = tune(), mixture =0) %>%
  set_engine("glmnet")

ridge_results <- fit_sample(ridge_spec,
                      preprocessor = rec_Pobre,
                      grid = ridge_grid,
                      resample = Auto_cv)

ridge_results %>%
  collect_metrics()%>%
  filter(.metric=="rmse") %>%
  arrenge(mean)


ridge_final_spec <- logistic_reg(penalty = , mixture = 0) %>%
  set_engine("glmnet")

##LOOK auto_split
ridge_fit <- last_fit (ridge_final_spec,
                 rec_Pobre,
                 split =auto_split)
ridge_fit %>%
  collect_metrics()

####Creating Recipe for Lasso mixture =1
lasso_grid <- expand_grid(penalty = seq (0,100, by=10))

lasso_spec <- logistic_reg(penalty = tune(), mixture =1) %>%
  set_engine("glmnet")
lasso_results <- fit_sample(ridge_spec,
                      preprocessor = rec_Pobre,
                      grid = lasso_grid,
                      resample = Auto_cv)

lasso_results %>%
  collect_metrics()%>%
  filter(.metric=="rmse") %>%
  arrenge(mean)


lasso_final_spec <- logistic_reg(penalty = , mixture =1 ) %>%
  set_engine("glmnet")

##LOOK auto_split
lasso_fit <- last_fit (lasso_final_spec,
                 rec_Pobre,
                 split =auto_split)
lasso_fit %>%
  collect_metrics()

####Fitting model and grid and for Elastic Net

ElasticNet_grid <- expand_grid(penalty = seq (0,100, by=10),
                    mixture = seq(0.1,0.9,by =0.1))

ElasticNet_spec <- logistic_reg(penalty = tune(), mixture =tune()) %>%
  set_engine("glmnet")%>% 
  translate()

ElasticNet_results <- fit_sample(ElasticNet_spec,
                      preprocessor = rec_Pobre,
                      grid = ElasticNet_grid,
                      resample = Auto_cv)

ElasticNet_results %>%
  collect_metrics()%>%
  filter(.metric=="F1") %>%
  arrenge(mean)


ElasticNet_final_spec <- logistic_reg(penalty = , mixture = ) %>%
  set_engine("glmnet")
  
##LOOK auto_split
ElasticNet_fit <- last_fit (ElasticNet_final_spec,
                 rec_Pobre,
                 split =auto_split)
ElasticNet_fit %>%
  collect_metrics()




############################2.Regression Model##################################



