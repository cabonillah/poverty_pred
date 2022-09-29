rm(list = ls())
source("../scripts/train_test_impute.R")
library(knitr)
library(kableExtra)

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
tableLM <- data.frame(Modelo = "LINEAL", 
                      tune_resultLM %>% show_best())

                       
                          
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
tableLOG <- data.frame(Modelo = "LOGIT", 
                       tune_resultLOG %>% show_best())

resultados <- rbind(tableLM[1:6], tableLOG[1:6])
colnames(resultados) <- c("Modelo", "Medida de desempeño", "Estimador","Media", "n", "s.d Error")

kable(resultados, digits = 2) %>%
  kable_styling()




