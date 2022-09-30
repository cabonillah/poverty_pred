rm(list = ls())
source("../scripts/train_test_impute.R")
library(knitr)
library(kableExtra)

##### LOGIT MODEL - CLASSIFICATION PROBLEM #######


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
tableLOG <- data.frame(Problema = "Clasificación", Modelo = "LOGIT", 
                       Penalidad = "N/A", Mixtura = "N/A", 
                       tune_resultLOG %>% show_best())
tableLOG <- tableLOG %>% slice_min(mean)
saveRDS(tableLOG, file = "../stores/BEST_LOGIT_Class.rds")
colnames(tableLOG) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempe?o", "Estimador","Media", "Fold", "s.d Error")
kable(tableLOG[1:9], digits = 4) %>%
  kable_styling()