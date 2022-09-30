rm(list = ls())
source("../scripts/train_test_impute.R")
library(knitr)
library(kableExtra)

##### LINEAL MODEL - REGRESSION PROBLEM #######
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
tableLM <- data.frame(Problema = "Regresión", Modelo = "LINEAL", 
                      Penalidad = "N/A", Mixtura = "N/A",
                      tune_resultLM %>% show_best())
tableLM <- tableLM %>% slice_min(mean)
saveRDS(tableLM, file = "../stores/BEST_LM_Res.rds")
colnames(tableLM) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempeño", "Estimador","Media", "Fold", "s.d Error")
kable(tableLM[1:9], digits = 4) %>%
  kable_styling()