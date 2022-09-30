rm(list = ls())

library(knitr)
library(kableExtra)

BEST_LOGIT_Class <- readRDS("../stores/BEST_LOGIT_Class.rds")
BEST_LASSO_Class <- readRDS("../stores/BEST_Lasso_Class.rds")
BEST_RIDGE_Class <-readRDS("../stores/BEST_Ridge_Class.rds")
BEST_ELASTICNET_Class <-readRDS("../stores/BEST_ElasticNet_Class.rds")
BEST_LM_Res <- readRDS("../stores/BEST_LM_Res.rds")
BEST_LASSO_Res <- readRDS("../stores/BEST_Lasso_Res.rds")
BEST_RIDGE_Res <- readRDS("../stores/BEST_Ridge_Res.rds")
BEST_ELASTICNET_Res <- readRDS("../stores/BEST_ElasticNet_Res.rds")

resultados1 <- rbind(BEST_LOGIT_Class[1:9], BEST_LM_Res[1:9])
colnames(resultados1) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempe?o", "Estimador","Media", "Fold", "s.d Error")

resultados2 <- rbind(BEST_LASSO_Class[1:9], BEST_RIDGE_Class[1:9], BEST_ELASTICNET_Class[1:9],BEST_LASSO_Res[1:9], BEST_RIDGE_Res[1:9], BEST_ELASTICNET_Res[1:9] )
colnames(resultados2) <- c("Problema", "Modelo","Penalidad","Mixtura", "Medida de desempe?o", "Estimador","Media", "Fold", "s.d Error")

resultados <- rbind(resultados1, resultados2)

kable(resultados, digits = 4) %>%
  kable_styling()

stargazer(resultados, dep.var.labels = c("Ln(income)"), out = "../views/Best_Models")
