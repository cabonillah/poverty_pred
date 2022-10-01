rm(list = ls())

library(knitr)
library(kableExtra)
library(skimr)
#saveRDS(data, file = "../stores/datadescriptive.rds") 
#### Hitogram chart ln(ingpcug) ######

datadescriptive <- readRDS("../stores/datadescriptive.rds")
gglog_Ingpcug <- ggplot(datadescriptive, aes(x = Ingpcug)) +
  geom_histogram(bins = 50, fill = "blue") +
  ggtitle("Ingreso percápita del hogar") +
  labs(x = "Log(Ingreso percápita hogar)", y = "Cantidad") +
  theme_bw()
ggsave("../views/Log_Ingpcup.png",gglog_Ingpcug)

#### Table Descriptive statistics ######
install.packages("xtable")
library(xtable)
library(stargazer)



sumstat <- datadescriptive %>%
  
  # Select and rename five variables 
  select(
    `log del ingreso` = Ingpcug,
    `Linea de probreza` = Lp,
    `Proporcion personas hogar` = Nper_poly_1,
    `Proporcion ocupados` = Oc_poly_1,
    `Numero cuartos hogar` = P5000_poly_1,
    `Numero de dormitorios` = P5010_poly_1,
    `Proporcion mujeres` = P6020_poly_1,
    `Proporcion afiliados a salud` = P6090_poly_1,
    `Edad promedio personas hogar` = P6040_poly_1
  ) %>%
  
  # Find the mean, st. dev., min, and max for each variable 
  summarise_each(funs(mean, sd, min, max)) %>%
  
  # Move summary stats to columns
  gather(key, value, everything()) %>% 
  separate(key, into = c("variable", "stat"), sep = "_") %>%
  spread(stat, value) %>%
  
  # Set order of summary statistics 
  select(variable, mean, sd, min, max) %>%
  
  # Round all numeric variables to one decimal point
  mutate_each(funs(round(., 4)), -variable)

sumstat
stargazer(sumstat, out = "../views/continuas_descrip.tex", summary = FALSE)


datadescriptive2 <- readRDS("../stores/data.rds")

sumstat2 <- datadescriptive2 %>%
  
  # Select and rename five variables 
  select(
    `Clasificacion pobre` = Pobre,
    `Cotizacion pension` = P6920,
    `Trabajo semana anterior` = P7040,
    `Dinero proveniente otras fuentes` = P7505,
    `Departamentos` = Departamento,
    `Propiedad vivienda` = Propiedad,
    `Max nivel educacion` = Educ
  ) 

sumstat2 <- skim(sumstat2) %>% dplyr::select(skim_variable, n_missing, complete_rate)

stargazer(aDR , out = "../views/categoricas_descrip.tex", summary = FALSE)


#### Final results models #####
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
