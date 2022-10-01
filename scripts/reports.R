rm(list = ls())

library("tidyverse")
library("skimr")
library("stargazer")

#### Hitogram chart ln(ingpcug) ######

data <- readRDS("../stores/data.rds")
ingpcug <- ggplot(data, aes(x = Ingpcug)) +
  geom_histogram(bins = 50, fill = "blue") +
  ggtitle("Ingreso percápita del hogar") +
  labs(x = "Log(Ingreso percápita hogar)", y = "Cantidad") +
  theme_bw()
ggsave("../views/Log_Ingpcup.png", ingpcug)

#### Table Descriptive statistics ######
sumstat <- data %>%
  # Select and rename five variables
  select(
    `log del ingreso` = Ingpcug,
    `Linea de probreza` = Lp,
    `Proporcion personas hogar` = Nper,
    `Proporcion ocupados` = Oc,
    `Numero cuartos hogar` = P5000,
    `Numero de dormitorios` = P5010,
    `Proporcion mujeres` = P6020,
    `Proporcion afiliados a salud` = P6090,
    `Edad promedio personas hogar` = P6040
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

add_over_ratio <- function(df) {
  if ("over_ratio" %in% colnames(df)) {
    df <- df %>% mutate(over_ratio = as.character(over_ratio))
  } else {
    df <- df %>% mutate(over_ratio = "N/A")
  }
  df
}
var_selection <- function(df) {
  if ("mixture" %in% colnames(df)) {
    df <- df %>% select(-penalty, -mixture, -.config, -n, -.estimator)
  } else {
    df <- df %>% select(-.config, -n, -.estimator)
  }
  df
}

#### Final results models #####
lm_reg <- readRDS("../stores/lm_reg.rds")
lm_reg <- add_over_ratio(lm_reg)
lm_reg <- var_selection(lm_reg)

lm_clas <- readRDS("../stores/lm_clas.rds")
lm_clas <- add_over_ratio(lm_clas)
lm_clas <- var_selection(lm_clas)


lasso_reg <- readRDS("../stores/lasso_reg.rds")
lasso_reg <- add_over_ratio(lasso_reg)
lasso_reg <- var_selection(lasso_reg)

lasso_clas <- readRDS("../stores/lasso_clas.rds")
lasso_clas <- add_over_ratio(lasso_clas)
lasso_clas <- var_selection(lasso_clas)


ridge_reg <- readRDS("../stores/ridge_reg.rds")
ridge_reg <- add_over_ratio(ridge_reg)
ridge_reg <- var_selection(ridge_reg)

ridge_clas <- readRDS("../stores/ridge_clas.rds")
ridge_clas <- add_over_ratio(ridge_clas)
ridge_clas <- var_selection(ridge_clas)


elastic_reg <- readRDS("../stores/elastic_reg.rds")
elastic_reg <- add_over_ratio(elastic_reg)
elastic_reg <- var_selection(elastic_reg)

elastic_clas <- readRDS("../stores/elastic_clas.rds")
elastic_clas <- add_over_ratio(elastic_clas)
elastic_clas <- var_selection(elastic_clas)


rf_reg <- readRDS("../stores/rf_reg.rds")
rf_reg <- var_selection(rf_reg)

rf_clas <- readRDS("../stores/rf_clas.rds")
rf_clas <- var_selection(rf_clas)
rf_clas <- rf_clas %>% select(-over_ratio)

xgb_reg <- readRDS("../stores/xgb_reg.rds")
xgb_reg <- var_selection(xgb_reg)

xgb_clas <- readRDS("../stores/xgb_clas.rds")
xgb_clas <- var_selection(xgb_clas)
xgb_clas <- xgb_clas %>% select(-over_ratio)

linear <- bind_rows(
  lm_reg,
  lm_clas,
  ridge_reg,
  ridge_clas,
  lasso_reg,
  lasso_clas,
  elastic_reg,
  elastic_clas
) %>% rename(Metrica = .metric, Valor = mean, Error = std_err)

rf <- bind_rows(
  rf_reg,
  rf_clas
) %>% rename(Metrica = .metric, Valor = mean, Error = std_err)

xgb <- bind_rows(
  xgb_reg,
  xgb_clas
) %>% rename(Metrica = .metric, Valor = mean, Error = std_err)

stargazer(linear, out = "../views/linear_report.tex", summary = FALSE)
stargazer(rf, out = "../views/rf_report.tex", summary = FALSE)
stargazer(xgb, out = "../views/xgb_report.tex", summary = FALSE)
