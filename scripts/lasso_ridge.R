rm(list=ls())

setwd("C:\\Users\\juan.velasquez\\OneDrive - Universidad de los Andes\\Maestria\\Semestres\\2022-2\\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\\Talleres\\Problem-Set2\\poverty_pred")
# Charging libraries tidyverse 
library("tidyverse")
library("stargazer")
library("skimr")
library("pacman")
library("kableExtra")
p_load(dplyr, tidyverse, caret, MLmetrics, tidymodels, themis, glmnet)


data <- readRDS("./stores/data.rds")


# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.7)
train <- data_split %>% training()
test <- data_split %>% testing()

# Impute train and test sets
rec <- recipe(Pobre ~ ., data = train)

impute <- rec %>%
  step_impute_mode(
    P6920, P7040, P7090, P7505, P6210
  ) %>%
  prep()

imputed_train <- impute %>% bake(new_data = NULL)

imputed_test <- impute %>% bake(new_data = test)

glimpse(imputed_train)
glimpse(imputed_test)

variables_numericas <- c("P5010", "P6020", "P6040","P6090","P5000","Oc","Nper")

for (v in variables_numericas) {
  imputed_train[, v] <- as.numeric(imputed_train[, v, drop = T])
}

for (v in variables_numericas) {
  imputed_test[, v] <- as.numeric(imputed_test[, v, drop = T])
}
##################Scaling Numeric variables####################################

cols = c('Nper', "P5010", "P6020", "P6040")

pre_proc_val <- preProcess(imputed_train[,cols], method = c("center", "scale"))

imputed_train[,cols] = predict(pre_proc_val, imputed_train[,cols])
imputed_test[,cols] = predict(pre_proc_val, imputed_test[,cols])

skim(imputed_train)


# Then we make dummys from the base
df1_train <- model.matrix(~ Pobre + Depto + P5000 +poly(P5000,3) +
                            P5010 +poly(P5010,3) + P5090 + P6920 + P7040 + 
                            P7090 + P7505 + 
                            P6020+ poly(P6020,3) + P6090 + poly(P6090,3) + P6040 +poly(P6040,3)+ Oc +poly(Oc,3)+
                            P6210 + Nper+ poly(Nper,3)-Ingpcug, imputed_train) %>%
  as.data.frame()

df1_test <- model.matrix(~ Pobre + Depto + P5000 +poly(P5000,3) +
                           P5010 +poly(P5010,3) + P5090 + P6920 + P7040 + 
                           P7090 + P7505 + 
                           P6020+ poly(P6020,3) + P6090 + poly(P6090,3) + P6040 +poly(P6040,3)+ Oc +poly(Oc,3)+
                           P6210 + Nper+ poly(Nper,3)-Ingpcug, imputed_test) %>%
  as.data.frame()

df2_train <- model.matrix(~ Ingpcug + Depto + P5000 +poly(P5000,3) +
                            P5010 +poly(P5010,3) + P5090 + P6920 + P7040 + 
                            P7090 + P7505 + 
                            P6020+ poly(P6020,3) + P6090 + poly(P6090,3) + P6040 +poly(P6040,3)+ Oc +poly(Oc,3)+
                            P6210 + Nper+ poly(Nper,3)-Pobre, imputed_train) %>%
  as.data.frame()

df2_test <- model.matrix(~ Ingpcug + Depto + P5000 +poly(P5000,3) +
                            P5010 +poly(P5010,3) + P5090 + P6920 + P7040 + 
                            P7090 + P7505 + 
                            P6020+ poly(P6020,3) + P6090 + poly(P6090,3) + P6040 +poly(P6040,3)+ Oc +poly(Oc,3)+
                            P6210 + Nper+ poly(Nper,3)-Pobre, imputed_test) %>%
  as.data.frame()


y_train <- df1_train[,"Pobre"]
X_train <- select(df1_train, -Pobre)
y_test <- df1_test[,"Pobre"]
X_test <- select(df1_test, -Pobre)



# Para obtener un ajuste con regularización Lasso se indica argumento alpha = 1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo_lasso <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 1,
  nlambda = 300,
  standardize = FALSE
)

# Analicemos cómo cambian los coeficientes para diferentes lambdas
regularizacion <- modelo_lasso$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo_lasso$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10",
                                  scales::math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización (Lasso)", x = "Lambda", y = "Coeficientes") +
  theme_bw() +
  theme(legend.position="bottom")


# ¿Cómo escoger el mejor lambda? 
# Veamos cuál es el mejor prediciendo (fuera de muestra)
# En este caso vamos a crear la predicción para cada uno de los
# 300 lambdas seleccionados
predicciones_lasso <- predict(modelo_lasso, 
                              newx = as.matrix(X_test))
lambdas_lasso <- modelo_lasso$lambda

# Cada predicción se va a evaluar
resultados_lasso <- data.frame()
for (i in 1:length(lambdas_lasso)) {
  l <- lambdas_lasso[i]
  y_hat_out2 <- predicciones_lasso[, i]
  r22 <- R2_Score(y_pred = y_hat_out2, y_true = y_test)
  rmse2 <- RMSE(y_pred = y_hat_out2, y_true = y_test)
  resultado <- data.frame(Modelo = "Lasso",
                          Muestra = "Fuera",
                          Lambda = l,
                          R2_Score = r22, 
                          RMSE = rmse2)
  resultados_lasso <- bind_rows(resultados_lasso, resultado)
}

ggplot(resultados_lasso, aes(x = Lambda, y = RMSE)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

ggplot(resultados_lasso, aes(x = Lambda, y = R2_Score)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(labels = scales::comma)

filtro <- resultados_lasso$RMSE == min(resultados_lasso$RMSE)
mejor_lambda_lasso <- resultados_lasso[filtro, "Lambda"]

# Guardamos el mejor Lasso
y_hat_in2 <- predict.glmnet(modelo_lasso,
                            newx = as.matrix(X_train),
                            s = mejor_lambda_lasso)
y_hat_out2 <- predict.glmnet(modelo_lasso,
                             newx = as.matrix(X_test),
                             s = mejor_lambda_lasso)

# Métricas dentro y fuera de muestra. Paquete MLmetrics
r2_in2 <- R2_Score(y_pred = exp(y_hat_in2), y_true = exp(y_train))
rmse_in2 <- RMSE(y_pred = exp(y_hat_in2), y_true = exp(y_train))

r2_out2 <- R2_Score(y_pred = exp(y_hat_out2), y_true = exp(y_test))
rmse_out2 <- RMSE(y_pred = exp(y_hat_out2), y_true = exp(y_test))

# Guardamos el desempeño
resultados2 <- data.frame(Modelo = "Lasso", 
                          Muestra = "Dentro",
                          R2_Score = r2_in2, RMSE = rmse_in2) %>%
  rbind(data.frame(Modelo = "Lasso", 
                   Muestra = "Fuera",
                   R2_Score = r2_out2, RMSE = rmse_out2))





