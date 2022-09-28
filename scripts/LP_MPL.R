install.packages("skimr")
library(skimr)
skim(imputed_train)

#### LM ######
imputed_train$Ingpcug <- imputed_train$Ingpcug+1e-15
imputed_test$Ingpcug <- imputed_test$Ingpcug+1e-15
mod_LM1 <- lm(log(Ingpcug) ~. , data = imputed_train)
summary(mod_LM1)

# Evaluamos el modelo de regresión lineal

install.packages("MLmetrics")
library(MLmetrics)
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)


y_hat_in1 <- predict(mod_LM1, newdata = imputed_train)
y_hat_out1 <- predict(mod_LM1, newdata = imputed_test)

# Convertimos la probabilidad en una predicción

acc_insample1 <- Accuracy(y_pred = y_hat_in1, y_true = imputed_train$Ingpcug)
acc_outsample1 <- Accuracy(y_pred = y_hat_out1, y_true = imputed_test$Ingpcug)

pre_insample1 <- Precision(y_pred = y_hat_in1, y_true = imputed_train$Ingpcug, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_out1, y_true = imputed_test$Ingpcug, positive = 1)

rec_insample1 <- Recall(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)

f1_insample1 <- F1_Score(y_pred = y_hat_insample1, y_true = train$infielTRUE, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, y_true = test$infielTRUE, positive = 1)

metricas_insample1 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample1,
                                 "Precision" = pre_insample1,
                                 "Recall" = rec_insample1,
                                 "F1" = f1_insample1)

metricas_outsample1 <- data.frame(Modelo = "Regresión lineal", 
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample1,
                                  "Precision" = pre_outsample1,
                                  "Recall" = rec_outsample1,
                                  "F1" = f1_outsample1)

metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)
metricas1 %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

