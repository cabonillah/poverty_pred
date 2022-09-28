rm(list=ls())

setwd("C:\\Users\\juan.velasquez\\OneDrive - Universidad de los Andes\\Maestria\\Semestres\\2022-2\\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\\Talleres\\Problem-Set2\\poverty_pred")
# Cargamos la librería tidyverse 
library("tidyverse")
library("stargazer")
library("skimr")
library("pacman")
library("kableExtra")
p_load(dplyr, tidyverse, caret, MLmetrics, tidymodels, themis)

df_per <- readRDS("./stores/train_personas.Rds")
df2_per <- readRDS("./stores/test_personas.Rds")
df_hog <- readRDS("./stores/train_hogares.Rds")
df2_hog <- readRDS("./stores/test_hogares.Rds")


#########We take a look on the frequence of poor homes in the data set##########

ggplot(df_hog, aes(x = Pobre)) +
  geom_bar(fill = "darkblue") +
  theme_bw() +
  labs(title = "Frecuencia de hogares pobres",
       x = "",
       y = "Frecuencia") +
  coord_flip()

prop.table(table(df_hog$Pobre))

############ The percentage  of poor homes in the data set is near to 20%###############

#skim(df_hog)

glimpse(df_hog)

df_hog$Dominio <- factor(df_hog$Dominio)
df_hog$P5090 <- factor(df_hog$P5090)
df_hog$Depto <- factor(df_hog$Depto)
df_hog$Clase <- factor(df_hog$Clase)
#LOOK "P5130" 
data_hog <- df_hog %>% select(-c("Clase", "P5100", "P5130", "P5140","Li", "Lp", "Fex_c","Fex_dpto","Depto","Ingtotugarr", "Ingtotug","Nindigentes","Ingpcug","Indigente","Npobres","Nindigentes"))

# Dummyficamos ANTES de partir la base en train/test
df <- model.matrix(~ . -id -1, data_hog)

# Dividimos train/test (70/30)
set.seed(666)

dt = sort(sample(nrow(df), nrow(df)*.7))
train<-df[dt,]
test<-df[-dt,]


#skim(df)
variables_numericas <- c("P5000", "P5010", "P50902",
                         "Nper","P50903","P50904"
                         ,"P50905","P50906","Npersug")
escalador <- preProcess(train[, variables_numericas])
train_s <- train
test_s <- test
train_s[, variables_numericas] <- predict(escalador, train[, variables_numericas])
test_s[, variables_numericas] <- predict(escalador, test[, variables_numericas])

train_s <- data.frame(train_s)
test_s <- data.frame(test_s)
train <- data.frame(train)
test <- data.frame(test)


train_s$Pobre <- as.numeric(train_s$Pobre)
modelo1 <- lm(formula = Pobre ~ ., data = train_s)
summary(modelo1)
probs_insample1 <- predict(modelo1, train_s)
probs_insample1[probs_insample1 < 0] <- 0
probs_insample1[probs_insample1 > 1] <- 1
probs_outsample1 <- predict(modelo1, test_s)
probs_outsample1[probs_outsample1 < 0] <- 0
probs_outsample1[probs_outsample1 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample1 <- as.numeric(probs_insample1 > 0.5)
y_hat_outsample1 <- as.numeric(probs_outsample1 > 0.5)

acc_insample1 <- Accuracy(y_pred = y_hat_insample1, y_true = train$Pobre)
acc_outsample1 <- Accuracy(y_pred = y_hat_outsample1, y_true = test$Pobre)

pre_insample1 <- Precision(y_pred = y_hat_insample1, y_true = train$Pobre, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_outsample1, y_true = test$Pobre, positive = 1)

rec_insample1 <- Recall(y_pred = y_hat_insample1, y_true = train$Pobre, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_outsample1, y_true = test$Pobre, positive = 1)

f1_insample1 <- F1_Score(y_pred = y_hat_insample1, y_true = train$Pobre, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, y_true = test$Pobre, positive = 1)

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



########################### Implementamos UNDERsampling#########################

train_s$Ing <- factor(train_s$Pobre)
train_s2 <- recipe(Pobre ~ ., data = train_s) %>%
  themis::step_smote(Pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_s$Pobre))

nrow(train_s)

prop.table(table(train_s2$Pobre))
nrow(train_s2)

train_s2$Pobre <- as.numeric(train_s2$Pobre) - 1
modelo2 <- lm(formula = "Pobre ~ .", data = train_s2)
probs_insample2 <- predict(modelo2, train_s2)
probs_insample2[probs_insample2 < 0] <- 0
probs_insample2[probs_insample2 > 1] <- 1
probs_outsample2 <- predict(modelo2, test_s)
probs_outsample2[probs_outsample2 < 0] <- 0
probs_outsample2[probs_outsample2 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample2 <- as.numeric(probs_insample2 > 0.5)
y_hat_outsample2 <- as.numeric(probs_outsample2 > 0.5)

acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = train_s2$Pobre)
acc_outsample2 <- Accuracy(y_pred = y_hat_outsample2, y_true = test$Pobre)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = train_s2$Pobre, positive = 1)
pre_outsample2 <- Precision(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = train_s2$Pobre, positive = 1)
rec_outsample2 <- Recall(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = train_s2$Pobre, positive = 1)
f1_outsample2 <- F1_Score(y_pred = y_hat_outsample2, y_true = test$Pobre, positive = 1)

metricas_insample2 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample2,
                                 "Precision" = pre_insample2,
                                 "Recall" = rec_insample2,
                                 "F1" = f1_insample2)

metricas_outsample2 <- data.frame(Modelo = "Regresión lineal", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample2,
                                  "Precision" = pre_outsample2,
                                  "Recall" = rec_outsample2,
                                  "F1" = f1_outsample2)

metricas2 <- bind_rows(metricas_insample2, metricas_outsample2)
metricas <- bind_rows(metricas1, metricas2)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)

###################### Implementamos oversampling###############################
train_s$Pobre <- factor(train_s$Pobre)
train_s3 <- recipe(Pobre ~ ., data = train_s) %>%
  themis::step_downsample(Pobre) %>%
  prep() %>%
  bake(new_data = NULL)

prop.table(table(train_s$Pobre))
nrow(train_s3)

train_s3$Pobre <- as.numeric(train_s3$Pobre) - 1
modelo3 <- lm(formula = "Pobre ~ .", data = train_s3)
probs_insample3 <- predict(modelo3, train_s3)
probs_insample3[probs_insample3 < 0] <- 0
probs_insample3[probs_insample3 > 1] <- 1
probs_outsample3 <- predict(modelo3, test_s)
probs_outsample3[probs_outsample3 < 0] <- 0
probs_outsample3[probs_outsample3 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample3 <- as.numeric(probs_insample3 > 0.5)
y_hat_outsample3 <- as.numeric(probs_outsample3 > 0.5)

acc_insample3 <- Accuracy(y_pred = y_hat_insample3, y_true = train_s3$Pobre)
acc_outsample3 <- Accuracy(y_pred = y_hat_outsample3, y_true = test$Pobre)

pre_insample3 <- Precision(y_pred = y_hat_insample3, y_true = train_s3$Pobre, positive = 1)
pre_outsample3 <- Precision(y_pred = y_hat_outsample3, y_true = test$Pobre, positive = 1)

rec_insample3 <- Recall(y_pred = y_hat_insample3, y_true = train_s3$Pobre, positive = 1)
rec_outsample3 <- Recall(y_pred = y_hat_outsample3, y_true = test$Pobre, positive = 1)

f1_insample3 <- F1_Score(y_pred = y_hat_insample3, y_true = train_s3$Pobre, positive = 1)
f1_outsample3 <- F1_Score(y_pred = y_hat_outsample3, y_true = test$Pobre, positive = 1)

metricas_insample3 <- data.frame(Modelo = "Regresión lineal", 
                                 "Muestreo" = "Undersampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample3,
                                 "Precision" = pre_insample3,
                                 "Recall" = rec_insample3,
                                 "F1" = f1_insample3)

metricas_outsample3 <- data.frame(Modelo = "Regresión lineal", 
                                  "Muestreo" = "Undersampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample3,
                                  "Precision" = pre_outsample3,
                                  "Recall" = rec_outsample3,
                                  "F1" = f1_outsample3)

metricas3 <- bind_rows(metricas_insample3, metricas_outsample3)
metricas <- bind_rows(metricas, metricas3)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


####Optimizar mi Threshold#######################################################


# Esto no se debería hacer sobre la base de testeo pero se hace solo a modo ilustrativo
thresholds <- seq(0.1, 0.9, length.out = 100)
opt_t <- data.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(probs_outsample1 > t)
  f1_t <- F1_Score(y_true = test$Pobre, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]

ggplot(opt_t, aes(x = t, y = F1)) +
  geom_point(size = 0.7) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = mejor_t, linetype = "dashed", 
             color = "red") +
  labs(x = "Threshold")

# Convertimos la probabilidad en una predicción
y_hat_insample4 <- as.numeric(probs_insample1 > mejor_t)
y_hat_outsample4 <- as.numeric(probs_outsample1 > mejor_t)

acc_insample4 <- Accuracy(y_pred = y_hat_insample4, y_true = train_s$Pobre)
acc_outsample4 <- Accuracy(y_pred = y_hat_outsample4, y_true = test$Pobre)

pre_insample4 <- Precision(y_pred = y_hat_insample4, y_true = train_s$Pobre, positive = 1)
pre_outsample4 <- Precision(y_pred = y_hat_outsample4, y_true = test$Pobre, positive = 1)

rec_insample4 <- Recall(y_pred = y_hat_insample4, y_true = train_s$Pobre, positive = 1)
rec_outsample4 <- Recall(y_pred = y_hat_outsample4, y_true = test$Pobre, positive = 1)

f1_insample4 <- F1_Score(y_pred = y_hat_insample4, y_true = train_s$Pobre, positive = 1)
f1_outsample4 <- F1_Score(y_pred = y_hat_outsample4, y_true = test$Pobre, positive = 1)

metricas_insample4 <- data.frame(Modelo = "Regresión lineal - Threshold óptimo", 
                                 "Muestreo" = NA, 
                                 "Evaluación" = "Dentro de muestra",
                                 "Accuracy" = acc_insample4,
                                 "Precision" = pre_insample4,
                                 "Recall" = rec_insample4,
                                 "F1" = f1_insample4)

metricas_outsample4 <- data.frame(Modelo = "Regresión lineal - Threshold óptimo", 
                                  "Muestreo" = NA, 
                                  "Evaluación" = "Fuera de muestra",
                                  "Accuracy" = acc_outsample4,
                                  "Precision" = pre_outsample4,
                                  "Recall" = rec_outsample4,
                                  "F1" = f1_outsample4)

metricas4 <- bind_rows(metricas_insample4, metricas_outsample4)
metricas <- bind_rows(metricas, metricas4)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)