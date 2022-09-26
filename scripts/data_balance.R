rm(list=ls())

setwd("C:\\Users\\juan.velasquez\\OneDrive - Universidad de los Andes\\Maestria\\Semestres\\2022-2\\BIG DATA & MACHINE LEARNING FOR APPLIED ECONOMICS\\Talleres\\Problem-Set2\\poverty_pred")
# Cargamos la librería tidyverse 
library("tidyverse")
library("stargazer")

df_per <- readRDS("./stores/train_personas.Rds")
df_hog <- readRDS("./stores/train_hogares.Rds")
df2_hog <- readRDS("./stores/test_hogares.Rds")
df_hog$Dominio <- factor(df_hog$Dominio)

skim(df_hog)

# Dummyficamos ANTES de partir la base en train/test
df <- model.matrix(~ .+Nper + Ingpcug  - Pobre -id - 1, df_hog)

# Dividimos train/test (70/30)
smp_size <- floor(0.7*n)
set.seed(666)
train_ind <- sample(1:n, size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]