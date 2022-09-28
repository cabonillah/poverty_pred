library("tidyverse")
library("tidymodels")

data <- readRDS("../stores/data.rds")

# Create dummies

data <- recipe(~., data = data) %>%
    step_impute_mode(P6210) %>%
    step_dummy(
        Depto, P5090, P6210
    ) %>%
    prep() %>%
    bake(new_data = NULL)

# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.7)
train <- data_split %>% training()
test <- data_split %>% testing()


# Impute and normalize train and test sets
rec <- recipe(~., data = train)

impute_norm <- rec %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_normalize(
        Nper, P5000, P5010, P6040
    ) %>%
    prep()

imputed_train_all <- impute_norm %>% bake(new_data = NULL)
imputed_train_reg <- imputed_train_all %>% select(-Pobre, -Lp)
imputed_train_clas <- imputed_train_all %>% select(-Ingpcug, -Lp)

imputed_test_all <- impute_norm %>% bake(new_data = test)
imputed_test_reg <- imputed_test_all %>% select(-Pobre, -Lp)
imputed_test_clas <- imputed_test_all %>% select(-Ingpcug, -Lp)

# Delete unnecesary variables
rm(
    data,
    data_split,
    test,
    train,
    impute_norm,
    rec,
    imputed_train_all,
    imputed_test_all
)
