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

imputed_train <- impute_norm %>% bake(new_data = NULL)

imputed_test <- impute_norm %>% bake(new_data = test)
