library("tidyverse")
library("tidymodels")

data <- readRDS("../stores/data.rds")

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
