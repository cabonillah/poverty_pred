library("tidyverse")
library("tidymodels")
library("themis")

data_train <- readRDS("../stores/data.rds")
data_test <- readRDS("../stores/data_test.rds")

# Create dummies
data_train_reg <- recipe(~., data = data_train) %>%
    step_rm(Pobre) %>%
    step_impute_mode(P6210) %>%
    step_dummy(
        Depto, P5090, P6210
    ) %>%
    step_interact(
        terms = ~ Nper:starts_with("P6210") +
            Oc:starts_with("P6210") +
            P6020:starts_with("P6210") +
            P6040:starts_with("P6210")
    ) %>%
    step_poly(
        Nper, Oc, P5000, P5010, P6020, P6090, P6040,
        degree = 3
    ) %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_dummy(
        P6920, P7040, P7090, P7505
    ) %>%
    step_rm(tidyselect::contains(c("P75051", "Depto", "P6210"))) %>%
    prep() %>%
    bake(new_data = NULL)

data_train_clas <- recipe(~., data = data_train) %>%
    step_rm(Ingpcug) %>%
    step_impute_mode(P6210) %>%
    step_dummy(
        Depto, P5090, P6210
    ) %>%
    step_interact(
        terms = ~ Nper:starts_with("P6210") +
            Oc:starts_with("P6210") +
            P6020:starts_with("P6210") +
            P6040:starts_with("P6210")
    ) %>%
    step_poly(
        Nper, Oc, P5000, P5010, P6020, P6090, P6040,
        degree = 3
    ) %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_dummy(
        P6920, P7040, P7090, P7505
    ) %>%
    step_rm(tidyselect::contains(c("P75051", "Depto", "P6210"))) %>%
    prep() %>%
    bake(new_data = NULL)

data_test <- recipe(~., data = data_test) %>%
    step_impute_mode(P6210) %>%
    step_dummy(
        Depto, P5090, P6210
    ) %>%
    step_interact(
        terms = ~ Nper:starts_with("P6210") +
            Oc:starts_with("P6210") +
            P6020:starts_with("P6210") +
            P6040:starts_with("P6210")
    ) %>%
    step_poly(
        Nper, Oc, P5000, P5010, P6020, P6090, P6040,
        degree = 3
    ) %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_dummy(
        P6920, P7040, P7090, P7505
    ) %>%
    step_rm(tidyselect::contains(c("P75051", "Depto", "P6210"))) %>%
    prep() %>%
    bake(new_data = NULL)



# Recipes for imputing in regression and classification
rec_reg <- recipe(Ingpcug ~ ., data = data_train_reg) %>%
    step_rm(Lp)

rec_clas <- recipe(Pobre ~ ., data = data_train_clas) %>%
    step_rm(Lp)

rec_reg_rf <- rec_reg %>%
    step_rm(tidyselect::contains(c("P75051", "Depto", "P6210")))

rec_clas_rf <- rec_clas %>%
    step_rm(tidyselect::contains(c("P75051", "Depto", "P6210")))
