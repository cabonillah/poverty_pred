library("tidymodels")
library("ranger")
library("xgboost")

specs <- function(model, reg_clas) {

    # RF - Regression
    if (model == "rf" && reg_clas == "reg") {
        spec <- rand_forest(
            trees = 200,
            mtry = 12,
            min_n = 10,
        ) %>%
            set_engine("ranger", verbose = TRUE, num.threads = 3) %>%
            set_mode("regression")
    }

    # RF - Classification
    if (model == "rf" && reg_clas == "clas") {
        spec <- rand_forest(
            trees = 200,
            mtry = 12,
            min_n = 30
        ) %>%
            set_engine("ranger", verbose = TRUE, num.threads = 3) %>%
            set_mode("classification")
    }

    spec
}
