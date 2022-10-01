library("tidymodels")
library("ranger")
library("xgboost")

specs <- function(model, reg_clas) {

    # Linear - Regression
    if (model == "lm" && reg_clas == "reg") {
        spec <- linear_reg() %>%
            set_engine("lm")
    }

    # Linear - Classification
    if (model == "lm" && reg_clas == "clas") {
        spec <- logistic_reg() %>%
            set_engine("glm")
    }

    # Ridge/Lasso/Elastic - Regression
    if (model %in% c("ridge", "lasso", "elastic") && reg_clas == "reg") {
        spec <- linear_reg(
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")
    }

    # Ridge/Lasso/Elastic - Classification
    if (model %in% c("ridge", "lasso", "elastic") && reg_clas == "clas") {
        spec <- logistic_reg(
            penalty = tune(),
            mixture = tune()
        ) %>%
            set_engine("glmnet")
    }

    # RF - Regression
    if (model == "rf" && reg_clas == "reg") {
        spec <- rand_forest(
            trees = 200,
            mtry = tune(),
            min_n = tune(),
        ) %>%
            set_engine("ranger", verbose = TRUE, num.threads = 3) %>%
            set_mode("regression")
    }

    # RF - Classification
    if (model == "rf" && reg_clas == "clas") {
        spec <- rand_forest(
            trees = 200,
            mtry = tune(),
            min_n = tune(),
        ) %>%
            set_engine("ranger", verbose = TRUE, num.threads = 3) %>%
            set_mode("classification")
    }

    # XGB - Regression
    if (model == "xgb" && reg_clas == "reg") {
        spec <- boost_tree(
            trees = 200,
            mtry = tune(),
            min_n = tune(),
            sample_size = tune(),
            stop_iter = 5
        ) %>%
            set_engine("xgboost", nthread = 3) %>%
            set_mode("regression")
    }

    # XGB - Classification
    if (model == "xgb" && reg_clas == "clas") {
        spec <- boost_tree(
            trees = 200,
            mtry = tune(),
            min_n = tune(),
            sample_size = tune(),
            stop_iter = 5
        ) %>%
            set_engine("xgboost", nthread = 3) %>%
            set_mode("classification")
    }

    spec
}
