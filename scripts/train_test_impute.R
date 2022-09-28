library("tidyverse")
library("tidymodels")
library("themis")

data <- readRDS("../stores/data.rds")

# Create dummies

data <- recipe(~., data = data) %>%
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
    prep() %>%
    bake(new_data = NULL)

# Create train and test samples
set.seed(10)
data_split <- data %>% initial_split(prop = 0.7)
train <- data_split %>% training()
test <- data_split %>% testing()


validation_split <- vfold_cv(train, v = 5)

# Recipes for imputing in regression and classification
rec_reg <- recipe(Ingpcug ~ ., data = train) %>%
    step_rm(Lp, Pobre) %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_dummy(
        P6920, P7040, P7090, P7505
    )

rec_clas <- recipe(Pobre ~ ., data = train) %>%
    step_rm(Lp, Ingpcug) %>%
    step_impute_mode(
        P6920, P7040, P7090, P7505
    ) %>%
    step_dummy(
        P6920, P7040, P7090, P7505
    )

# Delete unnecesary variables
rm(
    data
)


f_score <- function(data,
                    truth,
                    estimate,
                    beta,
                    estimator,
                    na_rm,
                    case_weights,
                    ...) {
    f_meas(
        data = data,
        truth = !!rlang::enquo(truth),
        estimate = !!rlang::enquo(estimate),
        beta = 1,
        estimator = NULL,
        na_rm = TRUE,
        case_weights = NULL,
        ...
    )
}

f_score <- new_class_metric(f_score, "maximize")
