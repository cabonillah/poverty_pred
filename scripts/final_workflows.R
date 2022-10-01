source("../scripts/final_impute.R")
source("../scripts/final_specs.R")

workflows <- function(model, reg_clas) {
    spec <- specs(model, reg_clas)

    # RF Regression
    if (model == "rf" && reg_clas == "reg") {
        workflow <- workflow() %>%
            add_recipe(rec_reg_rf) %>%
            add_model(spec)
    }

    # RF Classification
    if (model == "rf" && reg_clas == "clas") {
        workflow <- workflow() %>%
            add_recipe(rec_clas_rf) %>%
            add_model(spec)
    }

    # XGB Regression
    if (model == "xgb" && reg_clas == "reg") {
        workflow <- workflow() %>%
            add_recipe(rec_reg_rf) %>%
            add_model(spec)
    }

    # XGB Classification
    if (model == "xgb" && reg_clas == "clas") {
        workflow <- workflow() %>%
            add_recipe(rec_clas_rf) %>%
            add_model(spec)
    }


    workflow
}
