source("../scripts/train_test_impute.R")
source("../scripts/specs.R")

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

    # Regression
    if (reg_clas == "reg") {
        workflow <- workflow() %>%
            add_recipe(rec_reg) %>%
            add_model(spec)
    }

    # Classification
    if (reg_clas == "clas") {
        workflow <- workflow() %>%
            add_recipe(rec_clas) %>%
            add_model(spec)
    }

    workflow
}
