grids <- function(model, reg_clas) {
    # Overall parameters
    penalty <- seq(0.0001, 0.001, length.out = 5)
    mixture <- seq(0.1, 0.9, length.out = 4)
    over_ratio <- c(0.75)
    mtry <- c(5, 8, 12)
    min_n <- c(10, 20, 30)
    sample_size <- c(1)

    # XGB parameters
    mtry_xgb <- c(8)
    min_n_xgb_clas <- c(10)
    min_n_xgb_reg <- c(20, 40, 100)
    tree_depth <- c(6, 8, 10)
    learn_rate <- c(0.00001, 0.001, 0.01)
    loss_reduction <- c(0.00001, 0.001, 0.01)

    # Linear - Classification
    if (model == "lm" && reg_clas == "clas") {
        grid <- expand.grid(
            over_ratio = over_ratio
        )
    }

    # Lasso - Regression
    if (model == "lasso" && reg_clas == "reg") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 1
        )
    }

    # Ridge - Regression
    if (model == "ridge" && reg_clas == "reg") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 0
        )
    }

    # Elastic - Regression
    if (model == "elastic" && reg_clas == "reg") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = mixture
        )
    }

    # Lasso - Classification
    if (model == "lasso" && reg_clas == "clas") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 1,
            over_ratio = over_ratio
        )
    }

    # Ridge - Classification
    if (model == "ridge" && reg_clas == "clas") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = 0,
            over_ratio = over_ratio
        )
    }

    # Elastic - Classification
    if (model == "elastic" && reg_clas == "clas") {
        grid <- expand.grid(
            penalty = penalty,
            mixture = mixture,
            over_ratio = over_ratio
        )
    }

    # RF - Regression
    if (model == "rf" && reg_clas == "reg") {
        grid <- expand.grid(
            mtry = mtry,
            min_n = min_n
        )
    }


    # RF - Classification
    if (model == "rf" && reg_clas == "clas") {
        grid <- expand.grid(
            over_ratio = over_ratio,
            mtry = mtry,
            min_n = min_n
        )
    }
    # XGB - Regression
    if (model == "xgb" && reg_clas == "reg") {
        grid <- expand.grid(
            mtry = mtry_xgb,
            min_n = min_n_xgb_reg,
            sample_size = sample_size,
            tree_depth = tree_depth,
            learn_rate = learn_rate,
            loss_reduction = loss_reduction
        )
    }

    # XGB - Classification
    if (model == "xgb" && reg_clas == "clas") {
        grid <- expand.grid(
            over_ratio = over_ratio,
            mtry = mtry_xgb,
            min_n = min_n_xgb_clas,
            sample_size = sample_size,
            tree_depth = tree_depth,
            learn_rate = learn_rate,
            loss_reduction = loss_reduction
        )
    }

    grid
}
