library("yardstick")
library("tune")

f_score <- function(data,
                    truth,
                    estimate,
                    beta,
                    estimator,
                    na_rm,
                    case_weights,
                    ...) {
    yardstick::f_meas(
        data = data,
        truth = !!rlang::enquo(truth),
        estimate = !!rlang::enquo(estimate),
        beta = 1.75,
        estimator = NULL,
        na_rm = TRUE,
        case_weights = NULL,
        ...
    )
}

f_score <- new_class_metric(f_score, "maximize")

tuning <- function(object, model, reg_clas, grid, resamples, ...) {
    if (model == "lm" && reg_clas == "reg") {
        tune <- tune::tune_grid(
            object = object,
            metrics = yardstick::metric_set(rmse),
            resamples = resamples
        )
    }

    if (model %in% c("lm", "lasso", "ridge", "elastic", "rf", "xgb") && reg_clas == "clas") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid,
            metrics = yardstick::metric_set(f_score),
            resamples = resamples
        )
    }

    if (model != "lm" && reg_clas == "reg") {
        tune <- tune::tune_grid(
            object = object,
            grid = grid,
            metrics = yardstick::metric_set(rmse),
            resamples = resamples
        )
    }

    tune
}
