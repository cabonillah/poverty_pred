source("../scripts/train_test_impute.R")
source("../scripts/workflows.R")
source("../scripts/grids.R")
source("../scripts/tuning.R")

wf <- workflows("xgb", "reg")
grid <- grids("xgb", "reg")
cl <- parallel::makeCluster(7)
set.seed(10)
result <- wf %>% tuning("xgb", "reg", grid, validation_split)
parallel::stopCluster(cl)

result %>% collect_metrics()

# Select best model
best <- select_best(result, metric = "rmse")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)
# Fit on training, predict on test, and report performance
lf <- last_fit(final_wf, data_split)
# Performance metric on test set
metric <- rmse(
    data.frame(
        test["Ingpcug"],
        lf %>% extract_workflow() %>% predict(test)
    ),
    Ingpcug,
    .pred
)$.estimate

# Final report for this model
report <- data.frame(
    Problema = "Reg.", Modelo = "XGBoost",
    result %>% show_best(n = 1) %>% mutate(mean = metric)
)

saveRDS(report, file = "../stores/xgb_reg.rds")
