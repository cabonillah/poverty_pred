source("../scripts/train_test_impute.R")
source("../scripts/workflows.R")
source("../scripts/grids.R")
source("../scripts/tuning.R")

wf <- workflows("lm", "clas")
grid <- grids("lm", "clas")
result <- wf %>% tuning("lm", "clas", grid, validation_split)
result %>% collect_metrics()

# Select best model
best <- select_best(result, metric = "f_score")
# Finalize the workflow with those parameter values
final_wf <- wf %>% finalize_workflow(best)
# Fit on training, predict on test, and report performance
lf <- last_fit(final_wf, data_split)
# Performance metric on test set
metric <- f_score(
    data.frame(
        test["Pobre"],
        lf %>% extract_workflow() %>% predict(test)
    ),
    Pobre,
    .pred_class
)$.estimate

# Final report for this model
report <- data.frame(
    Problema = "Clas.", Modelo = "Lineal",
    Penalidad = "N/A", Mixtura = "N/A",
    result %>% show_best(n = 1) %>% mutate(mean = metric)
)

saveRDS(report, file = "../stores/lm_clas.rds")
