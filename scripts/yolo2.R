source("../scripts/xgb_clas.R")
saveRDS(result, file = "../../results3.rds")
source("../scripts/xgb_reg.R")
saveRDS(result, file = "../../results4.rds")

XGB_CLASS <- readRDS("../../results3.rds")
autoplot(XGB_CLASS)

XGB_RES <- readRDS("../../results4.rds")
autoplot(XGB_RES)
