source("../scripts/rf_clas.R")
saveRDS(result, file = "../../results1.rds")
source("../scripts/rf_reg.R")
saveRDS(result, file = "../../results2.rds")

RF_CLASS <- readRDS("../../results1.rds")
autoplot(RF_CLASS)

RF_RES <- readRDS("../../results2.rds")
autoplot(RF_RES)