source("../scripts/rf_clas.R")
saveRDS(result, file = "../../results1.rds")
source("../scripts/rf_reg.R")
saveRDS(result, file = "../../results2.rds")
