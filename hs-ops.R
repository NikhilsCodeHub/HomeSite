### --- hs-ops.R
### 


source("hs-loadnclean.R")

loadData()

processData(dtrain, dtest)



lst.fits <- as.list(fit)