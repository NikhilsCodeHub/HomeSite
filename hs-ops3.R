## --- t-ops.R

###
# OPS Scripts that performs the magic
###

library(rpart)
library(randomForest)
library(caret)
library(gbm)
library(dplyr)
library(lubridate)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(doMC)
registerDoMC(cores = 4)


source("t-functions.R", echo = FALSE)

print(paste(date(), " - Loading and Cleaning Data."))
source("t-loaddata.R")

## -------------------------------------------------

print(paste(date(), " - Partition Data for Training and Validate on GeographicField64."))

lstrows <- createDataPartition(dtrain_final$GeographicField64 ,p = .7, list = FALSE)
dftrain <- dtrain_final[lstrows,]
dfvalidate <- dtrain_final[-lstrows,]

lst_dsTrain <- createDataSplits(df = dftrain, "GeographicField64")
lst_dsValidate <- createDataSplits(df = dfvalidate, "GeographicField64")
#   rm(dftrain)
#   rm(dfvalidate)

## -------------------------------------------------



print(paste(date(), " - Generating Rpart Models."))

lst.fit.caret <- vector('list', length = length(lst_dsTrain))
for (i in 1:length(lst_dsTrain))
{
  # lst.fit.caret[[i]] <- createCaretModelFits(df = lst_dsTrain[[i]], method = "rpart" )
  # print(paste(date(), " - Generating RF Model - ", i))
  # lst.fit.caret[[i]] <- randomForest(as.factor(QuoteConversion_Flag)~., lst_dsTrain[[i]], ntree = 1000)
  print(paste(date(), " - Generating GBM Model - ", i))
  lst.fit.caret[[i]] <- gbm(formula = as.factor(QuoteConversion_Flag)~., data =  lst_dsTrain[[i]], n.trees = 500, cv.folds = 10, n.cores = 3, verbose = FALSE)

}
print(paste(date(), " - GBM Models Completed "))

# fit.rpart <- lapply(lst_dsTrain, createRpartFits, minsplit = 7, cp = 0.01 ,xval=30)
# fit.rf <- randomForest(as.factor(QuoteConversion_Flag)~., lst_dsTrain[[1]], ntree = 1000)
# fit.gbm <- gbm(formula = as.factor(QuoteConversion_Flag)~., data =  dftrain, n.trees = 500, cv.folds = 10, n.cores = 3, verbose = FALSE)


## -------------------------------------------------

lstPreds <- generatePredictionList(lstFit = lst.fit.caret, lstnewData = lst_dsValidate, ptype = "prob")


lst.cnfMx <- calcConfusionMxList(lstPreds, lst_dsValidate)



## -------------------------------------------------
## Assuming good Models and moving to generating Test prdictions for submission
## -------------------------------------------------

dtest_final <- cbind(QuoteNumber= dtest$QuoteNumber, dtest_final)

lst_dsTest <- createDataSplits(df = dtest_final, "GeographicField64")

lstPreds.Test <- generatePredictionList(lstFit = lst.fit.caret, lstnewData = lst_dsTest, ptype = "prob")

for(i in 1:length(lstPreds.Test))
{
  sol <- data.frame(QuoteNumber = lst_dsTest[[i]]$QuoteNumber, QuoteConversion_Flag= round(lstPreds.Test[[i]]$X1))
  print(dim(sol))
  if(i>1)
  {
    dfResult <- rbind(dfResult, sol)
  }
  else {
    dfResult <- sol
  }
}

dfResult <- dfResult %>% tbl_df() %>% arrange(QuoteNumber)
dfResult <- data.frame(dfResult)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(dfResult, file=paste0("HomeSiteSubmission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)


