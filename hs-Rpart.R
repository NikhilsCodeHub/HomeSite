### ---- hs-Rpart.R
###
###


  library(rpart)
  library(randomForest)
  library(caret)
  library(dplyr)
  library(doMC)
  registerDoMC(cores = 4)


  source("t-functions.R", echo = FALSE)

  source("hs-loadnclean.R")



    fit.rpart <- rpart(as.factor(QuoteConversion_Flag)~., data = dtrain_final, method = "class", control = rpart.control(minsplit = 10, cp = 0.01, xval = 20))

    pred.rpart <- predict(fit.rpart, dtrain_final, "prob" )

    dfValidate <- data.frame(QuoteConversion_Flag = dtrain_final$QuoteConversion_Flag, pred.rf = pred.rpart)

    write.csv(dfValidate, file=paste0("validate_train_rf", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)

    pred.rpart <- predict(fit.rpart, dtest_final, type = "class")

    dfpred_test <- data.frame(QuoteNumber = dtest$QuoteNumber, QuoteConversion_Flag = pred.rpart)

    write.csv(dfpred_test, file=paste0("submission_rpart", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)

