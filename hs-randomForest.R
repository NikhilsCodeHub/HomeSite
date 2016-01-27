### 
### --- hs-RandomForest
### 


library(rpart)
library(randomForest)
library(caret)
library(dplyr)
library(doMC)
registerDoMC(cores = 6)

  source("t-functions.R", echo = FALSE)
  
  source("hs-loadnclean.R")
  

  fit.rf <- randomForest(formula = as.factor(QuoteConversion_Flag)~., dtrain_final, nodesize = 10 )
  
  pred.rf <- predict.train(fit.rf, dtrain_final, type = "prob")
  
  dfValidate <- data.frame(QuoteConversion_Flag = dtrain_final$QuoteConversion_Flag, pred.rf = pred.rf)
  
  
  write.csv(dfValidate, file=paste0("validate_train_rf", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)
  
  pred.rf <- predict.train(fit.rf, dtest_final, type = "prob")
  
  dfpred_test <- data.frame(QuoteNumber = dtest_final$QuoteNumber, QuoteConversion_Flag = pred.rf)
  
  write.csv(dfpred_test, file=paste0("submission_rf", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)