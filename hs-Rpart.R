### ---- hs-Rpart.R
###
###


  library(rpart)
  library(randomForest)
  library(caret)
  library(dplyr)



  source("t-functions.R", echo = FALSE)

  print(paste(date(), " - Loading and Cleaning Data."))
  source("hs-loadnclean.R")

  print(paste(date(), " - Creating Training Folds."))
  nfolds <- 4
  set.seed(2016)
  ftrain <- createFolds(dtrain_final$GeographicField64, k = nfolds , list = TRUE, returnTrain = FALSE)

  print(paste(date(), " - Creating Fold Datasets for Training."))
  lst_dsTrain <- vector("list", length = nfolds)
  for (i in 1:nfolds)
  {
    lst_dsTrain[[i]] <- dtrain_final[ftrain[[i]],]
  }

  print(paste(date(), " - Creating Validation Folds."))
  fValidate <- createFolds(dtrain_final$QuoteConversion_Flag, 2, list = TRUE, returnTrain = TRUE)

  print(paste(date(), " - Creating Fold Datasets for Validation."))
  lst_dsValidate <- vector("list", length = 2)
  for (i in 1:2)
  {
    lst_dsValidate[[i]] <- dtrain_final[fValidate[[i]],]
  }

  print(paste(date(), " - Generating Rpart Models."))
  fit.rpart <- lapply(lst_dsTrain, createRpartFits)


  print(paste(date(), " - Generating Prediction on Validation Dataset."))
  pred.rpart.raw <- generatePredictions(fit.rpart, lst_dsValidate[[1]], "class")

  print(paste(date(), " - Converting Predictions to Dataframe."))
  dfPred <- cbind(lst_dsValidate[[1]]$QuoteConversion_Flag, pred.rpart.raw)
  colnames(dfPred) <- c("QuoteConversion_Flag", paste("p", seq(1:nfolds), sep = ""))

  pred.rpart.prob <- generatePredictions(fit.rpart, lst_dsValidate[[1]], "prob")

  print(paste(date(), " - Converting Probability Predictions to Dataframe."))
  dfPred.prob <- cbind(lst_dsValidate[[1]]$QuoteConversion_Flag, pred.rpart.prob[,c(-seq(1,(2*(nfolds-1))+1,2))])
  ## -seq(1,(2*(nfolds-1))+1,2) : -1, -3, -5, -7
  ##

  colnames(dfPred.prob) <- c("QuoteConversion_Flag", paste("p", seq(1:nfolds), sep = ""))


  print(paste(date(), " - Creating Confusion Matrix for Evaluation."))
  cnfMx <- calcConfusionMx(pred.rpart.raw, lst_dsValidate[[1]]$QuoteConversion_Flag)

    print(paste(date(), " - Extracting False Positives and False Negatives."))
      dfMisClass <- extractFalsePN(cnfMatrixGrid = dfPred, df = dtrain_final)

    print(paste(date(), " - Creating Model fit for False Pos and Neg."))
    fit.misclass <- createRpartFits(df = dfMisClass, minsplit = 3, cp = 0.05, xval = 20)

    print(paste(date(), " - Generating Predictions for Miss Classifications."))
    pred.misclass <- predict(fit.misclass, lst_dsValidate[[1]], "class" )

    pred.misclass.prob <- predict(fit.misclass, lst_dsValidate[[1]], "prob" )

    print(paste(date(), " - Appending MissClassification Predictions to Pred dataset."))
    dfPred <- cbind(dfPred, "pMiss" =pred.misclass)
    dfPred.prob <- cbind(dfPred.prob, "pMiss" = pred.misclass.prob[,2])

    print(paste(date(), " - Creating Combo Fit for stage 1 predictions."))
#     fit.combo <- createglmFit(df = dfPred.prob)
#     fit.combo <- lm(QuoteConversion_Flag~., dfPred.prob)
    fit.combo <- createRpartFits(df = dfPred.prob,minsplit = 3,xval = 20, cp = 0.01)

##
## ---- Validation Testing
##
    print(paste(date(), " - Generating predictions on validation set for evaluation."))

    pred.val <- generatePredictions(fit.rpart, lst_dsValidate[[2]],"prob")
    dfPred.prob.val <- cbind("QuoteConversion_Flag" =lst_dsValidate[[2]]$QuoteConversion_Flag, pred.val[,c(-seq(1,(2*(nfolds-1))+1,2))])

    colnames(dfPred.prob.val) <- c("QuoteConversion_Flag", paste("p", seq(1:nfolds), sep = ""))

    pred.val <- predict(fit.misclass, lst_dsValidate[[2]], "prob" )
    dfPred.prob.val <- cbind(dfPred.prob.val, "pMiss" = pred.val[,2])

    pred.combo <- predict(fit.combo, dfPred.prob.val, type = "class")

    print(paste(date(), " - Append final Prediction to pred dataset."))
    dfPred.prob.val <- cbind(dfPred.prob.val, "pcombo" = pred.combo)

      print(paste(date(), " - Calculate Confusion Matrix for Eval."))

      # Calculate Confusion Matrix to evaluate model effectiveness
      cnfMx <- confusionMatrix(dfPred.prob.val$pcombo , dfPred.prob.val$QuoteConversion_Flag)

      saveRDS(cnfMx, "validation2.rds")


#
# Process Test Data
#
      print(paste(date(), " - Begin Processing Test Data."))
      print(paste(date(), " - Generating stage 1 predictions."))
      pred.test <- generatePredictions(fit.rpart, dtest_final, "prob")

      print(paste(date(), " - Append stage 1 predictions to pred dataset."))
      dfPred.test <- cbind(pred.test[,c(-seq(1,(2*(nfolds-1))+1,2))])
      colnames(dfPred.test) <- c(paste("p", seq(1:nfolds), sep = ""))

      print(paste(date(), " - Generate Prediction on Missclassification model."))
      pred.misclass.test <- predict(fit.misclass, dtest_final, "prob" )
      dfPred.test <- cbind(dfPred.test, "pMiss" = pred.misclass.test[,2])

      print(paste(date(), " - Generate Predictions for Combo fit."))
      pred.combo.test <- predict(fit.combo, dfPred.test, "class")

    dfpred_test <- data.frame(QuoteNumber = dtest$QuoteNumber, QuoteConversion_Flag = pred.combo.test)

    print(paste(date(), " - Create submission file."))
    write.csv(dfpred_test, file=paste0("submission_rpart_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)

