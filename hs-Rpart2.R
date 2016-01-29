### ------  hs-Rpart2.R


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
   source("hs-loadnclean.R")

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
   fit.rpart <- lapply(lst_dsTrain, createRpartFits, minsplit = 7, cp = 0.01 ,xval=30)
   fit.rf <- randomForest(as.factor(QuoteConversion_Flag)~., lst_dsTrain[[1]], ntree = 1000)
   fit.gbm <- gbm(formula = as.factor(QuoteConversion_Flag)~., data =  dftrain, n.trees = 500, cv.folds = 10, n.cores = 3, verbose = FALSE)


   # fancyRpartPlot(fit.rpart[[1]])


   print(paste(date(), " - Generating Prediction on Validation Dataset."))
   #lstPred.raw <- generatePredictionList(fit.rpart, lst_dsValidate, "class")
   #lstPred.prob <- generatePredictionList(fit.rpart, lst_dsValidate, "prob")

   set.seed(2016)
   lstPred.raw <- vector('list', length = length(fit.rpart))
   for (i in 1:length(fit.rpart))
   {
      lstPred.raw[[i]] <- data.frame(predict(fit.rpart[[i]], lst_dsValidate[[i]], type = 'prob'))
      #colnames(lstPred.raw[[i]]) <- c("x0", "x1")
      lstPred.raw[[i]] <- cbind("QF" = lst_dsValidate[[i]]$QuoteConversion_Flag,  lstPred.raw[[i]])
   }


   print(paste(date(), " - Appending Reference Column from Validation Dataset for Evaluation."))


   print(paste(date(), " - Creating Confusion Matrix for Evaluation."))

   cnfMx <- vector('list', length = length(lstPred.raw))
   for (i in 1:length(lstPred.raw))
   {
      cnfMx[[i]] <- confusionMatrix(lstPred.raw[[i]]$pred, lstPred.raw[[i]]$QF)
   }





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

   ## -------------------------------------------------

