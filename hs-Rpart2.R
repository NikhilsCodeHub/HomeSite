### ------  hs-Rpart2.R


   library(rpart)
   library(randomForest)
   library(caret)
   library(dplyr)
   library(lubridate)


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
   rm(dftrain)
   rm(dfvalidate)

   ## -------------------------------------------------

   print(paste(date(), " - Generating Rpart Models."))
   fit.rpart <- lapply(lst_dsTrain, createRpartFits)


   print(paste(date(), " - Generating Prediction on Validation Dataset."))
   #lstPred.raw <- generatePredictionList(fit.rpart, lst_dsValidate, "class")
   #lstPred.prob <- generatePredictionList(fit.rpart, lst_dsValidate, "prob")

   set.seed(2016)
   lstPred.raw <- vector('list', length = length(fit.rpart))
   for (i in 1:length(fit.rpart))
   {
      lstPred.raw[[i]] <- data.frame(predict(fit.rpart[[i]], lst_dsValidate[[i]], type = 'class'))
      colnames(lstPred.raw[[i]]) <- paste("p", i, sep = "")
   }


   print(paste(date(), " - Appending Reference Column from Validation Dataset for Evaluation."))
   for(i in 1:length(lst_dsValidate))
   {
      lstPred.raw[[i]] <- cbind(lst_dsValidate[[i]]$QuoteConversion_Flag,  lstPred.raw[[i]])
      #lstPred.prob[[i]] <- cbind(lst_dsValidate[[i]]$QuoteConversion_Flag,  lstPred.prob[[i]][,2])
      #colnames(lstPred.raw[[i]]) <- c(paste("QF", i, sep=""), paste("p", seq(1:dim(lstPred.raw[[i]])[2]-1), sep = ""))
      #colnames(lstPred.prob[[i]]) <- c(paste("QF", i, sep=""), paste("p", seq(1:dim(lstPred.prob[[i]])[2]-1), sep = ""))
   }

#    dfPred <- cbind(lst_dsValidate[[1]]$QuoteConversion_Flag, pred.rpart.raw)
#    colnames(dfPred) <- c("QuoteConversion_Flag", paste("p", seq(1:nfolds), sep = ""))



#    print(paste(date(), " - Converting Probability Predictions to Dataframe."))
#    dfPred.prob <- cbind(lst_dsValidate[[1]]$QuoteConversion_Flag, pred.rpart.prob[,c(-seq(1,(2*(nfolds-1))+1,2))])
#    ## -seq(1,(2*(nfolds-1))+1,2) : -1, -3, -5, -7
#    ##
#
#    colnames(dfPred.prob) <- c("QuoteConversion_Flag", paste("p", seq(1:nfolds), sep = ""))
#

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

   ## -------------------------------------------------

