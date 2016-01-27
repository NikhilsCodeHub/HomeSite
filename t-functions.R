createFormula <- function(yterm, xterm){
  fmla <- paste(yterm, sapply(xterm, paste,collapse="+"), sep = "~")
  return(sapply(fmla, formula))
}


createModels <- function(fmla, idata, modelNm,...){
  
  ### modelNm in "lm", "rpart", "rf", "gbm", "glm"
  fit <- sapply(fmla, train, data= idata, method=modelNm, simplify = FALSE,...)
  ## simplify = TRUE --- outputs the fit in matrix format.
  ## simplify = FALSE --- outputs the fit in object format.
  ## fit <- train(fmla, data= idata, method="rf", ntrees=1000)
  return(fit)
}

createFits <- function(df){
  fitControl <- trainControl(method = "cv", number = 5)
  fit <- train(QuoteConversion_Flag~., data = df, method = "rf", trControl = fitControl)
  
}


createModelFits <- function(df, method){
  print(date())
  #print(colnames(df)[2])
  set.seed(2016)
  fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, classProbs = FALSE)
  fit <- train(as.factor(QuoteConversion_Flag)~., data = df, method = method, trControl = fitControl)
  print(date())
  return(fit)
}

## lstFit -- List of model fits
## ptype -- raw or prob
## newData -- Test dataset to predict on
generatePredictions <- function(lstFit, newData, ptype){
  print(names(lstFit))
  set.seed(2016)
  pred <- lapply(lstFit, predict.train, newdata=newData, type=ptype)
  return(as.data.frame(pred))
}

calculateRMSE <- function(predictons, reference){
  pRMSE <- sapply(as.data.frame(predictons)[,1:3], RMSE, m[15:20,1])
}

calcConfusionMx <- function(predictons, reference){
  cnfMx <- sapply(predictons, confusionMatrix, reference, simplify = FALSE)
  return(cnfMx)
}


process_personal_16_17_18_19 <- function(dfData){
  dfData$PersonalField16A <- substr(x = dfData$PersonalField16,start = 1,stop = 1)
  dfData$PersonalField16B <- substr(x = dfData$PersonalField16,start = 2,stop = 2)
  
  dfData$PersonalField17A <- substr(x = dfData$PersonalField17,start = 1,stop = 1)
  dfData$PersonalField17B <- substr(x = dfData$PersonalField17,start = 2,stop = 2)
  
  dfData$PersonalField18A <- substr(x = dfData$PersonalField18,start = 1,stop = 1)
  dfData$PersonalField18B <- substr(x = dfData$PersonalField18,start = 2,stop = 2)
  
  dfData$PersonalField19A <- substr(x = dfData$PersonalField19,start = 1,stop = 1)
  dfData$PersonalField19B <- substr(x = dfData$PersonalField19,start = 2,stop = 2)
  
#    dfData <- dfData[,-"PersonalField16"]
#    dfData <- dfData[,-"PersonalField17"]
#    dfData <- dfData[,-"PersonalField18"]
#    dfData <- dfData[,-"PersonalField19"]
   dfData <- tbl_df(dfData)
   dfData <- select(dfData,-PersonalField16, -PersonalField17, -PersonalField18, -PersonalField19)
  
  return(data.frame(dfData))
}

set_factor_levels <- function(dfData, dfSummary){

  counter <- 1
  for(col in colnames(dfData)){
    print("-------------------------")
    print(paste("Processing - ", col))
    print(dfSummary[dfSummary$colNames==col,"typeOfCol"])
    print(dfSummary[dfSummary$colNames==col,"colLevels"])
    rnames <- strsplit(dfSummary[dfSummary$colNames==col,"colLevels"],",")[[1]]
    print(dfSummary[dfSummary$colNames==col,"colNames"])
    if (dfSummary[dfSummary$colNames==col,"typeOfCol"] == "character")    
    {
      print("this is a character column type")
      print(dfSummary[dfSummary$colNames==col,"nlevels"])
      if (length(grepl("Y|N",rnames))==length(rnames) & length(rnames) == 2)
      {
        print("Found Y and N")
        print(rnames)
#         newLevels <- paste(rnames,substr(col, 1,2), counter, sep = "" )
#         dfData[,col] <- factor(dfData[,col], levels=rnames, labels = newLevels)
        dfData[,col] <- factor(dfData[,col], levels=rnames)
        
      } 
      else if (length(grepl("[A-Z]",rnames))==length(rnames)  & any(grepl("[A-Z]",rnames)))
      {
        print("Found A - Z")
        print(rnames)
#         newLevels <- paste(rnames, substr(col, 1,2), counter, sep = "" )
#         dfData[,col] <- factor(dfData[,col], levels=rnames, labels = newLevels)
        dfData[,col] <- factor(dfData[,col], levels=rnames)
      
      }
    }
    else if (dfSummary[dfSummary$colNames==col,"typeOfCol"] == "integer" & dfSummary[dfSummary$colNames==col,"nlevels"] < 27)
    {
      print("Found integers")
      dfData[,col] <- factor(dfData[,col], levels=rnames)
    } 
    else
    {
      print("Not a character type")
    }
    
    counter <- counter +1
  }
    
  return(dfData)
}


impute_missing_values <- function(col_vector){
  if (typeof(col_vector) == "integer"){
    col_vector[is.na(col_vector)] <- -10
  }
  if (length(unique(col_vector)) < 27){
    rnames <- rownames(table(col_vector))
    if (length(grepl("Y|N",rnames))==length(rnames) & length(rnames) == 2){
      misng <- length(is.na(col_vector)) 
      ratioYN <- length(col_vector[col_vector=="Y" & !is.na(col_vector)])/length(col_vector[col_vector=="N" & !is.na(col_vector)])


      if (ratioYN < 1){
        y_counts <- round(length(col_vector[is.na(col_vector)]) * ratioYN)
        if(y_counts>0){
          col_vector[is.na(col_vector)][1:y_counts] <- "Y"
          col_vector[is.na(col_vector)] <- "N"
        }
        else{
          col_vector[is.na(col_vector)] <- "N"
        }
      }
      else {
        n_counts <- round(length(col_vector[is.na(col_vector)]) / ratioYN)
        if(n_counts>0){
          col_vector[is.na(col_vector)][1:n_counts] <- "N"
          col_vector[is.na(col_vector)] <- "Y"
        }
        else{
          col_vector[is.na(col_vector)] <- "Y"
        }
      }
        
    }
  
  }  
  return(col_vector)
}


tabulate_missing_values <- function(dfData) {
  dfData <- data.frame(dfData)
  na.cols <- sapply(dfData[,1:dim(dfData)[2]],anyNA)
  
  na.cols <- data.frame(ColNames=rownames(data.frame(na.cols)),ColNA = na.cols, stringsAsFactors = FALSE)
  
  na.cols <- na.cols[na.cols$ColNA,]
  na.cols <- tbl_df(na.cols)
  
  for (col in na.cols$ColNames)
  {
    #print(col)
    na.cols$typeOfCol[na.cols$ColNames==col] <- typeof(dfData[,col])
    na.cols$uniqueGroupsCount[na.cols$ColNames==col] <- as.integer(length(unique(dfData[,col]))-1)
    if (length(unique(dfData[,col]))-1 < 27){
      na.cols$uniqueGroups[na.cols$ColNames==col] <- paste(rownames(table(dfData[,col])), collapse = ",")
    }
    else {
      na.cols$uniqueGroups[na.cols$ColNames==col] <- c(">27")
    }
    na.cols$naRowCount[na.cols$ColNames==col] <- length(dfData[is.na(dfData[,col]),col])
  }
  return(na.cols)
}

dataset_summary <- function(dfTrain, dfTest, colOutcome){
  dfTest[,colOutcome] <- NA
  dfData <- rbind(dfTrain, dfTest)
  dfData <- data.frame(dfData)

  df <-  data.frame("colNames"=colnames(dfData), stringsAsFactors = FALSE)
  for (col in colnames(dfData))
  {
    #print(col)
    df$typeOfCol[df$colNames==col] <- typeof(dfData[,col])
    #print(typeof(dfData[,col]))
    df$nlevels[df$colNames==col] <- as.integer(length(unique(dfData[,col])))
    #print(as.integer(length(unique(dfData[,col]))))
    
    if (length(unique(dfData[,col]))[1] < 27){
      df[df$colNames==col, "colLevels"] <- paste(rownames(table(dfData[,col])), collapse = ",")
      #print(paste(rownames(table(dfData[,col])), collapse = ","))
    }
    else {
      df$colLevels[df$colNames==col] <- c("More than 27")
      #print("More Than 27")
    }
    df$naRowCount[df$colNames==col] <- length(dfData[is.na(dfData[,col]),col])[1]
  }
  return(df)
  
  
}

##
## --- To be called when requiring multiple datasets based on list of rowindexes.
## 
createTrainDS <- function(ls, df){
  return(df[ls])
}

createTestDS <- function(ls, df){
  return(df[-ls])
}


split_datasets <- function(dfData){
  
  dfData <- tbl_df(dfData)
  
  dfpersonal <- select(dfData, QuoteConversion_Flag, starts_with("Field"), starts_with("PersonalField"))
  dfproperty <- select(dfData, QuoteConversion_Flag, starts_with("PropertyField"))
  dfgeo <- select(dfData, QuoteConversion_Flag, starts_with("GeographicField"))
  dfsales <- select(dfData, QuoteConversion_Flag, starts_with("SalesField"), starts_with("CoverageField"))
  dsList <- list(dfpersonal = dfpersonal, dfsales = dfsales, dfgeo= dfgeo, dfproperty=dfproperty)
  return(dsList)
}



# feature.names=names(dtrain_final)
# 
# for (f in feature.names) {
#   if (class(dtrain_final[[f]])=="factor") {
#     levels <- unique(c(dtrain_final[[f]]))
#     print(f)
#     print(levels(dtrain_final[[f]]))
#     make.names(levels(dtrain_final[[f]]))
#   }
# }

check_colNames <- function(df){
  for(col in colnames(df))
  {
    if(class(df[,col])=="factor")
    {
      print(col)
      print(paste(levels(df[,col]), collapse = ", "))
      #print(make.names(levels(df[,col])))
    }
    
  }
}
