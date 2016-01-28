## Cleaning Train Data
## Imputing Train Data
## Model Creation
##    Model Validation
##    Save Model validation results
##
##  Run Multiple Models.
##  Save validation Results.
##  Compare Validation Results using confusionmatrix and ROC and Density Plot.
##
##  Cleaning Test Data
##  Imputing Test Data
##  Run Data through Train Models
##  Generate submission File
##  Save submission to Dataframe for comparison
##
##


##
## Load required packages
##

  library(dplyr)

##
## Load Helper Functions
##

source("t-functions.R", echo = FALSE)



##-----
##  Load Data
##------

  print(paste(date(), " - Loading Train and Test Datasets."))
  dtrain <- read.csv("train.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))
  dtest <- read.csv("test.csv", stringsAsFactors = FALSE, na.strings = c(""," ", "NaN", "NA", "Inf"))

  print(paste(date(), " - Generating dataset summary."))
  dfSummary.pre <- dataset_summary(dtrain, dtest, colOutcome = "QuoteConversion_Flag")




##
## Process train and test data
##
    print(paste(date(), " - Pre-process train and test data."))
    dtrain_final <- process_personal_16_17_18_19(dtrain)

    dtest_final <- process_personal_16_17_18_19(dtest)



##
## --- Process the Train Dataset
##

    print(paste(date(), " - Process Train.."))
    print(paste(date(), " - Tabulate NA values."))
    dtrain_missing <- tabulate_missing_values(dtrain_final)


    dtrain_final[,dtrain_missing$ColNames] <- lapply(dtrain_final[,dtrain_missing$ColNames],impute_missing_values)

    dtrain_final$Field10 <- gsub(",","",dtrain_final$Field10)


    dtrain_final <- tbl_df(dtrain_final)

    print(paste(date(), " - Remove unwanted columns."))
    dtrain_final <- select(dtrain_final, -Original_Quote_Date, -QuoteNumber)

    dtrain_final <- data.frame(dtrain_final)

    #colnames(dtrain_final)  <- paste(colnames(dtrain_final), "i", sep="")

    #str(dtrain_final, list.len=20)


##
## --- Process the Test Dataset
##

    print(paste(date(), " - Process Test"))
    print(paste(date(), " - Tabulate NA values."))
    dtest_missing <- tabulate_missing_values(dtest_final)

    dtest_final[,dtest_missing$ColNames] <- lapply(dtest_final[,dtest_missing$ColNames],impute_missing_values)

    dtest_final$Field10 <- gsub(",","",dtest_final$Field10)

    print(paste(date(), " - Remove unwanted columns."))
    dtest_final <- dtest_final %>% tbl_df() %>% select( -Original_Quote_Date, -QuoteNumber)

    dtest_final <- data.frame(dtest_final)
    #colnames(dtest_final)  <- paste(colnames(dtest_final), "i", sep="")

    print(paste(date(), " - Generate updated Summary and setting factors."))

    dfSummary <- dataset_summary(dtrain_final, dtest_final, colOutcome = "QuoteConversion_Flag")
    dfSummary <- data.frame(dfSummary)

    dtrain_final <- set_factor_levels(dtrain_final, dfSummary)

    dtest_final <- set_factor_levels(dtest_final, dfSummary)



