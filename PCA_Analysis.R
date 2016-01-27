### PCA Method to identify variables

# colnames(train.trainset)
# [1] "PassengerId"     "Survived"        "Pclass"         
# [4] "Name"            "Sex"             "Age"            
# [7] "SibSp"           "Parch"           "Ticket"         
# [10] "Fare"            "Cabin"           "Embarked"       
# [13] "Age_Imputed"     "family_size"     "Fare_category"  
# [16] "Age_Imputed_Cat" "Women_Children" 
# 
# colnames(select(train.trainset, -4, -6, -9, -11))
# [1] "PassengerId"     "Survived"        "Pclass"         
# [4] "Sex"             "SibSp"           "Parch"          
# [7] "Fare"            "Embarked"        "Age_Imputed"    
# [10] "family_size"     "Fare_category"   "Age_Imputed_Cat"
# [13] "Women_Children" 
train.trainset.p<-select_(train.trainset, -1, -4, -6, -9, -11)

train.trainset.pre <- preProcess(train.trainset.p[,-2], method="pca", thresh = .85)
train.trainset.pc <- predict(train.trainset.pre, train.trainset.p[,-2])

train.trainset.fit<-train(as.factor(train.trainset.p$Survived)~.,method="rpart", data=train.trainset.pc)

train.testset.pc<-predict(train.trainset.pre, newdata = select_(train.testset,-1, -2,-4,-6,-9,-11))

p<- predict(train.trainset.fit,train.testset.pc,type = "raw")

confusionMatrix(train.testset$Survived, predict(train.trainset.fit,train.testset.pc))

### Running on TestData

testData.pc <- predict(train.trainset.pre, testData)

p<-predict(train.trainset.fit, testData.pc)

my_filedata <- cbind(as.numeric(testData$PassengerId), data.frame("Survived"=p))

write.csv(my_solution, file=paste0("Titanic_Submission_", format(Sys.time(), "%m%d%Y%H%M"),".csv") , row.names=FALSE)
