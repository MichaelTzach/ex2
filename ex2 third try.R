workspaceDir = "/Users/michaeltzach/Developer/R/ex2/Titanic"
setwd(workspaceDir)

###Read and preprocess the training data

#turn na strings to empty strings to avoid issues when creating the model
#also, avoid turingn strings to factor automatically
mapNamesToTitles = function(y) {
  namesAsChars = lapply(y, as.character)
  firstPartOfNamesBeforeDots = lapply(namesAsChars, function(x) strsplit(x, '.', fixed = TRUE)[[1]][[1]])
  titlesPartOfNamesAsList = lapply(firstPartOfNamesBeforeDots, function(x) strsplit(x, ', ', fixed = TRUE)[[1]][[2]])
  titlesPartOfNamesAsList = lapply(titlesPartOfNamesAsList, function(x) if (x == "Dona") NA else x)
  return(factor(unlist(titlesPartOfNamesAsList)))
}

preprocessData = function(data, isTestData) {
  data$Pclass = factor(data$Pclass)
  data$Title = mapNamesToTitles(data$Name)
  if (isTestData) {
    data = subset(data, select=-c(Ticket, Cabin, Name))  
  } else {
    data$Survived = factor(data$Survived)
    data = subset(data, select=-c(Ticket, Cabin, PassengerId, Name))  
  }
}

dataFromFile = function(fileName, isTestData) {
  data = read.csv(fileName, na.string="")
  preprocessedData = preprocessData(data, isTestData)
  return(preprocessedData)
}

trainingData = dataFromFile("train.csv", FALSE)
testData = dataFromFile("test.csv", TRUE)
testDataWithPassengerIds = testData
testDataPassengerIds<- testData$PassengerId
testData = subset(testData, select=-c(PassengerId))

#install C50 and add the lib
if(!require(C50)){
  install.packages('C50')
}
library('C50')
set.seed(123)

#Print some data on things
trainingData.Cols<- 1:dim(trainingData)[2]
trainingData.factors <- trainingData.Cols[sapply(trainingData,is.factor)]
trainingData.numerics <- trainingData.Cols[!sapply(trainingData,is.factor)]
if(!require(tidyr)) {
  install.packages("tidyr")
}
library(tidyr)
#tidy data
trainingData.tidy_factors<-gather(trainingData[,trainingData.factors],"feature","value",-1)
trainingData.tidy_numerics<-gather(cbind(Survived=trainingData[,1],trainingData[,trainingData.numerics]),"feature","value",-1)
if(!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)
qplot(x=value,data=trainingData.tidy_factors,fill=Survived) + facet_grid(~feature,scales="free")
qplot(x=value,data=trainingData.tidy_numerics,fill=Survived) + facet_grid(~feature,scales="free")

if(!require(caret)) {
  install.packages("caret")
}
library(caret)
if(!require(e1071)) {
  install.packages("e1071")
}
library(e1071)

control <- trainControl(method="cv", number=10)
fit.c50 <- train(Survived~., data=trainingData, method="C5.0", metric="Accuracy", trControl=control,na.action = na.pass)
fit.c50$xlevels[["Title"]] <- union(fit.c50$xlevels[["Title"]], levels(testData$Title))

fit.c50.testPredictions = predict(fit.c50,testData,na.action = na.pass)

#Create the result of the prediction
fit.c50.res <- cbind(PassengerId=testDataPassengerIds,Survived=as.character(fit.c50.testPredictions))

#Create the rpart model
if(!require(rpart)) {
  install.packages('rpart')
}
library(rpart)
if(!require(rattle)) {
  install.packages("https://togaware.com/access/rattle_5.0.14.tar.gz", repos=NULL, type="source")
}
library(rattle)
if(!require(rpart.plot)) {
  install.packages('rpart.plot')
}
library(rpart.plot)

#Use recursive partitioning to have more predictions
rpart.trainingDataModel <- rpart(Survived~., data = trainingData)
rpart.predictions <- predict(rpart.trainingDataModel, testData,type = "class")
rpart.res <- cbind(PassengerId=testDataPassengerIds,Survived=as.character(rpart.predictions))

#Plot the rpart model
fancyRpartPlot(rpart.trainingDataModel)

if(!require(caretEnsemble)) {
  install.packages('caretEnsemble')
}
library(caretEnsemble)

#Clean up the data for the ensamble
cleanUpBeforeEnsamble = function(data) {
  data = na.omit(data)
  feature.names=names(data)
  for (f in feature.names) {
    if (class(data[[f]])=="factor") {
      levels <- unique(c(data[[f]]))
      data[[f]] <- factor(data[[f]],labels=make.names(levels))
    }
  }
  return(data)
}
trainingData = cleanUpBeforeEnsamble(trainingData)

testData = cleanUpBeforeEnsamble(testDataWithPassengerIds)
passengerIdsPredictedWithensamble = testData$PassengerId
passengerIdsNotPredictedWithEnsamble = testDataPassengerIds[!(testDataPassengerIds %in% passengerIdsPredictedWithensamble)]
testData = subset(testData, select=-c(PassengerId))  
  
ensambleModel.TrainControl = trainControl(method = "cv", number = 10, savePredictions='final', classProbs=TRUE)
ensambleModel.tuneList.C50 = caretModelSpec(method="C5.0",tuneGrid=data.frame(.trials=50, .model = 'tree', .winnow =FALSE))
ensambleModel.tuneList.XCGT = caretModelSpec(
    method = "xgbTree",
    tuneGrid = data.frame(
      .nrounds=30,
      .max_depth=3,
      .eta=0.4,
      .gamma=0.1,
      .colsample_bytree=0.7,
      .min_child_weight=0.01,
      .subsample=0.7
    )
  )  
ensambleModel.tuneList = list(C50=ensambleModel.tuneList.C50, xgbTree=ensambleModel.tuneList.XCGT)
modelsForEnsamble<-caretList(Survived~., data=trainingData, trControl=ensambleModel.TrainControl, metric="Accuracy",tuneList=ensambleModel.tuneList)

resultsFromEnsamble <- resamples(modelsForEnsamble)
summary(resultsFromEnsamble)

ensamble <- caretEnsemble(modelsForEnsamble)
ensamblePredictions = predict(ensamble, newdata=testData, type="raw")
ensamblePredictions = unlist(lapply(as.character(ensamblePredictions), function(x) if (x == "X1") 0 else 1))
ensamblePredictions.res <- cbind(PassengerId=passengerIdsPredictedWithensamble,Survived=ensamblePredictions)
rpartResultsToCompleteResults = rpart.res[rpart.res[, "PassengerId"] %in% passengerIdsNotPredictedWithEnsamble,]
finalResults = rbind(ensamblePredictions.res, rpartResultsToCompleteResults)

write.csv(finalResults,file="try3.csv",row.names = F)
