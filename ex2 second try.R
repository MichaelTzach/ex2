workspaceDir = "/Users/michaeltzach/Developer/R/ex2/Titanic"
setwd(workspaceDir)

###Read and preprocess the training data

#turn na strings to empty strings to avoid issues when creating the model
#also, avoid turingn strings to factor automatically
mapNamesToTitles = function(y) {
  namesAsChars = lapply(y, as.character)
  firstPartOfNamesBeforeDots = lapply(namesAsChars, function(x) strsplit(x, '.', fixed = TRUE)[[1]][[1]])
  titlesPartOfNamesAsList = lapply(firstPartOfNamesBeforeDots, function(x) strsplit(x, ', ', fixed = TRUE)[[1]][[2]])
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

testPredictions = predict(fit.c50,testData,na.action = na.pass)

#Create the result of the prediction
res <- cbind(PassengerId=testDataPassengerIds,Survived=as.character(testPredictions))
write.csv(res,file="try2.csv",row.names = F)
