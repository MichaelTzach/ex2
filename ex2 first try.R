workspaceDir = "/Users/michaeltzach/Developer/R/ex2/Titanic"
setwd(workspaceDir)

###Read and preprocess the training data

#turn na strings to empty strings to avoid issues when creating the model
#also, avoid turingn strings to factor automatically
trainingData <- read.csv("train.csv", na.string="")
trainingData$Survived = factor(trainingData$Survived)
trainingData$Pclass = factor(trainingData$Pclass)

#head(trainingData)

#remove unwanted features from the data frame
trainingData = subset(trainingData, select=-c(Name, Ticket, Cabin, PassengerId))

### Read and preprocess the training data
#turn na strings to empty strings to avoid issues when creating the model
#also, avoid turingn strings to factor automatically
testData <- read.csv("test.csv", na.string="")
testData$Survived = factor(testData$Survived)
testData$Pclass = factor(testData$Pclass)

#head(testData)

#remove unwanted features from the data frame
testDataPassengerIds<- testData$PassengerId
testData = subset(testData, select=-c(Name, Ticket, Cabin, PassengerId))

##Divide the training data to a training part and evaluation part
indices <- sample(1:nrow(trainingData),nrow(trainingData)*0.75)
trainingData.train<- trainingData[indices,]
trainingData.test<- trainingData[-indices,]

#install C50 and add the lib
if(!require(C50)){
  install.packages('C50')
}
library('C50')
set.seed(123)

#Use C50 and predict on the test part
C50training <-C5.0(Survived ~., data=trainingData.train )
#plot(C50training)

trainingPredictions = predict(C50training, trainingData.test)
table(trainingPredictions,trainingData.test$Survived)
mean(trainingPredictions==trainingData.test$Survived)

#Create a prediction on the full training data and run it on the test data
C50 <-C5.0(Survived ~., data=trainingData )
plot(C50training)
testPredictions = predict(C50training, testData)

#Create the result of the prediction
res <- cbind(PassengerId=testDataPassengerIds,Survived=as.character(testPredictions))
write.csv(res,file="try1.csv",row.names = F)
