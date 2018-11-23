setwd("C:/Users/CHIRAG/Downloads/ACADgILd")
WLE<- read.csv("WLE.csv",header=T, na.strings=c("","NA"))
data<-WLE
View(data)
summary(data)
summary(data$classe)
names(data)
library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
library(helpRFunctions)
training<-data[1:4000,]
testing<-data[4001:4024,]
dim(training)
summary(training)
str(training)
predictorIdx <- c(grep("^accel", names(training)), grep("^gyros", names(train
                                                                        ing)),
                  grep("^magnet", names(training)), grep("^roll", names(train
                                                                        ing)), grep("^pitch", names(training)), grep("^yaw", names(training)), grep(
                                                                          "^total", names(training)))
trainPredSet <- training[, c(predictorIdx, 157)]
testPredSet <- testing[, c(predictorIdx, 157)]
length(predictorIdx)
sum(names(testing)[predictorIdx] != names(training)[predictorIdx])
#sum(is.na(trainPredSet)) color = trainPredSet$classe)
nearZeroVar(trainPredSet[, -7], saveMetric = TRUE)
qplot(x = trainPredSet[, "accel_belt_x"], y = trainPredSet[, "accel_arm_x"],c
      olor = trainPredSet$classe)
set.seed(125)
inTrain <- createDataPartition(y = trainPredSet$classe, p = 0.8, list = FALSE
)
cvTrain <- trainPredSet[inTrain, ]
cvTest <- trainPredSet[-inTrain, ]
fitCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(125)
modFit <- train(classe ~ ., data = cvTrain, method = "qda", preProcess = c("c
                                                                           enter","scale"), trControl = fitCtrl)

print(modFit)
ptrain <- predict(modFit, newdata = cvTrain)
equalPredTrain <- (ptrain == cvTrain$classe)
print(sum(equalPredTrain)/length(equalPredTrain))
confusionMatrix(data = ptrain, reference = cvTrain$classe)
ptest <- predict(modFit, newdata = cvTest)
equalPredTest <- (ptest == cvTest$classe)
print(sum(equalPredTest)/length(equalPredTest))
testPrediction <- predict(modFit, newdata = testing)
print(rbind(testing[1:20, 157], as.character(testPrediction)))

library(devtools)
install_github('adam-m-mcelhinney/helpRFunctions')
## Skipping install of 'helpRFunctions' from a github remote, the SHA1 (9eb16
e8c) has not changed since last install.
## Use `force = TRUE` to force installation
library(helpRFunctions)
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
training<-data[1:4000,]
testing<-data[4001:4024,]
dim(training)
## [1] 4000 157
str(training)

dim(training)
## [1] 4000 157
str(training$classe)
## Factor w/ 5 levels "A","B","C","D",..: 5 5 5 5 5 5 5 5 5 5 ...
summary(training$classe)
## A B C D E
## 1365 901 88 276 1370
names(testing)[names(testing) != names(training)]
## character(0)
sum(is.na(training))
## [1] 23834
sum(is.na(testing))
## [1] 142
names(training)
predictorIdx <- c(grep("^accel", names(training)), grep("^gyros", names(train
                                                                        ing)),
                  grep("^magnet", names(training)), grep("^roll", names(train
                                                                        ing)), grep("^pitch", names(training)), grep("^yaw", names(training)), grep(
                                                                          "^total", names(training)))
trainPredSet <- training[, c(predictorIdx, 157)]
testPredSet <- testing[, c(predictorIdx, 157)]
length(predictorIdx)
## [1] 52
sum(names(testing)[predictorIdx] != names(training)[predictorIdx])
## [1] 0
#sum(is.na(trainPredSet)) color = trainPredSet$classe)
nearZeroVar(trainPredSet[, -7], saveMetric = TRUE)

set.seed(125)
inTrain <- createDataPartition(y = trainPredSet$classe, p = 0.8, list = FALSE
)
cvTrain <- trainPredSet[inTrain, ]
cvTest <- trainPredSet[-inTrain, ]
fitCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(125)
modFit <- train(classe ~ ., data = cvTrain, method = "qda", preProcess = c("c
enter","scale"), trControl = fitCtrl)

print(modFit)

ptrain <- predict(modFit, newdata = cvTrain)
equalPredTrain <- (ptrain == cvTrain$classe)
print(sum(equalPredTrain)/length(equalPredTrain))
## [1] 0.9971884
confusionMatrix(data = ptrain, reference = cvTrain$classe)

ptest <- predict(modFit, newdata = cvTest)
equalPredTest <- (ptest == cvTest$classe)
print(sum(equalPredTest)/length(equalPredTest))
## [1] 0.9899875
testPrediction <- predict(modFit, newdata = testing)
print(rbind(testing[1:20, 157], as.character(testPrediction)))