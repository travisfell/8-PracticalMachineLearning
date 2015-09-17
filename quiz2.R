# quiz 2, Practical Machine Learning

# question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

# question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(mixtures$Superplasticizer)
hist(log(mixtures$Superplasticizer))
plot(mixtures$Superplasticizer)
plot(log(mixtures$Superplasticizer))
mean(mixtures$Superplasticizer)
sd(mixtures$Superplasticizer)
mean(log(mixtures$Superplasticizer)) # this yields -Inf, meaning an negative infinite number
sd(log(mixtures$Superplasticizer))  # this yields NaN, meaning not a number
mixtures$Superplasticizer
log(mixtures$Superplasticizer)
summary(mixtures$Superplasticizer)
str(mixtures$Superplasticizer)
inTrain

# question 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingil <- training[,58:69]
#trainingil <- cbind(training[,1], training[,58:69])
#trainingilpr <- prcomp(trainingil)
q3pca <- preProcess(trainingil, method="pca", thresh = .8)


# question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
q4train <- cbind(training[,1],training[,58:69])
q4test <-  cbind(testing[,1],testing[,58:69])
colnames(q4train)[1] <- "diagnosis"
colnames(q4test)[1] <- "diagnosis"
# investigate distribution of raw data, take log?
hist(q4train[,14])
featurePlot(x=q4train[,2:13], y=q4train$diagnosis, plot="pairs")

# w/o PCA
q4fit_nopca <- train(q4train$diagnosis ~ ., method = "glm", data = q4train)
confusionMatrix(q4test$diagnosis, predict(q4fit_nopca, q4test))

#w/PCA
q4fit_pca <- train(q4train$diagnosis ~ ., method = "glm", preProcess = "pca", data = q4train)
confusionMatrix(q4test$diagnosis, predict(q4fit_pca, q4test))

preProc <- preProcess(training[,58:69], method = "pca", thresh = .8)
trainPC <- predict(preProc, training[,58:69])
modelFit <- train(q4train$diagnosis ~ ., method = "glm", data = trainPC)
testPC <- predict(preProc, testing[,58:69])
confusionMatrix(q4test$diagnosis, predict(modelFit, testPC))
