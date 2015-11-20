# PML quiz 4

library(caret)
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)

# question 1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
head(vowel.train)
vowel.train$y <-as.factor(vowel.train$y)
vowel.test$y <-as.factor(vowel.test$y)
set.seed(33833)
q1rf<- train(y~., data = vowel.train, method = "rf")
save(q1rf, file = "q1rf.rda")
q1PredRF <- predict(q1rf, vowel.test)
confusionMatrix(vowel.test$y, q1PredRF)

q1gbm<- train(y~., data = vowel.train, method = "gbm", verbose = FALSE)
save(q1gbm, file = "q1gbm.rda")
q1PredGBM <- predict(q1gbm, vowel.test)
confusionMatrix(vowel.test$y, q1PredGBM)


# question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
q2rf <- train(diagnosis ~ .,  method = "rf", data = training, trControl = trainControl(method="cv"), number = 3)
save(q2rf, file = "q2rf.rda")
q2gbm<- train(diagnosis ~ ., data = training, method = "gbm", verbose = FALSE)
save(q2gbm, file = "q2gbm.rda")
q2lda <- train(diagnosis ~ ., data = training, method = "lda")
save(q2lda, file = "q2lda.rda")
q2PredRF <- predict(q2rf, testing)
q2PredGBM <- predict(q2gbm, testing)
q2PredLDA <- predict(q2lda, testing)
confusionMatrix(testing$diagnosis, q2PredRF)$overall
confusionMatrix(testing$diagnosis, q2PredGBM)$overall
confusionMatrix(testing$diagnosis, q2PredLDA)$overall
q2predDF <- data.frame(q2PredRF, q2PredGBM, q2PredLDA, diagnosis = testing$diagnosis)
q2combMod <- train(diagnosis ~ ., method = "rf", data = q2predDF, trControl = trainControl(method="cv"), number = 3)
q2predComb <- predict(q2combMod, testing)
confusionMatrix(testing$diagnosis, q2predComb)$overall


# question 3

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
library(lars)
#q3mod <- train(CompressiveStrength ~ ., data = training, method = "lasso")
x <- as.matrix(training[,1:8])
y <- as.matrix(training[,9])
#q3mod <- lars(x, y, type = "lasso")
q3mod <- enet(x, y, lambda = 0)
summary(q3mod)
plot.enet(q3mod)


# question 4
setwd("C:/Users/fellt/Desktop/Data Science/Coursera Data Science Specialization/08 - Practical Machine Learning/8-PracticalMachineLearning")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", destfile = "gaData.csv")
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
dat$date <- as.Date(dat$date)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
summary(dat)
str(dat)
q4mod <- bats(tstrain)
h <- nrow(testing)
q4fcast <- forecast(q4mod, h, level = 95)

plot(q4fcast)
