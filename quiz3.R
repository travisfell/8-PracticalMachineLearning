#  quiz 3

library(caret)
library(AppliedPredictiveModeling)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(rattle)

# question 1
data(segmentationOriginal)
head(segmentationOriginal)
View(segmentationOriginal)
str(segmentationOriginal)
inTrain <- createDataPartition(y=segmentationOriginal$Case)

#training <- segmentationOriginal[inTrain,]
#testing <- segmentationOriginal[-inTrain,]

training <- subset(segmentationOriginal, Case == 'Train')
testing <- subset(segmentationOriginal, Case == 'Test')

set.seed(125)
modFit <- train(Class ~., method = "rpart", data = training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="classification tree")
text (modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(modFit$finalModel)

# question 2


# question 3
library(pgmm)
data(olive)
olive = olive[,-1]

inTrain <- createDataPartition(y = olive$Area, p = .7, list = FALSE)
training <- olive[inTrain,]
testing <- olive[-inTrain,]
newdata = as.data.frame(t(colMeans(olive)))
modFit <- train(Area ~., method = "rpart", data=training)
print(modFit$finalModel)
predict(modFit, newdata = newdata)

#  question 4
library(ggplot2)
library(ElemStatLearn)
data(SAheart)
set.seed(13234)
#SAheart$chd <- as.factor(SAheart$chd) #previously got error message when training model
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F) #sampling 50% of the population
trainSA = SAheart[train,]
testSA = SAheart[-train,]
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
#q4fit <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, family = "binomial")
q4fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, data = trainSA, method = "glm", family = "binomial")
predTrain <- predict(q4fit, trainSA)
predTest <- predict(q4fit, testSA)
missClass(trainSA$chd, predTrain)
missClass(testSA$chd, predTest)

sum(((prediction > 0.5)*1) != values)/length(values)
sum(((predTrain > 0.5)*1) != trainSA)/length(trainSA)


#question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
set.seed(33833)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
q5fit <- train(y~., data = vowel.train, method = "rf", prox=TRUE)
varImp(q5fit)

