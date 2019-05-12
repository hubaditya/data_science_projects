#install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(caret)
library(tidyr)

data("iris")
head(iris, 10)

########Splitting data into training and testing############

index <- createDataPartition(iris$Species, p = 0.7, list = F)
trainSet <- iris[index,]
testSet <- iris[-index,]

##########################EDA##############################

#Get count to check for over/under sampling
table(iris$Species)

#Get uniqueness of independent variables
length(unique(iris$Sepal.Length))

#Plot Correlation matrix
library(corrplot)
corrplot(cor(iris[,-5]), method = "color", type = "lower")

#Column classes
str(trainSet)

#Mean, Median, Std. Dev. Q1, Q3
summary(trainSet)

#Types of outcomes
levels(trainSet$Species)

####################Visualization###########################

#Histogram
hist(trainSet$Sepal.Width)

#Scatter Plots
plot(trainSet$Petal.Length, trainSet$Petal.Width)

#Show 4 graphs in 1
par(mfrow=c(1,4))

#Box Plot
for(i in 1:4) {
  boxplot(trainSet[,i], main=names(trainSet)[i])
}

#GGPlot
ggplot(trainSet, aes(x = Sepal.Width)) +
  geom_histogram(binwidth=0.2, color="black", aes(fill=Species)) +
  xlab("Sepal Width") +
  ylab("Frequency") +
  ggtitle("Histogram of Sepal Width")

ggplot(data=trainSet, aes(x = Petal.Length, y = Petal.Width))+
  geom_point(aes(color=Species, shape=Species)) +
  xlab("Petal Length") +
  ylab("Petal Width") +
  ggtitle("Petal Length-Width")

###############Algorithms############################

trainSet[-5] <- scale(trainSet[-5]) 
testSet[-5] <- scale(testSet[-5])

#Multinomial Logistic Regression

library(nnet)
fit <- multinom(Species ~ ., data = trainSet)
train_pred <- predict(fit, newdata = trainSet[-5], type = "probs")
test_pred <- predict(fit, newdata = testSet[-5], type = "probs")

#K-NN

library(class)
test_pred <- knn(train = trainSet[, -5],
                 test = testSet[, -5],
                 cl = trainSet[, 5],
                 k = 5)

cm <- table(testSet[, 5], test_pred)

#Kernel SVM

library(e1071)
fit <- svm(Species ~ .,
           data = trainSet,
           kernel = "radial")

train_pred <- predict(fit, newdata = trainSet[-5], type = "C-classification")
cm <- table(trainSet[, 5], train_pred)

test_pred <- predict(fit, newdata = testSet[-5], type = "C-classification")
cm <- table(testSet[, 5], test_pred)

#Decision Tree

library(rpart)
fit <- rpart(Species ~ ., data = trainSet)

train_pred <- predict(fit, newdata = trainSet[-5], type = "class")
cm <- table(trainSet[, 5], train_pred)

test_pred <- predict(fit, newdata = testSet[-5], type = "class")
cm <- table(testSet[, 5], test_pred)

#Random Forest

library(randomForest)
fit <- randomForest(x = trainSet[-5], y = trainSet$Species, ntree = 100)

train_pred <- predict(fit, newdata = trainSet[-5])
cm <- table(trainSet[, 5], train_pred)

test_pred <- predict(fit, newdata = testSet[-5])
cm <- table(testSet[, 5], test_pred)

#XGBoost using K-Fold cross validation

library(xgboost)
library(stringi)

folds <- createFolds(trainSet$Species, k = 5)

cv <- lapply(folds, function(x){
  training_fold <- trainSet[-x,]
  test_fold <- testSet[x,]
  fit <- xgboost(data = as.matrix(training_fold[,-5]),
                 label = training_fold$Species,
                 nrounds = 5)
  test_pred <- predict(fit, newdata = as.matrix(test_fold[,-5]))
  test_pred <- (test_pred >= 0.5)
  cm <- table(test_fold[,5], test_pred)
  accuracy <- (cm[1,1]+cm[2,1]+cm[3,1])/45
  return(accuracy)
})

accuracy <- mean(as.numeric(cv))
