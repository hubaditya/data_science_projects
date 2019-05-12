#install.packages('car', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(caret)
library(tidyr)

train_set <- read.csv("Boston Housing Data.csv")
head(train_set, 10)

##########################EDA##############################

#Column classes
str(train_set)

#Mean, Median, Std. Dev. Q1, Q3
summary(train_set)

#Get uniqueness of independent variables
length(unique(train_set$CRIM))
length(unique(train_set$ZN))
length(unique(train_set$INDUS))
length(unique(train_set$CHAS))
length(unique(train_set$NOX))
length(unique(train_set$RM))
length(unique(train_set$AGE))
length(unique(train_set$DIS))
length(unique(train_set$RAD))
length(unique(train_set$TAX))
length(unique(train_set$PTRATIO))
length(unique(train_set$B))
length(unique(train_set$LSTAT))
length(unique(train_set$MEDV))

#Get count to check for over/under sampling
table(train_set$CRIM)
table(train_set$ZN)
table(train_set$INDUS)
table(train_set$CHAS)
table(train_set$NOX)
table(train_set$RM)
table(train_set$AGE)
table(train_set$DIS)
table(train_set$RAD)
table(train_set$TAX)
table(train_set$PTRATIO)
table(train_set$B)
table(train_set$LSTAT)
table(train_set$MEDV)

#############Visualization###################################

#Histogram
hist(train_set$CRIM)
hist(train_set$ZN)
hist(train_set$INDUS)
hist(train_set$CHAS)
hist(train_set$NOX)
hist(train_set$RM)
hist(train_set$AGE)
hist(train_set$DIS)
hist(train_set$RAD)
hist(train_set$TAX)
hist(train_set$PTRATIO)
hist(train_set$B)
hist(train_set$LSTAT)
hist(train_set$MEDV)

#Plot Correlation matrix
library(corrplot)
corrplot(cor(train_set), method = "color", type = "lower")

# Removing columns which are collinear
train_set$ZN <- NULL
train_set$DIS <- NULL
train_set$RAD <- NULL
train_set$AGE <- NULL
train_set$NOX <- NULL
train_set$CRIM <- NULL
train_set$B <- NULL
train_set$CHAS <- NULL

########Splitting data into training and testing############

index <- createDataPartition(train_set$MEDV, p = 0.7, list = F)
trainSet <- train_set[index,]
testSet <- train_set[-index,]

###############Algorithms############################

#Multiple Linear Regression

fit <- lm(MEDV ~ ., data = trainSet)
summary(fit)

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-6]), trainSet$MEDV))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-6]), testSet$MEDV))

trainRMSE <- RMSE(train_pred$V1, train_pred$V2)
testRMSE <- RMSE(test_pred$V1, test_pred$V2)
rm(fit, test_pred, train_pred, testRMSE, trainRMSE)

#Kernel SVM

library(e1071)
fit <- svm(MEDV ~ .,
           data = trainSet,
           kernel = "radial")

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-6], type = "eps-regression"), trainSet$MEDV))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-6], type = "eps-regression"), testSet$MEDV))

trainRMSE <- RMSE(train_pred$V1, train_pred$V2)
testRMSE <- RMSE(test_pred$V1, test_pred$V2)
rm(fit, test_pred, train_pred, testRMSE, trainRMSE)

#Decision Tree

library(rpart)
fit <- rpart(MEDV ~ ., data = trainSet)

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-6]), trainSet$MEDV))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-6]), testSet$MEDV))

trainRMSE <- RMSE(train_pred$V1, train_pred$V2)
testRMSE <- RMSE(test_pred$V1, test_pred$V2)
rm(fit, test_pred, train_pred, testRMSE, trainRMSE)

#Random Forest

library(randomForest)
fit <- randomForest(x = trainSet[-6], y = trainSet$MEDV, ntree = 100)

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-6]), trainSet$MEDV))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-6]), testSet$MEDV))

trainRMSE <- RMSE(train_pred$V1, train_pred$V2)
testRMSE <- RMSE(test_pred$V1, test_pred$V2)
rm(fit, test_pred, train_pred, testRMSE, trainRMSE)

#XGBoost using K-Fold cross validation

library(xgboost)
library(stringi)

folds <- createFolds(trainSet$MEDV, k = 10)

cv <- lapply(folds, function(x) {
  training_fold <- trainSet[-x,]
  test_fold <- trainSet[x,]
  fit <- xgboost(data = as.matrix(training_fold[,-6]),
                 label = training_fold$MEDV,
                 nrounds = 20)
  test_pred <<- as.data.frame(cbind(predict(fit, newdata = as.matrix(test_fold[,-6])), test_fold$MEDV))
  meanError <- RMSE(test_pred$V1, test_pred$V2)
  return(meanError)
})

meanError <- mean(as.numeric(cv))
rm(cv, meanError)
