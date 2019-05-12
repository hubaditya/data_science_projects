# install.packages('data.table', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(caret)
library(tidyr)

train_set <- read.csv("Product_Sales_Data.csv")
head(train_set, 10)

##########################EDA##############################

#Column classes
str(train_set)

#Get count to check for over/under sampling
table(train_set$Item_Fat_Content)
table(train_set$Item_Type)
table(train_set$Outlet_Identifier)
table(train_set$Outlet_Identifier)
table(train_set$Outlet_Establishment_Year)
table(train_set$Outlet_Size)
table(train_set$Outlet_Location_Type)
table(train_set$Outlet_Type)

#Get uniqueness of independent variables
length(unique(train_set$Item_Identifier))
length(unique(train_set$Outlet_Identifier))

#Mean, Median, Std. Dev. Q1, Q3
summary(train_set)


#############Data Preprocessing#############################

# set item weight with the mean of weight of that item
for(i in unique(train_set$Item_Identifier)) {
  train_set$Item_Weight[train_set$Item_Identifier == i] <- ifelse(
    is.na(train_set$Item_Weight[train_set$Item_Identifier==i]), 
    mean(train_set$Item_Weight[train_set$Item_Identifier==i], na.rm  = T), 
    train_set$Item_Weight[train_set$Item_Identifier==i])
}

# Set Outlet Size depending on mode of outlet size when individual outlet type selected
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv <- uniqv[!uniqv %in% ""]
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

for(i in unique(train_set$Outlet_Type)) {
  train_set$Outlet_Size[train_set$Outlet_Type == i] <- ifelse(
    train_set$Outlet_Size[train_set$Outlet_Type==i] == "", 
    as.character(getmode(train_set$Outlet_Size[train_set$Outlet_Type == i])), 
    as.character(train_set$Outlet_Size[train_set$Outlet_Type == i]))
}

# Remove 0 as factor
write.csv(train_set, "x.csv", row.names = F)
train_set <- read.csv("x.csv")

# Visibility of an item cannot be 0 so replacing it with mean values
for(i in unique(train_set$Item_Identifier)) {
  train_set$Item_Visibility[train_set$Item_Identifier == i] <- ifelse(
    train_set$Item_Visibility[train_set$Item_Identifier==i] == 0, 
    mean(train_set$Item_Visibility[train_set$Item_Identifier==i][train_set$Item_Visibility[train_set$Item_Identifier==i] != 0]), 
    train_set$Item_Visibility[train_set$Item_Identifier==i])
}

# Dividing item visibility with mean of that item's visibility as more visible means more sales
for(i in unique(train_set$Item_Identifier)) {
  train_set$Item_Visibility[train_set$Item_Identifier == i] <- train_set$Item_Visibility[train_set$Item_Identifier == i]/(mean(train_set$Item_Visibility[train_set$Item_Identifier==i]))
}

# Creating a broad category of items
train_set$Item_Type <- as.factor(ifelse(substr(train_set$Item_Identifier, 0, 2) == "FD", "Food", 
                                        ifelse(substr(train_set$Item_Identifier, 0, 2) == "NC", "Non-Consumable",
                                               ifelse(substr(train_set$Item_Identifier, 0, 2) == "DR", "Drinks", "Others"))))

# Uniformity among item fat content name and fat is non edible if non-consumable
train_set$Item_Fat_Content <- as.factor(ifelse((as.character(train_set$Item_Fat_Content) == "LF" | as.character(train_set$Item_Fat_Content) == "low fat") , "Low Fat", 
                                                ifelse(as.character(train_set$Item_Fat_Content) == "reg", "Regular", as.character(train_set$Item_Fat_Content))))
train_set$Item_Fat_Content <- as.factor(ifelse(as.character(train_set$Item_Type) == "Non-Consumable", "Non-Edible", as.character(train_set$Item_Fat_Content)))

#Year establishment converting to ordinal
train_set$Outlet_Establishment_Year <- ifelse(train_set$Outlet_Establishment_Year <= 1996, 3, 
                                                        ifelse(train_set$Outlet_Establishment_Year <= 2003, 2, 1))

#train_set$Outlet_Establishment_Year <- 2013 - train_set$Outlet_Establishment_Year

train_set <- train_set[!is.na(train_set$Item_Weight),]

# Rounding off numbers
train_set$Item_Weight <- round(train_set$Item_Weight, 1)
train_set$Item_MRP <- round(train_set$Item_MRP, 0)
train_set$Item_Visibility <- round(train_set$Item_Visibility, 2)
train_set$Item_Outlet_Sales <- round(train_set$Item_Outlet_Sales, 0)

# One Hot encoding
dmy <- dummyVars(" ~ .", data = train_set[-1])
train_set <- data.frame(predict(dmy, newdata = train_set[-1]))

#############Visualization###################################

#Plot Correlation matrix
library(corrplot)
corrplot(cor(train_set[,-30]), method = "color", type = "lower")

# Removing columns which are collinear
train_set$Outlet_Identifier.OUT013 <- NULL
train_set$Outlet_Identifier.OUT018 <- NULL
train_set$Outlet_Identifier.OUT027 <- NULL

########Splitting data into training and testing############

index <- createDataPartition(train_set$Item_Outlet_Sales, p = 0.7, list = F)
trainSet <- train_set[index,]
testSet <- train_set[-index,]

###############Algorithms############################

#Multiple Linear Regression

fit <- lm(Item_Outlet_Sales ~ ., data = trainSet)
train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-31]), trainSet$Item_Outlet_Sales))
test_pred <- predict(fit, newdata = testSet[-31])

#Kernel SVM

library(e1071)
fit <- svm(Item_Outlet_Sales ~ .,
           data = trainSet,
           kernel = "radial")

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-23], type = "eps-regression"), trainSet$Item_Outlet_Sales))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-23], type = "eps-regression"), testSet$Item_Outlet_Sales))

x <- RMSE(train_pred$V1, train_pred$V2)
y <- RMSE(test_pred$V1, test_pred$V2)

#Decision Tree

library(rpart)
fit <- rpart(Item_Outlet_Sales ~ ., data = trainSet)

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-31]), trainSet$Item_Outlet_Sales))
test_pred <- predict(fit, newdata = testSet[-31])

#Random Forest

library(randomForest)
fit <- randomForest(x = trainSet[-23], y = trainSet$Item_Outlet_Sales, ntree = 300)

train_pred <- as.data.frame(cbind(predict(fit, newdata = trainSet[-28]), trainSet$Item_Outlet_Sales))
test_pred <- as.data.frame(cbind(predict(fit, newdata = testSet[-28]), testSet$Item_Outlet_Sales))

x <- RMSE(train_pred$V1, train_pred$V2)
y <- RMSE(test_pred$V1, test_pred$V2)

#XGBoost using K-Fold cross validation

library(xgboost)
library(stringi)

folds <- createFolds(trainSet$Item_Outlet_Sales, k = 10)

cv <- lapply(folds, function(x) {
  training_fold <- trainSet[-x,]
  test_fold <- trainSet[x,]
  fit <- xgboost(data = as.matrix(training_fold[,-28]),
                 label = training_fold$Item_Outlet_Sales,
                 nrounds = 300)
  test_pred <- as.data.frame(cbind(predict(fit, newdata = as.matrix(test_fold[,-28])), test_fold$Item_Outlet_Sales))
  meanError <- RMSE(test_pred$V1, test_pred$V2)
  return(meanError)
})

meanError <- mean(as.numeric(cv))
