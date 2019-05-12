#install.packages('missForest', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(caret)
library(tidyr)

tr <- read.csv("Loan_Data.csv", na.strings = c("", "NA"))
head(tr, 10)


##########################EDA##############################

#Get count to check for over/under sampling
table(tr$Dependents)

#Get uniqueness of independent variables
length(unique(tr$LoanAmount))

#Column classes
str(tr)

#Mean, Median, Std. Dev. Q1, Q3
summary(tr)

#Types of outcomes
levels(tr$Loan_Status)


####################Data Preprocessing######################

library(plyr)
tr$Dependents <- revalue(tr$Dependents, c("3+" = "3"))
tr$Dependents <- as.factor(tr$Dependents)

#Imputing missing values
library(mice)
md.pattern(tr)
tempData <- mice(tr, method = "cart", m = 5)
tr <- complete(tempData, 2)
sapply(tr, function(x) sum(is.na(x)))
densityplot(tempData)
stripplot(tempData, pch = 20, cex = 1.2)

#Feature Engineering
tr$LoanAmount <- log(tr$LoanAmount)
tr$Income <- log(tr$ApplicantIncome+tr$CoapplicantIncome)
tr <- tr[, -c(7,8)]
tr$Loan_ID <- NULL

####################Visualization###########################

#Histogram
hist(tr$LoanAmount)
hist(tr$ApplicantIncome)
hist(tr$Income)

#Scatter Plots
plot(tr$ApplicantIncome)
plot(tr$LoanAmount)
plot(tr$CoapplicantIncome)

#Distribution Graph
ggplot(tr, aes(x = LoanAmount)) +
  geom_density(aes(fill = Education, alpha = 0.7))

#Distribution on categorical variable
ggplot(tr, aes(x = Property_Area)) +
  geom_bar(width=0.5, color="black", aes(fill = Loan_Status)) +
  xlab("Gender") +
  ylab("Frequency") +
  ggtitle("Loan Status by Gender")

#Missing Values graph
library(VIM)
mice_plot <- aggr(tr, numbers = T, sortVars = T,
                    labels=names(tr), cex.axis=.7,
                    gap = 2, ylab=c("Missing data","Pattern"))


########Splitting data into training and testing############

index <- createDataPartition(tr$Loan_Status, p = 0.7, list = F)
trainSet <- tr[index,]
testSet <- tr[-index,]


###############Algorithms############################

#Logistic Regression

fit <- glm(formula = Loan_Status ~ .,
           family = binomial,
           data = trainSet)

train_pred <- predict(fit, newdata = trainSet[, -10], type = "response")
train_pred <- ifelse(train_pred > 0.5, 'Y', 'N')
cm <- table(trainSet[, 10], train_pred)

test_pred <- predict(fit, newdata = testSet[, -10], type = "response")
test_pred <- ifelse(test_pred > 0.5, 'Y', 'N')
cm <- table(testSet[, 10], test_pred)

#Kernel SVM

library(e1071)
fit <- svm(Loan_Status ~ .,
           data = trainSet,
           kernel = "radial")

train_pred <- predict(fit, newdata = trainSet[-10], type = "C-classification")
cm <- table(trainSet[, 10], train_pred)

test_pred <- predict(fit, newdata = testSet[-10], type = "C-classification")
cm <- table(testSet[, 10], test_pred)

#Decision Tree

library(rpart)
fit <- rpart(Loan_Status ~ ., data = trainSet)

train_pred <- predict(fit, newdata = trainSet[-10], type = "class")
cm <- table(trainSet[, 10], train_pred)

test_pred <- predict(fit, newdata = testSet[-10], type = "class")
cm <- table(testSet[, 10], test_pred)

#Random Forest

library(randomForest)
fit <- randomForest(x = trainSet[-10], y = trainSet$Loan_Status, ntree = 5)

train_pred <- predict(fit, newdata = trainSet[-10])
cm <- table(trainSet[, 10], train_pred)

test_pred <- predict(fit, newdata = testSet[-10])
cm <- table(testSet[, 10], test_pred)
