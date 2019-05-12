#install.packages('tseries', dependencies=TRUE, repos='http://cran.rstudio.com/')

df <- read.csv('Traffic Prediction.csv', stringsAsFactors = F)
df$ID <- NULL

#########################EDA#######################################

df$Datetime <- as.POSIXct(df$Datetime, format = "%d/%m/%Y %H:%M", tz = Sys.timezone())
plot(df)

# Converting Date Time to hours, days, week, month, year
library(plyr)
library(lubridate)

df <- mutate(df, time = ymd_hms(Datetime), 
             day = day(time), month = month.abb[month(time)], year = year(time), hour = hour(time))
df$weekDay <- weekdays(as.Date(df$Datetime, format = "%d/%m/%Y %H:%M", tz = Sys.timezone()))
df$time <- NULL
df$weekend <- ifelse(df$weekDay == "Saturday" | df$weekDay == "Sunday", 1, 0)
df$weekDay <- NULL

######################VISUALIZATION#####################################

library(caret)

# traffic will increase as the years pass by
ggplot(df, aes(x = year, weights = Count)) +
  geom_histogram(binwidth = 0.2, aes(fill = Count)) + 
  xlab("Year") +
  ylab("Passengers") +
  ggtitle("Yearwise Comparison")

# increase in traffic from May to October
ggplot(df, aes(x = month, weights = Count)) +
  geom_histogram(binwidth = 0.4, aes(fill = Count)) + 
  xlab("Month") +
  ylab("Passengers") +
  ggtitle("Monthwise Comparison")

# daywise traffic
ggplot(df, aes(x = day, weights = Count)) +
  geom_histogram(binwidth = 0.4, aes(fill = Count)) + 
  xlab("Day") +
  ylab("Passengers") +
  ggtitle("Daywise Comparison")

# traffic will be more during peak hours
ggplot(df, aes(x = hour, weights = Count)) +
  geom_histogram(binwidth = 0.4, aes(fill = Count)) + 
  xlab("Hour") +
  ylab("Passengers") +
  ggtitle("Hourwise Comparison")

# traffic will be more on weekdays
ggplot(df, aes(x = weekend, weights = Count)) +
  geom_histogram(binwidth = 0.2, aes(fill = Count)) + 
  xlab("WeekEnd") +
  ylab("Passengers") +
  ggtitle("Weekend Comparison")

# traffic year-month wise
ggplot(df, aes(x = paste0(df$month, " ", df$year), y = Count)) + 
  stat_summary(fun.y = sum, geom = "point", colour = "red", size = 1)

#########################TIME SERIES############################

# Making time series analysis daywise
train_set <- df
train_set$Datetime <- gsub(" .*", "", train_set$Datetime)
train_set <- aggregate(Count ~ Datetime, train_set, sum)
train_set$Datetime <- as.Date(train_set$Datetime, format = "%d/%m/%Y")
train_set <- train_set[order(train_set$Datetime),]
train_set$Datetime <- NULL

# Creating time series object
library(forecast)
library(tseries)

df_ts <- ts(train_set, start = c(2012, 238), frequency = 365)
df_ts
start(df_ts)
end(df_ts)
summary(df_ts)

plot(df_ts)
boxplot(df_ts~cycle(df_ts))

# draws mean value at different intervals of time
abline(reg=lm(df_ts~time(df_ts))) 
plot(aggregate(df_ts,FUN=mean))

# Making data stationary

# Variance and Covariance equal
plot(log(df_ts))

# Mean, Variance and Covariance equal. Data becomes stationary
plot(diff(log(df_ts)))

# Decomposing data to trend, seasonality and Irregularity
ddata <- decompose(df_ts, "multiplicative")
plot(ddata)

# Dickey Fuller test to check the stationarity
adf.test(diff(log(df_ts)), alternative = "stationary", k = 0)

# Calculate Moving Average (q)
acf(diff(log(df_ts)))

# Calculate Auto Regression (p)
pacf(diff(log(df_ts)))

# use D = 1 for forced seasonality
fit <- auto.arima(df_ts, D = 1)
fit <- arima(log(df_ts), c(3, 1, 3), seasonal = list(order = c(0, 1, 0), period = 365))
fit

train_pred <- forecast(fit, h = 90)
plot(train_pred)

prediction <- rbind(train_set, data.frame(Count = 2.71^as.numeric(train_pred$mean)))
test_ts <- ts(prediction, start = c(2012, 238), frequency = 365)
plot(test_ts)
