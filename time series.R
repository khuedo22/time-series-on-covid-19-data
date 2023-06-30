install.packages("tidyverse") 
install.packages("forecast")
install.packages("xts")
install.packages("tsbox")


library(tidyverse)
library(tseries)
library(forecast)
library(xts)
library(tsbox)




#load data 
data<- read_csv("states.timeseries.csv")


#subset data to only include washington state
washington <- data %>% select(state, date, actuals.newCases) %>% filter(state == "WA") %>% filter(actuals.newCases != "NA")

#transform into ts object
washington$date <- as.Date(washington$date)
washington_xts <- xts(washington$actuals.newCases, washington$date)
washington_xts_subset <- window(washington_xts, start="2021-1-01", end="2021-12-31")
washington_ts <- ts(washington_xts_subset, frequency=7)


#train/test split
washington_training <- subset(washington_ts, end = length(washington_ts)-16)
washington_testing <- subset(washington_ts, start=length(washington_ts)-15)


#plotting training data
plot(washington_training, xlab="Date", ylab="New Cases", main="New Covid-19 Cases in Washington")


#decomposing data into components

washington_ts_decompose <- decompose(washington_training)
washington_ts_decompose_plot <- plot(washington_ts_decompose)
plot(washington_ts_decompose)


# building SARIMA model

ndiffs(washington_training)
washington_diff <- diff(washington_training)
plot(washington_diff, ylab="Differenced Cases", main="Difference of 1")
adf.test(washington_diff)
ggtsdisplay(washington_diff)


fit1 <- Arima(washington_training, order=c(2, 1, 3), seasonal=c(3, 0, 0)) 
fit1 %>% checkresiduals()
fit2 <- Arima(washington_training, order=c(5, 1, 3), seasonal=c(3, 0, 0)) 
fit2 %>% checkresiduals()
fit3 <- Arima(washington_training, order=c(5, 1, 5), seasonal=c(3, 0, 0)) 
fit3 %>% checkresiduals()

#comparing model with testing data
fit7 %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)
fit6 %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)

autoplot(washington_training, series="training") + autolayer(fitted(fit6, h=15), series="model and predictions")
test <- Arima(washington_testing, model=fit6)
accuracy(test)



#extra
auto_fit <- auto.arima(washington_diff)
residuals <- checkresiduals(auto_fit)
auto_fit %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)

