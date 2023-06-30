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
washington$date <- as.Date(washington$date)
plot(washington$date, washington$actuals.newCases,type="l")
washington_xts <- xts(washington$actuals.newCases, washington$date)
washington_xts_subset <- window(washington_xts, start="2021-1-01", end="2021-12-31")
plot(washington_xts_subset)
washington_ts <- ts(washington_xts_subset, frequency=7)
washington_training <- subset(washington_ts, end = length(washington_ts)-16)
washington_testing <- subset(washington_ts, start=length(washington_ts)-15)
#convert actuals.cases to time series



washington_plot <- plot(washington_ts, ylab= "New Cases", xlab="Time", main = "New COVID-19 cases for Washington State")

washington_ts_sma <- SMA(washington_ts, n=25)
washington_ts_sma_plot <- plot.ts(washington_ts_sma)

washington_ts_decompose <- decompose(washington_ts)
washington_ts_decompose_plot <- plot(washington_ts_decompose)
plot(washington_ts_decompose)




# using sarima

ndiffs(washington_training)
washington_diff <- diff(washington_training)
plot(washington_diff)
adf.test(washington_diff)
ggtsdisplay(washington_diff)


fit6 <- Arima(washington_training, order=c(2, 1, 3), seasonal=c(3, 0, 0)) 
fit6 %>% checkresiduals()
fit7 <- Arima(washington_training, order=c(5, 1, 3), seasonal=c(3, 0, 0)) 
fit7 %>% checkresiduals()
fit8 <- Arima(washington_training, order=c(5, 1, 5), seasonal=c(3, 0, 0)) 
fit8 %>% checkresiduals()

fit7 %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)
fit6 %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)

autoplot(washington_training, series="training") + autolayer(fitted(fit6, h=15), series="model and predictions")
test <- Arima(washington_testing, model=fit6)
accuracy(test)

auto_fit <- auto.arima(washington_diff)
residuals <- checkresiduals(auto_fit)
auto_fit %>% forecast(h=15) %>% autoplot() + autolayer(washington_testing)

