#setwd("C:/Users/bailo/OneDrive/Documents/DataScienceCA4")

res_price_indx <- read.csv("Ireland_Residential_Property_Price_Index.csv")

# convert the data for national all residential properties to time series
ts_natl_res_price_indx <- ts(data=res_price_indx$National_all_residential_properties, 
                             frequency = 12, start=c(2013,1), end=c(2019,12)) 
ts_natl_res_price_indx
class(ts_natl_res_price_indx)

# plot the time series
plot(ts_natl_res_price_indx)
start(ts_natl_res_price_indx)
end(ts_natl_res_price_indx)
frequency(ts_natl_res_price_indx)

# CHECK FOR MISSING VALUES - View the records with NA
na_records <- ts_natl_res_price_indx[!complete.cases(ts_natl_res_price_indx)]
sum(na_records)

# First steps of time series 
# ma() allows us to smooth the time series 

library(forecast)
opar <- par(no.readonly = TRUE)
par(mfrow=c(2,2))

ylim <- c(min(ts_natl_res_price_indx), max(ts_natl_res_price_indx))
plot(ts_natl_res_price_indx, main = "Raw time series")
plot(ma(ts_natl_res_price_indx, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(ts_natl_res_price_indx, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(ts_natl_res_price_indx, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

#==========================
# MULTIPLICATIVE OR ADDITIVE MODEL

# examining whether is is additive or multiplicative 
plot(ts_natl_res_price_indx, xlab = "Date", 
     ylab = "Price Index",
     main = "Irish Residential property price Index")

# add a straight line to show linear linearity 
abline(reg = lm(ts_natl_res_price_indx~time(ts_natl_res_price_indx)))

# examine trend in the data - shows the trend that year on year it is increasing
plot(aggregate(ts_natl_res_price_indx, FUN = mean))


# seasonality
boxplot(ts_natl_res_price_indx ~ cycle(ts_natl_res_price_indx), 
        xlab = "Date", 
        ylab = "Price index",
        main = "National Residential Property Price Index (Ireland) - 2013 to 2019 ")

# Multiplicative or additive model?
seasonal_decomposition <- stl(ts_natl_res_price_indx, s.window = "period")
plot(seasonal_decomposition)


# Test the seasonality of the time series

# remove the seasonality - uniform peaks now
log_ts_natl_res_price_indx <- log(ts_natl_res_price_indx)
plot(log_ts_natl_res_price_indx)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))
plot(ts_natl_res_price_indx)
plot(log_ts_natl_res_price_indx)
par((opar))


adf.test(log_ts_natl_res_price_indx, alternative = "stationary")

acf(log_ts_natl_res_price_indx)
ndiffs(log_ts_natl_res_price_indx) # shift 1 session to the left
diff_ts_natl_res_price_indx <- diff(log_ts_natl_res_price_indx, lag =4)

# check to see if data needs further differencing
ndiffs(diff_ts_natl_res_price_indx) # now its 0
# prove by running the acf
acf(diff_ts_natl_res_price_indx)




#====================================================================================================

# STATIONARITY
# Test if a time series is stationary
library(tseries)
suggested_k <- trunc((length(ts_natl_res_price_indx)-1)^(1/3))
suggested_k

# Method 1: Test stationarity of the time series (ADF)

# p-value < 0.05 indicates the TS is stationary
# In this eample, price index data is not stationary

adf.test(ts_natl_res_price_indx, alternative = "stationary")
#data:  ts_natl_res_price_indx
#Dickey-Fuller = -0.13734, Lag order = 4, p-value = 0.99
#alternative hypothesis: stationary


adf.test(ts_natl_res_price_indx, alternative = "stationary", k = 12)
#data:  ts_natl_res_price_indx
#Dickey-Fuller = -2.4488, Lag order = 12, p-value = 0.3918
#alternative hypothesis: stationary
#still not stationary

# Method 2 : Test stationarity of the time series (Autocorrelation)
# method 1 is conclusive already - so no need to use method 2

library(forecast)
acf(ts_natl_res_price_indx)

pacf(ts_natl_res_price_indx)


# DIFFERENCING
library(forecast)
nsdiffs(ts_natl_res_price_indx)

log_natl_res_price_indx <- log(ts_natl_res_price_indx)

# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_natl_res_price_indx, main = "Original Natl Residential Price Index dataset")
plot(log_natl_res_price_indx, main = "Log Natl Residential Price Index dataset")
par(opar)

# to remove one unequal variance
nsdiffs(log_natl_res_price_indx)


diff_natl_res_price_indx <- diff(log_natl_res_price_indx, lag = 12, differences = 2)

# Show both side-by-side for comparison
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(ts_natl_res_price_indx, main = "Original Natl Price index dataset")
plot(diff_natl_res_price_indx, main = "Differenced Natl Price index dataset")
par(opar)

# check if nsdiffs has removed the stationarity
nsdiffs(diff_natl_res_price_indx)

# check if the data is now stationary
adf.test(diff_natl_res_price_indx, alternative = "stationary")

acf(diff_natl_res_price_indx)

pacf(diff_natl_res_price_indx)

seasonal_decomposition <- stl(ts_natl_res_price_indx, s.window="period")
plot(seasonal_decomposition)

# fit the sarima model
fit <- arima(ts_natl_res_price_indx, 
             c(1,2,1), 
             seasonal = list(order = c(1,2,1), 
                             period = 12))
fit

# auto arima model fitting
auto_arima_model <- auto.arima(ts_natl_res_price_indx)
auto_arima_model

# gettting the accuracy of both models
accuracy(fit)
accuracy(auto_arima_model)


# Evaluating the auto arima models
qqnorm(auto_arima_model$residuals, main = "Auto ARIMA Normal Q-Q Plot")
qqline(auto_arima_model$residuals)


# Evaluating the SARIMA models
qqnorm(fit$residuals, main = "SARIMA Normal Q-Q Plot")
qqline(fit$residuals)

Box.test(auto_arima_model$residuals, type = "Ljung-Box")
Box.test(fit$residuals, type = "Ljung-Box")

# Choosing training and testing data
ts_natl_res_price_indx_train <- window(x = ts_natl_res_price_indx, start=c(2013), end=c(2017, 12))
ts_natl_res_price_indx_test <- window(x = ts_natl_res_price_indx, start=c(2018))

ts_natl_res_price_indx_train
ts_natl_res_price_indx_test

# fit the training
fit <- arima(ts_natl_res_price_indx_train, 
             c(1,2,1), 
             seasonal = list(order = c(1,2,1), 
                             period = 12))
fit

auto_arima_model <- auto.arima(ts_natl_res_price_indx_train)
auto_arima_model


# forecast for the years 2018 and 2019
forecast_auto_ARIMA <- forecast(auto_arima_model, 2 * 12)
forecast_auto_ARIMA

forecast_manual_ARIMA <- forecast(fit, 2 * 12)
forecast_manual_ARIMA

# check performance on the test data

class(forecast_manual_ARIMA)

actuals_predictions <- data.frame(cbind(actuals = ts_natl_res_price_indx_test, 
                                        predicted = forecast_manual_ARIMA))
head(actuals_predictions)

auto_predictions <- data.frame(cbind(actuals = ts_natl_res_price_indx_test, 
                                        predicted = forecast_auto_ARIMA))
head(auto_predictions)


correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

correlation_auto_accuracy <- cor(auto_predictions)
correlation_auto_accuracy


# re-train the model for the entire dataset before making a forecast for another 3 years

class(forecast_manual_price_indx)

forecast_manual_price_indx <- forecast(fit, 3 * 12)
forecast_manual_price_indx


forecast_auto_price_indx <- forecast(auto_arima_model, 3 * 12)
forecast_auto_price_indx


#autoplot(forecast_manual_price_indx)
plot(forecast_manual_price_indx, xlab = "Year", ylab = "Price index", main = "3-Year Natl Residential Property Price Index Manual Forecast")


#autoplot(forecast_auto_price_indx)
plot(forecast_auto_price_indx, xlab = "Year", ylab = "Price index", main = "3-Year Natl Residential Property Price Index Auto Forecast")


