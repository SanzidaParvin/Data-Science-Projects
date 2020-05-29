# Name: Sanzida Parvin

#install.packages("openxlsx")
library(openxlsx)

# Read the excel sheet of clean data
abbeville_data = read.xlsx("C:/Users/...../Clean_Dataset.xlsx", sheet = 2, startRow = 1, colNames = TRUE)
abbeville_data

# time series plot of the exam request
plot.ts(abbeville_data$Request, main = "Pattern of Raw Data", ylab = "Number of Exams", xlab = "Months")

# install MICE package for imputing the missing values
#install.packages("mice")
#ibrary(mice)

# see the pattern of the data, how many are missing
md.pattern(abbeville_data)

# Imputing the values using mice package
imputed_data = complete(mice(abbeville_data))
imputed_data

# read first few rows of the imputed data
head(imputed_data)
par(mfrow = c(2,1))

# time series plot of the exam request
plot.ts(abbeville_data$Request, main = "Pattern of Data Before Imputation", ylab = "Number of Exams", xlab = "Months", col = "blue")
# time series plot of the imputed data
plot.ts(imputed_data$Request, main = "Pattern of Data After Imputation", ylab = "Number of Exams", xlab = "Months", col = "red")
par(mfrow = c(1,1))

# install Forecast package to forcast the value
install.packages("forecast")
require(forecast)

# forecast using Simple Exponential Smoothing
fit_data <- ets(imputed_data$Request, model = "ANN")
fit_data
accuracy(fit_data)
forecast_value <- forecast(fit_data, 12)
forecast_value
plot(forecast_value, main = "Simple Forecast for the Next 12 Months", ylab = "Number of Exams", xlab = "Months")
accuracy(forecast_value)
summary(forecast_value)

# forecast using Double Exponential Smoothing
fit_double <- ets(imputed_data$Request, model = "AAN")
fit_double
forecast_double <- forecast(fit_double, 12)
forecast_double
plot(forecast_double, main = "Double Exponential Smoothing Forecast for the Next 12 Months", ylab = "Number of Exams", xlab = "Months")
accuracy(forecast_double)
summary(forecast_double)

# ARIMA forecast of the full data
# built a time series of the request from full data set
myTS <- ts(imputed_data$Request)
myTS
plot(myTS, ylab = "Number of requests", xlab = "Months")

# Assess the time series using ACF and PACF
acf(myTS)
pacf(myTS)

# use diffing for data transformation. R can find optimal diffing
ndiffs(x = myTS)
# plot to see the effect of diffing
plot(diff(myTS, 1))

# fit the ARIMA model
fit_arima <- auto.arima(x = myTS)
# review the ARIMA model
fit_arima
accuracy(fit_arima) # check for MAPE, MAD, and MSE

# check the ACF and PACF of the ARIMA model residuals
acf(fit_arima$residuals)
pacf(fit_arima$residuals)

# check the coefficients
coef(fit_arima)
forecast_arima <- forecast(fit_arima, h=12)
forecast_arima
plot(forecast_arima, main = "ARIMA Forecast for the Next 12 Months", ylab = "Number of Exams", xlab = "Months")
accuracy(forecast_arima)
summary(forecast_arima)
