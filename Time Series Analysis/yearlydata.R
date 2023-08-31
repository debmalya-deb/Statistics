
# Importing the libraries

library(ggplot2)
library(ggseas)
library(dplyr)
library(forecast)
library(tseries)
library(fpp2)
install.packages("zoo")
library(zoo)

# getting the current directory
getwd()
# set the working directory to the folder containing the CSV file
setwd("/Users/debmalyadeb/Documents/NCI_Learning/Statistics/TABA")

# reading csv file
data_monthly <- read.csv("nity18442004.csv")
data_monthly
# shows the class of the raw data
class(data_monthly)

# Converting the above data class to a time series class object
timeseries <- ts(data_monthly$x, start = c(1844, 1), frequency = 1)
head(timeseries)
class(timeseries)

#checks from where the timeseries starts 
start(timeseries)
#checks at where the timeseries ends 
end(timeseries)
#checks the frequency of the timeseries 
frequency(timeseries)

#Statistical Analysis of the time series 
# Create a time plot of the data
par(mfrow =c(1,1))
plot(timeseries, main = "Yearly time series of average temperatures in Armagh")

#########

# Stationary check

#########
# The trend of the time series is stationary
# To prove that fact do the ADF test that the data is actually stationary or not
adf.test(timeseries, alternative = "stationary")

# Now decompose the graph and check

ndiffs(timeseries)

# Differencing the data to make it stationary
timeseries_df <- diff(timeseries)
timeseries_df
plot(timeseries_df, main = "Time plot of differenced yearly average temperature in Armagh")

# To prove that fact do the ADF test that the data is actually stationary or not
adf.test(timeseries_df, alternative = "stationary")


# Now visually also it looks good

#############

#Seasonality check

###########

# Check seasonality using ggseasonplot
ggseasonplot(timeseries_df) +
  ggtitle("Monthly basis Seasonal Plot") +
  ylab("Frequency")

# Checking seaasonality in another subseries plot 
ggsubseriesplot(timeseries_df)+ggtitle("Monthly basis Seasonal Plot")+ylab("Frquency")

##########

#Visualization

###############

# Create a boxplot of the time series
boxplot(timeseries_df, main = "Boxplot of yearly temperatures in Armagh")

# Create ACF and PACF plots
ggtsdisplay(timeseries_df, main = "ACF and PACF of yearly temperatures in Armagh")


#Plot a density plot of the time series
ggplot() +
  geom_density(aes(x = timeseries_df), fill = "steelblue", alpha = 0.7) +
  labs(title = "Density plot of yearly temperatures in Armagh") +
  theme_bw()

#Plot a histogram of the time series
ggplot() +
  geom_histogram(aes(x = timeseries_df), fill = "steelblue", alpha = 0.7, bins = 15) +
  labs(title = "Histogram of yearly temperatures in Armagh") +
  theme_bw()

#Plot a time series with a 12-month moving average
moving_avg <- ma(timeseries_df, order = 1)
ggplot() +
  geom_line(aes(x = time(timeseries_df), y = timeseries_df), color = "steelblue") +
  geom_line(aes(x = time(moving_avg), y = moving_avg), color = "red") +
  labs(title = "Yearly temperatures in Armagh with 1 year moving average") +
  theme_bw()

# Split the data into training and test sets based on dates
train <- window(timeseries_df, end = c(2003))
test <- window(timeseries_df, start = c(2004))

# Apply the moving average method to the training data
model <- rollmean(train, k=12, align="right")
print(summary(model))

# Use the model to make forecasts for the test data
forecast <- rep(tail(model, 1), length(test))
print(summary(forecast))
accuracy(forecast, test)

# Plot the forecasts along with the actual values
par(mfrow =c(1,1))
plot(timeseries_df, type="l", main="Moving Average forecast of yearly temperatures in Armagh")
lines(train, col="black")
lines(index(test), forecast, col="red")
points(test, col="green", pch=20)
legend("topleft", legend=c("Actual", "Training", "Forecast"), col=c("green", "black", "red"), lty=1, cex=0.8)


# Apply Holt's method with additive seasonality to the training data
#model <- HoltWinters(train, seasonal = "additive")
#print(summary(model))
#checkresiduals(model)

# Use the model to make forecasts for the test data
#forecast <- forecast(model, h = length(test))
#print(summary(forecast))
#checkresiduals(forecast)
#round(accuracy(forecast, test),3)

# Plot the forecasts along with the actual values
#autoplot(forecast) +
#  autolayer(test, series = "Actual") +
#  labs(title = "Exponential Smoothing forecast of monthly temperatures in Armagh") +
#  ylab("Temperature") +
#  theme_bw()

# Exponential Smoothing Model 

# Fit the Exponential Smoothing model on the training data
fit <- ets(train, model = "ZZZ", damped = TRUE)
# Print the model summary
print(summary(fit))
# Check the residuals
checkresiduals(fit)

# Make forecasts
fc <- forecast(fit, h = length(test))
# Print the model summary
print(summary(fc))
# Check the residuals
checkresiduals(fc)
round(accuracy(fc, test), 3)


# Plot the forecasts along with the actual values
autoplot(fc) +
  autolayer(test, series = "Actual") +
  labs(title = "Exponential Smoothing forecast of yearly temperatures in Armagh") +
  ylab("Temperature") +
  theme_bw()

#####

# Auto-Arima-Model

######

# Fit a ARIMA model to the time series data
fit <- auto.arima(train, seasonal = FALSE, stepwise = TRUE,
                  approximation = FALSE, allowdrift = TRUE)

# Print the model summary
print(summary(fit))

# AIC 210.93

# Check the residuals
checkresiduals(fit)

#p value 0.3546

# ARIMA(0,0,1) with zero mean 

# Normal probability plot of the residuals.
par(mfrow =c(1,2))
qqnorm(fit$residuals)
qqline(fit$residuals)

#fit data with the test data

forecasts_model <- forecast(fit, h=length(test))
#forecasts
print(forecasts_model)
print(summary(forecasts_model))

# Plot the forecasts
par(mfrow =c(1,1))
plot(forecasts_model, xlab="Year", ylab="", main="ARIMA Forecast")
# Add the actual test data to the plot
lines(test, col="red")
# Add a legend
legend("topleft", legend = c("Forecasts", "Actual", "Test"), col = c("black", "blue", "red"), lty = 1)

# Evaluate the forecasts against the actual data for 2004
accuracy <- round(accuracy(forecasts_model, test), 3)
print(accuracy)

#Try test check arima manually

fit_arima1 <- arima(train, order = c(0,0,3))

# Print the model summaries
print(summary(fit_arima1))

# Check the residuals
checkresiduals(fit_arima1) #p-value = 0.09453(Best Model)

#Normal probability plot of the residuals.
par(mfrow =c(1,2))
qqnorm(fit_arima1$residuals)
qqline(fit_arima1$residuals)



