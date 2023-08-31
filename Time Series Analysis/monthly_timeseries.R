
# Importing the libraries

library(ggplot2)
library(ggseas)
library(dplyr)
library(forecast)
library(tseries)
library(fpp2)

# getting the current directory
getwd()
# set the working directory to the folder containing the CSV file
setwd("/Users/debmalyadeb/Documents/NCI_Learning/Statistics/TABA")

# reading csv file

data_monthly <- read.csv("nitm18442004.csv")
data_monthly
# shows the class of the raw data
class(data_monthly)

# Converting the above data class to a time series class object

timeseries <- ts(data_monthly$x, start = c(1844, 1), frequency = 12)
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
plot(timeseries, main = "Monthly time series of average temperatures in Armagh")
#########

# Stationary check

#########
# The trend of the time series is stationary
# To prove that fact do the ADF test that the data is actually stationary or not
adf.test(timeseries, alternative = "stationary")

# Now decompose the graph and check

decomp <- stl(timeseries, s.window = "periodic")

autoplot(decomp) +
  labs(title = "Seasonal decomposition of monthly temperatures in Armagh") +
  theme_bw()

# Now visually also it looks good

#############

#Seasonality check

###########

# Check seasonality using ggseasonplot
ggseasonplot(timeseries) +
  ggtitle("Monthly basis Seasonal Plot") +
  ylab("Frequency")

# Checking seaasonality in another subseries plot 
ggsubseriesplot(timeseries)+ggtitle("Monthly basis Seasonal Plot")+ylab("Frquency")



##########

#Visualization

###############

# Create a boxplot of the time series
boxplot(timeseries, main = "Boxplot of monthly temperatures in Armagh")

# Identify and plot outliers using the "identify" function
#outliers <- identify(boxplot(timeseries_df, plot = FALSE)$out)
#points(outliers, timeseries_df[outliers], col = "red", pch = 19)


# Decompose the time series using STL
decomp <- stl(timeseries, s.window = "periodic")

# Plot the decomposed components
autoplot(decomp) +
  labs(title = "Seasonal decomposition of monthly temperatures in Armagh") +
  theme_bw()

# Create ACF and PACF plots
ggtsdisplay(timeseries, main = "ACF and PACF of monthly temperatures in Armagh")


#Plot a density plot of the time series
ggplot() +
  geom_density(aes(x = timeseries), fill = "steelblue", alpha = 0.7) +
  labs(title = "Density plot of monthly temperatures in Armagh") +
  theme_bw()

#Plot a histogram of the time series
ggplot() +
  geom_histogram(aes(x = timeseries), fill = "steelblue", alpha = 0.7, bins = 15) +
  labs(title = "Histogram of monthly temperatures in Armagh") +
  theme_bw()

#Plot a time series with a 12-month moving average
moving_avg <- ma(timeseries, order = 12)
ggplot() +
  geom_line(aes(x = time(timeseries), y = timeseries), color = "steelblue") +
  geom_line(aes(x = time(moving_avg), y = moving_avg), color = "red") +
  labs(title = "Monthly temperatures in Armagh with 12-month moving average") +
  theme_bw()

#Plot a time series with a seasonal plot
ggseasonplot(timeseries, year.labels = TRUE, year.labels.left = TRUE, year.labels.right = FALSE) +
  labs(title = "Seasonal plot of monthly temperatures in Armagh") +
  theme_bw()


# Split the data into training and test sets based on dates
train <- window(timeseries, end = c(2003, 12))
test <- window(timeseries, start = c(2004, 1))

# Define the window size
window_size <- 12

# Calculate the moving average of the training data
moving_avg <- ma(train, order = window_size)
print(summary(moving_avg))
checkresiduals(moving_avg)

# Use the moving average to make forecasts for the test data
forecast <- rep(moving_avg[length(moving_avg)], length(test))
print(summary(forecasts))
round(accuracy(forecasts, test),3)

# Calculate the forecast accuracy using MAE
mae <- mean(abs(forecast - test))

# Calculate the forecast accuracy using RMSE
rmse <- sqrt(mean((forecast - test)^2))

# Print the forecast accuracy measures
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")

# Plot the original time series and the forecasts
plot(timeseries, type = "l", main = "Monthly time series of average temperatures in Armagh")
lines(test, col = "blue")
lines(forecast, col = "red")

#######
# Exponential Smoothing Model 
######

# Apply Holt's method with additive seasonality to the training data
model <- HoltWinters(train, seasonal = "additive")
print(summary(model))
checkresiduals(model)

# Use the model to make forecasts for the test data
forecast <- predict(model, n.ahead = length(test))
print(summary(forecast))
checkresiduals(forecast)
round(accuracy(forecast, test),3)

# Calculate the forecast accuracy using MAE
mae <- mean(abs(forecast - test))

# Calculate the forecast accuracy using RMSE
rmse <- sqrt(mean((forecast - test)^2))

# Print the forecast accuracy measures
cat("MAE:", mae, "\n")
cat("RMSE:", rmse, "\n")

# Plot the original time series and the forecasts
plot(timeseries, type = "l", main = "Monthly time series of average temperatures in Armagh")
lines(test, col = "blue")
lines(forecast$pred, col = "red")

# Automated Exponential Smoothing Model 

# Fit the Exponential Smoothing model on the training data
fit <- ets(train, model = "AAN", damped = TRUE)
# Print the model summary
print(summary(fit))
# Check the residuals
checkresiduals(fit)

# Print the model summary
summary(fit)

# Make forecasts for the next 12 months
fc <- forecast(fit, h = 12)
# Print the model summary
print(summary(fc))
# Check the residuals
checkresiduals(fc)
round(accuracy(fc, test), 3)


# Plot the forecasts along with the actual values
autoplot(fc) +
  autolayer(test, series = "Actual") +
  labs(title = "Exponential Smoothing forecast of monthly temperatures in Armagh") +
  ylab("Temperature") +
  theme_bw()

#####

# Auto-Arima-Model

######

# Fit a SARIMA model to the time series data
fit <- auto.arima(train, seasonal = TRUE, stepwise = FALSE,
                  approximation = FALSE, allowdrift = TRUE)

# Print the model summary
summary(fit)

checkresiduals(fit)

# Make forecasts for the test set using the fitted model
fc <- forecast(fit, h = length(test))
# Print the model summary
summary(fc)

# Plot the forecasted values along with the actual values
autoplot(fc) +
  autolayer(test, series = "Test data") +
  xlab("Year") +
  ylab("Monthly temperature") +
  ggtitle("SARIMA forecast of monthly temperatures in Armagh")










