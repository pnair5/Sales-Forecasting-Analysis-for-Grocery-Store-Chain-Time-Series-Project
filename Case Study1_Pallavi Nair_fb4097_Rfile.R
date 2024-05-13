install.packages("zoo")  # Install the zoo package if you haven't already
library(zoo)             # Load the zoo package to use the rollmean() function
install.packages("forecast")
install.packages("lmtest")
library(forecast)
library(lmtest)

data <- read.csv("/Users/pallavinair/Desktop/CSUEB BA/BAN 673/673_case1.csv", stringsAsFactors = FALSE)
##1 (a)
data$Month <- as.Date(data$Month, format="%m/%d/%Y")
sales.ts <- ts(data$Sales, start=c(2015, 1), end=c(2021, 12), frequency=12)
##1 (b) Plot the time series
plot(sales.ts, xlab="Time", ylab="Sales (in millions)", main="Historical Monthly Sales", type="o", col="blue")
##1 (c)Plot the ACF
acf(sales.ts, main="Autocorrelation Function (ACF) for Monthly Sales")
# Compute and plot autocorrelation using ggplot
acf <- Acf(sales.ts, plot=FALSE)
acf.df <- data.frame(Lag=as.numeric(acf$lag), ACF=as.numeric(acf$acf))
acf.df

##2 (a)# Partitioning the data
# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
training  <- window(sales.ts, start=c(2015,1), end=c(2019,12))
validation <- window(sales.ts, start=c(2020,1))
nValid <- length(validation) # Number of periods to forecast
nTrain <- length(sales.ts) - nValid 

# 2 (b) Calculate moving averages
ma2 <- rollmean(training, 2, align='right')
ma6 <- rollmean(training, 6, align='right')
ma12 <- rollmean(training, 12, align='right')
ma2
ma6
ma12

# 2 (c) forecast() function to create a trailing MA forecast 
forecast_ma2 <- forecast(ma2, h=24) # for window width of 2
forecast_ma6 <- forecast(ma6, h=24) # for window width of 6
forecast_ma12 <- forecast(ma12, h=24) # for window width of 12
forecast_ma2
forecast_ma6
forecast_ma12

# 2 (d) accuracy() function to compare accuracy of the 3 trailing MA forecasts  
acc_ma2<-accuracy(forecast_ma2, validation)
acc_ma6<-accuracy(forecast_ma6, validation)
acc_ma12<-accuracy(forecast_ma12, validation)
#Print
acc_ma2
acc_ma6
acc_ma12

#3 (a) Model Development:
# Assuming 'training' is your training time series
training_model_tslm <- tslm(training ~ trend + season)
summary(training_model_tslm)
# Forecast using this model
training_forecast_tslm <- forecast(training_model_tslm, h = nValid, level = c(80, 95))
training_forecast_tslm
# Plot or extract the forecast values
plot(training_forecast_tslm)
## IDENTIFY FORECAST ACCURACY
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
# Use the accuracy function correctly with the forecast object and validation data
accuracy_measures <- round(accuracy(training_forecast_tslm, validation), 3)
accuracy_measures
#3 (b) Identify Residuals:
sales_train_res <- training_model_tslm$residuals
sales_train_res
# Trailing MA on residuals
training_ma_res <- rollmean(sales_train_res, 2, align='right')
training_ma_res
# Regression residuals in validation period.
sales_valid_res <- validation - training_forecast_tslm$mean
sales_valid_res
# Create residuals forecast for validation period.
sales_valid_res_forecast <- forecast(training_ma_res, h = nValid, level = c(80, 95))
# Present the forecast of residuals 
sales_valid_res_forecast
#3 (c) Two-Level Forecast:Combine Forecasts:
# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst_2level <- training_forecast_tslm$mean + sales_valid_res_forecast$mean
fst_2level
# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
validation_df <- data.frame(
  Actual_Sales = round(as.numeric(validation), 3),
  Regression_Forecast = round(training_forecast_tslm$mean, 3),
  Trailing_MA_Residuals = round(training_ma_res[1:24], 3),  # Ensure this is correctly limited to 24 months
  Combined_Forecast = round(fst_2level[1:24], 3)  # Adjust if necessary to ensure length is 24
)
validation_df
round(accuracy(training_forecast_tslm$mean, validation), 3)
round(accuracy(fst_2level, validation), 3)

#3 (d & e) USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET. 
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.
# Fit a regression model with linear trend and seasonality for
# entire data set.
tot_model_tslm <- tslm(sales.ts ~ trend  + season)
summary(tot_model_tslm)
# Create regression forecast for future 12 periods.
tot_forecast_tslm <- forecast(tot_model_tslm, h = 12, level = c(80, 95))
tot_forecast_tslm
# Identify and display regression residuals for entire data set.
tot_sales_res <- tot_model_tslm$residuals
tot_sales_res
# Use trailing MA to forecast residuals for entire data set.
tot_ma_res <- rollmean(tot_sales_res, k = 2, align = "right")
tot_ma_res
# Create forecast for trailing MA residuals for future 12 periods.
tot_ma_res_forcast <- forecast(tot_ma_res, h = 12, level = c(80, 95))
tot_ma_res_forcast
# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot_fst_2level <- tot_forecast_tslm$mean + tot_ma_res_forcast$mean
tot_fst_2level
# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12_df <- round(data.frame(
  Regression_Forecast = tot_forecast_tslm$mean,
  Trailing_MA_Residuals_Forecast = tot_ma_res_forcast$mean,
  Combined_Forecast = tot_fst_2level
), 3)
names(future12_df) <- c("Regression_Fst", "MA_Residuals_Fst", "Combined_Fst")
future12_df
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(tot_forecast_tslm$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
round(accuracy(fst_2level, sales.ts), 3)

#4 (a) # Develop HW model with automated selection for the training data
hw_model_ZZZ <- ets(training, model="ZZZ")  # "ZZZ" allows automatic model selection
hw_model_ZZZ 
# Summarize the model
summary(hw_model_ZZZ)
# Forecast for the validation period
hw_forecast_validation <- forecast(hw_model_ZZZ, h=nValid)
hw_forecast_validation 
round(accuracy(hw_forecast_validation$mean, validation), 3)
#4 (b)Forecast for the Next 12 Months
# Develop HW model with automated selection for the entire dataset
tot_hw_model_ZZZ <- ets(sales.ts, model="ZZZ")
# Summarize the model
summary(tot_hw_model_ZZZ)
# Forecast for the next 12 months
hw_forecast_future <- forecast(tot_hw_model_ZZZ, h=12)
#4 c # Identify performance measures for HW forecast.
round(accuracy(hw_forecast_future$fitted, sales.ts), 3)
round(accuracy((naive(sales.ts))$fitted, sales.ts), 3)
round(accuracy((snaive(sales.ts))$fitted, sales.ts), 3)
