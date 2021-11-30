##############################################
# This file loads Class 8 net orders 1996-2021
# per month
# I Will use data to forecast the next year 2022
# net orders
#
# Created by: Amirreza Goharjo  11/18/2021
##############################################


# Clear out variable that might have stuck in memory 
rm(list=ls())

# Load the package needed for forecasting
# ggplot package is part of this library 
# which i need to do my forecast 

library(fpp2)

# Load the data

data <- read.csv(file.choose())
# Group by Data to combine US and Canada
data <- aggregate(data["Units"], by=data["Date"], sum)

# Sort based on date
data <- data[order(as.Date(data$Date, format = "%m/%d/%Y")),]

# Declare it as TS data
Y <- ts(data[,1],start=c(1996,1),frequency =12)

data
##########################################
# Preliminary Analysis
##########################################
# time plot 
autoplot(Y) + 
  ggtitle ("Time Plot : Class 8 Net Units")

# data has a trend upward 
# Take the first difference of the data to remove the trend

DY <- diff(Y)


# time plot 
autoplot(DY) + 
  ggtitle ("Time Plot : Class 8 Net Units")

# Check for seasonality 

ggseasonplot(DY) + ggtitle("Seasonal Plot : Change in mountly units")
  
# Another Seasonal plot to investigate furthe, the subseries plot

ggsubseriesplot(DY)


########################################
# Summary of what I have Done
##############################
#Use a benchmark method to forecast
#############################

fit <- naive(DY)                  #SD = 7533.2057
print(summary(fit))
checkresiduals(fit)



fit_ets <- ets(Y)              #SD = 4879.148
print(summary(fit_ets))
checkresiduals(fit_ets)

###ARIMA


fit_arima <- auto.arima(Y,d=1,D=1,stepwise = FALSE, approximation = FALSE, trace=TRUE)  #4878.067
print(summary(fit_arima))
checkresiduals(fit_arima)


###########################
#forecast
##########################

fcst <- forecast(fit_arima,h=12)
autoplot(fcst)
autoplot(fcst,include = 180)
