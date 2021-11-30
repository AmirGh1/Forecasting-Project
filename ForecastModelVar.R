##############################################
# This file loads Class 8 net orders 1996-2021
# per month
# I Will use data to forecast the next year 2022
# net orders using VAR method
#
# Created by: Amirreza Goharjo  11/18/2021
##############################################


#Loading  required packages for runing VAR


library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)



#Load Data Set
data <- read.csv(file.choose())
#Total Class 8 Units In US&Canada  (Target for forecast)
class8 <- ts(data$SUMClass8,start = c(1996,1),frequency = 12)
#Total Class 67 Units In US&Canada  (Target for forecast)
class67 <- ts(data$SUMClass67,start = c(1996,1),frequency = 12)
##################################################################
# Nominated Macroeconomic Drivers 
##################################################################
# New Oder Index
NewOrdersIndex <- ts(data$ISMINDEXNewOrders,start = c(1996,1),frequency = 12)
# PMI Manufacturing Index
PMI_MANU <- ts(data$ISMINDEXPMIManufacturingCompositeIndex ,start = c(1996,1),frequency = 12)
# Total industry production 
production <- ts(data$IndustrialProductionIndex ,start = c(1996,1),frequency = 12)
# Effective Federal fund Rate
Fed_Fund <- ts(data$Effective.Federal.Funds.Rate*10,start = c(1996,1),frequency = 12)

# Treasury Yield - Yield Spread
Treasury_Yield <- ts(data$Yield.Spread..Treasury.Yield..10.Year.Minus.2.Year,start = c(1996,1),frequency = 12)
# Export Index
Export_index <-ts(data$ISMINDEXExports,start = c(1996,1),frequency = 12)

##################################################################
# Nominated Freight Data
##################################################################

# Truck Tonnage Index (start 2020,Jan 1)
Truck_Tonnage <- ts(data$Tonnage,start = c(1996,1),frequency = 12)





plot(cbind(class8,class67,NewOrdersIndex,PMI_MANU,production,Fed_Fund,Treasury_Yield,Export_index))
#OLS

OLS1 <- lm(class8 ~ NewOrdersIndex+class67+Export_index)
summary(OLS1)


#Determine the Persistance of the model

pacf(class8)
acf(class8)




# Fining Optimal Lag
OL <- cbind(class8,as.factor(NewOrdersIndex),as.factor(PMI_MANU),as.factor(production),class67,as.factor(Export_index),as.factor(Fed_Fund))
OL <- na.omit(OL)

lagselect <-VARselect(OL, lag.max = 12,type = "const")
lagselect$selection
lagselect$criteria


#Building VAR
modelvar <-VAR(OL,p=2,type = "const",season = NULL ,exog = NULL)
summary(modelvar)


#Diagnostic the VAR
OL_2 <- cbind(class8,class67,Fed_Fund)
OL_2 <- na.omit(OL_2)

plot(cbind(class8,class67,Fed_Fund))

VARselect(OL_2, lag.max = 12,type = "const")

modelvar_2 <-VAR(OL_2,ic = 'AIC',p=3,type = "const",season = NULL ,exog = NULL)
summary(modelvar_2)
acf(residuals(modelvar_2)[,1])
acf(residuals(modelvar_2)[,2])
acf(residuals(modelvar_2)[,3])
acf(residuals(modelvar_2)[,4])
#Serial Correlation

serial1 <- serial.test(modelvar_2,lags.pt = 12,type = "PT.asymptotic")
serial1            # did not pass the test since the p-value is <0.05



serial2 <- serial.test(modelvar_2,lags.pt = 12,type = "PT.asymptotic")
serial2   #passes


#Heteroscedasticity (Checking for volatility)

Arch1 <- arch.test(modelvar_2, lags.multi = 12, multivariate.only = TRUE)
Arch1  #passes
 


#Normal Distributional of the residuals 


Norm1 <- normality.test(modelvar_2,multivariate.only = TRUE)
Norm1  


#Testing for structural breaks and the residuals   

stability1 <- stability(modelvar_2,type = "OLS-CUSUM")
plot(stability1)     #passes 



#Granger Casualty 

modelvar_2
GrangerClass8 <- causality(modelvar_2, cause = "class67")
GrangerClass8


#Impulse Response Functions

Class8irf <-irf(modelvar_2,impulse = "Fed_Fund",response = "class8",n.ahead = 12,boot = TRUE)
plot(Class8irf)




#Variance Decompose 

FEVD1 <- fevd(modelvar_2,n.ahead =12)
FEVD1
plot(FEVD1)



forecast <- predict(modelvar_2,n.ahead = 12,ci=0.95)
fanchart(forecast,names = "class8")
plot(forecast)






OL_3 <- cbind(class8,NewOrdersIndex,PMI_MANU,production,class67,Export_index)
OL_3 <- na.omit(OL_3)
modelvar_3 <-VAR(OL_3,ic = 'AIC',p=2,type = "const",season = NULL ,exog = NULL)
VARselect(OL_3, lag.max = 12,type = "const")

forecast <- predict(modelvar_3,n.ahead = 40,ci=0.95)
fanchart(forecast,names = "class8")
plot(forecast)



class8_TEST <- ts(data$SUMClass8,start = c(1996,1),end = c(2019,1),frequency = 12)
#Total Class 67 Units In US&Canada  (Target for forecast)
class67_TEST <- ts(data$SUMClass67,start = c(1996,1),end = c(2019,1),frequency = 12)
##################################################################
# Nominated Macroeconomic Drivers 
##################################################################
# New Oder Index
NewOrdersIndex_TEST <- ts(data$ISMINDEXNewOrders,start = c(1996,1),end = c(2019,1),frequency = 12)
# PMI Manufacturing Index
PMI_MANU_TEST <- ts(data$ISMINDEXPMIManufacturingCompositeIndex ,start = c(1996,1),end = c(2019,1),frequency = 12)
# Total industry production 
production_TEST <- ts(data$IndustrialProductionIndex ,start = c(1996,1),end = c(2019,1),frequency = 12)
# Effective Federal fund Rate
Fed_Fund_TEST <- ts(data$Effective.Federal.Funds.Rate*10,start = c(1996,1),end = c(2019,1),frequency = 12)

# Treasury Yield - Yield Spread
Treasury_Yield_TEST <- ts(data$Yield.Spread..Treasury.Yield..10.Year.Minus.2.Year,start = c(1996,1),end = c(2019,1),frequency = 12)
# Export Index
Export_index_TEST <-ts(data$ISMINDEXExports,start = c(1996,1),end = c(2019,1),frequency = 12)

##################################################################
# Nominated Freight Data
##################################################################

# Truck Tonnage Index (start 2020,Jan 1)
Truck_Tonnage_TEST <- ts(data$ATA.Truck.Tonnage.Index..SA.,start = c(1996,1),end = c(2019,1),frequency = 12)






OL_4 <- cbind(class8_TEST,NewOrdersIndex_TEST,PMI_MANU_TEST,production_TEST,class67_TEST,Export_index_TEST)
OL_4 <- na.omit(OL_4)
modelvar_4 <-VAR(OL_4,p=3,type = "const",season = NULL ,exog = NULL)
summary(modelvar_4 )
forecast <- predict(modelvar_4,n.ahead = 24,ci=0.95)
fanchart(forecast,names = "class8_TEST")
plot(class8)

plot(forecast)



checkresiduals(forecast)



# Data partitioning
library(splitTools)
library(ranger)


# Split data into partitions
set.seed(3451)
inds <- partition(data$SUMClass8, p = c(train = 0.6, valid = 0.2, test = 0.2))
str(inds)

# List of 3
# $ train: int [1:182] 2 3 5 6 8 9 10 11 12 13 ...
# $ valid: int [1:65] 1 4 24 26 29 30 33 35 38 39 ...
# $ test : int [1:62] 7 31 54 59 62 65 69 72 75 78 ...

train <- data[inds$train, ]
valid <- data[inds$valid, ]
test <- data[inds$test, ]

train

# Root-mean-squared error function used to evaluate results
rmse <- function(y, pred) {
  sqrt(mean((y - pred)^2))
}

# Tune mtry on validation data
valid_mtry <- numeric(ncol(train) - 1)


for (i in seq_along(valid_mtry)) {
  fit <- ranger(class8 ~ ., data = data, mtry = i)
  valid_mtry[i] <- rmse(valid$SUMClass8, predict(fit, valid)$predictions)
}



summary(valid_mtry)

(best_mtry <- which.min(valid_mtry))



final_fit <- ranger(SUMClass8 ~ ., data = train, mtry = best_mtry)
summary(rmse(test$SUMClass8, predict(final_fit, test)$predictions))


# Split into training and test
inds <- partition(data$SUMClass8, p = c(train = 0.8, test = 0.2))

train <- data[inds$train, ]
test <- data[inds$test, ]



# Get stratified cross-validation fold indices
folds <- create_folds(train$SUMClass8, k = 5)



# Tune mtry by GridSearchCV
valid_mtry <- numeric(ncol(train) - 1)

for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(SUMClass8 ~ ., data = train[fold, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[-fold, "SUMClass8"], predict(fit, train[-fold, ])$predictions))
  }
  valid_mtry[i] <- mean(cv_mtry)
}


# Result of cross-validation
valid_mtry



(best_mtry <- which.min(valid_mtry))


# Use optimal mtry to make model
final_fit <- ranger(SUMClass8 ~ ., data = train, mtry = best_mtry)
rmse(test$SUMClass8, predict(final_fit, test)$predictions)

# Repeated Cross-Validation
# We start by making repeated, stratified cross-validation folds
folds <- create_folds(train$SUMClass8, k = 5, m_rep = 3)
length(folds)



for (i in seq_along(valid_mtry)) {
  cv_mtry <- numeric()
  for (fold in folds) {
    fit <- ranger(SUMClass8 ~ ., data = train[fold, ], mtry = i)
    cv_mtry <- c(cv_mtry, 
                 rmse(train[-fold, "SUMClass8"], predict(fit, train[-fold, ])$predictions))
  }
  valid_mtry[i] <- mean(cv_mtry)
}

# Result of cross-validation
valid_mtry
(best_mtry <- which.min(valid_mtry))

# Use optimal mtry to make model
final_fit <- ranger(SUMClass8 ~ ., data = train, mtry = best_mtry)
rmse(test$SUMClass8, predict(final_fit, test)$predictions)


# We first create a time series and derive lagged features for training.
# Then, again, we optimize mtry of a random forest by time-series cross-validation.
# We evaluate the optimized model on the last 10% of the time series.





# Forecast Using ARIMA model (multivariate)

Arima_Data <- cbind(class8,PMI_MANU,Truck_Tonnage)
summary(Arima_Data)
ldata <- Arima_Data
ldata <- na.omit(ldata)

plot(ldata)


par(mgp=c(2,0.7,0),mar=c(3,3,3,1))
acf(ldata)
pacf(ldata)


var.auto <-ar(ldata,order.max = 10)
summary(var.auto)

var.auto$aic

acf(var.auto$resid,na.action = na.omit)
pacf(var.auto$resid,na.action = na.omit)


ks.test(var.auto$resid[,1]/sd(var.auto$resid[,1],na.rm = TRUE),"pnorm")
ks.test(var.auto$resid[,2]/sd(var.auto$resid[,2],na.rm = TRUE),"pnorm")


(var1 <- VAR(ldata,p=3,type="both"))
summary(var1)

vars.1 <- VARselect(ldata,type = "const",lag.max = 6)
vars.1


(var1l <- VAR(ldata,p=2))
summary(var1l)



serial.test(x = var1l)

arch.test(var1l)
normality.test(var1l)

vars.test <- function(x){
  
  norm <- sapply(normality.test(x)[[2]], function(xx) xx$p.value)
  arch <- arch.test(x)[[2]]$p.value
  ser <- serial.test(x)[[2]]$p.value
  return(c(norm,"arch"=arch,"serial"=ser))
  
}

vars.test(var1l)

par(mgp=c(2,0.7,0),mar=c(3,3,3,1))

plot(var1l)
var1l

summary(var1l)


forecast <- predict(var1l,n.ahead =14,ci=0.95)
fanchart(forecast,names = "class8")

forecast
plot(forecast)

library(ggfortify)

fortify(forecast,names = "class8")




ldata
data.frct = data.frame(actual=class8[304:309],forecast = as.vector(forecast$fcst))
print(data.frct)
ts.plot(data.frct)





