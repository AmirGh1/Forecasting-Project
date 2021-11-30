

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)


data <- read.csv(file.choose())

autoplot(Y_2)

ggplot(data=data) + geom_point(mapping = aes(x = data$SUM.Class8 ,y = data$SUM.Class67))


class_8 <- ts(data[,7],start=c(1996,1),frequency =12)

class_67 <-ts(data[,4],start=c(1996,1),frequency =12)

autoplot(cbind(class_8,class_67))

#OLS

OLS1 <- lm(class_8 ~ class_67)
summary(OLS1)


#detemine the Persistence of the model

acf(class_8,main = "ACF for class 8 units")
pacf(class_8,main = "PACF for class 8 units")


acf(class_67,main = "ACF for class 8 units")
pacf(class_67,main = "PACF for class 8 units")


data.bv <- cbind(class_8,class_67)

lagselect <- VARselect(data.bv,lag.max = 10,type = "const")
lagselect$selection

#model VAR
model_1 <-data.bv 

df <- diff(class_8)

components.ts = decompose(df)
plot(components.ts)
