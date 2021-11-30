

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(corrplot)
library(xts)
library(aTSA)


data <- read.csv(file.choose())


#simple Graph to see correlation btw class8 units VS class67 units

ggplot(data = data) + geom_point(mapping = aes(x=log(data$SUMClass8)*10, y = log(data$SUMClass67)*10 ))
ggplot(data = data) + geom_point(mapping = aes(x=log(SUMClass8)*10, y = ISMINDEXNewOrders))

#Total Class 8 Units In US&Canada  (Target for forecast)
class8 <- ts(data$SUMClass8,start = c(1996,1),frequency = 12)

#Total Class 67 Units In US&Canada  (Target for forecast)
class67 <- ts(log(data$SUMClass67) ,start = c(1996,1),frequency = 12)


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

Truck_Tonnage <- ts(data$ATA.Truck.Tonnage.Index..SA.,start = c(1996,1),frequency = 12)



plot(cbind(class8,class67,NewOrdersIndex,PMI_MANU,production,Fed_Fund,Treasury_Yield,Export_index,Truck_Tonnage))
autoplot(cbind(class8,class67,NewOrdersIndex,PMI_MANU,production,Fed_Fund,diff(Treasury_Yield),Export_index,Truck_Tonnage))

All_Data <-cbind(class8,class67,NewOrdersIndex,PMI_MANU,production,Fed_Fund,Treasury_Yield,Export_index,Truck_Tonnage)
All_Data <-na.omit(All_Data)

# check to see which of these variables are not stationary 
pptab <-NULL
for(i in 1:ncol(All_Data)){
  pp<-PP.test(All_Data[,i])
  pptab<-rbind(pptab,pp$p.value)
  
}

pptab



#redo with differences 
All_Data <-cbind(diff(class8),diff(class67),diff(NewOrdersIndex),diff(PMI_MANU),diff(production),diff(Fed_Fund),diff(Treasury_Yield),diff(Export_index),diff(Truck_Tonnage))
All_Data <-na.omit(All_Data)

pptab <-NULL
for(i in 1:ncol(All_Data)){
  pp<-PP.test(All_Data[,i])
  pptab<-rbind(pptab,pp$p.value)
  
}

pptab
# All of my TSs are stationary 

autoplot(All_Data)

#plot IRFs
#Loop to only make response of CA
#default setting

var <- All_Data
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 1:9){
  irf1<-irf(var1,impulse = colnames(var1$datamat[i]),response = colnames(var1$datamat[1]),n.head =12, ortho = TRUE,ci=0.95,boot=TRUE,runs=100,cumulative=FALSE)
  plot(irf1)
}

var1<-VAR(var,type = c("const"),lag.max = 2)

#GET FEVDs at 1,,4,8,12-months horizons
fevd1<-fevd(var1,n.head=12)
fevd2<-fevd1$class8
fevdtab <-fevd2
rownames(fevdtab) <- c(1,4,8,12)
print(fevdtab)




# Granger Causality tests: I make pairwise Vars, then a nice table 

gctab<-NULL
var<-All_Data
All_Data
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 2:9){
   var2<-VAR(var[,c(i,1)],type = c("const"),lag.max = 4,ic="SC")
  gc<-causality(var2,cause = colnames(var1$datamat[i]))
  gc1<-cbind(as.numeric(gc$Granger$statistic),gc$Granger$p.value)
  gctab<-rbind(gctab,gc1)
  
}

colnames(gctab)<- c("Statistic","p_val.")
rownames(gctab)<-colnames(var1$datamat[c(2:9)])
gctab<-round(gctab,3)
print(gctab)


#OLS

OLS1 <- tslm(class8 ~ class67 + production +NewOrdersIndex +PMI_MANU+Truck_Tonnage)

summary(OLS1)

#bulding model

Model <- cbind(class8,class67,production,NewOrdersIndex,PMI_MANU,Truck_Tonnage)
Model <-na.omit(Model)
Model

lagselect <-VARselect(Model, lag.max = 12,type = "both")
lagselect$selection
lagselect$criteria

Model1 <- VAR(Model, p = 3, season = NULL, exog = NULL, type = "const")
forecast(Model1) %>% autoplot() 

forecast1 <- predict(Model1,n.ahead = 12, ci =0.95)
forecast1

fanchart(forecast, names = "class8")


