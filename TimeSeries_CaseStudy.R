#Setting up working directory
setwd("F:/Data Science/Time Series/Case Study")

#importing file
retail<- read.csv("Global Superstore.csv", header = T)

#Loading required packages
library(forecast)
library(dplyr)
library(stringr)
library(ggplot2)
library(tseries)
library(lubridate)
require(graphics)

#----------Data Preperation--------
#Converting Data to a standard format
retail$Order.Date<- str_replace_all(retail$Order.Date, "\\-", "/")
retail$Order.Date<-as.POSIXct(retail$Order.Date, format="%d/%m/%Y")
retail$Ship.Date<- str_replace_all(retail$Ship.Date, "\\-", "/")
retail$Ship.Date<-as.POSIXct(retail$Ship.Date, format="%d/%m/%Y")

#checking columns having NA values
sapply(retail, function(x) sum(is.na(x))) #All 41296 NA values are in PostalCode column

#Hence removing this column also as its not required.
retail$Postal.Code<- NULL

#checking for any duplicated values
sum(duplicated(retail)) #No values

#For all rowIDs, orderIDs and CustomerIDs; removing them as they are not required for the analysis
retail$Row.ID<-NULL
retail$Order.ID<-NULL
retail$Customer.ID<-NULL

#As per guidance in Data preparation, considering relavant attributes for furthur analysis
Segment_Analysis<- retail[, c(1,5,9,15,16,18)]

#checking levels for segment and Market
levels(retail$Segment) #3 unique
levels(retail$Market) #7 unique

#As asked to make 21 market segment, below df contains 21 combinations of Market and Segment
Market_Segment_DF <- expand.grid(Market=unique(Segment_Analysis$Market),Segment=unique(Segment_Analysis$Segment))

Segment_Profit <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(Segment_Profit) <- c("Market","Segment","Total Profit","Profit Var") #profit CV= Co-efficient of variation

for(i in seq(1, nrow(Market_Segment_DF))){
  Row_df<-Segment_Analysis[(Segment_Analysis$Market==Market_Segment_DF$Market[i] & Segment_Analysis$Segment==Market_Segment_DF$Segment[i]),]#for each market-segment combination
  Row_df$Order.Date <- format(Row_df$Order.Date,"%Y-%m") #As asked to calculate at monthly values
  Row_df <- Row_df[order(Row_df$Order.Date),]
  Sum_df <- Row_df %>% group_by(Order.Date) %>% summarise(Total.Sales=sum(Sales),Total.Quantity=sum(Quantity),Total.Profit=sum(Profit))
  Sum_df <- Sum_df[1:(nrow(Sum_df)-6), ]
  Segment <- data.frame(Market=Row_df$Market[1],Segment=Row_df$Segment[1],"Total Profit" = sum(Sum_df$Total.Profit),"Profit_Var" = sd(Sum_df$Total.Profit)/mean(Sum_df$Total.Profit))
  Segment_Profit <- rbind(Segment_Profit,Segment)
}

View(Segment_Profit)
#From above we get that:

#APAC Consumer and EU Consumer are most profitable

#******************************************************
#----------------Analysing APAC-Consumer---------------#
#******************************************************

APAC_Consumer<- subset(retail, retail$Segment=="Consumer" & retail$Market=="APAC") #creating a df consisting APAC-consumer
APAC_Consumer<- APAC_Consumer[,c(1,15,16,18)] #Considering only relevat columns
APAC_Consumer<-APAC_Consumer[order(APAC_Consumer$Order.Date),] #sorting in order of orderDate
APAC_Consumer$Order.Date<-format(APAC_Consumer$Order.Date, "%Y-%m") #making only year-month
APAC_Consumer<- APAC_Consumer %>% group_by(Order.Date) %>% summarise(Sales=sum(Sales), Quantity=sum(Quantity), Profit=sum(Profit))
APAC_Consumer$Month<-1:nrow(APAC_Consumer) #Adding a month column with sequential data

#breaking train and test datasets
Train_APAC_Consumer<- APAC_Consumer[1:(nrow(APAC_Consumer)-6),]
Test_APAC_Consumer<- APAC_Consumer[(nrow(APAC_Consumer)-5):nrow(APAC_Consumer),] #consists last 6 months


#------------------plotting TimeSeries for sales-----
APAC_Consumer_TS<-ts(Train_APAC_Consumer$Sales)
plot(APAC_Consumer_TS)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- stats::filter(APAC_Consumer_TS, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Consumer_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Ploting the smoothed time series

lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
Month_Time_train <- Train_APAC_Consumer$Month
smooth_df <- as.data.frame(cbind(Month_Time_train, as.vector(smoothedseries)))
colnames(smooth_df) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit<-lm(Sales~ sin(0.5*Month) * poly(Month, 3) + cos(0.5*Month) * poly(Month, 3)
            + Month, data= smooth_df)
global_pred<- predict(lmfit, Month=Month_Time_train)
lines(Month_Time_train, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series

#-----------------We will model it as an ARMA series--------------

local_pred <- APAC_Consumer_TS- global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary")
kpss.test(residual)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

Month_Time_test <- Test_APAC_Consumer$Month
global_pred_out <- predict(lmfit,data.frame(Month = Month_Time_test))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class <- accuracy(fcast, Test_APAC_Consumer$Sales)[5]
MAPE_class #31.07429

#Now ploting the predictions along with original values

class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Consumer_TS, col = "black")
lines(class_pred, col = "red")

APAC_Sales_next6 <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]
#lines(APAC_Sales_nxt6mths)

#--------------------Now let's do an ARIMA fit-------------

autoarima_1 <- auto.arima(APAC_Consumer_TS)
autoarima_1
tsdiag(autoarima_1)
plot(autoarima_1$x, col="black")
lines(fitted(autoarima_1), col="red")
accuracy(autoarima_1)

#Again, let's check if the residual series is white noise

resi_auto_arima_1 <- APAC_Consumer_TS - fitted(autoarima_1)

adf.test(resi_auto_arima_1,alternative = "stationary")
kpss.test(resi_auto_arima_1)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_1 <- predict(autoarima_1, n.ahead = 6)

MAPE_auto_arima_1 <- accuracy(fcast_auto_arima_1$pred,Test_APAC_Consumer$Sales)[5]
MAPE_auto_arima_1 #27.689
#Conclusion that Auto Arima is better than Classical Decomposition as MAPE valus is lower here

#Lastly, let's plot the predictions along with original values, to visualize the fit

auto_arima_pred_1 <- c(fitted(autoarima_1),ts(fcast_auto_arima_1$pred))
plot(APAC_Consumer_TS, col = "black")
lines(auto_arima_pred_1, col = "red")

fcast_Next6months_APAC_Sales <- predict(autoarima_1, n.ahead = 12)$pred[7:12]
fcast_Next6months_APAC_Sales

#---------APAC Quantity------

#Plotting Time Series for Quantity
APAC_Quantity_TS<- ts(Train_APAC_Consumer$Quantity)
plot(APAC_Quantity_TS)

#Smoothing time series
w <-1
APAC_Quantity_Smooth <- stats::filter(APAC_Quantity_TS, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- APAC_Quantity_Smooth[w+2] - APAC_Quantity_Smooth[w+1]
for (i in seq(w,1,-1)) {
  APAC_Quantity_Smooth[i] <- APAC_Quantity_Smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Quantity_TS)
diff <- APAC_Quantity_Smooth[n-w] - APAC_Quantity_Smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  APAC_Quantity_Smooth[i] <- APAC_Quantity_Smooth[i-1] + diff
}

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

APAC_Quantity_Smooth_df <- as.data.frame(cbind(Month_Time_train, as.vector(APAC_Quantity_Smooth)))
colnames(APAC_Quantity_Smooth_df) <- c('Month', 'Quantity')

#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_2<- lm(Quantity~ sin(0.5*Month)*poly(Month,3)+cos(0.5*Month)*poly(Month,3)+Month, data=APAC_Quantity_Smooth_df)

# Locally predictable series(ARMA series)

global_pred_1 <- predict(lmfit_2, data.frame(Month=Month_Time_train))
lines(Month_Time_train, global_pred_1, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- APAC_Quantity_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit_2<-auto.arima(local_pred)
tsdiag(armafit_2)

#Checking if the residual series is white noise

residual <- local_pred-fitted(armafit_2)

adf.test(residual,alternative = "stationary")
kpss.test(residual)

#Evaluating the model using MAPE

global_pred_3 <- predict(lmfit_2,data.frame(Month=Month_Time_test))
fcast <- global_pred_3

#Comparing the predicted values with the actual values using MAPE

MAPE_class_Quant <- accuracy(fcast, Test_APAC_Consumer$Quantity)[5]
MAPE_class_Quant #62.1

APAC_Demand_next6 <- predict(lmfit_2,data.frame(Month=seq(1:54)))[49:54]

#Considering AutoArima now:

APAC_Quantity_AR<- auto.arima(APAC_Quantity_TS)
APAC_Quantity_AR

plot(APAC_Quantity_AR$x, col="Blue")
residual_Auto<- APAC_Quantity_TS - fitted(APAC_Quantity_AR)

adf.test(resi_auto_arima_1, alternative = "stationary")
kpss.test(resi_auto_arima_1)

fact_Autoarima<- predict(APAC_Quantity_AR, n.ahead = 6)
MAPE_auto_arima_2<- accuracy(fact_Autoarima$pred, Test_APAC_Consumer$Quantity)[5]
MAPE_auto_arima_2 #26.244

#ploting prediction vs actual values

autoArima<- c(fitted(APAC_Quantity_AR), ts(fact_Autoarima$pred))
plot(ts(APAC_Quantity_TS))
lines(autoArima, col="green")

fcast_Next6months_APAC_Demand <- predict(APAC_Quantity_AR, n.ahead = 12)$pred[7:12]
fcast_Next6months_APAC_Demand
#Auto Arima is better than Classical Decomposition


#***************************************************************
#----------------------Analysing EU Consumer------
#***************************************************************

EU_Consumer<- subset(retail, retail$Segment=="Consumer" & retail$Market=="EU") #creating a df consisting APAC-consumer
EU_Consumer<- EU_Consumer[,c(1,15,16,18)] #Considering only relevat columns
EU_Consumer<- EU_Consumer[order(EU_Consumer$Order.Date),] #sorting in order of orderDate
EU_Consumer$Order.Date<-format(EU_Consumer$Order.Date, "%Y-%m") #making only year-month
EU_Consumer<- EU_Consumer %>% group_by(Order.Date) %>% summarise(Sales=sum(Sales), Quantity=sum(Quantity), Profit=sum(Profit))
EU_Consumer$Month<-1:nrow(EU_Consumer) #Adding a month column with sequential data

#breaking train and test datasets
Train_EU_Consumer<- EU_Consumer[1:(nrow(EU_Consumer)-6),]
Test_EU_Consumer<- EU_Consumer[(nrow(EU_Consumer)-5):nrow(EU_Consumer),] #consists last 6 months

#------------------plotting TimeSeries for sales-----
EU_Consumer_TS<-ts(Train_EU_Consumer$Sales)
plot(EU_Consumer_TS)

#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- stats::filter(EU_Consumer_TS, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Consumer_TS)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Ploting the smoothed time series

lines(smoothedseries, col="red", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
Month_Time_train <- Train_EU_Consumer$Month
smooth_df <- as.data.frame(cbind(Month_Time_train, as.vector(smoothedseries)))
colnames(smooth_df) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit<-lm(Sales~ sin(0.5*Month) * poly(Month, 3) + cos(0.5*Month) * poly(Month, 3)
          + Month, data= smooth_df)
global_pred<- predict(lmfit, Month=Month_Time_train)
lines(Month_Time_train, global_pred, col='blue', lwd=2)

#Now, let's look at the locally predictable series

#-----------------We will model it as an ARMA series--------------

local_pred <- EU_Consumer_TS- global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

residual <- local_pred-fitted(armafit)
adf.test(residual,alternative = "stationary")
kpss.test(residual)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

Month_Time_test <- Test_EU_Consumer$Month
global_pred_out <- predict(lmfit,data.frame(Month = Month_Time_test))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class <- accuracy(fcast, Test_EU_Consumer$Sales)[5]
MAPE_class #92.957

#Now ploting the predictions along with original values

class_pred <- c(ts(global_pred),ts(global_pred_out))
plot(EU_Consumer_TS, col = "black")
lines(class_pred, col = "red")

EU_Sales_next6 <- predict(lmfit,data.frame(Month=seq(1:54)))[49:54]
#lines(APAC_Sales_nxt6mths)

#--------------------Now let's do an ARIMA fit-------------

autoarima_2 <- auto.arima(EU_Consumer_TS)
autoarima_2
tsdiag(autoarima_2)
plot(autoarima_2$x, col="black")
lines(fitted(autoarima_2), col="red")
accuracy(autoarima_2)

#Again, let's check if the residual series is white noise

resi_auto_arima_2 <- EU_Consumer_TS - fitted(autoarima_2)

adf.test(resi_auto_arima_2,alternative = "stationary")
kpss.test(resi_auto_arima_2)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_2 <- predict(autoarima_2, n.ahead = 6)

MAPE_auto_arima_2 <- accuracy(fcast_auto_arima_2$pred,Test_EU_Consumer$Sales)[5]
MAPE_auto_arima_2 #28.9226
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_2 <- c(fitted(autoarima_2),ts(fcast_auto_arima_2$pred))
plot(EU_Consumer_TS, col = "black")
lines(auto_arima_pred_2, col = "red")

fcast_Next6months_EU_Sales <- predict(autoarima_2, n.ahead = 12)$pred[7:12]
fcast_Next6months_EU_Sales

#Conclusion that Auto Arima is better than Classical Decomposition as MAPE valus is lower here

#---------EU Quantity------

#Plotting Time Series for Quantity
EU_Quantity_TS<- ts(Train_EU_Consumer$Quantity)
plot(EU_Quantity_TS)

#Smoothing time series
w <-1
EU_Quantity_Smooth <- stats::filter(EU_Quantity_TS, 
                                      filter=rep(1/(2*w+1),(2*w+1)), 
                                      method='convolution', sides=2)

#Smoothing left end of the time series

diff <- EU_Quantity_Smooth[w+2] - EU_Quantity_Smooth[w+1]
for (i in seq(w,1,-1)) {
  EU_Quantity_Smooth[i] <- EU_Quantity_Smooth[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Quantity_TS)
diff <- EU_Quantity_Smooth[n-w] - EU_Quantity_Smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  EU_Quantity_Smooth[i] <- EU_Quantity_Smooth[i-1] + diff
}

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

EU_Quantity_Smooth_df <- as.data.frame(cbind(Month_Time_train, as.vector(EU_Quantity_Smooth)))
colnames(EU_Quantity_Smooth_df) <- c('Month', 'Quantity')

#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_2<- lm(Quantity~ sin(0.5*Month)*poly(Month,3)+cos(0.5*Month)*poly(Month,3)+Month, data=EU_Quantity_Smooth_df)

# Locally predictable series(ARMA series)

global_pred_2 <- predict(lmfit_2, data.frame(Month=Month_Time_train))
lines(Month_Time_train, global_pred_2, col="red", lwd=2)

# d) Manual Arima ***

local_pred <- EU_Quantity_TS - global_pred
plot(local_pred, col='red')

acf(local_pred)
acf(local_pred, type="partial")

armafit_2<-auto.arima(local_pred)
tsdiag(armafit_2)

#Checking if the residual series is white noise

residual <- local_pred-fitted(armafit_2)

adf.test(residual,alternative = "stationary")
kpss.test(residual)

#Evaluating the model using MAPE

global_pred_3 <- predict(lmfit_2,data.frame(Month=Month_Time_test))
fcast <- global_pred_3

#Comparing the predicted values with the actual values using MAPE

MAPE_class_Quant <- accuracy(fcast, Test_EU_Consumer$Quantity)[5]
MAPE_class_Quant #30.39

EU_Demand_next6 <- predict(lmfit_2,data.frame(Month=seq(1:54)))[49:54]


#Considering AutoArima now:

EU_Quantity_AR<- auto.arima(EU_Quantity_TS)
EU_Quantity_AR

plot(EU_Quantity_AR$x, col="Blue")
residual_Auto<- EU_Quantity_TS - fitted(EU_Quantity_AR)

adf.test(resi_auto_arima_2, alternative = "stationary")
kpss.test(resi_auto_arima_2)

fact_Autoarima<- predict(EU_Quantity_AR, n.ahead = 6)
MAPE_auto_arima_2<- accuracy(fact_Autoarima$pred, Test_EU_Consumer$Quantity)[5]
MAPE_auto_arima_2 #30.13

#ploting prediction vs actual values

autoArima<- c(fitted(autoarima_2), ts(fact_Autoarima$pred))
plot(ts(EU_Consumer$Quantity), col="blue")
lines(autoArima, col="green")

fcast_Next6months_EU_Demand <- predict(EU_Quantity_AR, n.ahead = 12)$pred[7:12]
fcast_Next6months_EU_Demand

#Auto Arima is better than Classical Decomposition


#----------Forecasting-------

#Plots to forecast for Sales
forecast_Sales <- data.frame(cbind(fcast_Next6months_APAC_Sales, fcast_Next6months_EU_Sales, Month = seq(48:52)))

Plot_Sales<- ggplot()
Plot_Sales<- Plot_Sales + geom_line(data=forecast_Sales, aes(x=Month, y=fcast_Next6months_APAC_Sales, color="blue"))
Plot_Sales<- Plot_Sales + geom_line(data=forecast_Sales, aes(x=Month, y=fcast_Next6months_EU_Sales, color="red"))
Plot_Sales<- Plot_Sales + xlab("Month")+ylab("Sales")+ggtitle("Sales Forecast")
Plot_Sales

#Plots to forecast for Demand
forecast_Demand <- data.frame(cbind(fcast_Next6months_APAC_Demand, fcast_Next6months_EU_Demand, Month = seq(48:52)))

Plot_Demand<- ggplot()
Plot_Demand<- Plot_Demand + geom_line(data=forecast_Demand, aes(x=Month, y=fcast_Next6months_APAC_Demand, color="blue"))
Plot_Demand<- Plot_Demand + geom_line(data=forecast_Demand, aes(x=Month, y=fcast_Next6months_EU_Demand, color="red"))
Plot_Demand<- Plot_Demand + xlab("Month")+ylab("Demand")+ggtitle("Demand Forecast")
Plot_Demand
