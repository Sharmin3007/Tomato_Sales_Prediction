
# Read CSV File

my_data <- read.csv("E:/NDSU/NDSU_ Classes/Applied Regression/Project/Final Project/Tomato5.csv")

#See the Summary of the data

summary(my_data)

#Check out the library
library(fpp2)

#Convert the data frame into time series data

Y<-ts(my_data[,2],start=c(2014,1),frequency = 12)

#Plot the main time series v alues

autoplot(Y)+ggtitle("Time Plot: Tomato Sales Per Day")+
  ylab("Sales Quantity Per Day")

#Differencing the Y value

DY<-diff(Y)

#Plot the modified time series values

autoplot(DY)+ggtitle("Time Plot: Tomato Sales Per Day")+
  ylab("Sales Quantity Per Day")

#Plot a seasonal chart

ggseasonplot(DY)+
  ggtitle("Seasonal Plot: Change in Daily Sales")+
  ylab("Sales Quantity Per Day")

#Plot a subseries chart

ggsubseriesplot(DY)


#Seasonal Naive Method as our benchmark #Residual sd: 45.699
#y_t=y_{t-s}+e_t

fit<-snaive(DY) 
print(summary(fit))
checkresiduals(fit)


#Fit on ARIMA Model

fit_arima<-auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
sqrt(162703)

#Forecasting on ARIMA Model

fcst<-forecast(fit_arima,h=24)
autoplot(fcst,include=48)
print(summary(fcst))

#Check with Original vs Predicted Value

YTest<-ts(my_data[,2],frequency=12,start=c(2014,1),end=c(2019,12))
fitTest<-auto.arima(YTest,d=1,D=1,stepwise=FALSE,approximation = FALSE,trace=TRUE)
ftest<-forecast(fitTest,h=11)
print(summary(ftest))

#Testing With Original Data

original_2020<-tail(Y,11)
print(original_2020)





