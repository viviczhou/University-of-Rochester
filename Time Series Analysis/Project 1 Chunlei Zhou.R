#### Project 1 Chunlei Zhou

airline_miles <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Project1_DataSet.csv', header=TRUE, sep=",")
summary(airline_miles)
head(airline_miles)
ts.airline_miles <- ts(airline_miles$Miles..in.Millions)
airline_miles$Month <- as.Date(airline_miles$Month)

# 1. Create a time series of the plot of the data provided.
library(ggplot2)
library(scales)
ggplot(airline_miles, aes(x = Month, y = Miles..in.Millions)) + xlab("Month") + ylab("Airline Miles") + ggtitle("Time Series Plot For Airline Miles Flown in the UK") + geom_line() +scale_x_date(labels=date_format("%m/%y")) + stat_smooth(color = 3, fill = 2,method = "loess") 

# 2. Compute ACF and display it in a plot. 
acf(ts.airline_miles, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)

# 3. Compute a moving average for the data and overlay on the original time-series plot.
library(smooth)
library(TTR)
plot(ts.airline_miles,type='o',pch='o',lty=1, xlab='Month',ylab='Airline Miles', main = 'Time Series Plot For Airline Miles Flown in the UK')
for (i in 1:20){
  sma <- SMA(ts.airline_miles,n=i)
  assign(paste('SMA',i,sep = ''), sma)
  lines(sma,col=i)
}
# I noticed that the proper moving average window length should between 10 to 15.
for (i in 10:15){
  sma <- SMA(ts.airline_miles,n=i)
  #sma(ts.airline_miles, n=i,silent = T,interval = T)
  #movavg(ts.airline_miles, i, type=c("s", "t", "w", "m", "e", "r"))
  assign(paste('SMA',i,sep = ''), sma)
  lines(sma,col=i)
}
# It is obvious that the best moving average window length should be 12.
sma <- SMA(ts.airline_miles,n=12)
lines(sma,col=12)
legend("topleft",legend=c("Original Data",'SMA(12)'),col=c(1,12),lty = 1)

# 5. Compute the first difference of the data
first_diff <- diff(ts.airline_miles)
# plot the ACF for the differenced data.
first_diff.acf <- acf(first_diff, lag.max = 80, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
# plot the PACF for the differenced data.
first_diff.pacf <- pacf(first_diff, lag = length(ts.airline_miles) - 1, pl = TRUE)

# 6. perform a first seasonal difference with the seasonal period = 12
seasonal<-diff(first_diff, lag = 12)
# plot the ACF for the seasonal differenced data.
seasonal.acf <- acf(seasonal, lag.max = 80, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
# plot the PACF for the seasonal differenced data.
seasonal.pacf <- pacf(seasonal, lag = length(ts.airline_miles) - 1, pl = TRUE)

# 7. Develop a suitable SARIMA model
ts.train <- ts(airline_miles$Miles..in.Millions, start=c(1964, 1), end=c(1969, 12), frequency=12)
# vary the model parameters (p,d,q) and (P,D,Q) 
library(forecast)
aic <- NA
min_aic = 1000
best_model = NA
for (i in 0:4){
  for (j in 0:4){
    for (a in 0:4){
      for (b in 0:4){
        try(model <- Arima(ts.train, order = c(i, 1, j), seasonal = list(order = c(a, 1, b), period = 12), method = "ML"), silent = T)
        try(aic <- c(aic,model$aic),silent = T)
        try(
          if (model$aic < min_aic){
          min_aic = model$aic
          best_model = model}, silent = T)
}}}}
aic <- aic[-1]

# determine the best choice of parameters (p,d,q,P,D,Q)
best_aic <- min(aic)
best_aic
min_aic
best_model
plot(aic, col = 4, pch = '*')
lines(aic, col = 4)

# 8. forecast the 7th year data.
forecast_1970 <- forecast(best_model,h=12,level = 95)
autoplot(forecast_1970)
forecast_1970
# compare with the actual values
ts.test = ts.airline_miles[73:84]
ts.test
accuracy(forecast_1970,x=ts.test)
plot(forecast_1970,ylim= c(5,20),main ='Forecasted best_model')
par(new = T)
lines(forecast_1970$fitted, col = 3)
par(new = T)
plot(ts.airline_miles, col = 2, ylim=c(5,20),axes = F, xlab = "", ylab = "")
legend('topleft', legend=c("Actual Values", "Fitted Values","Forecasted Values"), col=c(2,3,4), lty=1)
plot(forecast_1970$mean,ylim= c(5,20),col = 3,ylab = "Values", main="Compare Forecasted Values with Actual Values")
par(new = T)
plot(ts.test, col = 2, ylim=c(5,20),axes = F, xlab = "", ylab = "")
lines(ts.test, col = 2)
legend('topleft', legend=c("Actual", "Forecasted"), col=c(2,3), lty=1)

# Try auto.arima:
auto_model <- auto.arima(ts.train, max.p = 4, max.q = 4, max.P = 4, max.Q = 4, D = 1, d = 1, max.order = 18, ic = 'aic')
auto_model
forecast_auto <- forecast(auto_model,h=12,level = 95)
autoplot(forecast_auto)
forecast_auto
accuracy(forecast_auto,x=ts.test)
plot(forecast_auto,ylim= c(5,20),main="Forecasted auto_model")
par(new = T)
lines(forecast_auto$fitted, col = 3)
par(new = T)
plot(ts.airline_miles, col = 2, ylim=c(5,20),axes = F, xlab = "", ylab = "")
legend('topleft', legend=c("Actual Values", "Fitted Values","Forecasted Values"), col=c(2,3,4), lty=1)

# Compare two models
plot(forecast_auto$mean,ylim= c(5,25),col = 3,ylab = "Values", main="Compare two models")
par(new = T)
plot(ts.test, col = 2, ylim=c(5,25),axes = F, xlab = "", ylab = "")
lines(ts.test, col = 2)
par(new = T)
plot(forecast_1970$mean,ylim= c(5,25),col = 4, axes = F, xlab = "", ylab = "")
legend('topleft', legend=c("Actual Values", "best_model","auto_model"), col=c(2,4,3), lty=1)