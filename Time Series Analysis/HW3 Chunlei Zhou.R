# HW3 Chunlei Zhou

# Q1 Use file Measurement_Q1.xls exhibits a linear trend. Apply the following models.
measurement <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Measurement_Q1.csv', header=TRUE, sep=",")
summary(measurement)
head(measurement)
ts.measurement <- ts(measurement$Measurement)

# (a) IMA(1,1) model:
library(stats)
library(forecast)
model_1_a <- Arima(ts.measurement,order=c(0,1,1),include.constant = T)
# Overlay the model output on the original time series.
plot(model_1_a$x,type='o',col='red',pch='o',lty=1, xlab='Time',ylab='Measurement',main = 'Measurement')
lines(fitted(model_1_a),col="blue",lty=2)
points(fitted(model_1_a),col="blue",pch='+')
legend(10,300,legend=c("Original Data","IMA(1,1)"),col=c("red","blue"),lty=c(1,2),pch=c("o","+"))

# (b) Compute and plot the first differences of the data.
plot(diff(ts.measurement), type="o", main="First Difference",xlab='Time',ylab='Measurement')
first_diff <- diff(ts.measurement)

# (c) Develop an MA(1) model on the first difference. 
model_1_c <- Arima(first_diff,order=c(0,0,1),include.constant = T)
# Overlay the model output on the first difference computed in (b).
plot(model_1_a$x,type='o',col='red',pch='o',lty=1, xlab='Time',ylab='Measurement',main = 'Measurement & Fitst Difference',ylim=c(-110, 800))
lines(fitted(model_1_c),col="green",lty=3)
points(fitted(model_1_c),col="green",pch='*')
legend('topleft',legend=c("Original Data","IMA(1,1)","First Diff MA(1)"),col=c("red","blue","green"),lty=c(1,2,3),pch=c("o","+","*"))

# (d) Compare Two Models
summary(model_1_a)
summary(model_1_c)

# Q2 Use GlobalAirTemperature.xls.
airtemp <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/GlobalAirTemperature.csv', header=TRUE, sep=",")
summary(airtemp)
head(airtemp)
ts.airtemp <- ts(airtemp$Anomaly..C)
airtemp$Year <- as.Date(airtemp$Year)

# (a) IMA(1,1) 
model_2_a <- Arima(ts.airtemp,order=c(0,1,1),include.constant = T)
# Overlay the output on the original data
plot(model_2_a$x,type='o',col='red',pch='o',lty=1, xlab='Time',ylab='Air Temperature',main = 'Global Air Temperature')
lines(fitted(model_2_a),col="blue",lty=2)
points(fitted(model_2_a),col="blue",pch='+')
legend('topleft',legend=c("Original Data","IMA(1,1)"),col=c("red","blue"),lty=c(1,2),pch=c("o","+"))
# Calculate the SSE by comparing the model output with the data
sse_2_a = sum(model_2_a$residuals**2) #2.182826

# (b) IMA(1,2) model
model_2_b <- Arima(ts.airtemp,order=c(0,1,2),include.constant = T)
# Calculate the SSE
sse_2_b = sum(model_2_b$residuals**2) # 2.045327

# Q3 Use Measurement_Q3.xls 
annual_measure <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Measurement_Q3.csv', header=TRUE, sep=",")
summary(annual_measure)
head(annual_measure)
ts.annual_measure <- ts(annual_measure$Measurement)
annual_measure$Year <- as.Date(annual_measure$Year)

# (a) Plot the time series
plot(ts.annual_measure,type='o',col='red', pch='o',lty=1, xlab='Year',ylab='Measurement',main = 'Annual Measurement')
library(ggplot2)
library('scales')
ggplot(annual_measure, aes(x = Year, y = Measurement)) + xlab("Year") + ylab("Measurement") + geom_line() +scale_x_date(labels=date_format("%y")) + stat_smooth(color = 3, fill = 2,method = "loess")
# Plot ACF
acf(ts.annual_measure, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
acf(ts.annual_measure, lag.max = 4, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
# Plot PACF
pacf (ts.annual_measure, lag = length(ts.annual_measure) - 1, pl = TRUE)
pacf (ts.annual_measure, lag.max = 4, pl = TRUE)

# (b) Plot First Difference
first_diff_2b <- diff(ts.annual_measure)
plot(first_diff_2b, type="o", main="First Difference")
# Plot ACF
acf(first_diff_2b, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
acf(first_diff_2b, lag.max = 4, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)
# Plot PACF
pacf (first_diff_2b, lag = length(ts.annual_measure) - 1, pl = TRUE)
pacf (first_diff_2b, lag.max = 4, pl = TRUE)

