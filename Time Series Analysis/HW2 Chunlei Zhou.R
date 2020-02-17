# Homework #2 Chunlei Zhou

# Q1 Consider the measurement data provided in the file: Measurement_Q1.xls
measurement <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/MeasurementData_Q1.csv', header=TRUE, sep=",")
summary(measurement)
head(measurement)
n_measurement <- na.omit(measurement)
measurement$Year <- as.Date(measurement$Year)

#### a 
# Use a 10-period simple moving average to smooth the data. 
install.packages('smooth')
install.packages('greybox')
library(smooth)
sma(measurement$Measurement, n=10,silent = F,interval = T)
psma.measurement <- ts(measurement$Measurement)
psma.measurement<-na.omit(psma.measurement)
plot(psma.measurement,type='o',col='red',pch='o',lty=1, xlab='Time',ylab='Measurement')

# Plot both the smoothed data and the original data values on the same axes. 
library(TTR)
psma10 <- SMA(psma.measurement,n=10)
lines(psma10,col='blue',lty=2)
points(psma10,pch='*',col='blue')
legend(2.5,560,legend=c("Original Data","10 Periods MA"),col=c("red","blue"),lty=c(1,2),pch=c("o","*"))

#### b
# Repeat the procedure with a 4-period simple moving average. 
sma(measurement$Measurement, n=4,silent = F,interval = T)
# Overlay this result on the same plot above. 
psma4 <- SMA(psma.measurement,n=4)
lines(psma4,col='green',lty=3)
points(psma4,pch='+',col='green')
legend(2.5,560,legend=c("Original Data","10 Periods MA","4 Periods MA"),col=c("red","blue","green"),lty=c(1,2,3),pch=c("o","*","+"))

#Q2 FOR GRADUATE STUDENTS – DSC 475 ONLY
#### Please refer to the pdf

#Q3 The file: Yield_Data.xls presents data on the hourly yield from a chemical process. 
yield <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Yield_Data.csv', header=TRUE, sep=",")
summary(yield)
head(yield)
hour.yield<- ts(yield$Yield...)
plot(hour.yield,type='o',col=2,pch='o',lty=1, xlab='Hour',ylab='Yield',ylim=c(70, 110))

#### a 
# Use simple (first order) exponential smoothing with λ = 0.2 to smooth the data.
foem_0.2 <- HoltWinters(hour.yield, alpha=0.2, beta=F, gamma=F)
# Overlay the smoothed plot on the original time series. 
plot(foem_0.2,ylim=c(70, 110))
tb_0.2 <- as.table(foem_0.2$fitted)
lines(tb_0.2[,1],col=3,lty=2)
points(tb_0.2[,1],col=3,pch='*')
legend(20,80,legend=c("Original Data","HW_lamda_0.2"),col=c(2,3),lty=c(1,2),pch=c("o","*"))

#### b
# Change λ to 0.8, and smooth the data with new λ. 
foem_0.8 <- HoltWinters(hour.yield, alpha=0.8, beta=F, gamma=F)
# Overlay the smoothed plot on the same plot made in (a). 
plot(foem_0.8,ylim=c(70, 110))
tb_0.8 <- as.table(foem_0.8$fitted)
lines(tb_0.8[,1],col=4,lty=3)
points(tb_0.8[,1],col=4,pch='+')
legend(20,83,legend=c("Original Data","HW_lamda_0.2","HW_lamda_0.8"),col=c(2,3,4),lty=c(1,2,3),pch=c("o","*","+"))

# Compute the Mean Square Difference between the original data and the smoothed data for λ = 0.2 and λ = 0.8. 
# Since we do not have y0 in first order exponential smoothing, I remove the first row from the original data.
install.packages("Metrics")
library(Metrics)
hy_1 <- yield[-c(1),]
hy_2 <- yield[-c(50),]
# λ = 0.2
mse(hy_1$Yield...,tb_0.2[,1])
mean((hy_1$Yield... - tb_0.2[,1])^2)
mean((hy_2$Yield... - tb_0.2[,1])^2) #15.98977
# The result returned is 24.98573
# λ = 0.8
mse(hy_1$Yield...,tb_0.8[,1])
mean((hy_1$Yield... - tb_0.8[,1])^2)
mean((hy_2$Yield... - tb_0.8[,1])^2) #0.3009854
# The result returned is 7.536988

