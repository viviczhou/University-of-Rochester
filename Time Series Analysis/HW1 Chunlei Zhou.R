############## Homework 1 Chunlei Zhou

####### Q1
##b)	Create a time series plot of the data
airtravel <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/BTS_Air_Rail_Vehicle_Miles.csv', header=TRUE, sep=",")
summary(airtravel)
head(airtravel)
airtravel$Month <- as.Date(airtravel$Month)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library('scales')
ggplot(airtravel, aes(x = Month, y = Air.RPM..000s.)) + xlab("Time") + ylab("Air RPM") + geom_line() +scale_x_date(labels=date_format("%y")) + stat_smooth(color = 3, fill = 2,method = "loess") 


####### Q2
##a) time series plot for other two travel methods
ggplot(airtravel, aes(x = Month, y = Rail.PM)) + xlab("Time") + ylab("Railway Passenger Miles") + geom_line() +scale_x_date(labels=date_format("%y")) + stat_smooth(color = 3, fill = 2,method = "loess")
ggplot(airtravel, aes(x = Month, y = VMT..billions.)) + xlab("Time") + ylab("Vehivle Mile Traveled") + geom_line() +scale_x_date(labels=date_format("%y")) + stat_smooth(color = 3, fill = 2,method = "loess")


####### Q3
shampoosale <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Shampoo_Sales.csv', header=TRUE, sep=",")
summary(shampoosale)
head(shampoosale)
shampoosale$Month <- as.Date(shampoosale$Month)
ggplot(shampoosale, aes(x = Month, y = Shampoo.Sales)) + xlab("Time") + ylab("Units Sold") + geom_line() +scale_x_date(labels=date_format("%m/%y")) + stat_smooth(color = 3, fill = 2,method = "loess")
ggplot(shampoosale, aes(x = Month, y = Shampoo.Sales)) + xlab("Time") + ylab("Units Sold") + geom_point() +scale_x_date(labels=date_format("%m/%y")) + stat_smooth(color = 3, fill = 2,method = "loess")


####### Q4
bevship <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/Beverages_Shipment_2019 copy.csv', header=TRUE, sep=",")
summary(bevship)
head(bevship)
library(stats)
acf(bevship$Dollars..in.Millions, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)


####### Q5
coalprod <- read.csv(file='~/Desktop/DATA SCIENCE/Time Series Analysis/U.S. Coal Production.csv', header=TRUE, sep=",")
summary(coalprod)
head(coalprod)

##a)	Plot the coal production data and the sample autocorrelation function. 
coalprod$Year <- as.Date(coalprod$Year)
ggplot(coalprod, aes(x = Year, y = Coal.Production..Short.Tons.in.Thousands)) + xlab("Time") + ylab("Coal Production in Tons") + geom_line() +scale_x_date(labels=date_format("%d/%m/%y")) + stat_smooth(color = 3, fill = 2, method = "loess")
acf(coalprod$Coal.Production..Short.Tons.in.Thousands, lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)

##c)	Plot the first difference of the time series.
coal.production <- ts(coalprod$Coal.Production..Short.Tons.in.Thousands)
plot(diff(coal.production), type="o", main="first difference")
plot(coal.production)
acf(coal.production, lag.max = 1, plot = F)
acf(coal.production, plot = F)

