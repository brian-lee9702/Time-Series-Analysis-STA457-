fileRead <- read.csv("IBM.csv")
IBMdata <- fileRead[c(203:1208),] 
prices <- IBMdata$Adj.Close
dates <- as.Date(IBMdata$Date, format='%Y-%m-%d')

length <- length(prices[]) #length of dataset

#Displaying first 30 data points, roughly 1 1/2 months
firstMonth <- IBMdata[c(976:1006),]
fmDates <- as.Date(firstMonth$Date, format='%Y-%m-%d')
fmPrices <- as.numeric(firstMonth$Adj)
plot(fmDates, fmPrices, type="b", main="IBM Stock Prices,
Jan-Feb 2015", xlab="Date", ylab="Price $")

priceDelta <- -diff(prices)
#ACF and PACF plots of stock price
acf(prices, 40, main="ACF plot of IBM stock price")
pacf(prices, 40, main="PACF plot of IBM stock price")

#ACF and PACF plots of change in stock price
acf(priceDelta, 40)
pacf(priceDelta, 40)

#Ljung-Box test
Box.test(prices, lag=5, type='Ljung-Box')

#Fitting an AR(1) model to stock price data
fitAR1 = arima(prices, order=c(1,0,0))
print(fitAR1)

#Testing white noise
Box.test(fitRes, lag=5, type="Ljung-Box")

#Stationary?
plot(dates, prices, type="l", main="Time series plot of IBM stock price, 2015-2018",
     xlab="Date", ylab="Price $")

#Gaussian Noise?
#Plotting KDE and fitted normal distribution
d <- density(fitRes, adjust=1.2)
plot(d, main="Density estimate of IBM stock price")
legend(-10, 0.25, legend=c("Kernel Density Estimate", "Fitted Normal Density"),
       col=c("black", "red"), lty=1:1, cex=0.8)
normfit = fitdistr(fitRes, densfun="normal")
fitMean = as.numeric(normfit$estimate[1])
fitSd = as.numeric(normfit$estimate[2])
xval = seq(-12, 12, length=10000)
yval = dnorm(xval, mean=fitMean, sd = fitSd)
lines(xval, yval, col="red", lwd=1.5)

#Normal QQ plot
qqnorm(fitRes)
qqline(fitRes, col = "steelblue", lwd=2)

#Shapiro-wilk test of normality
shapiro.test(prices)

#FittinG ARIMA using AIC
auto.arima(prices, max.p = 20, max.q = 0, d=0, ic="aic")
fitAR2 = arima(prices, order=c(2,0,0), optim.control=list(maxit = 1000))
print(fitAR2)
fitRes2 = residuals(fitAR2)
acf(fitRes2)
pacf(fitRes2)
Box.test(fitRes2, lag=10, type="Ljung-Box")
print(fitAR2)

#Fitting t-distrib
tFit2 = fitdistr(fitRes2, densfun="t", start =list(m=mean(fitRes2), s=sd(fitRes2), df=3), 
                 lower=c(-1,0.001,1))
print(tFit2)

#Evaluating fit
library(sgt)
tMean <- as.numeric(tFit$par[1])
tSd <- as.numeric(tFit$par[2])
tNu <- as.numeric(tFit$par[3])

#Extracting parameters
tMean2 <- as.numeric(tFit2$estimate[1])
tSd2 <- as.numeric(tFit2$estimate[2])
tNu2 <- as.numeric(tFit2$estimate[3])

#Density Estimate
d1 <- density(fitRes2, adjust=1)
plot(d1, lwd=1.5, col="black", ylim=c(0,0.65), main="Density Estimate")
legend(-12, 0.6, legend=c("Kernel Density Estimate", "Fitted t-Distribution Density"),
       col=c("black", "red"), lty=1:1, cex=0.8)
xval1 = seq(-10, 10, length=10000)
yval1 = dsgt(xval1, mu=tMean, sigma=tSd, lambda=0, p=2, q=tNu/2)
lines(xval1, yval1, col="red", lwd=1.5)

##QQPLOT
scaledQuan = qsgt(pppoints(1006))
qqplot(scaledQuan, fitRes2, xlab="Theoretical Quantiles", ylab="Residuals", main="QQ Plot of residuals against theoretical t-distribution quantiles")
qqline(scaledQuan, fitRes2, col="steelblue", lwd=1.5)
polyroot(c(1, -0.9963, 0.0062))

##Model-based resampling
#Forecasting 30 days based on fitted model
predictions = predict(fitAR2, n.ahead=30) 

#Plotting the last 150 days of the time series
lastDates = dates[c(1:150)] 
lastPrices = prices[c(1:150)]
plot(lastDates, lastPrices, type='l', xlim=as.Date(c("2018-05-01","2019-02-01")))

#Plotting predictions and confidence intervals
dateseq = seq(as.Date("2019/01/01"), by = "day", length.out = 30)
addDates = c(lastDates, dateseq)
lines(dateseq, predictions$pred, col="red")
lines(dateseq, predictions$pred + 1.96*predictions$se, col="blue")
lines(dateseq, predictions$pred - 1.96*predictions$se, col="blue")

#logreturns
#Calculating log returns
returns = -diff(prices)/prices[-length(prices)]
logReturns = log(1+returns)  

par(mfrow=c(1,2))
acf(logReturns, main="ACF plot of log returns")
pacf(logReturns, main="PACF plot of log returns")

#Time series plot of log returns
plot(dates[-c(1)], logReturns, type="l", main="Log return of IBM stock price", xlab="Date", ylab="Log Return")

#KDE of the log returns
d2 <- density(logReturns, adjust=1)
plot(d2, lwd=1.5, col="black", main="Density Estimate")

#Fitting a t-distribution to log returns
tFit3 = stdFit(logReturns)
fitMean3 = tFit3$par[1]
fitSd3 = tFit3$par[2]
fitNu3 = tFit3$par[3]

#Comparing fitted t-distribution with KDE
xval3 = seq(-0.05, 0.05, length=10000)
yval3 = dsgt(xval3, mu=fitMean3, sigma=fitSd3, lambda=0, p=2, q=fitNu3/2)
lines(xval3, yval3, col="red", lwd=1.5)
legend(-0.08, 30, legend=c("Kernel Density Estimate", "Fitted t-distrbution Density"),
col=c("black", "red"), lty=1:1, cex=0.8)