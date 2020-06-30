library("MASS")
library("fGarch")
fileRead <- read.csv(file = "BTC.csv")
btcData <- fileRead[-c(1022:2365),]

prices <- as.numeric(gsub(",","", btcData$Close.USD))
priceMean <- mean(prices)
priceSd <- sd(prices)

sqrt.Price  <- sqrt(prices)
sqrt.PriceMean <- mean(sqrt.Price)
sqrt.PriceSd <- sd(sqrt.Price)

log.Price <- log(prices)
log.PriceMean <- mean(log.Price)
log.PriceSd <- sd(log.Price)


#Untransoformed Data
#QQ plot
qqnorm(prices, main="Normal QQ plot of Untransformed Data")
qqline(prices, col = "steelblue", lwd=2)

#Boxplot
boxplot(prices, main = "Boxplot of BTC Closing Prices", ylab = "USD", xlab = "BTC")

#KDE
d <- density(prices, adjust=1.5)
plot(d, main="KDE of BTC Price", ylim=c(0,0.00012), lwd = 2)
xval = seq(-1500, 20000, length=10000)
yval = dnorm(xval, mean=priceMean, sd=priceSd)
lines(xval, yval, col="red", lwd = 0.5)

#Sqrt-transformed data
#QQ plot
qqnorm(sqrt.Price, main="Normal QQ Plot of Square Root Transformed Data")
qqline(sqrt.Price, col = "steelblue", lwd=2)

#Boxplot
boxplot(sqrt.Price, main = "Boxplot of Square Root Transformed Data", ylab = "sqrt(USD)", xlab = "BTC")

#KDE
d1 <- density(sqrt.Price, adjust=1.6)
plot(d1, main="KDE of sqrt(BTC) Price", ylim=c(0,0.02), lwd=2)
xval1 = seq(0, 150, length=10000)
yval1 = dnorm(xval1, mean=sqrt.PriceMean, sd=sqrt.PriceSd)
lines(xval1, yval1, col="red", lwd=0.5)


#Log-transformed data
#QQ plot
qqnorm(log.Price, main="Normal QQ plot of log-transformed data")
qqline(log.Price, col = "steelblue", lwd=2)

#Boxplot
boxplot(log.Price, main = "Boxplot of log-transformed BTC prices", ylab = "log(USD)", xlab = "BTC")

#KDE
d2 <- density(log.Price, adjust=1.4)
plot(d2, main="KDE of log(BTC) Price", lwd=2)
xval2 = seq(6, 11, length=10000)
yval2 = dnorm(xval2, mean=log.PriceMean, sd=log.PriceSd)
lines(xval2, yval2, col="red", lwd=0.5)


#Boxcox
bcPrice <- boxcox(prices~1, lambda=seq(0,1,1/100))
obj <- bcPrice$y
maxObj <- max(obj)
estimate <- match(maxObj, obj)/100

#CI
range(bcPrice$x[bcPrice$y > max(bcPrice$y)-qchisq(0.99,1)/2])


#Fit skew-t-distrib
skewFit <- sstdFit(prices)

##Problem 2

library("fGarch")

fileRead <- read.csv(file="LBMA-GOLD.csv")
goldData <- fileRead[-c(709:13093),]
price <- as.numeric(goldData$USD..PM.)
dates <- as.Date(goldData$Date)
dates <- dates[-c(1, 204,207,457,460)]
price <- price[-c(204,207,457,460)]

#Log returns
returns = -diff(price)/price[-length(price)]
logReturns = log(1+returns)  
plot(dates, logReturns, main="Log Returns of Gold Pric")

lreturnMean = mean(logReturns)
lreturnSd = sd(logReturns)

#QQ plot
qqnorm(logReturns)
qqline(logReturns, col = "steelblue", lwd=2)

#Boxplot
boxplot(logReturns, main="Boxplot of Gold Price", ylab="Log Returns", xlab="Gold")

#KDE
d <- density(logReturns, adjust=1.2)
plot(d, main="KDE of Gold Price", lwd=2)
xval = seq(-0.0225, 0.29, length=10000)
yval = dnorm(xval, mean=lreturnMean, sd=lreturnSd)
lines(xval, yval, col="red", lwd=0.5) 

#Fitting T-Distribution for Real
Y=logReturns
loglike_std = function(x) {
  f = -sum(dstd(Y, x[1], x[2], x[3], log=TRUE))
  f}
start = c(mean(Y), sd(Y), 4)
fit_std = optim(start, loglike_std, method = "L-BFGS-B",
                lower = c(-0.1, 0.001, 2.1),
                upper = c(0.1, 1, 20), hessian = TRUE)
cat("MLE =", round(fit_std$par, digits = 5))
minus_logL_std = fit_std$value
AIC_std = 2*minus_logL_std+2*length(fit_std$par)
BIC_std = 2*minus_logL_std+log(length(logReturns))*length(fit_std$par)


#Fitting Skew-T distribution for Real
Y=logReturns
loglike_sstd = function(x) {
  f = -sum(dsstd(Y, x[1], x[2], x[3], x[4], log=TRUE))
  f}
start = c(mean(Y), sd(Y), 4, 1.5)
fit_sstd = optim(start, loglike_sstd, method = "L-BFGS-B",
                 lower = c(-0.1, 0.001, 2.1, -0.99),
                 upper = c(0.1, 1, 20, 0.99), hessian = TRUE)
cat("MLE =", round(fit_sstd$par, digits = 5))
minus_logL_sstd = fit_sstd$value
AIC_sstd = 2*minus_logL_sstd+2*length(fit_sstd$par)
BIC_sstd = 2*minus_logL_sstd+log(length(logReturns))*length(fit_sstd$par)
\end{lstlisting}

\subsection{Problem 3 entire code}
\begin{lstlisting}[language=R]
library(fGarch)

fileRead <- read.csv("IBM.csv")
IBMdata <- fileRead[-c(705:1258),]
price <- IBMdata$Adj.Close

###1
priceMean <- mean(price)
ppriceSd <- sd(price)
skew <- skewness(price)
kurt <- kurtosis(price)


###2
tFit = stdFit(price)
tFit$par

###3
##Model-free
avs <- c()
for (ct in 1:1000){
  rsample <- sample(price, 704, replace=T)
  avs <- c(avs, mean(rsample))
}
mfMean <- mean(avs)

##t-distribution model-based
tavgs <- c()
for (ct1 in 1:1000){
  tsample <- rt(704, df=2.631856)*13.542733+138.311938
  tavgs <- c(tavgs, mean(tsample))
}
mbMean <- mean(tavgs)


###4

#model-free
qqnorm(avs, main="Normal QQ Plot of Model-Free Means")
qqline(avs, col = "steelblue", lwd =1)
#model-free
d <- density(avs, adjust=1)
plot(d, main="KDE of Model-Free Sample Means", lwd=2)
xval = seq(135, 150, length=10000)
yval = dnorm(xval, mean=mean(avs), sd=sd(avs))
lines(xval, yval, col="red", lwd=0.5)

#model-based
qqnorm(tavgs, main="Normal QQ Plot of Model-Based Means")
qqline(tavgs, col="steelblue", lwd=1)
#model-based
d1 <- density(tavgs, adjust=1)
plot(d1, main="KDE of t-distribution based sample means", ylim=c(0, 0.5), xlim=c(135,142), lwd=2)
xval1 = seq(-135, 150, length=10000)
yval1 = dnorm(xval1, mean=mean(tavgs), sd=sd(tavgs))
lines(xval1, yval1, col="red", lwd=0.5)

#Boxplots
boxplot(avs, tavgs, main="Boxplot of Model-Free and Model-Based Means", ylab="Price", names=c("Model-free", "Model- based"))


###5
#Model-free
cimf_low <- mfMean - sd(avs)*qnorm(0.95, mean=0, sd=1)
cimf_up <- mfMean + sd(avs)*qnorm(0.95, mean=0, sd=1)

#Model-based
cimb_low <- mbMean - sd(tavgs)*qnorm(0.95, mean=0, sd=1)
cimb_up <- mbMean + sd(tavgs)*qnorm(0.95, mean=0, sd=1)


###6
mfBias <- mfMean - priceMean
mbBias <- mbMean - priceMean

###7
mfMSE <- sd(avs)^2+mfBias^2
mbMSE <- sd(tavgs)^2+mbBias^2
