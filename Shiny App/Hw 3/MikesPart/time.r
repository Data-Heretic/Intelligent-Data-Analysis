require(xlsx)
library("forecast")
library("fpp")
library("fpp2")
library("gridExtra")
library("ggplot2")
library(plotly)


time <- read.xlsx("../data_g10.xlsx",sheetName = "6.41 r")


# 1.PLOT AND COMMENT

freq <- 12

time.ts <- ts(time[,2], start = 1995, frequency = freq)
p <- plot_ly(x = time$Fecha, y = time$Tipo, mode = 'lines')
p
#trying logarithmic transformation,needed for number 2
logp <- plot_ly(x = time$Fecha, y = log(time$Tipo), mode = 'lines')
logp
# trying Box Cox transformation
lambda <- BoxCox.lambda(usgdp)
bc <- plot_ly(x = time$Fecha, y = BoxCox(time$Tipo, lambda), mode = 'lines')
bc
#seasonality
ggmonthplot(log(time.ts))
ggseasonplot(log(time.ts))
acf2(log(time.ts))
# Exploring correlations of lagged observations
gglagplot(time.ts, lag=9, do.lines=FALSE)
ggAcf(time.ts)

# Stationary Tests
adf.test(diff(log(time$Tipo)))


##   Comments:
# Non stationary
# Trend: the measures tend to decrease over time = downward trend
# Seasonality : we cannot see any seasonality on the measures,they have no pattern on highs and lows
# Cyclicality : we can see that there might be a cyclicality on the measures as there are highs that apear over time
# needed for the next step: logarithmic transformation makes no difference so we wont need Multiplicative decomposition


# 2. More stuff
tsdisplay(time.ts, plot.type="partial")
tsdisplay(time.ts, plot.type="scatter")
tsdisplay(time.ts, plot.type="spectrum")
#log,we can see that the changes are really small.
tsdisplay(log(time.ts), plot.type="scatter")

#We use additive decomposition then 1.decompose()
timdec1 <- decompose(log(time.ts))
pd1<-plot(timdec1)
#spec.pgram(time.ts) 
# 2.stl with t.window
timdec2 <- stl(log(time.ts), s.window = "periodic", t.window = 15, robust=TRUE)#t.window controls wiggliness of trend component. 
pd2 <- plot(timdec2)
acf2(timdec2$time.series[,3])
#forecast(timdec2) 

#forecasting correctly,we can select which dicomposition we like.Method:ETS
fcst=forecast(timdec2, method="ets", h=24)
plot(fcst)
fcst$mean
#forecasting correctly,we can select which dicomposition we like.Method:Arima
fcst=forecast(timdec2, method="arima", h=24)
plot(fcst)
fcst$mean
#forecasting correctly,we can select which dicomposition we like.Method:naive
fcst=forecast(timdec2, method="naive", h=24)
plot(fcst)
fcst$mean
#forecasting correctly,we can select which dicomposition we like.Method:rwdrift
fcst=forecast(timdec2, method="rwdrift", h=24)
plot(fcst)
fcst$mean
# we can check the residuals
checkresiduals(timdec2$random)

#lets check the differences,i dont know which plot represents our data better,so i put the ones i found
adjusted_diffts <- time.ts - timdec1$seasonal

acf(adjusted_diffts)
acf(adjusted_diffts, lag.max = 12, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)

pacf(adjusted_diffts)
Pacf(adjusted_diffts, lag.max = 12, plot = TRUE, na.action = na.contiguous, demean = TRUE)

sd(log(time.ts))
sd(diff(log(time.ts)))
acf2(diff(log(time.ts)))
sd(diff(diff(log(time.ts))))
acf2(diff(diff(log(time.ts))))

ndiffs(log(time.ts))
nsdiffs(log(time.ts))


model.1 <- Arima(log(time.ts),order=c(1,2,0), seasonal=list(order=c(0,0,0), period=12))
model.2 <- Arima(log(time.ts),order=c(0,2,1), seasonal=list(order=c(0,0,0), period=12))
model.3 <- Arima(log(time.ts),order=c(1,2,1), seasonal=list(order=c(0,0,0), period=12))
model.4 <- Arima(log(time.ts),order=c(2,2,0), seasonal=list(order=c(0,0,0), period=12))
model.5 <- Arima(log(time.ts),order=c(0,2,2), seasonal=list(order=c(0,0,0), period=12))
model.6 <- Arima(log(time.ts),order=c(2,2,1), seasonal=list(order=c(0,0,0), period=12))
model.7 <- Arima(log(time.ts),order=c(1,2,2), seasonal=list(order=c(0,0,0), period=12))
model.8 <- Arima(log(time.ts),order=c(2,2,2), seasonal=list(order=c(0,0,0), period=12))

model.1$aic
model.2$aic
model.3$aic
model.4$aic
model.5$aic
model.6$aic
model.7$aic
model.8$aic

#check correlations between model coefficients
cov2cor(model.1$var.coef)
cov2cor(model.2$var.coef)
cov2cor(model.3$var.coef)
cov2cor(model.4$var.coef)
cov2cor(model.5$var.coef)
cov2cor(model.6$var.coef)
cov2cor(model.7$var.coef)
cov2cor(model.8$var.coef)

#Based on the AICc, the winner is model.3. Check assumptions.
plot(model.3$residuals)
t.test(model.3$residuals)
Box.test(model.3$residuals, lag=20, fitdf=5, type="L")
jarque.bera.test(model.3$residuals)
which.max(model.3$residuals)
jarque.bera.test(model.3$residuals[-c(169,193)])


# You see in these plots why model.3 is better
train=window(log(time.ts),end=2013-0.01)
test=window(log(time.ts),start=2013)
model.2 <- Arima(train,order=c(2,1,0),seasonal=list(order=c(3,1,0), period=12))
fc2 = forecast(model.2,h=12)
accuracy(fc2,test)


#Lets find the values of (p,d,q);(P,D,Q) madafaka
#----METHOD 1----- USING RMSE---
#I used the RMSE to find out the best parameters for ARIMA
getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]   #train data end
  test.start <- time(x)[length(x)-h+1]  #test data start
  train <- window(x,end=train.end) #extract train data
  test <- window(x,start=test.start)  #extract test data
  fit <- Arima(train,...) # fit model with train data
  fc <- forecast(fit,h=h) # forecast with model
  return(accuracy(fc,test)[2,"RMSE"]) #compare forecast with test data, extract the rmse
}
#We already know d=1 and D=1
getrmse(log(time.ts),h=12,order=c(1,2,0),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(0,2,1),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(1,2,1),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(2,2,0),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(0,2,2),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(2,2,1),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(1,2,2),seasonal=c(0,0,0))
getrmse(log(time.ts),h=12,order=c(2,2,2),seasonal=c(0,0,0))

# automatic model
ggb.auto = auto.arima(log(time.ts), d=2, D=0, max.p=3, max.q=3, max.P=3, max.Q=3)
ggb.auto

