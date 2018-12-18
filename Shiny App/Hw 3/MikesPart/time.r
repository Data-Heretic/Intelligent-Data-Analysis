require(xlsx)
library("forecast")
library("fpp")
library("fpp2")
library("gridExtra")
library("ggplot2")
library(plotly)


time<-read.xlsx("data_g10.xlsx",sheetName = "6.41 r")


# 1.PLOT AND COMMENT

freq<-100

time.ts <- ts(time[,2],frequency = freq)
p <- plot_ly(x = time$Fecha, y = time$Tipo, mode = 'lines')
#trying logarithmic transformation,needed for number 2
logp<-plot_ly(x = time$Fecha, y = log(time$Tipo), mode = 'lines')
p
logp
#seasonality
ggmonthplot(time.ts)
ggseasonplot(time.ts)
# Exploring correlations of lagged observations
gglagplot(time.ts, lag=9, do.lines=FALSE)
ggAcf(time.ts)


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
timdec1<-decompose(time.ts)
pd1<-plot(timdec1)
#spec.pgram(time.ts) 
# 2.stl with t.window
timdec2<-stl(time.ts,freq,t.window=15, robust=TRUE)#t.window controls wiggliness of trend component. 
pd2<-plot(timdec2)
#forecast(timdec2) 
# 3. stl with s.window
timdec3<-stl(time.ts,freq,s.window="periodic", robust=TRUE) #we keep the seasonal component identical across years
pd3<-plot(timdec3)
#forecast(timdec3)

#forecasting correctly,we can select which dicomposition we like.Method:ETS
fcst=forecast(timdec2, method="ets", h=24)
plot(fcst)
fcst$mean
#forecasting correctly,we can select which dicomposition we like.Method:Arima
fcst=forecast(timdec3, method="arima", h=24)
plot(fcst)
fcst$mean
# we can check the residuals
checkresiduals(timdec1$random)

#lets check the differences,i dont know which plot represents our data better,so i put the ones i found
adjusted_diffts <- time.ts - timdec1$seasonal

acf(adjusted_diffts)
acf(adjusted_diffts, lag.max = 12, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.contiguous, demean = TRUE)

pacf(adjusted_diffts)
Pacf(adjusted_diffts, lag.max = 12, plot = TRUE, na.action = na.contiguous, demean = TRUE)
