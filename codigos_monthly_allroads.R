library(forecast)
library(xts)
library(tseries)

#monthly data allroads roads from 1995 to 2013

Spainmonthly95 <- read.csv("monthlydata_allroads_1995.csv", sep= ";")

SpainmonthlylFatal95 <- ts(Spainmonthly95$fatalities, start=c(1995,1), end= c(2013,12), frequency=12)

summary(SpainmonthlylFatal95)

logSpainmonthlylFatal95 <- log(SpainmonthlylFatal95)
plot(logSpainmonthlylFatal95)
plot(SpainmonthlylFatal95)

logSpainmonthlylFatal95
SpainmonthlylFatal95
dim(as.matrix(logSpainmonthlylFatal95))

fit95 <- decompose(SpainmonthlylFatal95, type="additive")
plot(fit95)

graficoallroads <- ts(SpainmonthlylFatal95, frequency = 12, start = 1995)
plot(as.xts(graficoallroads), major.format = "%Y-%m", ylab= "Fatalities (urban + inter-urban roads)")

#he utilizado la funcion auto.arima para ver el mejor modelo 

arima2=Arima(logSpainmonthlylFatal95,order=c(2,1,3), seasonal=c(2,2,1), include.drift=TRUE)
summary(arima2) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso solo ar1 No es significativo
confint(arima2) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima2)
plot(forecast(arima2, h=84))

arima5=Arima(logSpainmonthlylFatal95,order=c(0,1,1), seasonal=c(0,1,1))
summary(arima5) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso todos son significativos
confint(arima5) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima5)

#dos modelos: arima2 and arima5, se ha elegido arima5 porque presenta mayor log likelihood y menor AIC y BIC 
arima5.forecast=forecast.Arima(arima5, h=84)
arima5.forecast
plot(arima5.forecast, ylab= "Fatalities (urban + inter-urban roads)")


fc<-forecast(arima5,h=84)
fc$mean<-exp(fc$mean)
fc$upper<-exp(fc$upper)
fc$lower<-exp(fc$lower)
fc$x<-exp(fc$x)
plot(fc, xlim= c(1995, 2020), ylab= "Fatalities (urban + inter-urban roads)")

fit <- Arima(logSpainmonthlylFatal95, order=c(0,1,1), seasonal=c(0,1,1))
plot(fit$x,col="black",  xlim= c(1995, 2013))
lines(fitted(fit),col="red")


fit <- Arima(SpainmonthlylFatal95, order=c(0,1,1), seasonal=c(0,1,1))
plot(fit$x,col="black",  xlim= c(1995, 2013), ylab= "Fatalities (urban + inter-urban roads)")
lines(fitted(fit),col="red")

#residuals diagnostics,  residual is the gap between the fitted and actual data
plot.ts(arima5$residuals)
Box.test(arima5$residuals, lag=20, type="Ljung-Box")
#The Ljung-Box statistic provides an indication of whether the model is correctly specified, in the sense it allows testing the global nullity of the autocorrelation of the residuals. In our case, the hypothesis is accepted because the 0,467 value of the Ljung-Box statistics is more than 0.05. Therefore, there is no reason to reject the model (no hay autocorrelacion)
acf(arima5$residuals, lag.max=24, main="ACF of the Model") #we dont have autocorrelation problems
jarque.bera.test(arima5$residuals) # no sale con problema de normalidad porque el p-value es mas grande que 0.05

