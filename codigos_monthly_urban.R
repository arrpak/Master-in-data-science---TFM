library(forecast)
library(xts)
library(tseries)
library(TSPred)

#monthly data urban roads from 1995 to 2013

Spainmonthlyurban <- read.csv("monthlydata_urban_1995.csv", sep= ";")

SpainmonthlylFatal95urban <- ts(Spainmonthlyurban$fatalities, start=c(1995,1), end= c(2013,12), frequency=12)

summary(SpainmonthlylFatal95urban)

logSpainmonthlylFatal95urban <- log(SpainmonthlylFatal95urban)
plot(logSpainmonthlylFatal95urban)
plot(SpainmonthlylFatal95urban)

logSpainmonthlylFatal95urban
SpainmonthlylFatal95urban


fit95urban <- decompose(SpainmonthlylFatal95urban, type="additive")
plot(fit95urban)

graficourbanroads <- ts(SpainmonthlylFatal95urban, frequency = 12, start = 1995)
plot(as.xts(graficourbanroads), major.format = "%Y-%m", ylab= "Fatalities (urban roads)")

plot(SpainmonthlylFatal95urban, xlim= c(1995,2013), ylim= c(10,90), type="l", xlab="Years", ylab= "Fatalities") 

# fundion autoarima para ver cual es el mejor modelo 

arima6=auto.arima(logSpainmonthlylFatal95urban, trace=TRUE, test="kpss", ic="bic")#resultado Best model: ARIMA(2,1,2) (1,0,1) with drift 
arima7=auto.arima(logSpainmonthlylFatal95urban, trace=TRUE, test=c("kpss","adf","pp"), ic=c("aicc", "aic", "bic")) #resultado Best model: ARIMA(2,1,2)(1,0,1)[12] with drift 
summary(arima6) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso todos son significativos
summary(arima7) 
confint(arima6) #"cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
confint(arima7) # ar2, ma1, sar1, sma1 No son significativos

arima8=Arima(logSpainmonthlylFatal95urban,order=c(0,1,1), seasonal=c(1,1,2), include.drift=TRUE)
summary(arima8) # sar1, sma1 y sma2 NO son significativos
confint(arima8) 

arima9=Arima(logSpainmonthlylFatal95urban,order=c(2,1,2), seasonal=c(1,0,1), include.drift=TRUE) 
summary(arima9)
confint(arima9) #ar2, ma1, sar1, sma1 No son significativos

arima10=Arima(logSpainmonthlylFatal95urban,order=c(0,1,1), seasonal=c(0,1,1)) 
summary(arima10) #todos los parametros son significativos 
confint(arima10)

arima11=Arima(logSpainmonthlylFatal95urban,order=c(1,1,1), seasonal=c(1,0,1)) 
summary(arima11) # ar1 No significativo
confint(arima11)

forecast(arima10)
plot(forecast(arima10, h=84))

arima10.forecast=forecast.Arima(arima10, h=84)
arima10.forecast
plot(arima10.forecast, ylab= "Fatalities (urban roads)")

#el ARIMA(2,1,2) (1,0,1) with drift al final no es un buen modelo como nos decia el auto.arima, muchos coeficientes no son significativos 
#se ha elegido el ARIMA (0,1,1)(0,1,1) que es el arima10

fcurban<-forecast(arima10,h=84)
fcurban$mean<-exp(fcurban$mean)
fcurban$upper<-exp(fcurban$upper)
fcurban$lower<-exp(fcurban$lower)
fcurban$x<-exp(fcurban$x)
plot(fcurban, xlim= c(1995, 2020), ylab= "Fatalities (urban roads)")

fiturban <- Arima(SpainmonthlylFatal95urban, order=c(0,1,1), seasonal=c(0,1,1))
plot(fiturban$x,col="black", xlim= c(1995, 2013), ylab= "Fatalities (urban roads)")
lines(fitted(fiturban),col="red")


#residuals diagnostics,  residual is the gap between the fitted and actual data
plot.ts(arima10$residuals)
Box.test(arima10$residuals, lag=20, type="Ljung-Box")
#The Ljung-Box statistic provides an indication of whether the model is correctly specified, in the sense it allows testing the global nullity of the autocorrelation of the residuals. In our case, the hypothesis is accepted because the 0,221 value of the Ljung-Box statistics is more than 0.05. Therefore, there is no reason to reject the model (no hay autocorrelacion)
acf(arima10$residuals, lag.max=24, main="ACF of the Model") #we dont have autocorrelation problems
jarque.bera.test(arima10$residuals) # no sale con problema de normalidad porque el p-value es mas grande que 0.05
