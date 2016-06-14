library(forecast)
library(xts)
library(tseries)
library(TSPred)

#monthly data interurban roads from 1995 to 2013

Spainmonthlyinter <- read.csv("monthlydata_interurban_1995.csv", sep= ";")

SpainmonthlylFatal95inter <- ts(Spainmonthlyinter$fatalities, start=c(1995,1), end= c(2013,12), frequency=12)

summary(SpainmonthlylFatal95inter)

logSpainmonthlylFatal95inter <- log(SpainmonthlylFatal95inter)
plot(logSpainmonthlylFatal95inter)
plot(SpainmonthlylFatal95inter)

logSpainmonthlylFatal95inter
SpainmonthlylFatal95inter

fit95inter <- decompose(SpainmonthlylFatal95inter, type="additive")
plot(fit95inter, xlim= c(1995,2013))
fit95inter

graficointerroads <- ts(SpainmonthlylFatal95inter, frequency = 12, start = 1995)
plot(as.xts(graficointerroads), major.format = "%Y-%m", ylab= "Fatalities (inter-urban roads)")

plot(SpainmonthlylFatal95inter, xlim= c(1995,2013), ylim= c(50,500), type="l", xlab="Years", ylab= "Fatalities") 

plot(SpainmonthlylFatal95inter, ylab="Fatalities (inter-urban roads)", xlab="Year")

#he utilizado la funcion auto.arima para ver el mejor modelo 

arima1=Arima(logSpainmonthlylFatal95inter,order=c(0,1,1), seasonal=c(1,1,2), include.drift=TRUE)
summary(arima1) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso sar1 y sma2 NO son significativos
confint(arima1) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima1)
plot(forecast(arima1, h=84))

arima1.forecast=forecast.Arima(arima1, h=84)
arima1.forecast
plot(arima1.forecast, ylab= "Fatalities (inter-urban roads)")

fcinter<-forecast(arima1,h=84)
fcinter$mean<-exp(fcinter$mean)
fcinter$upper<-exp(fcinter$upper)
fcinter$lower<-exp(fcinter$lower)
fcinter$x<-exp(fcinter$x)
plot(fcinter, xlim= c(1995, 2020), ylab= "Fatalities (inter-urban roads)")

fitinter <- Arima(SpainmonthlylFatal95inter, order=c(0,1,1), seasonal=c(1,1,2))
plot(fitinter$x,col="black", xlim= c(1995, 2013), ylab= "Fatalities (inter-urban roads)")
lines(fitted(fitinter),col="red")


arima3=Arima(logSpainmonthlylFatal95inter,order=c(2,1,3), seasonal=c(1,1,2), include.drift=TRUE)
summary(arima3) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso sar1, sma1 y sma2 NO son significativos
confint(arima3) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima3)
plot(forecast(arima3, h=84))
arima3.forecast=forecast.Arima(arima3, h=84)
arima3.forecast

arima4=Arima(logSpainmonthlylFatal95inter,order=c(0,1,1), seasonal=c(0,1,1))
summary(arima4) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso todos son significativos
confint(arima4) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima4)
plot(forecast(arima4, h=84))
arima4.forecast=forecast.Arima(arima4, h=84)
arima4.forecast


#hay tres buenos modelos (arima1, arima 3 and arima4): se ha elegido el arima 1 basado en el forecasting de enero 2014 a marzo 2016 


#residuals diagnostics,  residual is the gap between the fitted and actual data
plot.ts(arima1$residuals)
Box.test(arima1$residuals, lag=20, type="Ljung-Box")
#The Ljung-Box statistic provides an indication of whether the model is correctly specified, in the sense it allows testing the global nullity of the autocorrelation of the residuals. In our case, the hypothesis is accepted because the 0,467 value of the Ljung-Box statistics is more than 0.05. Therefore, there is no reason to reject the model (no hay autocorrelacion)
acf(arima1$residuals, lag.max=24, main="ACF of the Model") #we dont have autocorrelation problems
jarque.bera.test(arima1$residuals) # no sale con problema de normalidad porque el p-value es mas grande que 0.05

######################################################################################

#training and testing dataset

data.train = window(logSpainmonthlylFatal95inter, start=c(1995,1), end=c(2010,12))
plot(data.train)
data.test = window(logSpainmonthlylFatal95inter, start=c(2011,1), end=c(2013,12))
plot(data.test)


arima1=Arima(data.train,order=c(0,1,1), seasonal=c(1,1,2))
summary(arima1) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso todos son significativos
confint(arima1) #cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"
forecast(arima1)
plot(forecast(arima1, h=36))

arima1_=auto.arima(data.train, trace=TRUE, test="kpss", ic="bic")#resultado Best model: ARIMA(2,1,0) (2,0,0)  
arima1__=auto.arima(data.train, trace=TRUE, test=c("kpss","adf","pp"), ic=c("aicc", "aic", "bic")) #resultado Best model: ARIMA(2,1,0)(2,0,0)[12]  
summary(arima1_) #para que sea significativo el valor del coeficiente dividido por el standard error tiene q sera > 3, en este caso todos son significativos
confint(arima1_) #"cuando sale de negativo (en la primera columna) a positivo (en la segunda) o vice-cersa NO es significativo"

arima1.forecast=forecast.Arima(arima1, h=36)
arima1.forecast
plot(arima1.forecast, ylab= "Fatalities (inter-urban roads)")

plotarimapred(data.test, arima1, xlim=c(2011, 2013), range.percent = 0.05)
accuracy(arima1.forecast, data.test)
