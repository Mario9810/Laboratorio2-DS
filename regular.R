install.packages("devtools")
devtools::install_github("facebook/prophet", subdir='R')


library(xts)
library(tseries)
library(forecast)
library(ggfortify)
library(lmtest)
library(fUnitRoots)

regularC <- read.csv("seriec.csv")
regularI <- read.csv("seriei.csv")
regularI
regularC

#Regular consumo
regC.ts<-ts(regularC$GasolinaRegular,start = c(2001,1),frequency = 12)
#grafico 
plot(regC.ts)#La grafica demuestra que no hay estacionariedad 

#inicio
start(regC.ts)
#fin
end(regC.ts)
#frequencia
frequency(regC.ts)
#se transforma la serie para que sea estacionaria 
logRegularc<- log(regC.ts)
plot(logRegularc)
acf(regC.ts)
acf(logRegularc) #autocorrelacion
acf(diff(logRegularc),12)#autocorrelacion corregida
pacf(diff(logRegularc))

adf.test(regC.ts) #test ADF

auto.arima(regC.ts)#obtecion del modelo

fit <- arima(log(regC.ts), c(2, 1, 1),seasonal = list(order = c(2, 0, 0), period = 12)) #modleo arima con valores dados por la funcion auto.arima()
pred <- predict(fit, n.ahead = 10*12)
ts.plot(regC.ts,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(regC.ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit, level = c(95), h = 5)
forecastAD <- forecast(fit2, level = c(95), h = 5)

#analisis residuales
checkresiduals(fit)
checkresiduals(fit2)

coeftest(fit)

qqline(fit$residuals)
checkresiduals(fit)

autoplot(forecastAP)
autoplot(forecastAD)
#--------------------------------
#regular importacion 

regI.ts<-ts(regularI$GasolinaRegular,start = c(2001,1),frequency = 12)
#grafico
plot(regI.ts)#La grafica demuestra que no hay estacionariedad


#inicio
start(regI.ts)
#fin
end(regC.ts)
#frecuencia
frequency(regI.ts)

logRegulari<- log(regI.ts)
plot(logRegulari)

acf(logRegulari) #autocorrelacion
acf(diff(logRegularc),12)#autocorrelacion corregida
pacf(diff(logRegularc))

adf.test(regI.ts) #test ADF

auto.arima(regI.ts)#obtecion del modelo

fiti <- arima(log(regI.ts), c(1, 1, 2),seasonal = list(order = c(2, 0, 0), period = 12)) #modleo arima con valores dados por la funcion auto.arima()
pred <- predict(fit, n.ahead = 10*12)
ts.plot(regI.ts,2.718^pred$pred, log = "y", lty = c(1,3))

fit2i <- arima(log(regI.ts),seasonal = list(order = c(0, 1, 0), period = 12))

#revision de residuales
checkresiduals(fiti)
checkresiduals(fit2i)

coeftest(fiti)

qqline(fit$residuals)
checkresiduals(fiti)

autoplot(forecastAP)





forecastAP <- forecast(fit, level = c(95), h = 36)
forecastAD <- forecast(fit2, level = c(95), h = 36)# a 3 años

forecastAP <- forecast(fit, level = c(95), h = 6)# a 6 meses 
forecastAD <- forecast(fit2, level = c(95), h = 6)

autoplot(forecastAP)
autoplot(forecastAD) #fue mejor el primer modelo modelo
