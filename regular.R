install.packages("httpuv")
library(xts)
library(tseries)
library(forecast)
library(ggfortify)
library(prophet)

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

forecastAP <- forecast(fit, level = c(95), h = 120)
forecastAD <- forecast(fit2, level = c(95), h = 120)

autoplot(forecastAP)
autoplot(forecastAD) #fue mejor el segundo modelo
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
