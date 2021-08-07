library(dplyr)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)


#Lectura de DataFrame de consumo
consumo<-read.csv("C:/Users/DiegoAlegria/CABI/Universidad Provisional/Data Science/Lab 2/DatosConsumoCombustibles.csv")

consumo<-cbind(consumo['Anio'],
      consumo['Mes'],
      consumo['GasolinaSuper'],
      consumo['GasolinaSuper'],
      consumo['GasolinaRegular'],
      consumo['Diesel'],
      consumo['DieselLS'])

consumo$Diesel[is.na(consumo$Diesel)]<-consumo$DieselLS[is.na(consumo$Diesel)]



#Lectura de DataFrame de importación
imp<-read.csv("C:/Users/DiegoAlegria/CABI/Universidad Provisional/Data Science/Lab 2/DatosImportacionCombustibles.csv")

imp<-cbind(imp['Anio'],
               imp['Mes'],
               imp['GasolinaSuper'],
               imp['GasolinaSuper'],
               imp['GasolinaRegular'],
               imp['Diesel'],
               imp['DieselLS'])

imp$Diesel[is.na(imp$Diesel)]<-imp$DieselLS[is.na(imp$Diesel)]
 

# Agregar columna con datos de fecha

library(zoo)

consumo$Date<-as.yearmon(paste(consumo$Anio,consumo$Mes),"%Y %m")
imp$Date<-as.yearmon(paste(imp$Anio,imp$Mes),"%Y %m")


# Convertir a TS

consumoTS<-ts(consumo$GasolinaSuper,start=c(2000,01),end=c(2021,05),12)
impTS<-ts(imp$GasolinaSuper,start=c(2000,01),end=c(2021,05),12)

#Inicio
start(consumoTS)
start(impTS)

#Final de los datos

end(consumoTS)
end(impTS)

#Frecuencia
frequency(consumoTS)
frequency(impTS)

#Gráficos Exploratorios
plot(consumoTS)
abline(reg=lm(consumoTS~time(consumoTS)), col=c("red"))


plot(impTS)
abline(reg=lm(impTS~time(impTS)), col=c("red"))


#Gráficos de descomposición

descConsumo<-decompose(consumoTS)
descImp<-decompose(impTS)

plot(descConsumo)

plot(descImp)


# Train Test Split

trainConsumo<-head(consumoTS,round(length(consumoTS)*0.7))
trainImp<-head(impTS,round(length(impTS)*0.7))


testConsumo<-tail(consumoTS,round(length(consumoTS)*0.3))
testImp<-tail(impTS,round(length(impTS)*0.3))

# Diferencia entre conjuntos

h_consumo<-length(trainConsumo)-length(testConsumo)
h_imp<-length(trainImp)-length(testImp)


#Transformación Logarítmica por no poseer estacionalidad en media
logconsumo<-log(trainConsumo)
logImp<-log(trainImp)

plot(decompose(logconsumo))
plot(decompose(logImp))

# Dickey Fuller Test
adfTest(trainConsumo)
adfTest(trainImp)

# Prueba de Raíces Unitarias
unitrootTest(trainConsumo)
unitrootTest(trainImp)

# Parámetros p y q

acf(logconsumo,100,na.action = na.pass)
acf(logImp,100,na.action=na.pass)

pacf(logconsumo,100,na.action = na.pass)
pacf(logImp,100,na.action=na.pass)

Acf(diff(logconsumo), 36)
Pacf(diff(logconsumo), 36)

Acf(diff(logImp), 36)
Pacf(diff(logImp), 36)


# Arima
Arima_Consumo<-arima(log(trainConsumo),order=c(2,1,3),seasonal = c(1,1,0))
Arima_Consumo2<-arima(log(trainConsumo),order=c(2,1,3),seasonal = c(0,1,1))


