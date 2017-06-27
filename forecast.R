library(rdatamarket)
library(zoo)
library(ggplot2)

# Instalando forecast desde el repositorio oficial en GitHub
install.packages("githubinstall")
library(githubinstall)
# Necesario instalar RTools (en caso de no tenerlo se instala por defecto)
githubinstall('forecast') # robjhyndman/forecast  forecast package for R

# Instalando desde CRAN
install.packages('forecast')

# Cargamos la librería
library(forecast)

# Cargamos los datos desde la API de datamarket (necesaria conexión a internet)
accidentes <- as.ts(dmseries('http://data.is/1yFXOBi'))
# Plotting
plot(accidentes)
# Training y test 
acc.train <- window(x = accidentes, start = c(1960,1), end = c(1973,12))
acc.test <- window(x = accidentes, start = c(1974,1))
# Plotting con ggplot del training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train)), y = coredata(as.zoo(acc.train))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test))), col = 'red')
p
# Función genérica para plotting de test vs pred a través de ggplot2
combine <- function(test, pred) {
  require(ggplot2)
  p <- ggplot() +
    geom_line(aes(x = index(as.zoo(test)), y = coredata(as.zoo(test)), colour = 'Test')) +
    geom_line(aes(x = index(as.zoo(test)), y = pred, colour = 'Prediccion')) +
    scale_color_manual(name = 'Leyenda', values = c('Test' = 'black', 'Prediccion' = 'red'),
                       labels = c('Test','Prediccion'))
  p
}
# Frecuencia estacional de nuestros datos
findfrequency(x = acc.train)
monthdays(x = acc.train)
seasonplot(x = acc.train, s = 12)
seasonplot(x = acc.train, s = findfrequency(acc.train))
# Descomposición aditiva de una serie temporal e independencia de los residuos (STATS PACKAGE)
acc.train.decomp.adi <- decompose(x = acc.train, type = 'additive')
seasonal(object = acc.train.decomp.adi)
trendcycle(object = acc.train.decomp.adi)
remainder(object = acc.train.decomp.adi)
plot(acc.train.decomp.adi)
Box.test(x = acc.train.decomp.adi$random, type = 'Ljung-Box') # Son ruido blanco
# Descomposición multplicativa de una serie temporal e independencia de los residuos (STATS PACKAGE)
acc.train.decomp.mult <- decompose(x = acc.train, type = 'multiplicative')
seasonal(object = acc.train.decomp.mult)
trendcycle(object = acc.train.decomp.mult)
remainder(object = acc.train.decomp.mult)
plot(acc.train.decomp.mult)
Box.test(x = acc.train.decomp.mult$random, type = 'Ljung-Box') # Son ruido blanco
# Desestacionalización a través de objectos decompose
acc.train.seasadj <- seasadj(acc.train.decomp.mult)
plot(acc.train.seasadj)
ndiffs(x = acc.train.seasadj, test = 'kpss', alpha = 0.05) # 1
ndiffs(x = acc.train.seasadj, test = 'adf') # 0
ndiffs(x = acc.train.seasadj, test = 'pp') # 0
# Visualizando la tendencia
tend <- ma(x = acc.train, order = 12)
autoplot(tend)


# Prediciendo nuevos valores con la media
model.1 <- meanf(y = acc.train, h = 12)
class(model.1) # forecast
autoplot(model.1)
model.1$mean[1] == mean(acc.train) # TRUE
combine(model.1$mean, acc.test)
# Medimos la precisión del modelo
accuracy(f = model.1, x = acc.test) # 36.28770
# Revisamos los residuos
Box.test(x = model.1$residuals, type = 'Ljung-Box')
checkresiduals(model.1) # No son ruido blanco
#  Modelo ingenuo en el que se predice con el valor anterior observado
model.2 <- naive(y = acc.train, h = 12)
class(model.2)
autoplot(model.2)
combine(model.2$mean, acc.test)
accuracy(f = model.2, x = acc.test) # 33.83333
checkresiduals(model.2)
#  Modelo ingenuo en el que se predice con el valor anterior observado con constante != 0
model.3 <- rwf(y = acc.train, h = 12, drift = TRUE)
autoplot(model.3)
combine(model.3$mean, acc.test)
accuracy(f = model.3, x = acc.test) # 33.27645
checkresiduals(model.3) # No son ruido blanco
# Modelo ingenuo que tiene en cuenta la estacionalidad
model.4 <- snaive(y = acc.train, h = 12)
autoplot(model.4)
combine(model.4$mean, acc.test)
accuracy(f = model.4, x = acc.test) # 22.58333
checkresiduals(model.4) # No son ruido blanco
# Plotting de todos los modelos ingenuos
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test)), colour = 'Test')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.1$mean, colour = 'Meanf')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.2$mean, colour = 'Naive')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.4$mean, colour = 'Snaive')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.3$mean, colour = 'Rwf')) +
  scale_color_manual(name = '', values = c('Test' = 'black',
                                           'Meanf' = 'red', 'Naive' = 'blue', 'Snaive' = 'green',
                                           'Rwf' = 'yellow'),
                     labels = c('Meanf','Naive', 'Rwf', 'Snaive', 'Test')) +
  ylab('Número de accidentes de tráfico') + xlab('Año 1974') +
  scale_x_continuous(breaks = c(), labels = c())
p


# Ajuste de un modelo lineal con tendencia y estacionalidad a los datos
model <- tslm(formula  = acc.train ~ trend + season)
class(model)
model
pred <- forecast(object = model, h = 12)
pred
class(pred)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(f = pred, x = acc.test) # 19.44597
checkresiduals(model) # No son ruido blanco


# Damped HoltWinters Aditivo
model.adit <- hw(y = acc.train, h = 12, damped = TRUE, seasonal = 'additive',
            initial = 'optimal') 
summary(model.adit)
autoplot(model.adit)
combine(model.adit$mean, acc.test)
accuracy(f= model.adit, x = acc.test) # 19.67722
checkresiduals(model.adit) # No son ruido blanco
# Damped HoltWinters Mutiplicativo
model.mult <- hw(y = acc.train, h = 12, damped = TRUE, seasonal = 'multiplicative',
            initial = 'optimal')
class(model.mult)
summary(model.mult)
autoplot(model.mult)
combine(model.mult$mean, acc.test)
accuracy(f = model.mult, x = acc.test) # 15.04762
checkresiduals(model.mult) # No son ruido blanco
# Plotting de los Holt-Winters
p.1 <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train)),
                y = coredata(as.zoo(acc.train)), colour = 'Serie original')) +
  geom_line(aes(x = index(as.zoo(fitted(model.adit))),
                y = coredata(as.zoo(fitted(model.adit))), colour = 'H-W Aditivo')) +
  geom_line(aes(x = index(as.zoo(fitted(model.mult))),
                y = coredata(as.zoo(fitted(model.mult))), colour = 'H-W Multiplicativo')) +
  scale_color_manual(name = '', values = c('Serie original' = 'black', 'H-W Multiplicativo' = 'red',
                                           'H-W Aditivo' = 'green'),
                     labels = c('H-W Aditivo','H-W Multiplicativo','Serie original')) +
  xlab('Año') + ylab('Número de accidentes de\ntráfico')
p.1

p.2 <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test)), colour = 'Serie original')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.adit$mean, colour = 'H-W Aditivo')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = model.mult$mean, colour = 'H-W Multiplicativo')) +
  scale_color_manual(name = '', values = c('Serie original' = 'black', 'H-W Multiplicativo' = 'red',
                                           'H-W Aditivo' = 'green'),
                     labels = c('H-W Aditivo','H-W Multiplicativo','Serie original')) +
  xlab('Año') + ylab('Número de accidentes de tráfico') + scale_x_continuous(breaks = c(), labels = c())
p.2


# Red Neuronal por defecto
# (Las medidas de precisión pueden variar debido al caracter aleatorio de las redes neuronales)
model <- nnetar(y = acc.train)
pred <- forecast(object = model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # 25.... Overfitting
# Estudiamos el decay
install.packages('Metrics')
library(Metrics)
metrics.decay <- c()
for (i in seq(0, 10, 0.1)) {
  model <- nnetar(acc.train, decay = i)
  pred <- forecast(model, h = 12)
  metrics.decay <- c(metrics.decay, mae(pred$mean, acc.test))
}
m.decay <- data.frame(decay = seq(0, 10, 0.1), MAE = metrics.decay)

comp <- ggplot() +
  geom_line(data = m.decay, aes(x = decay, y = MAE)) +
  xlab('Parámetro de regularización (Decay)') + ylab('MAE')
comp
# Red Neuronal personalizada 1
model <- nnetar(y = acc.train, decay = 9.5)
pred <- forecast(object = model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(f = pred, x = acc.test) # 14....
# Red Neuronal personalizada 2
model <- nnetar(y = acc.train, repeats = 25, size = 20, decay = 9.5, p = 20, P = 4)
pred <- forecast(object = model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(f = pred, x = acc.test) # 13....


# Técnicas ARIMA
autoplot(acc.train)
# Ajustamos la varianza con log y con Box Cox
autoplot(log(acc.train))
acc.train.trans <- BoxCox(x = acc.train, lambda = BoxCox.lambda(x = acc.train))
autoplot(acc.train.trans)
# ¿Nuestra serie es estacionaria?
ndiffs(acc.train) # 1 
nsdiffs(acc.train) # 0
# Diferenciamos la serie (no estacionalmente)
autoplot(diff(acc.train))
autoplot(Acf(diff(acc.train))) # MA -> 0 | 2 (12)
autoplot(Pacf(diff(acc.train))) # AR -> 0 | 0 (12)
# Ajustamos un SARIMA(0,1,0)(0,0,2)12
model <- Arima(y = acc.train, order = c(0,1,0), seasonal = c(0,0,2))
summary(model) # AIC = 1550.38
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(f = pred, x = acc.test) # MAE = 22.19949
checkresiduals(model) # No son ruido blanco
# Diferenciamos estacionalmente (se pierde la estacionalidad)
accidentes.diff12 <- diff(accidentes, 12) # perdemos un año de observaciones
acc.train.diff12 <- window(accidentes.diff12, start = c(1961,1), end = c(1973,12))
acc.test.diff12 <- window(accidentes.diff12, start = c(1974,1))
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.diff12)),
                y = coredata(as.zoo(acc.train.diff12))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.diff12)),
                y = coredata(as.zoo(acc.test.diff12))), col = 'red')
p

autoplot(Acf(acc.train.diff12)) # MA -> 1 | 1 (12)
autoplot(Pacf(acc.train.diff12)) # AR -> 1 | 2 (12)
# Ajustamos un SARIMA(1,0,1)(2,1,1)12 (Sin drift)
model.sin <- Arima(y = acc.train, order = c(1,0,1), seasonal = c(2,1,1), include.drift = FALSE) # Pasamos del log, apenas hay diferencias
summary(model.sin) # AIC = 1367.68
pred.sin <- forecast(model.sin, h = 12)
autoplot(pred.sin)
combine(pred.sin$mean, acc.test)
accuracy(pred.sin, acc.test) # MAE = 19.22651
checkresiduals(model.sin) # 0.04321 No ruido blanco
# Ajustamos un SARIMA(1,0,1)(2,1,1)12 (Con drift)
model.con <- Arima(y = acc.train, order = c(1,0,1), seasonal = c(2,1,1), include.drift = TRUE) # Pasamos del log, apenas hay diferencias
summary(model.con) # AIC = 1360.58
pred.con <- forecast(model.con, h = 12)
autoplot(pred.con)
combine(pred.con$mean, acc.test)
accuracy(pred.con, acc.test) # MAE = 17.0999
checkresiduals(model.con) # 0.2748 Ruido blanco
# Comparando predicciones respecto al drift
comp <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test)), colour = 'Test')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = pred.sin$mean, colour = 'Sin intercepto')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = pred.con$mean, colour = 'Con intercepto')) +
  scale_color_manual(name = '', values = c('Test' = 'black', 'Sin intercepto' = 'red',
                                           'Con intercepto' = 'green'),
                     labels = c('Con intercepto', 'Sin intercepto', 'Test')) + ylab('Número de accidentes de tráfico') +
  xlab('Año 1974') + scale_x_continuous(breaks = c(), labels = c())
comp
# Selección automática del modelo (SARIMA(1,0,0)(1,0,0)12)
model <- auto.arima(y = acc.train, test = 'adf')
summary(model) # AIC = 1514.22
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 14.86938
checkresiduals(model) # No son ruido blanco
# Selección automática del modelo (sin ahorrar coste computacional) (SARIMA (2,0,0)(2,0,0)12)
model <- auto.arima(acc.train, test = 'adf', max.order = 7, stepwise = FALSE, approximation = FALSE)
summary(model) # AICc = 1499.24
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 14.96979
checkresiduals(model) # No son ruido blanco
# Selección automática del modelo (sin ahorrar coste computacional) (SARIMA (1,1,4)(2,0,0)12)
model <- auto.arima(acc.train, max.order = 7, stepwise = FALSE, approximation = FALSE)
summary(model) # AICc = 1485.29
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 14.96979
checkresiduals(model) # No son ruido blanco





