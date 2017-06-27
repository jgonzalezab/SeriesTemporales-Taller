library(ggplot2)
library(zoo)
library(forecast)
library(rdatamarket)

# cargamos la libreria
install.packages('tseries')
library(tseries)

# Cargamos los datos desde la API de datamarket (necesaria conexión a internet)
accidentes <- as.ts(dmseries('http://data.is/1yFXOBi'))
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


# Ajustamos estacionalmente la serie y volvemos a dividir en dos conjuntos
decomposition <- decompose(x = accidentes, type = 'additive')
accidentes.adj <- seasadj(decomposition) # Forecast
acc.train.adj <- window(accidentes.adj, start = c(1960,1), end = c(1973,12))
acc.test.adj <- window(accidentes.adj, start = c(1974,1))
# Plotting con ggplot del training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.adj)), y = coredata(as.zoo(acc.train.adj))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.adj)), y = coredata(as.zoo(acc.test.adj))), col = 'red')
p
# ¿Es estacionaria?
adf.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
adf.test(acc.train.adj, alternative = 'explosive') # No es explosiva
pp.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
pp.test(acc.train.adj, alternative = 'explosive') # No es explosiva
kpss.test(acc.train.adj, null = 'Trend') # 0.055 dudamos sobre la estacionariedad en tendencia
# Diferenciamos para hacerla estacionaria en tendencia (evidencias visuales)
accidentes.dif.adj <- diff(accidentes.adj) 
acc.train.dif.adj <- window(accidentes.dif.adj, start = c(1960,2), end = c(1973,12)) # Perdemos una observación
acc.test.dif.adj <- window(accidentes.dif.adj, start = c(1974,1))
# Repetimos los tests
adf.test(acc.train.dif.adj, alternative = 'stationary') # Es estacionaria
adf.test(acc.train.dif.adj, alternative = 'explosive') # No es explosiva
pp.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
pp.test(acc.train.adj, alternative = 'explosive') # No es explosiva
kpss.test(acc.train.dif.adj, null = 'Trend') # Es estacionaria en tendencia
# Plotting con ggplot del nuevo training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)), y = coredata(as.zoo(acc.train.dif.adj))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.dif.adj)), y = coredata(as.zoo(acc.test.dif.adj))), col = 'red')
p


# Linealidad en media
terasvirta.test(acc.train.dif.adj) # Es lineal en media
white.test(acc.train.adj) # Es lineal en media


# Bootstrap
boots.block <- tsbootstrap(acc.train.dif.adj, type = 'block')
boots.stationary <- tsbootstrap(acc.train.dif.adj, type = 'stationary')
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(boots.block)), y = coredata(as.zoo(boots.block))), col = 'blue')
p
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(boots.stationary)), y = coredata(as.zoo(boots.stationary))), col = 'blue')
p
initial.acf <- function(serie) {
  return(acf(serie, plot = FALSE)$acf[2:8])
}
tsbootstrap(acc.train.dif.adj, nb= 500, type = 'stationary',
            statistic = initial.acf)
# Surrogate
surr <- surrogate(acc.train.dif.adj, ns = 1, fft = TRUE, amplitude = TRUE)
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(surr)), y = coredata(as.zoo(surr))), col = 'blue')
p


# Estudiamos los correlogramas
autoplot(Acf(acc.train.dif.adj))
autoplot(Pacf(acc.train.dif.adj))
# Ajustamos un ARiMA(1,1,1)
model.1 <- arma(x = acc.train.dif.adj, order = c(1,1), include.intercept = FALSE) # AIC =  1419.3
summary(model.1)
autoplot(Acf(model.1$residuals))
jarque.bera.test(na.remove(model.1$residuals))
bds.test(na.remove(model.1$residuals))
# Ajustamos un ARiMA(2,1,1)
model.2 <- arma(x = acc.train.dif.adj, order = c(2,1), include.intercept = FALSE) # AIC = 1414.81
summary(model.2)
autoplot(Acf(model.2$residuals))
jarque.bera.test(na.remove(model.2$residuals))
bds.test(na.remove(model.2$residuals))
# Graficamos ambos modelos
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(fitted(model.1))), y = coredata(as.zoo(fitted(model.1)))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(fitted(model.2))), y = coredata(as.zoo(fitted(model.2)))), col = 'red')
p
