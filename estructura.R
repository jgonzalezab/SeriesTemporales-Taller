setwd("C:/Users/Ordenador/Desktop/Taller/Codigo")
######################## formato ts
# datos mensuales
acc <- read.csv('./monthly-traffic-fatalities-in-on.csv', sep = ',')
head(acc)
str(acc)

acc.ts <- ts(data = acc)
head(acc.ts)
plot(acc.ts, type = 'l')

acc.ts <- ts(data = acc, start = c(1960, 1), frequency = 12)
acc.ts

plot(acc.ts, type = 'l')

# datos cuatrimestrales
prc <- read.csv('./quarterly-australian-national-ac.csv')
head(prc)
str(prc)

prc.ts <- ts(data = prc, start = c(1959, 4), frequency = 4)
prc.ts
plot(prc.ts, type = 'l')

# windows
sub.prc.ts <- window(prc.ts, start = c(1960, 1), end = c(1963, 4)) 
plot(sub.prc.ts)


###################### formato zoo
install.packages('zoo')
library(zoo)

dt <- seq.Date(from = as.Date('1960-01-01'), to = as.Date('1974-12-01'), by = 'month')
acc.zoo <- zoo(x = acc, order.by = dt)
acc.zoo
plot(acc.zoo) # plot.zoo

# tratamiento NAs
acc.zoo[c(3, 6, 12, 20, 34, 60, 61, 62)] <- NA # añadimos NAs
plot(acc.zoo)

acc.zoo.sol.1 <- na.aggregate(object = acc.zoo, FUN = mean) # NAs por medias
plot(acc.zoo.sol.1)

acc.zoo.sol.3 <- na.locf(object = acc.zoo) # NAs por el valor anterior
plot(acc.zoo.sol.3)

acc.zoo.sol.2 <- na.approx(object = acc.zoo) # NAs por una interpolacion lineal
plot(acc.zoo.sol.2)

acc.zoo.sol.3 <- na.spline(object = acc.zoo) # NAs por splines
plot(acc.zoo.sol.3)

#  aplicando funciones sobre subconjuntos de la serie
dt <- seq.Date(from = as.Date('1960-01-01'), to = as.Date('1974-12-01'), by = 'month') 
acc.zoo <- zoo(x = acc, order.by = dt)

ap1 <- rollapply(data = acc.zoo, width = 5, FUN = mean, align = 'right')
plot(ap1)

ap2 <- rollapply(data = acc.zoo, width = 10, FUN = sum, align = 'right')
plot(ap2)

ap3 <- rollapply(data = acc.zoo, width = 12, FUN = mean, align = 'right', by = 12) 
plot(ap3)

# medias moviles mensual
ma1 <- rollmean(x = acc.zoo, k = 6, align = 'right')
ma2 <- rollmean(x = acc.zoo, k = 12, align = 'right')
ma3 <- rollmean(x = acc.zoo, k = 24, align = 'right')

install.packages('ggplot2')
library(ggplot2)

p <- ggplot() +
  geom_line(aes(x = index(acc.zoo), y = coredata(acc.zoo), colour = 'original')) +
  geom_line(aes(x = index(ma1), y = coredata(ma1), colour = 'k = 6')) +
  geom_line(aes(x = index(ma2), y = coredata(ma2), colour = 'k = 12')) +
  geom_line(aes(x = index(ma3), y = coredata(ma3), colour = 'k = 24')) +
  scale_color_manual(name = '',
                     values = c('k = 6' = 'blue', 'k = 12' = 'red', 'k = 24' = 'green', 'original' = 'black')) +
  xlab('Año') + ylab('Número de accidentes de tráfico')
p

# medias moviles cuatrimestral
prc <- read.csv('./quarterly-australian-national-ac.csv', sep = ',')
prc.ts <- ts(data = prc, start = c(1959, 4), frequency = 4)

ma1 <- rollmean(x = prc.ts, k = 2, align = 'right')
ma2 <- rollmean(x = prc.ts, k = 4, align = 'right')

p <- ggplot() +
  geom_line(aes(x = index(prc.ts), y = coredata(prc.ts), colour = 'original')) +
  geom_line(aes(x = index(ma1), y = coredata(ma1), colour = 'k = 2')) +
  geom_line(aes(x = index(ma2), y = coredata(ma2), colour = 'k = 4')) +
  scale_color_manual(name = '',
                     values = c('k = 2' = 'blue', 'k = 4' = 'red', 'original' = 'black')) +
  xlab('Año') + ylab('Precios')
p


###################### formato xts
install.packages('xts')
library(xts)

xts.acc <- xts(x = acc, order.by = dt)
plot(xts.acc)

# Subsetting de observaciones
xts.acc['1963-12'] # diciembre de 1963
xts.acc['1963'] # el año completo de 1963
xts.acc['/1963-7'] # todas las observaciones hasta julio de 1963
xts.acc['1963-7/'] # todas las observaciones a partir de julio de 1963
xts.acc['1962-7/1963-7'] # todas las observaciones comprendidas entre julio de 1962 y de 1963


###################### RDataMarket API
install.packages('rdatamarket')
library(rdatamarket)

accidentes <- as.ts(dmseries('http://data.is/1yFXOBi'))


