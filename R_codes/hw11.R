library(TSA)
library(tseries)

data("boardings")

data = boardings[, 'log.boardings']

plot(data, type= 'l', ylab= 'Boardings number')
points(data, x= time(data), pch= as.vector(season(data)))

adf.test(data)
kpss.test(data)

acf(data)
acf(as.vector(data), ci.type= 'ma', lag= 60)

#C
model = arima(data, order= c(0,0,3), seasonal= list(order= c(1,0,0), period= 12))

model

#D

model2=arima(data,order=c(0,0,4),seasonal=list(order=c(1,0,0),period=12)); model2

# 11.15
data("airmiles")

data2 = airmiles

as.vector(data2)
acf(diff(diff(window(log(data2), end= c(2001,8)), 12)), lag= 48, main= 'acf for airmiles')

# (b)

mod = arima(window(log(data2), end= c(2001,8)), order= c(0,1,1), seasonal= list(order= c(0,1,0), period= 12))
plot(residuals(mod), ylab= 'residuals', type= 'o')

detectAO(mod)

mod2 = arima(window(log(data2), end= c(2001,8)), order= c(0,1,1), seasonal= list(order= c(0,1,0), period= 12), io= c(25))
plot(residuals(mod2), ylab= 'residuals', type= 'o', main=' residual plots after outlier is removed')

acf(as.vector(residuals(mod2)), lag= 60, main= 'acf for residuals after removing outliers')

# (d)

mod3 = arimax(window(log(data2), end= c(2001,8)), order= c(0,1,1), seasonal= list(order= c(0,1,1), period= 12), io= c(25))
plot(residuals(mod3), ylab= 'residuals', type= 'o', main=' residual plots after outlier is removed')
mod3

acf(as.vector(residuals(mod3)), lag= 60, main= 'acf for residuals after removing outliers')

