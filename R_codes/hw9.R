library(TSA)
uninstall.packages(forecast)
library(forecast)
data(robot)

mod1= arima(robot, order=c(1,0,0))
res1 = residuals(mod1)
mod1

par(mfrow=c(3,1))
plot(res1, type = 'o', ylab= 'residual for AR(1)')
abline(h=0)
hist(res1, main= 'Histogram for AR(1)')
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

acf(res1, main= 'acf for AR(1)')
Box.test(res1, lag = 2,type = "Ljung")
Box.test(res1, lag = 3,type = "Ljung")

# ima(1,1) 

mod2 = arima(robot, order=c(0,1,1))
res2 = residuals(mod2)
mod2

par(mfrow= c(1,1))
plot(res2, type = 'o', ylab= 'residual for IMA(1,1)')
hist(res2, main= 'Histogram for IMA(1,1)')
qqnorm(res2); qqline(res2)

t.test(res2)
shapiro.test(res2)

acf(res2, main= 'acf for IMA(1,1)')
Box.test(res2, lag = 2,type = "Ljung")
Box.test(res2, lag = 10,type = "Ljung")


mod1$aic
mod2$aic

BIC(mod1)
BIC(mod2)

# 8.10

data(deere3)

mod3= arima(deere3, order=c(1,0,0))
res3 = residuals(mod3)
mod3

par(mfrow=c(1,1))
plot(res3, type = 'o', ylab= 'residual for AR(1)')
abline(h=0)
hist(res3, main= 'Histogram for AR(1)')
qqnorm(res3); qqline(res3)

t.test(res3)
shapiro.test(res3)

acf(res3, main= 'acf for AR(1)')
Box.test(res1, lag = 4,type = "Ljung")
Box.test(res1, lag = 3,type = "Ljung")

# 9.21 
data(deere3)
mod1=arima(deere3,order=c(1,0,0))
plot(forecast(mod1,  h = 10), main= 'AR(1) forecast for next 10 values')
predict(mod1, n.ahead = 10)$pred
plot(mod1,n.ahead=10)$pred
plot(mod1,n.ahead=10,n1=25)
abline(h=coef(mod1)[names(coef(m1.color))=='intercept'])

# 9.22 

data(days)
daysmod=days; daysmod[63]=35; daysmod[106]=35; daysmod[129]=35
mod2 = Arima(daysmod, order= c(0,0,2))
predict(mod2, n.ahead = 10)$pred
plot(forecast(mod2,  h = 10), main= 'MA(2) forecast for next 10 values')
pch=23, type= 'o')
abline(h=coef(mod2)[names(coef(mod2))=='intercept'])

# 9.23

data(robot)
mod3 <- Arima(robot, order= c(0,1,1))
predict(mod3, n.ahead = 5)$pred
a <- forecast(mod3, h= 5, level= 0.95)
plot(a)
plot(forecast(mod3,  h = 5), main= 'IMA(1,1) forecast for next 5 values')
# autoplot

mod3 <- Arima(robot, order= c(1,0,1))
predict(mod3, n.ahead = 5)$pred
a <- forecast(mod3, h= 5, level= 0.95)
plot(forecast(mod3, h=5), main= 'ARMA(1,1) forecast for the next 5 values')
