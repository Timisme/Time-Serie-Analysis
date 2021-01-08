library(TSA)


## Simulation
# SARIMA(0,0,1)*(0,0,1)
n=(12*40)+13
theta= -0.5
stheta = 0.8

et=rnorm(n)
yt=et[-(1:13)] - theta*(et[-n][-(1:12)])- stheta*(et[-((n-11):n)][-1])+theta*stheta*et[-((n-12):n)]


plot(yt,type='o')
acf(yt,lag=60)


# SARIMA(1,0,0)*(0,0,1)
n=(12*40)+11
phi = 0.6
stheta = -0.8

et=rnorm(n)
yt=c(et[12]/sqrt(1-phi^2))


for(i in 13:(n)){

yt=c(yt,phi*yt[(i-11)-1] + et[i] - stheta*et[i-12] )

}


length(yt)

plot(yt,type='o')
acf(yt,lag=60)



# data set CO2
data(co2)
win.graph(width=4.875, height=3,pointsize=8)
plot(co2,ylab='CO2',main='Monthly Carbon Dioxide Levels at Alert, NWT, Canada')

plot(window(co2,start=c(2000,1)),main='Carbon Dioxide Levels with Monthly Symbols', ylab='CO2')
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(co2,start=c(2000,1)),pch=Month)

monthplot(co2)

## Lecture model~~SARIMA(0,1,1)*(0,1,1)
par(mfrow=c(2,1))
acf(co2,lag.max=36,
main=expression(Sample~~ACF~~of~~CO[2]~~Levels))
pacf(co2,lag.max=36,
main=expression(Sample~~PACF~~of~~CO[2]~~Levels))

par(mfrow=c(1,1))
plot(diff(co2),main=expression(Time~~Series~~Plot~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels), ylab=expression(First~~Difference~~of~~CO[2]))

par(mfrow=c(2,1))
acf(as.vector(diff(co2)),lag.max=36,
main=expression(Sample~~ACF~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels))
pacf(as.vector(diff(co2)),lag.max=36,
main=expression(Sample~~PACF~~of~~the~~First~~Differences~~of~~
CO[2]~~Levels))

par(mfrow=c(1,1))
plot(diff(diff(co2),lag=12),main=expression(Time~~Series~~Plot~~of~~the~~First~~and~~
Seasonal~~Differences~~of~~CO[2]~~Levels),
ylab=expression(First~~and~~Seasonal~~Difference~~of~~C~O[2]))

par(mfrow=c(2,1))
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma',
main=expression(Sample~~ACF~~of~~the~~First~~and~~Seasonal~~Differences~~of~~
CO[2]~~Levels))
pacf(as.vector(diff(diff(co2),lag=12)),lag.max=36,
main=expression(Sample~~ACF~~of~~the~~First~~and~~Seasonal~~Differences~~of~~
CO[2]~~Levels))

m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2

res1 = residuals(m1.co2)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
acf(as.numeric(res1),lag = 36)
Box.test(res1, lag = 9,type = "Ljung")
Box.test(res1, lag = 22,type = "Ljung")

m1.co2

## Prediction

plot(m1.co2,n1=c(2004,1),n.ahead=48,col='red',xlab='Year',type='o',
ylab=expression(CO[2]~~Levels),
main=expression(Long~~Term~~Forecasts~~'for'~~the~~CO[2]~~Model))