library(TSA)
library(tseries)
data(co2)

# deterministic seasonal means + linear time trend

seasonal_means = season(co2)
time_trend = time(co2)

mod = lm(co2 ~seasonal_means+time_trend)
summary(mod)

acf(residuals(mod))


# ¿À©w
plot(co2)
BoxCox.ar(co2)
plot(diff(co2))


adf.test(diff(co2))
acf(diff(co2))
pacf(diff(co2))


arima(co2, odrder= c())


# --------------------------------------------

library(TSA)

## Simulation
# SARIMA(0,0,1)*(1,0,0)
n=(12*40)+12
sphi = 0.6
theta = 0.8

et=rnorm(n)
yt = c()

for(i in 1:12){
  
  yt=c(yt,et[i+1]-theta * et[i])
  
}


for(i in 13:(n)){
  
  yt=c(yt,sphi*yt[i-12] + et[i]-theta * et[i-1])
  
}

length(yt)

plot(yt,type='o')
acf(yt,lag=60)
pacf(yt, lag=60)

## Simulation
# SARIMA(1,0,0)*(1,0,0)
n=(12*40)+12
sphi = 0.5
theta = 0.8
phi = 0.5

et=rnorm(n)
yt=c(et[13]/sqrt(1-phi^2))

(1,0,0)

for(i in 2:13){
  
  yt=c(yt,phi * yt[i-1] + et[i])
  
}

for(i in 14:(n)){
  
  yt=c(yt,sphi * yt[i-12] - phi * sphi * yt[i-13] + phi * yt[i-1] + et[i])
  
}

yt

length(yt)

plot(yt,type='o')
acf(yt,lag=60)
pacf(yt, lag= 60)



