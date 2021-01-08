library(aTSA)
library(TSA)
library(tseries)

## Simulate AR(1) with mean mu

n = 500
et = rnorm(n+1)
mu=1
phi = 0.8
yt=c(et[1]/sqrt(1-phi^2) + mu)

for(i in 2:(n+1)){

yt=c(yt,mu + phi*(yt[i-1]-mu) + et[i])

}

xt=yt[2:101]

par(mfrow=c(3,1))
plot(xt,type='o')
acf(xt)
pacf(xt)

xt=yt[2:500]

par(mfrow=c(3,1))
plot(xt,type='o')
acf(xt)
pacf(xt)

par(mfrow=c(1,2))
acf(xt)
acf(xt,ci.type='ma')

## Simulate AR(2)
n = 200

et = sqrt(0.01)*rnorm(n+1)
var(et)
phi1 = 1.3
phi2 = -0.7

c2=phi1/(1-phi2)
c1=sqrt(1+c2^2)

wt=c(c1*et[1])
wt=c(wt,c2*wt[1] + et[2])

for(i in 3:(n+1)){
wt=c(wt, phi1*wt[i-1] + phi2*wt[i-2]  + et[i])
}

par(mfrow=c(3,1))
plot(wt, type = "l")
acf(wt)
pacf(wt)

## Augmented Dickey¡VFuller test
n=500
et = rnorm(n)
adf.test(et)

yt = c(et[1])
for(i in 2:n){
yt = c(yt, yt[i-1] + et[i])
}
adf.test(yt)
kpss.test(yt)
adf.test(diff(yt))
kpss.test(diff(yt))

# dataset = color
data(color)

par(mfrow=c(3,1))
plot(color, type = "l")
acf(color)
pacf(color)
par(mfrow=c(1,1))
acf(color,ci.type='ma')
eacf(color)# an error due to the inadequate lags
eacf(color, ar.max = 7, ma.max = 7)

## dataset = hare
win.graph(width=4, height=4,pointsize=8)
data(hare)
plot(hare, type = "l")
par(mfrow=c(2,1))
acf(hare)
pacf(hare)

par(mfrow=c(1,1))
bxh=BoxCox.ar(hare)
bxh$mle # the mle of the power parameter
bxh$ci # corresponding 95% C.I.
par(mfrow=c(3,1))
plot(hare^.5, type = "l")
acf(hare^.5)
pacf(hare^.5)
eacf(hare^.5) # an error due to the inadequate lags
eacf(hare^.5, ar.max = 7, ma.max = 7)

## dataset = oil.price
data(oil.price)
par(mfrow=c(1,1))
plot(oil.price, type = 'o')
BoxCox.ar(oil.price)
BoxCox.ar(oil.price,method = "ols",lambda = seq(-0.5, 0.5, 0.01))
plot(log(oil.price), type = 'o')
adf.test(log(oil.price))
kpss.test(log(oil.price))
yt = diff(log(oil.price))
adf.test(yt)
plot(yt)

par(mfrow=c(3,1))
plot(yt)
acf(yt)
pacf(yt)
eacf(yt)

par(mfrow=c(1,1))
res = armasubsets(yt, nar=7, nma = 7, ar.method = 'ols')
plot(res)