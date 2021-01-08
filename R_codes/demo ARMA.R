library(TSA)

##  MA(1) simulation
m = 101
theta = 0.9
et = rnorm(m,0,1)
yt = et[2:m] - theta*et[1:(m-1)]
n=length(yt)
plot(yt,type='o')
plot(yt[1:(n-1)],yt[2:n])

##  MA(2)
m = 102
theta1 = 1
theta2 = -0.6
et = rnorm(m,0,1)
yt = et[3:m] - theta1*et[2:(m-1)] - theta2*et[1:(m-2)]
n=length(yt)
plot(yt,type='o')
plot(yt[1:(n-1)],yt[2:n])
plot(yt[1:(n-2)],yt[3:n])

ARMAacf(ma = c(-1,0.6),lag.max = 20)
plot(ARMAacf(ma = c(-1,0.6),lag.max = 20),type='h')
abline(h=0)
## AR(1) simulation

n = 100
et = rnorm(n+1)
phi = 0.5
yt=c(et[1]/sqrt(1-phi^2))

for(i in 2:(n+1)){

yt=c(yt,phi*yt[i-1] + et[i])

}

par(mfrow=c(3,1))
plot(yt,type='o')
acf(yt)
pacf(yt)


ar1.sim = arima.sim(list(order = c(1,0,0), ar = 0.5), n = 100)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(ar1.sim,type='o')
plot(ar1.sim[1:(n-1)],ar1.sim[2:n])
par(mfrow=c(3,1))
plot(ar1.sim,type='o')
acf(ar1.sim)
pacf(ar1.sim)




## AR(2) theoretical ACF

## 1st quadrant
phi1 = 0.5
phi2 = 0.3

## 2nd quadrant
 phi1 = -0.5   
 phi2 = 0.4

## 3rd quadrant
 phi1 = -1   
 phi2 = -0.5

## 4th quadrant
 phi1 = 1   
 phi2 = -0.5


ARMAacf(ar = c(phi1,phi2),lag.max = 20)

plot(ARMAacf(ar = c(phi1,phi2),lag.max = 20)[-1],type='h',
ylab='ACF',xlab='lag',ylim=c(-1,1))
abline(h=0)

## AR(2) simulation

n = 1000
## 1st quadrant
phi1 = 0.5
phi2 = 0.3

## 2nd quadrant
 phi1 = -0.5   
 phi2 = 0.4

## 3rd quadrant
# phi1 = -1   
# phi2 = -0.5

## 4th quadrant
 phi1 = 1   
 phi2 = -0.5


et = rnorm(n+2)
c2=phi1/(1-phi2)
c1=sqrt(1/(1-c2^2))
yt=c(c1*et[1])
yt=c(yt,c2*yt[1]+et[2])


## yt=c(et[1])
## yt=c(yt,phi1*yt[1]+et[2])

for(i in 3:(n+2)){

yt=c(yt,phi1*yt[i-1]+ phi2*yt[i-2]  + et[i])

}

par(mfrow=c(3,1))
plot(yt,type='o')
acf(yt)
pacf(yt)

ar2.sim = arima.sim(list(order = c(2,0,1), ar = c(0.5,0.25),ma=-0.5), n = 1000)
par(mfrow=c(3,1))
plot(ar2.sim,type='o')
acf(ar2.sim)
pacf(ar2.sim)