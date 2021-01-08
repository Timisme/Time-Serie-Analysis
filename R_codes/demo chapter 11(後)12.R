
library(TSA)


# outlier for CO2 data

data(co2)
plot(co2)

m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2

res1 = residuals(m1.co2)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

detectAO(m1.co2)
detectIO(m1.co2)
m2.co2=arimax(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12),io=c(57))
m2.co2

res2 = residuals(m2.co2)
par(mfrow=c(3,1))
plot(res2,type='o')
hist(res2)
qqnorm(res2); qqline(res2)

t.test(res2)
shapiro.test(res2)
shapiro.test(res2[-(1:12)])

detectAO(m2.co2)
detectIO(m2.co2)



# outlier for air miles data

data(airmiles)
plot(log(airmiles),ylab='Log(airmiles)',xlab='Year')

m3.air=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69),I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
method='ML')
m3.air

res3 = residuals(m3.air)
par(mfrow=c(3,1))
plot(res3)
hist(res3)
qqnorm(res3); qqline(res3)

t.test(res3)
shapiro.test(res3)
ks.test(res3,"pnorm",mean(res3),sd(res3))

detectAO(m3.air)
detectIO(m3.air)


m4.air=arimax(log(airmiles),order=c(0,1,1),
seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69),
I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
xreg=data.frame(Jan98=1*(seq(airmiles)==25),Sep02=1*(seq(airmiles)==81)),
method='ML')
m4.air

detectAO(m4.air)
detectIO(m4.air)

m5.air=arimax(log(airmiles),order=c(0,1,1),
seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69),
I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
xreg=data.frame(Jan98=1*(seq(airmiles)==25),Sep02=1*(seq(airmiles)==81),Dec02=1*(seq(airmiles)==84)),
method='ML')

m5.air

detectAO(m5.air)
detectIO(m5.air)

m51.air=arimax(log(airmiles),order=c(0,1,1),
seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69),
I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
io=c(25,81,84),method='ML')

m51.air

detectAO(m51.air)
detectIO(m51.air)


res5 = residuals(m51.air)
par(mfrow=c(3,1))
plot(res5)
hist(res5)
qqnorm(res5); qqline(res5)

t.test(res5)
shapiro.test(res5)



# Pseudo relation

n=200
et1=rnorm(n)
et2=rnorm(n)

phi1=0.9
phi2=0.9
xt=c(et1[1]/sqrt(1-phi1^2))
yt=c(et2[1]/sqrt(1-phi2^2))
for(i in 2:n){
xt = c(xt,phi1*xt[i-1]+et1[i])
yt = c(yt,phi2*yt[i-1]+et2[i])
}

cor(xt,yt)
mod1=lm(yt~xt)
summary(mod1)


n=200
rho=0.7
et1=rnorm(n)
et2=rnorm(n, mean = rho*et1, sd = sqrt(1-rho^2))
cor(et1,et2)

xt=c(et1[1])
yt=c(et2[1])
for(i in 2:n){
xt = c(xt,0.8*xt[i-1]+et1[i])
yt = c(yt,(-0.8)*yt[i-1]+et2[i])
}

cor(xt,yt)
mod1=lm(yt~xt)
summary(mod1)

#Causality (ccf)
n=200
xt  = rnorm(n+2)
et2 = rnorm(n+2)
yt  = 0.6*xt[1:n]+et2[3:(n+2)]
xt = xt[3:(n+2)]
cor(xt,yt)
cor(xt[1:(n-2)],yt[3:n])
ccf(xt,yt)

ccf(yt,xt)

# data set chap11.R

data(milk)
data(electricity)
milk.electricity=ts.intersect(milk,log(electricity))

plot(milk.electricity,yax.flip=T) 

ccf(as.numeric(milk.electricity[,1]),as.numeric(milk.electricity[,2]),
main='milk & electricity',ylab='CCF')

me.dif=ts.intersect(diff(diff(milk,12)),diff(diff(log(electricity),12)))
ccf(as.numeric(me.dif[,1]),as.numeric(me.dif[,2]),
main='milk & electricity',ylab='CCF')

prewhiten(as.numeric(me.dif[,1]),as.numeric(me.dif[,2]),
,ylab='CCF')

length(diff(diff(milk,12)))
res= lm(diff(diff(log(electricity),12))[4:131]~diff(diff(milk,12))[1:128])
summary(res)

# Exhibit 11.17
data(bluebird)
plot(bluebird,yax.flip=T)

# Exhibit 11.18
prewhiten(y=diff(bluebird)[,1],x=diff(bluebird)[,2],main="Price & Sales",ylab='CCF')
# As the time series are of unit period, there is no need to apply the as.numeric 
# function.

# Exhibit 11.19
sales=bluebird[,1]
price=bluebird[,2]
chip.m1=lm(sales~price,data=bluebird)
summary(chip.m1)

# Exhibit 11.20
acf(residuals(chip.m1),ci.type='ma')

# Exhibit 11.21
pacf(residuals(chip.m1))

# Exhibit 11.22
eacf(residuals(chip.m1))

# Exhibit 11.23
chip.m2=arima(sales,order=c(1,0,4),xreg=data.frame(price))
chip.m2

# MA(1)& MA(3) estimates are not significant, at 5% level,
# so they are constrained to be zero in the model fit below.
chip.m3=arima(sales,order=c(1,0,4),xreg=data.frame(price),fixed=c(NA,0,NA,0,NA,NA,NA))
chip.m3

# Now, the AR(1) coefficient estimate also seems not significant, so it is
# removed in the next fitted model.
chip.m4=arima(sales,order=c(0,0,4),xreg=data.frame(price),fixed=c(0,NA,0,NA,NA,NA))
chip.m4

# model diagnostic can be done by running the tsdiag command.
res1 = residuals(chip.m4)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 14,type = "Ljung")
Box.test(res1, lag = 22,type = "Ljung")
Box.test(res1, lag = 33,type = "Ljung")


# Treasury constant maturity rate
r1t = read.table("F:\\w-tb3ms.dat")
r2t = read.table("F:\\w-tb6ms.dat")

r1tnew = r1t[,2][258:2459]
r2tnew = r2t[,2]
ccf(r1tnew,r2tnew)

acf(r1tnew)
acf(r2tnew)

c1t = diff(r1tnew)
c2t = diff(r2tnew)

acf(c1t)
acf(c2t)

prewhiten(c1t,c2t,ylab='CCF')

acf(c2t)
pacf(c2t)
eacf(c2t)

m1=arima(c2t,order=c(2,0,0),xreg=c1t,include.mean=FALSE)
m1
et = residuals(m1)
acf(et)
Box.test(et, lag = 6,type = "Ljung")

acf(et)
pacf(et)
eacf(et)

m3=arima(et,order=c(0,0,6),include.mean=FALSE,fixed=c(0,0,0,NA,0,NA))
m3

res1 = residuals(m3)
par(mfrow=c(3,1))
plot(res1)
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1,lag = 36)
Box.test(res1, lag = 11,type = "Ljung")
Box.test(res1, lag = 16,type = "Ljung")

rsq=(sum(c2t^2)-sum(m3$residuals^2))/sum(c2t^2)
rsq

 

# chapter 12
# Time plot of CREF
win.graph(width=4.875, height=2.5,pointsize=8)
data(CREF)
plot(CREF)
t1=435;t2=508
polygon(x=c(t1,t1,t2,t2,t1),
y=c(min(CREF)-10,max(CREF)+10,max(CREF)+10,min(CREF)-10,min(CREF)-10),col='gray')
lines(CREF)

# Log return of CREF
r.cref=diff(log(CREF))*100
plot(r.cref)
t1=435;t2=507
polygon(x=c(t1,t1,t2,t2,t1),
y=c(-2,2.7,2.7,-2,-2),col='gray')
lines(r.cref)
abline(h=0)

# McLeod.Li test
win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=r.cref)

# ACF & PACF
par(mfrow=c(2,1))
acf(r.cref)
pacf(r.cref)
Box.test(r.cref, lag = 14,type = "Ljung")

# normality test
win.graph(width=4.875, height=3,pointsize=8)
qqnorm(r.cref)
qqline(r.cref)
shapiro.test(r.cref)

# simulation 
arch1.sim = garch.sim(alpha = c(0.01,0.7),n=500)
plot(arch1.sim,type='l',ylab=expression(r[t]),xlab='t')
var(arch1.sim)


# Fit an ARCH model
library(fGarch)

par(mfrow=c(2,1))
acf(arch1.sim)
pacf(arch1.sim)

win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=arch1.sim)
qqnorm(arch1.sim)
qqline(arch1.sim)


par(mfrow=c(2,1))
acf((arch1.sim)^2)
pacf((arch1.sim)^2)
eacf((arch1.sim)^2)

g1 = garchFit(q, data = arch1.sim, include.mean=F)
summary(g1)

vol = volatility(g1)
plot(vol,type='l')
res1 = arch1.sim/vol
head(arch1.sim)
head(res1)
var(res1)

par(mfrow=c(3,1))
plot(res1, type = 'l' )
hist(res1 )
qqnorm(res1 ); qqline(res1 )


t.test(res1)
shapiro.test(res1)

par(mfrow=c(2,1))
acf(res1)
acf(res1^2)
Box.test(res1 , lag = 3,type = "Ljung")
Box.test(res1^2 , lag = 7,type = "Ljung")
Box.test(res1^2 , lag = 12,type = "Ljung")

# ARMA + ARCH
obs = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 500, innov = arch1.sim)
plot(obs,type='l',ylab=expression(r[t]),xlab='t')

obs0 = arima.sim(list(order = c(1,0,0), ar = 0.7), n = 500, innov = arch1.sim)
obs = obs0[101:500]
plot(obs,type='l',ylab=expression(r[t]),xlab='t')


data = c(arch1.sim[1]/sqrt(1-0.7^2))
for(i in 2:500){
data = c(data, 0.7*data[i-1]+arch1.sim[i])
}
data

win.graph(width=4.875, height=3,pointsize=8)
plot(data,type='l',ylab=expression(r[t]),xlab='t')

McLeod.Li.test(y=data)
McLeod.Li.test(y=arima.sim(list(order = c(1,0,0), ar = 0.7), n = 500))
McLeod.Li.test(y=rnorm(500))

# Fit AR+ARCH
par(mfrow=c(2,1))
acf(data)
pacf(data)
eacf(data)

arma.mod = arima(data, order = c(1,0,0), include.mean=F)

arma.res = residuals(arma.mod)
par(mfrow=c(2,1))
acf(arma.res)
pacf(arma.res)

win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=arma.res)

par(mfrow=c(2,1))
acf((arma.res)^2)
pacf((arma.res)^2)
eacf((arma.res)^2)

g1 = garchFit(~garch(1, 0), data = arma.res, include.mean=F)
summary(g1)

vol = volatility(g1)
plot(vol,type='l')
res1 = arma.res/vol

var(res1)

par(mfrow=c(3,1))
plot(res1, type = 'l' )
hist(res1 )
qqnorm(res1 ); qqline(res1 )


t.test(res1)
shapiro.test(res1)

par(mfrow=c(2,1))
acf(res1)
acf(res1^2)
Box.test(res1 , lag = 4,type = "Ljung")
Box.test(res1^2 , lag = 7,type = "Ljung")
Box.test(res1^2 , lag = 12,type = "Ljung")

########## Fit ARMA+GARCH
arch1.sim = garch.sim(alpha = c(0.01,0.1), beta = 0.8,n=600)
obs0 = arima.sim(list(order = c(1,0,1), ar = 0.7, ma = 0.3), n = 600, innov = arch1.sim)
obs = obs0[101:600]

win.graph(width=4.875, height=3,pointsize=8)
plot(obs,type='l',ylab=expression(r[t]),xlab='t')


par(mfrow=c(2,1))
acf(obs)
pacf(obs)
eacf(obs)

arma.mod = arima(obs, order = c(1,0,1), include.mean=F)

arma.res = residuals(arma.mod)
par(mfrow=c(2,1))
acf(arma.res)
pacf(arma.res)
Box.test(arma.res, lag = 9,type = "Ljung")

win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=arma.res)

par(mfrow=c(2,1))
acf((arma.res)^2)
pacf((arma.res)^2)
eacf((arma.res)^2)

g1 = garchFit(~garch(2, 0), data = arma.res, include.mean=F)
summary(g1)

g2 = garchFit(~garch(1, 1), data = arma.res, include.mean=F)
summary(g2)


vol = volatility(g2)
plot(vol,type='l')
res1 = arma.res/vol

par(mfrow=c(3,1))
plot(res1, type = 'l' )
hist(res1 )
qqnorm(res1 ); qqline(res1 )


t.test(res1)
shapiro.test(res1)

par(mfrow=c(2,1))
acf(res1)
acf(res1^2)
Box.test(res1 , lag = 20,type = "Ljung")
Box.test(res1^2 , lag = 8,type = "Ljung")
Box.test(res1^2 , lag = 11,type = "Ljung")

##########

# Real data CREF
data(CREF)
rt = diff(log(CREF))*100
win.graph(width=4.875, height=3,pointsize=8)
plot(rt,type='l',ylab=expression(r[t]),xlab='t')

par(mfrow=c(2,1))
acf(rt)
pacf(rt)
eacf(rt)

win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=rt)

par(mfrow=c(2,1))
acf(rt^2)
pacf(rt^2)
eacf(rt^2)

g1 =  garchFit(~garch(1, 1), data = rt, include.mean=F)
summary(g1)

vol = volatility(g1)
plot(vol,type='l')
res1 = rt/vol
var(res1)

par(mfrow=c(3,1))
plot(res1, type = 'l' )
hist(res1)
qqnorm(res1 ); qqline(res1 )

t.test(res1)
shapiro.test(res1)

par(mfrow=c(2,1))
acf(res1)
acf(res1^2)
Box.test(res1 , lag = 14,type = "Ljung")
Box.test(res1^2 , lag = 15,type = "Ljung")

win.graph(width=4.875, height=3,pointsize=8)
McLeod.Li.test(y=res1)

plot(vol^2,type='l',ylab='conditional variance',xlab='t')
