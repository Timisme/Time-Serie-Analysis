library(TSA)


# Intervention

win.graph(width=4.875,height=2.5,pointsize=8)
data(airmiles)
plot(log(airmiles),ylab='Log(airmiles)',xlab='Year')
acf(log(airmiles),lag.max=48)
pacf(log(airmiles),lag.max=48)

acf(as.vector(diff(diff(window(log(airmiles),end=c(2001,8)),12))),lag.max=48)
pacf(as.vector(diff(diff(window(log(airmiles),end=c(2001,8)),12))),lag.max=48)
m1.air = arima(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,0),period=12))
acf(residuals(m1.air),lag.max=48)

m2.air=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69)),transfer=list(c(1,0)),
method='ML')
m2.air
acf(residuals(m2.air),lag.max=48)

win.graph(width=4.875,height=2.5,pointsize=8)
plot(log(airmiles),ylab='Log(airmiles)')
points(fitted(m2.air))

win.graph(width=4.875,height=2.5,pointsize=8)
Nine11p=1*(seq(airmiles)==69)
plot(ts(filter(Nine11p,filter=(m2.air$coef[3]),method='recursive', side=1)*
(m2.air$coef[4]),frequency=12,start=1996),ylab='9/11 Effects',
type='h'); abline(h=0)


m3.air=arimax(log(airmiles),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),
xtransf=data.frame(I911=1*(seq(airmiles)==69),I911=1*(seq(airmiles)==69)),transfer=list(c(0,0),c(1,0)),
method='ML')
m3.air
acf(residuals(m3.air),lag.max=48)

win.graph(width=4.875,height=2.5,pointsize=8)
plot(log(airmiles),ylab='Log(airmiles)')
points(fitted(m3.air))

win.graph(width=4.875,height=2.5,pointsize=8)
Nine11p=1*(seq(airmiles)==69)
plot(ts(Nine11p*(m3.air$coef[3])+filter(Nine11p,filter=(m3.air$coef[4]),method='recursive', side=1)*
(m3.air$coef[5]),frequency=12,start=1996),ylab='9/11 Effects',
type='h'); abline(h=0)


# Outlier
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
shapiro.test(res2[-(1:12)])

detectAO(m2.co2)
detectIO(m2.co2)


## Recall
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

res5 = residuals(m5.air)
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
et = mvrnorm(n,mu = c(0,0), Sigma=matrix(c(1,0.7,0.7,1),2,2))
et1=et[,1]
et2=et[,2]

xt=c(et1[1])
yt=c(et2[1])
for(i in 2:n){
xt = c(xt,0.8*xt[i-1]+et1[i])
yt = c(yt,(-0.8)*yt[i-1]+et2[i])
}

cor(xt,yt)
mod1=lm(yt~xt)
summary(mod1)

# chap11.R

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