library(TSA)

## dataset color

data(color)

par(mfrow=c(3,1))
plot(color, type = 'o')
acf(color)
pacf(color)
eacf(color)
eacf(color, ar.max = 5, ma.max = 5)

## MA(1)

arima(color, order = c(0,0,2))
mod1 = arima(color, order = c(0,0,1))
res1 = residuals(mod1)

par(mfrow=c(3,1))
plot(res1, type = 'o')
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1)
Box.test(res1, lag = 8,type = "Ljung")
Box.test(res1, lag = 10,type = "Ljung")

tsdiag(mod1, gof=15)

## AR(1)

mod2 = arima(color, order = c(1,0,0))
res2 = residuals(mod1)

par(mfrow=c(3,1))
plot(res2 , type = 'o')
hist(res2)
qqnorm(res2); qqline(res2)

t.test(res2)
shapiro.test(res2)

par(mfrow=c(1,1))
acf(res2)
Box.test(res2 , lag = 8,type = "Ljung")
Box.test(res2 , lag = 10,type = "Ljung")

## Compare

mod1$aic
mod2$aic

BIC(mod1)
BIC(mod2)

## Prediction

par(mfrow=c(3,1))
plot(mod1,n.ahead=10,n1=25)
abline(h=coef(mod1)[names(coef(m1.color))=='intercept'])
plot(mod2,n.ahead=10,n1=25)
abline(h=coef(mod2)[names(coef(m1.color))=='intercept'])


## Dataset hare

data(hare)
plot(hare, type = "l")
par(mfrow=c(2,1))
acf(hare, lag=25)
pacf(hare, lag=25)

bxh=BoxCox.ar(hare, lambda = seq(-1, 1, 0.01))
par(mfrow=c(2,1))
acf(hare^.5, lag=25)
pacf(hare^.5, lag=25)
eacf(hare^.5, ar.max = 5, ma.max = 5)

## AR(3)

m1.hare=arima(sqrt(hare),order=c(3,0,0))
m1.hare
m2.hare=arima(sqrt(hare),order=c(3,0,0),fixed=c(NA,0,NA,NA))
m2.hare

res1 = residuals(m2.hare)

par(mfrow=c(3,1))
plot(res1, type = 'o')
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1)
Box.test(res1, lag = 5,type = "Ljung")

## ARMA(6,4)

plot(armasubsets(sqrt(hare), nar=6, nma=5))
arima(sqrt(hare), order = c(6,0,4))
m3.hare= arima(sqrt(hare), order = c(6,0,3))
m3.hare= arima(sqrt(hare), order = c(6,0,3),fixed = c(NA,0,NA,0,0,NA,NA,NA,NA,NA))
res2 = residuals(m3.hare)


par(mfrow=c(3,1))
plot(res2 , type = 'o')
hist(res2)
qqnorm(res2); qqline(res2)

t.test(res2)
shapiro.test(res2)

par(mfrow=c(1,1))
acf(res2)
Box.test(res2 , lag = 5,type = "Ljung")


## Compare

m2.hare$aic
m3.hare$aic

## Prediction

par(mfrow=c(2,1))
plot(m2.hare,n.ahead=20)
abline(h=coef(m2.hare)[names(coef(m1.hare))=='intercept'])
plot(m3.hare,n.ahead=20)
abline(h=coef(m3.hare)[names(coef(m1.hare))=='intercept'])

mu=predict(m2.hare,10)$pred
sigma=predict(m2.hare,10)$se

plot(c(hare,(mu)^2+(sigma)^2),type='o',ylab='Hare')
lines(c(32:41),(mu+1.96*sigma)^2, lty=2)
lines(c(32:41),(mu-1.96*sigma)^2, lty=2)

plot(c(hare,(mu)^2+(sigma)^2),type='o',ylab='Hare')
lines(c(32:41),(mu+1.96*sigma)^2, lty=2)
lines(c(32:41),(pmax((mu-1.96*sigma),rep(0,length(mu))))^2, lty=2)

## dataset oil price

data(oil.price)
plot(oil.price, type = 'o')
length(oil.price)
oilp=ts(oil.price[1:(241-12)],start=c(1986,1),freq=12)

BoxCox.ar(oilp,lambda = seq(-0.5, 0.5, 0.01))
adf.test(log(oil.price))
yt = diff(log(oilp))
adf.test(yt)
plot(yt)

par(mfrow=c(2,1))
acf(yt)
pacf(yt)
eacf(yt)


arima(yt, order = c(2,0,0),include.mean=F)
arima(log(oilp), order = c(2,1,0),include.mean=F)
arima(log(oilp), order = c(0,1,1),include.mean=F)
arima(log(oilp), order = c(2,1,2),include.mean=F)
arima(log(oilp), order = c(1,1,2),include.mean=F)

oil.mod1=arima(log(oilp), order = c(2,1,0),include.mean=F)
oil.mod2=arima(log(oilp), order = c(0,1,1),include.mean=F)
oil.mod3=arima(log(oilp), order = c(1,1,0),include.mean=F)

res1 = residuals(oil.mod1)
res2 = residuals(oil.mod2)
res3 = residuals(oil.mod3)

## model 1
par(mfrow=c(3,1))
plot(res1 , type = 'o')
hist(res1)
qqnorm(res1); qqline(res1)

t.test(res1)
shapiro.test(res1)

par(mfrow=c(1,1))
acf(res1)
Box.test(res1 , lag = 13,type = "Ljung")
Box.test(res1 , lag = 15,type = "Ljung")
Box.test(res1 , lag = 20,type = "Ljung")

## model 2
par(mfrow=c(3,1))
plot(res2 , type = 'o')
hist(res2 )
qqnorm(res2 ); qqline(res2 )

t.test(res2 )
shapiro.test(res2 )

par(mfrow=c(1,1))
acf(res2 )
Box.test(res2 , lag = 13,type = "Ljung")
Box.test(res2 , lag = 15,type = "Ljung")
Box.test(res2 , lag = 20,type = "Ljung")

## model 3
par(mfrow=c(3,1))
plot(res3, type = 'o')
hist(res3)
qqnorm(res3); qqline(res3)

t.test(res3)
shapiro.test(res3)

par(mfrow=c(1,1))
acf(res3)
Box.test(res3, lag = 2,type = "Ljung")
Box.test(res3, lag = 15,type = "Ljung")
Box.test(res3, lag = 20,type = "Ljung")


## Compare

oil.mod1$aic
oil.mod2$aic
oil.mod3$aic


## Prediction

plot(oil.mod1,transform = exp,n1=c(2004,1),n.ahead=12)
plot(oil.mod2,transform = exp,n1=c(2004,1),n.ahead=12)
plot(oil.mod3,transform = exp,n1=c(2004,1),n.ahead=12)


plot(oil.mod1,transform = exp,n1=c(2004,1),n.ahead=12)
points(ts(oil.price[(241-11):241],start=c(2005,2),freq=12),pch=8)
plot(oil.mod2,transform = exp,n1=c(2004,1),n.ahead=12)
points(ts(oil.price[(241-11):241],start=c(2005,2),freq=12),pch=8)
plot(oil.mod3,transform = exp,n1=c(2004,1),n.ahead=12)
points(ts(oil.price[(241-11):241],start=c(2005,2),freq=12),pch=8)

## Prediction on deterministic trend
# Exhibit 9.2
# append 2 years of missing values to the tempdub data as we want to forecast
# the temperature for two years.
data(tempdub)
tempdub1=ts(c(tempdub,rep(NA,24)),start=start(tempdub),freq=frequency(tempdub)) 
plot(tempdub1)

# creates the first pair of harmonic functions and then fit the model
har.=harmonic(tempdub,1)
m5.tempdub=arima(tempdub,order=c(0,0,0),xreg=har.)
m5.tempdub

# The result is same as that from the fit using lm function.
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)

# Compute and plot the forecasts.
newhar.=harmonic(ts(rep(1,24), start=c(1976,1),freq=12),1)
win.graph(width=4.875, height=3,pointsize=8)
plot(m5.tempdub,n.ahead=24,n1=c(1972,1),newxreg=newhar.,
 type='b',ylab='Temperature',xlab='Year')
predict(m5.tempdub,n.ahead=24,newhar.) 


## Overfitted/Overdifference

# Simulation
n = 200

et = rnorm(n)
arima(et,order = c(1,0,1),include.mean = FALSE)
arima(et,order = c(0,1,1),include.mean = FALSE)


et=rnorm(100)
phi=0.57
yt = c(et[1]/sqrt(1-phi^2))

for(i in 2:100){

yt=c(yt,phi*yt[i-1] + et[i])

}

par(mfrow=c(2,1))
acf(yt)
pacf(yt)
eacf(yt, ar.max = 5, ma.max = 5)

arima(yt, order = c(2,0,1))

#dataset color
m1.color=arima(color, order = c(1,0,0))
win.graph(width=4.875, height=4.5)
tsdiag(m1.color,gof=15,omit.initial=F) 
m1.color

m2.color=arima(color,order=c(2,0,0))
m2.color

m3.color=arima(color,order=c(1,0,1))
m3.color

m4.color=arima(color,order=c(2,0,1))
m4.color

