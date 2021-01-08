library(TSA)
library(tseries)
data(deere3)

plot(deere3, type='o', ylab= 'Deviation',main='Deviations with Time for deere3 dataset')

acf(deere3)
win.graph(width=9,height=6,pointsize=8)
pacf(deere3)
eacf(deere3)

adf.test(deere3)

#----------- 6.36
  
data(robot); plot(robot,type='o',ylab='Robot End Position')
acf(robot)
pacf(robot)
eacf(robot)
plot(armasubsets(y=robot,nar=14,nma=14,y.name='Robot',ar.method='ols'))
# ---------- 6.39

## if rk > crital value ---> ho : rk = 0 is rejected 
## yt ~ (e(yt), sigma^2)
## 95% prob the actual sample value falls between 0 +- 1.96 * 1/square n

data(days)
plot(days,type='o',ylab='Days Until Payment',xlab='Order')
acf(days)
pacf(days)

days
boxplot(days)

days[days>45]

# --------------- 改離群值
daysmod=days; daysmod[63]=35; daysmod[106]=35; daysmod[129]=35
plot(daysmod,type='o',ylab='Days Until Payment',xlab='Order')
acf(daysmod, main= '去除離群值後的acf')
pacf(daysmod, main= '去除離群值後的pacf')
eacf(daysmod)


