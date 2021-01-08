library(TSA)
library(fGarch)
library(tseries)
data(usd.hkd)
abs(usd.hkd$hkrate)

plot(abs(usd.hkd$hkrate),type='l', xlab='day', ylab='absolute return', main= 'hkrate')


plot(ts(abs(usd.hkd$hkrate),freq=1), type= 'l')

plot(usd.hkd$hkrate^2,type='l', xlab='day', ylab='squared return', main= 'hkrate')


#12.9 

data(google)
plot(google)

acf(google^2)
pacf(google^2)

t.test(google, alternative = 'greater')

# McLeod-Li test

McLeod.Li.test(y= google)
# The McLeod-Li test suggests significant ARCH effects in the Google return series

# (d)
# °t¾A garch(1,1)

model = garch(google, order= c(1,1), na.action= na.omit)
summary(model)

res = residuals(model)
plot(res)

acf(res[2:n])
# (e)
plot(fitted.values(model))

#(f)

qqnorm(res); qqline(res)

# (g)
confint(model, 'b1', level=0.95)

# (h)
var(google)
mean(google)
## 12.9 ³Ì«á

# Monte Carlo Simulation

N = 200000
sim_h1 = rep(NA, N)
sim_h2 = rep(NA, N)
sim_h3 = rep(NA, N)
sim_h4 = rep(NA, N)
sim_h5 = rep(NA, N)

# set.seed(1234567)
for (i in 1:N){
  garch11.sim=garch.sim(alpha=c(0.00005302,0.1426),beta=0.7671,n=5)
  sim_h1[i] = garch11.sim[1]
  sim_h2[i] = garch11.sim[2]
  sim_h3[i] = garch11.sim[3]
  sim_h4[i] = garch11.sim[4]
  sim_h5[i] = garch11.sim[5]
  rm(garch11.sim)
}

h1_ci = quantile(sim_h1, c(.025, .975))
h2_ci = quantile(sim_h2, c(.025, .975))
h3_ci = quantile(sim_h3, c(.025, .975))
h4_ci = quantile(sim_h4, c(.025, .975))
h5_ci = quantile(sim_h5, c(.025, .975))

h5_ci


# plot(garch11.sim,type='l',ylab=expression(r[t]), xlab='t')
# sim = replicate(n= 100, garch11.sim)
# sim = garch.sim(alpha=c(0.00005302,0.1426),beta=0.7671,n=5)
# sim[1]
#               