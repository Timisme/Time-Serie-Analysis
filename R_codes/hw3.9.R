library(TSA)
win.graph(width=10, height=7,pointsize=12)
data(prescrip);plot(prescrip,type='o',col='4',main='Prescrip Data Plot')
points(y=prescrip,x=as.vector(time(prescrip)),
       pch=as.vector(season(prescrip)),cex=0.8)
#pct

pct <- diff(prescrip)/prescrip
plot(pct,type='o',col='4',main='Prescrip Percentage Change Plot')
points(y=pct,x=as.vector(time(pct)),pch=as.vector(season(pct)),cex=0.8)
# fit cosine wave to the model 

model_cosine <- lm(pct~harmonic(pct,1))
summary(model_cosine)

rstudent(model_cosine)

plot(model_cosine)
# 12 when the data are sampled monthly and the natural time period is a year
plot(ts(fitted(model_cosine),freq=12,start=c(1986,9)),type='l',ylab='pct',
     ylim=range(c(fitted(model_cosine),pct)),col='4',main='Percentage Change Plot')
points(pct,x=as.vector(time(pct)))

plot(y=rstudent(model_cosine),x=as.vector(time(pct)),
     xlab='Time',ylab='Standard Residual',type='o',col='4',main='Residual Plot')
points(y=rstudent(model_cosine),x=as.vector(time(pct)),pch=as.vector(season(pct)))

phi <- seq(-1, 1, 0.01)
var_ybar <- (1 + phi) / (1 - phi)
plot(var_ybar~phi)
