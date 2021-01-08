library(TSA)

# randomwalk

n = 100
et = rnorm(n,0,1)
yt = c(et[1])
for(i in 2:n){
yt = c(yt,yt[i-1]+et[i])
}

win.graph(width=4.875, height=2.5,pointsize=8)
plot(yt,type='o',ylab='y')

# deterministic trend
n = 100
et = rnorm(n,0,2)
ti = seq(1:n)
mt = 0.2*ti + 1
yt = mt + et
plot(yt,type='l',ylab='y')

model1=lm(yt~time(yt))
summary(model1)

# fitted and original values  
win.graph(width=4.875, height=2.5,pointsize=8)
plot(yt,type='o',ylab='y')
abline(model1,col='red')


model1=lm(yt~time(yt)+I(time(yt)^2))
summary(model1)

#######################################################

# Spencer 15-point
n = 100
et = rnorm(n,0,10)
ti = seq(1:n)
mt = 0.0015*ti^3 - 0.15*ti^2 -0.07*ti +0.5
yt = mt + et
plot(yt,type='l',ylab='y')

ar = c(-3,-6,-5,3,21,46,67,74,67,46,21,3,-5,-6,-3)/320

xt = c()
for(i in 1:(n-14)){
xt = c(xt,yt[i:(i+14)]%*%ar)
}

plot(yt,type='l',ylab='y')
points(seq(8,(n-7)),xt)

plot(yt[8:(n-7)]-xt,type='o')
abline(0,0)

#######################################################

# quadratic regression

n = 100
et = rnorm(n,0,10)
ti = seq(1:n)
mt = 0.15*ti^2 -0.07*ti +0.5
yt = mt + et
plot(yt,type='l',ylab='y')

model2 = lm(yt~ti+I(ti^2))
summary(model2)

pred <- predict(model2, newdata=data.frame(ti)) 
lines(ti, pred) 

# fitted and original values  
win.graph(width=4.875, height=2.5,pointsize=8)
plot(yt,type='l',ylab='y')
points(ti, pred) 

# residual values
plot(y=rstudent(model2),x=as.vector(time(yt)),ylab='Standardized Residuals',
xlab='Time',type='o')

# histogram and QQ-plot of residuals
par(mfrow=c(1,2))
hist(rstudent(model2),xlab='Standardized Residuals',main='')
qqnorm(rstudent(model2),main='')
qqline(rstudent(model2)) 


# ACF
win.graph(width=4.875, height=2.5,pointsize=8)
acf(rstudent(model2))


#######################################################


# data set: temperature
data(tempdub)
month.=season(tempdub)
win.graph(width=4.875, height=2.5,pointsize=8)
plot(tempdub,ylab='y')
points(tempdub,pch=as.vector(month.))
monthplot(tempdub)


# fit seasonal regression model without intercept term 
month.=season(tempdub) # the period sign is included to make the printout from
# the commands two line below clearer; ditto below.
model2=lm(tempdub~month.-1) # -1 removes the intercept term 
summary(model2)


# fit seasonal regression model with intercept term
month.=season(tempdub)
model3=lm(tempdub~month.) # -1 removes the intercept term 
summary(model3)

# fit cosine regression model
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.-1) # -1 removes the intercept term 
summary(model4)

# fitted and original values 
win.graph(width=4.875, height=2.5,pointsize=8)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
ylim=range(c(fitted(model4),tempdub))) # the ylim option ensures that the 
# y axis has a range that fits the raw data and the fitted values
points(tempdub)


% residual values
plot(y=rstudent(model2),x=as.vector(time(tempdub)),ylab='Standardized Residuals',
xlab='Time',type='o')

plot(y=rstudent(model4),x=as.vector(time(tempdub)),ylab='Standardized Residuals',
xlab='Time',type='o')

% histogram and QQ-plot of residuals
par(mfrow=c(1,2))
hist(rstudent(model2),xlab='Standardized Residuals',main='')
qqnorm(rstudent(model2),main='')
qqline(rstudent(model2)) 

par(mfrow=c(1,2))
hist(rstudent(model4),xlab='Standardized Residuals',main='')
qqnorm(rstudent(model4),main='') 
qqline(rstudent(model4)) 

% ACF
win.graph(width=4.875, height=2.5,pointsize=8)
acf(rstudent(model2))

win.graph(width=4.875, height=2.5,pointsize=8)
acf(rstudent(model4))

#######################################################

data(retail)

#(a) Time plot
win.graph(width=4.875, height=2.5,pointsize=8)
plot(retail,type='l',ylab='Prescription')
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(retail,pch=Month)

#(b)

month.=season(retail)
model2=lm(retail~time(month.)+month.-1)
summary(model2)

#(c)

res=ts(rstudent(model2),freq=12,start=c(1986,1))
plot(res,ylab='Standardized Residuals',type='l')
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(res,pch=Month)

# residual diagnosis

par(mfrow=c(1,2))
hist(rstudent(model2),xlab='Standardized Residuals',main='')
qqnorm(rstudent(model2),main='') 
qqline(rstudent(model2)) 

win.graph(width=4.875, height=2.5,pointsize=8)
acf(rstudent(model2))

#########PM2.5 data########################################

pm25 = read.csv("F:\\PM25.csv")

win.graph(width=4.875, height=2.5,pointsize=8)
plot(pm25$Mean_Concentration,type='l',ylab='PM2.5緻',main='糳芠代(2006~2015 A.D.)')

acf(pm25$Mean_Concentration,lag=508)
pacf(pm25$Mean_Concentration,lag=508)

# Boxcox Power Transformation
#BoxCox.ar(pm25$Mean_Concentration,lambda = seq(0.5, 1.5, 0.01))
#res_box = boxcox(pm25$Mean_Concentration, objective.name = "Log-Likelihood")

pm2.5 = pm25$Mean_Concentration

# The estimated trend component: 

ti = 1:length(pm2.5)
model1 = lm(pm2.5~ti+I(ti^2))
summary(model1)

win.graph(width=4.875, height=5,pointsize=8)
plot(fitted(model1),type='l',ylab='Trend',xlab='Time',main='糳芠代(Quadratic Trend)')

pm2.5 = pm2.5 - fitted(model1)

# The estimated seasonal component:

harmonic_adj = function(data,pair,s){
  time_lags = 1:length(data)
  
  res = data.frame()
  col_name = c()
  for(i in 1:pair){
    cos_temp = cos(2*pi*i*time_lags/s)
    sin_temp = sin(2*pi*i*time_lags/s)
    res = rbind(res,cos_temp,sin_temp)
    col_name = c(col_name,paste("Cosine",i),paste("Sin",i))
  }
  res = t(res)
  colnames(res) = col_name
  rownames(res) = time_lags
  return(data.frame(res))
}

h = harmonic_adj(pm2.5,3,51)
model2 = lm(pm2.5~.,data=h)
summary(model2)

win.graph(width=4.875, height=5,pointsize=8)
plot(fitted(model2),type='l',ylab='Trend',xlab='Time',main='糳芠代(Seasonal Trend)')


# The estimated irregular component:

irr = pm2.5 - fitted(model2)
win.graph(width=4.875, height=5,pointsize=8)
plot(irr,type='l',ylab='Trend',xlab='Time',main='糳芠代(Irregular Component)')


