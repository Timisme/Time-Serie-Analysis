library(TSA)
library(pander)
win.graph(width=7, height=5,pointsize=12)
data(winnebago); plot(winnebago,type='o',col='4',main='Winnebago Data Plot')

winnebago_fit_linear <- lm(winnebago ~ time(winnebago))
summary(winnebago_fit_linear)
abline(winnebago_fit_linear,col='red')

plot(y = rstudent(winnebago_fit_linear), x = as.vector(time(winnebago)),type='o', 
     xlab = "Time", ylab = "Studentized residuals",col='4',main='Residual Plot')

# logged
plot(log(winnebago),type='o',,col='4',main='logged Winnebago Data Plot')
winnebago_log_fit_linear <- lm(log(winnebago) ~ time(winnebago))
abline(winnebago_log_fit_linear,col='red')
summary(winnebago_log_fit_linear)

plot(x = as.vector(time(winnebago)),y = rstudent(winnebago_log_fit_linear),
     type='o', xlab='Time',ylab='Studentized residuals',main='Residual Plot of logged data from the linear model',
     col='4')

# fit seasonal means and linear time trend

win_log_fit_seasonal_linear <- lm(log(winnebago)~season(winnebago)+time(winnebago))
summary(win_log_fit_seasonal_linear)

plot(x=as.vector(time(winnebago)),y=rstudent(win_log_fit_seasonal_linear),
     type='o',xlab='time',ylab='Studentized residuals',col='4',
     main='Residual Plot after seasonal factor is considered')
