library(TSA)
library(tseries)
setwd("C:/Users/tom/Desktop/109-1課程/時間序列分析/期末專題")

df_aal = read.csv('AAL.csv')
df_covid = read.csv('us-covid.csv')

dates = as.Date(df_aal$Date,"%Y-%m-%d")
df_covid_new = df_covid[as.Date(df_covid$date,"%Y/%m/%d") %in% dates, ]

aal_data = df_aal$Close
covid_data = df_covid$cases

covid = df_covid_new$cases[101:240]
aal = aal_data[101:240]

# raw data 相關性檢定

plot(covid, aal)
ccf(covid, aal, na.action = na.omit) ## 需要prewhitening 

## stationary test 

adf.test(diff(diff(covid)))
kpss.test(diff(diff(covid)))

# acf

acf(diff(diff(covid)))
pacf(diff(diff(covid)))
# prewhitening 

library(forecast)

# auto 
auto.arima(covid,lambda="auto", seasonal = TRUE, trace= TRUE)
auto.arima(covid_data[101:346],lambda="auto", seasonal = TRUE, trace= TRUE)

## 組員

# sarima
# mod = arima(covid_data[101:346], order= c(0,2,2), seasonal = list(order= c(1,0,0), period= 7))
mod = arima(covid_data[101:346], order= c(1,2,1), seasonal = list(order= c(0,1,1), period= 7))

t.test(residuals(mod))
shapiro.test(residuals(mod))
acf(residuals(mod), na.action = na.omit)
pacf(residuals(mod), na.action = na.omit)
Box.test(residuals(mod), lag = 7,type = "Ljung")
# Box.test(residuals(mod), lag = 7,type = "Ljung")
AIC(mod)
BIC(mod)

# 直接arima
mod2 = arima(covid_data[101:346]^0.1688343, order= c(7,2,2))
res2 = mod$residuals
acf(res2)
pacf(res2)
AIC(mod2)
BIC(mod2)

## tbc
# test for arima 
test_mod = arima(covid^0.2, order= c(5,2,2))
acf(residuals(test_mod), 60)
pacf(residuals(test_mod), 60)
Box.test(residuals(test_mod), lag = 19,type = "Ljung")

# sarima
mod = arima(log(covid), order= c(0,2,2), seasonal = list(order= c(1,0,0), period= 5))
# mod = arima(covid^0.2, order= c(5,2,2))
AIC(mod)
x_new = mod$residuals

detectAO(mod)
detectIO(mod)

#檢驗是否為white noise

hist(x_new)
t.test(x_new)
shapiro.test(x_new)
acf(x_new, 60)
pacf(x_new, 60)
Box.test(x_new, lag = 19,type = "Ljung")

# 對原始 y 配適相同model
y_mod = arima(log(aal), order= c(3,1,0))
res_y = y_mod$residuals

# y_new = residuals(Arima(aal, model = mod))

ccf(x_new, res_y, main= 'CCF plot') #依照sarima配適後殘差

# 回歸模型
xt = x_new[1:139]
yt_lag1 = res_y[2:140]


mod2 = lm(yt_lag1~0+xt)
rsq = (sum(yt_lag1^2)-sum(mod2$residuals^2))/sum(yt_lag1^2)
rsq
summary(mod2)

xt_7 = x_new[1:133]
yt_lag7 = res_y[8:140]

mod3 = lm(yt_lag7~0+xt_7)
summary(mod3)

xt_7 = x_new[14:140]
yt_lag7 = res_y[1:127]
mod3 = lm(yt_lag7~0+xt_7)
summary(mod3)
