library(TSA)
library(tseries)
setwd("C:/Users/tom/Desktop/109-1課程/時間序列分析/期末專題")

df_aal = read.csv('AAL.csv')
df_covid = read.csv('us-covid.csv')

dates = as.Date(df_aal$Date,"%Y-%m-%d")
df_covid_new = df_covid[as.Date(df_covid$date,"%Y/%m/%d") %in% dates, ]

aal_data = df_aal$Close
covid_data = df_covid$cases


# -------------------------------- 資料轉換--------------------------------
# 平穩檢定

plot(aal_data, main='AAL stock price', xlab= 'day', ylab= 'price', type='o')
plot(covid_data, main='covid total cases in US', xlab= 'day', ylab= 'cases', type= 'o')

adf.test(aal_data) #非定態
kpss.test(aal_data)

adf.test(covid_data) #非定態
kpss.test(covid_data)

adf.test(diff(diff(log(covid_data[38:280]))))
kpss.test(diff(diff(log(covid_data[38:280]))))

# 插分後檢定

# ------------------------ aal -------------------------
# Q : aal -> 一階差分跟log return都通過，選哪個？

plot(diff(aal_data))
adf.test(diff(aal_data)) # 一階插分平穩
kpss.test(diff(aal_data)) #一階插分平穩

plot(diff(log(aal_data)))
adf.test(diff(log(aal_data))) # log return pass 平穩
kpss.test(diff(log(aal_data))) # log return pass

# ------------------------ covid -------------------------
# Q: 二次差分才通過 adf & kpss？

plot(log(covid_data))
plot(diff(log(covid_data))*100)
plot(diff(diff(covid_data))) 

# log -> 檢定仍不平穩，還要再差分？

adf.test(log(covid_data[38:280])) # 有過
kpss.test(log(covid_data[38:280])) # 沒過

# Q: log return -> 檢定仍不平穩，還要再差分？

adf.test(diff(log(covid_data[38:280]))) # 沒過
kpss.test(diff(log(covid_data[38:280]))) # 沒過

#二次插分就平穩
adf.test(diff(diff(covid_data[38:280]))) #二次插分有過
kpss.test(diff(diff(covid_data[38:280]))) # 有過

covid_data

# ------------------------------ 候選模型 ------------------------------

# aal log return 

acf(diff(log(aal_data)))
pacf(diff(log(aal_data))) #ar(1), ar(3)
eacf(diff(log(aal_data))) #arma(1,1)

# covid

acf(log(covid_data))
# Q：二階差分 ----> 人數二階差分後 acf & pacf 感覺有7天週期的原因？
acf(diff(diff(covid_data[38:280])), lag= 60) # acf感覺有點sarima?
pacf(diff(diff(covid_data[38:280]))) #感覺要配sarima
eacf(diff(diff(covid_data[38:280]))) #勉強arma(1,1)


# log return -----> 完全母湯
acf(diff(log(covid_data[38:280]))) # acf看就知道不stationary
pacf(diff(log(covid_data[38:280])))
eacf(diff(log(covid_data[38:280]))) # arma(1,0)

acf(diff(diff(log(covid_data[38:280]))), lag= 60)
pacf(diff(diff(log(covid_data[38:280]))), lag= 60)
eacf(diff(diff(log(covid_data[38:280]))))

acf(covid_data)
acf(diff(covid_data))

acf(diff(diff(covid_data[38:280])), lag= 60)
acf(diff(diff(diff(covid_data)),lag= 7), lag= 60)


# ------------------------------ 配適模型 ------------------------------


# ------------------------ aal ------------------------- 
# arima(1,1,0) -> 僅shapiro.test沒過

mod_aal = arima(log(aal_data), order = c(3,1,1)) #arima(1,1,1)
res_aal = residuals(mod_aal)
plot(res_aal, type = 'o')
hist(res_aal)
qqnorm(res_aal); qqline(res_aal)

t.test(res_aal)
shapiro.test(res_aal) #Ha: not normal
acf(res_aal)
Box.test(res_aal, lag = 3,type = "Ljung") # ha :ts not white noise
Box.test(res_aal, lag = 7,type = "Ljung")

# aal log return 後配garch 

mod_aal = arima(log(aal_data), order = c(3,1,1)) #arima(1,1,1)
res_aal = residuals(mod_aal)

garch_aal = garch(res_aal, c(1,1))
BIC(garch_aal)
res_g = residuals(garch_aal)

plot(res_g)

# par(mfrow=c(3,1))
plot(res_g, type = 'o')
hist(res_g)
qqnorm(res_g); qqline(res_g)

t.test(res_g)
shapiro.test(res_g) #  The null hypothesis for this test is that the data are normally distributed.

par(mfrow=c(1,1))
acf(res_g[2:225])
Box.test(res_g, lag = 7,type = "Ljung")

## AAL 新
BoxCox.ar(aal_data, lambda = seq(0,1, 0.01))

aal_new = aal_data^0.25

plot(diff(aal_new))
kpss.test(diff(aal_new))
adf.test(diff(aal_new))

aal_trans = diff(aal_new)
acf(aal_trans)
pacf(aal_trans)

mod = arima(aal_trans, order= c(3,0,0))
res = residuals(mod)

garch_mod = garch(res, order= c(1,1))

summary(garch_mod)
garch_res = residuals(garch_mod)
plot(garch_res)

par(mfrow=c(3,1))
plot(garch_res[2:225], type = 'o')
hist(garch_res[2:225])
qqnorm(garch_res[2:225]); qqlinegarch_res[2:225])

t.test(garch_res[2:225])
shapiro.test(garch_res[2:225]) #  The null hypothesis for this test is that the data are normally distributed.

par(mfrow=c(1,1))
acf(garch_res[2:225])
Box.test(garch_res[2:225], lag = 7,type = "Ljung")
# Box.test(res1, lag = 10,type = "Ljung")

# ------------------------ covid -------------------------
# arima(1,2,1)
mod_covid = arima(log(covid_data[38:280]), order= c(1,1,0))
res_covid = residuals(mod_covid)
plot(res_covid, type = 'o')
hist(res_covid)
qqnorm(res_covid); qqline(res_covid)

t.test(res_covid) # 有過
shapiro.test(res_covid) #沒過 Ha: not normal
acf(res_covid) #看就知道沒過 (一定要配季節性)
Box.test(res_covid, lag = 7,type = "Ljung") # ha :ts not white noise
Box.test(res_covid, lag = 10,type = "Ljung")

# ------------------- 用sarima配covid ------------------------
# 

acf(diff(diff(diff(log(covid_data[100:280]))), lag= 7), lag= 60)
pacf(diff(diff(diff(log(covid_data[100:280]))), lag= 7), lag= 60)


mod_covid2 = arima(log(covid_data[100:280]),order=c(0,2,1),seasonal=list(order=c(1,1,0),period=7))
mod_covid2
res_sarima = residuals(mod_covid2)

acf(diff(diff(diff(covid_data[38:324])), lag= 7), lag = 60)

plot(res_sarima)
hist(res_sarima)
qqnorm(res_sarima); qqline(res_sarima)

t.test(res_sarima) # 有過
shapiro.test(res_sarima) #沒過 Ha: not normal
acf(res_sarima) #看就知道沒過 (一定要配季節性)
Box.test(res_sarima, lag = 5,type = "Ljung") # ha :ts not white noise
Box.test(res_sarima, lag = 4,type = "Ljung")


acf(diff(diff(covid_data[38:280])))


#相關性檢定

covid = df_covid_new$cases[101:240]
aal = aal_data[101:240]
ccf(covid, aal, na.action = na.omit)

## 常態

## stationary test 

adf.test(diff(diff(log(covid)), lag = 5)) #OK
kpss.test(diff(diff(log(covid)), lag = 5)) # OK

## acf test 

acf(diff(diff(log(covid)), lag = 5), 60)
pacf(diff(diff(log(covid)), lag = 5), 60)

acf(diff(log(aal)), lag= 60)
pacf(diff(log(aal)), lag= 60)

acf(diff(diff(log(covid))))
pacf(diff(diff(log(covid))))

# prewhitening 

library(forecast)

mod = arima(log(covid), order= c(0,2,1), seasonal = list(order= c(1,1,0), period= 5))
x_new = mod$residuals
y_new = residuals(Arima(log(aal), model = mod))

ccf(x_new, y_new) #依照sarima配適後殘差

prewhiten(x= diff(log(covid)), y= diff(log(aal)), ylab= 'CCF') #依照AR內建配適

xt = covid[1:139]
yt_lag = aal[2:140]

mod2 = lm(yt_lag~xt)
summary(mod2)
