library(TSA)
library(tseries)
setwd("C:/Users/tom/Desktop/109-1課程/時間序列分析/期末專題")

df_eva = read.csv('2618.TW.csv')$Close

plot(df_eva)

df_ba = read.csv('BA.csv')

plot(diff(log(df_eva)))

adf.test(diff(log(df_eva)))

acf(diff(log(df_eva)), lag= 60)
pacf(diff(log(df_eva)), lag= 60)

df_oil = read.csv('CL=F.csv')
df_oil <- df_oil[!df_oil$Close == "null", ] 

ba = df_ba$Close
oil = df_oil$Close

typeof(oil)
oil = as.double(oil)

# --------------- 候選模型 ---------------

plot(ba, main='BA stock price', xlab= 'day', ylab= 'price', type='o')
plot(oil, main='Oil Price', xlab= 'day', ylab= 'Price', type= 'o')

oil = oil[oil>0] #離群值移除

adf.test(ba) #非定態
kpss.test(ba)

adf.test(oil) #非定態
kpss.test(oil)

# --------------- 平穩轉換 ---------------

# ba 

plot(diff(log(ba)))
adf.test(diff(log(ba))) #插分平穩
kpss.test(diff(log(ba))) #平穩


# oil

plot(diff(log(oil)))
adf.test(diff(log(oil))) #二次插分平穩
kpss.test(diff(log(oil))) #kpss aal平穩

# --------------- 候選模型 ---------------

# ba

acf(diff(log(ba))) # ma(0,1) ma(0,2)
pacf(diff(log(ba))) # ar(1,0) ma(2,0)
eacf(diff(log(ba))) # arma(0,3) arma(1,3)

# oil

acf(diff(log(oil)))
pacf(diff(log(oil)))
eacf(diff(log(oil)))

# --------------- 配適模型 ---------------

# ba
# sarima才合適

mod_ba = arima(log(ba), order = c(1,1,1)) #arima(1,1,0)
res_ba = residuals(mod_ba)
plot(res_ba, type = 'o')
hist(res_ba)
qqnorm(res_ba); qqline(res_ba)

t.test(res_ba)
shapiro.test(res_ba) #Ha: not normal
acf(res_ba, lag= 60)
Box.test(res_aal, lag = 3,type = "Ljung") # ha :ts not white noise
Box.test(res_aal, lag = 7,type = "Ljung")
