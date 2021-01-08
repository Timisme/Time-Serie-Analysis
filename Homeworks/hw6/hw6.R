library(dplyr)
library(xts)
library(lattice)
df <- read.csv("C:/Users/tom/Desktop/109-1課程/時間序列分析/hw6/^GSPC.csv", header=TRUE, sep=",",row.names="Date")
# win.graph(width=9,height=6,pointsize=8)
# plot(x= as.Date(row.names(df)),y = df$Adj.Close,xlab='2019 Date',ylab = 'Price',type='l', main='JNJ')
# df_xts <- as.xts(df['Adj.Close'])
# xyplot(df_xts, grid=TRUE)

ts <- xts(df$Adj.Close, as.Date(row.names(df), "%Y-%m-%d"))

# ts_q = apply.quarterly(ts,mean)
plot(ts, ylab= 'Price',col='blue',main='SP500',type='l')

log_price <- log(df$Adj.Close)
log_df <- data.frame('Adj.Close'=log_price)
row.names(log_df) <- row.names(df)
log_ts <- as.xts(log_df$Adj.Close,as.Date(row.names(df), "%Y-%m-%d"))
plot(log_ts, col='red',axes= FALSE, xlab = "", ylab = "",type='l')
# xyplot(log_df, grid=TRUE)
# win.graph(width=9,height=6,pointsize=8)
# plot(x= as.Date(row.names(df)),y = log_price,xlab='2019 Date',ylab = 'logged Price',type='l', main='JNJ')


# win.graph(width=9,height=6,pointsize=8)
plot(x= as.Date(row.names(slice(df,2:n()))),y = diff(df$Adj.Close)/lag(df$Adj.Close,1)[2:length(df$Adj.Close)],
     xlab='2019 Date',ylab = 'Price Pct',type='o',col='red', main='JNJ')
lines(as.Date(row.names(slice(df,2:n()))),diff(log_price),type='l',col="yellow")
# win.graph(width=9,height=6,pointsize=8)
# plot(x= as.Date(row.names(slice(df,2:n()))),y = diff(log_price),xlab='2019 Date',ylab = 'log Price Pct',type='l', main='JNJ')


