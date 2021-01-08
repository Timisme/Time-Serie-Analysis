library(dplyr)
library(xts)
library(lattice)

df <- read.csv("C:/Users/tom/Desktop/109-1課程/時間序列分析/hw6/JNJ.csv", header=TRUE, sep=",")
df$log_close <- log(df$Adj.Close)
ts <- xts(df$Adj.Close,as.Date(row.names(df), "%Y-%m-%d"))
ts_log <- xts(log(df$Adj.Close),as.Date(row.names(df), "%Y-%m-%d"))
# win.graph(width=9,height=6,pointsize=8)
plot(ts)

# win.graph(width=9,height=6,pointsize=8)
plot(ts_log,ylim=c(min(ts),5.1))

# BC=BoxCox.ar(df$Adj.Close)
# BC=BoxCox.ar(df$Adj.Close,lambda=seq(1,8,1))
plot(as.Date(df$Date), df$Adj.Close,type='o')
plot(as.Date(df$Date), df$log_close,type='o')

