library(TSA)

## oil prices

win.graph(width=4.875,height=3,pointsize=8)
data(oil.price)
plot(ts(oil.price[1:(12*13)],start = 1986, frequency = 12), ylab='Price per Barrel',type='l')

win.graph(width=4.875,height=3,pointsize=8)
data(oil.price)
plot(oil.price, ylab='Price per Barrel',type='l')

## IMA(2,2)

n = 200
et = rnorm(n+2)
theta1 = 1
theta2 = -0.6
Y0 = 0
Y1 = 2*Y0 + et[3] - theta1*et[2] - theta2*et[1]
Y2 = 2*Y1 - Y0 + et[4] - theta1*et[3] - theta2*et[2]
Yt = c(Y1,Y2)

for(i in 3:n){
j=i+2;
Yt = c(Yt, 2*Yt[i-1] - Yt[i-2] + et[j] - theta1*et[j-1] - theta2*et[j-2] )
}

plot(Yt)
acf(Yt)
acf(Yt,lag=50)
plot(diff(Yt),type='l')
acf(diff(Yt))
acf(diff(Yt),lag=50)
plot(diff(Yt,2),type='l')## incorrect difference
acf(diff(Yt,2),lag=50) ## incorrect difference
plot(diff(diff(Yt)),type='l')
acf(diff(diff(Yt)))
acf(diff(Yt,differences =2))

## electricity data

data(electricity)
plot(electricity,ylab='electricity')
BoxCox.ar(electricity)
plot(log(electricity),ylab='Log(electricity)')
acf(log(electricity))
plot(diff(log(electricity)), ylab='Difference of Log(electricity)' )
acf(diff(log(electricity)))


## Import Data

data = read.csv('F:\\sp2017.csv')
data[1:10,]
data = read.csv('F:\\sp2017.csv',head = F)
data = read.table('F:\\sp2017.txt')
data[1:10,]
price = data[,6]
plot(price,type='l')
data = read.csv('F:\\GSPC.csv',head = T)