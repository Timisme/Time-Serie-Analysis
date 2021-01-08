library(TSA)

data(deere1)


# 6.33 p.157 sol p.584
win.graph(width=9,height=6,pointsize=8)
plot(deere1, type= 'o', ylab= 'Deviation', main= 'deere1')
deere1[27] # outlier

acf(deere1)

deere1mod = deere1
deere1mod[27] = 7 #§ó§ïÂ÷¸s­È

acf(deere1mod)

#(d)
pacf(deere1mod)

# ------- 7.28

data(deere3)
win.graph(width=9,height=6,pointsize=8)
plot(deere3, type= 'o', ylab= 'Deviation', main= 'deere3')

#(a)
arima(deere3, order=c(1,0,0))

#(b)
arima(deere3, order= c(2,0,0))

# -------- 7.29 

data(robot)

# (a)
plot(robot, type= 'o', ylab= 'Distance', main= 'robot')
arima(robot, order= c(1,0,0))

# (b)
arima(robot, order= c(0,1,1))


# ------ 7.30 p.610

data(days)

plot(days, type= 'o', ylab= '# of Days', main= 'days')

daysmod=days; daysmod[63]=35; daysmod[106]=35; daysmod[129]=35

arima(daysmod, order= c(0,0,2))

# (b)

arima(daysmod, order= c(0,0,5))


x = c(1,2,3)
y = c(10,20,30)
plot(x[2:n],y[2:n])
