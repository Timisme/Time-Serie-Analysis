library(pacman)

pacman::p_load(dplyr...)


# ========== clean up ===========
p_unload(dplyr) # clear specific package
p_unload(all) # clear all packages
detach('package:""',unload= TRUE) # For base

# ========== Graphics ===========

library(datasets)

head(iris)

# ?plot get help 

plot(iris$Species) # categorical data 
plot(iris$Petal.Length) # quantitative variable
plot(iris$Species, iris$Petal.Width) # box plot (x: cate, y: conti)
plot(iris$Sepal.Length, iris$Petal.Width, # scatter plot
     col= 'red',
     pch= 19, #solid circles for points (point character)
     main= 'example plot',
     xlab = 'x',
     ylab = 'y'
     )

# function plot 

plot(cos, 0, 2*pi)
plot(exp, 1, 5)
plot(dnorm, -3, 3) # standard normal distribution

# bar plot 

head(mtcars)

barplot(mtcars$cyl) # what?

# a table with freq for each category 
cylinders = table(mtcars$cyl)
barplot(cylinders)


# hist plot 

hist(iris$Sepal.Length)

# put graphs in 3 rows and 1 col 
par(mfrow = c(3, 1)) # c - concatenate

hist(iris$Petal.Width [iris$Species == 'setosa'],
     xlim = c(0,3),
     breaks = 9) # breaks = num of bars

# restore graphic param 
par(mfrow= c(1,1))
