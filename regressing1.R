#R hackaton ML chicago 11.03.2020
library(raster)
library(rgdal)
r = raster("C:/Roma/ML/hackathon 04-2020/databases/2017120_ASCII_Data/US_SC15_PUD.TXT")
plot(r)
dat<-read.table("C:/Roma/ML/hackathon 04-2020/databases/2017120_ASCII_Data/US_SC15_PUD.TXT",
                header=FALSE)
hist(cars$speed)

typeof(dat)

dat <- data.frame(dat)
dim(dat)

rm( list=ls() ) #remove all your variables and data.

v = c(1,2,3,4)# to execute a line of code
              #press ctrl+enter
u = c(3,-1,0,1)

ls() #lists all the variables and data in the session

inp = sum(v*u)
inp
#create a matrix: 
A = cbind( c(1,2,3),c(4,5,6),c(0,1-1) )

#importing some data
data = cars
head(data) #shows us the first 
data
help(cars)
#ploting : there are two ways
#   1 using simple plot
#   2 using advanced ggplot2

plot(cars$speed, cars$dist, main="first plot",
     xlab="speed", ylab="stop distance",
     xlim=c(0,25),ylim=c(0,100)) 
lines(c(5,25),c(-5,70),col="red")

#correlations
# correlation between car speed and distance it takes
#to stop it
cor(cars$speed, cars$dist)

#easy linear regression using lm(
help(lm)
fit1 = lm(cars$dist ~ cars$speed)
fit1
names(fit1) # get the subvariables inside fit1
typeof(fit1) # kind of variable fit1 is
fit1$coefficients

inter = fit1$coefficients[1]
slope = fit1$coefficients[2]

x = seq(1,200)
y = inter + slope*x
lines(x,y,col="blue")

input = 9
dist = inter+slope*input
as.numeric(dist)

input = 10
dist = inter+slope*input
as.numeric(dist)

rss = mean(fit1$residuals^2)
sqrt(rss)
quad = cars$speed

#new fit with a quadratic term
fit2 = lm(cars$dist ~ cars$speed + quad)
rss = mean(fit2$residuals^2)
sqrt(rss)
quad = cars$speed

#data set 3
d3
head(d3)
fit3 = lm(d3$G1.y ~d3$studytime.x )
rss = mean(fit3$residuals^2)
sqrt(rss)


fit4 = lm(d3$G1.y ~d3$studytime.x + d3$goout.x )
rss = mean(fit4$residuals^2)
sqrt(rss)


#need to go to https://archive.ics.uci.edu/ml/datasets/student+performance
#and download the database
install.packages("ggplot2")
library("ggplot2")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

fit2 = lm(d3$romantic.x ~d3$G2.x)
help(ggplot2)

cov( cbind( d3$studytime.x , d3$Dalc.x, d3$failures.x ) )

