rm(list=ls())
library(quantmod)
library(e1071)
library(ggplot2)
library(moments)


data=data.frame(read.csv("Lect-1-TradingTS.csv"))
data=data[complete.cases(data), ]

price = data$price
tick = data$tick


s=0
m=0
Sum_s=0
Sum_m=0
price.seconds <- c()
price.minutes <- c()
for(i in 1:length(price)){
  s=s+1
  m=m+1
  Sum_s=Sum_s+price[i]
  if(s==5){
    price.seconds<- append(price.seconds,Sum_s/5)    
    Sum_s=0
    s=0
  }
  Sum_m=Sum_m+price[i]
  if(m==300){
    price.minutes<- append(price.minutes,Sum_m/300)    
    Sum_m=0
    m=0
  }
}


log.return.minutes<-c()
for(i in 2:length(price.minutes)){
  log.return.minutes <-append(log.return.minutes,log(price.minutes[i]/price.minutes[i-1]))
}
summary(log.return.minutes)
sd(log.return.minutes)
# Measure of the symmetry of the data 
skewness(log.return.minutes)
#Tail Shape of the data distribution
kurtosis(log.return.minutes)
plot(log.return.minutes)
plot(log.return.minutes, log="y", type='h', lwd=10, lend=2)
hist(log.return.minutes, breaks=100)


log.return.seconds<-c()
for(i in 2:length(price.seconds)){
  log.return.seconds <-append(log.return.seconds,log(price.seconds[i]/price.seconds[i-1]))
}
summary(log.return.seconds)
sd(log.return.seconds)
# Measure of the symmetry of the data 
skewness(log.return.seconds)
write.csv(log.return.minutes,"seconds.csv")
#Tail Shape of the data distribution
kurtosis(log.return.seconds)
plot(log.return.seconds)
plot(log.return.seconds, log="y", type='h', lwd=10, lend=2)
hist(log.return.seconds, breaks=140)

plot(price.minutes)

qqnorm(log.return.minutes)
