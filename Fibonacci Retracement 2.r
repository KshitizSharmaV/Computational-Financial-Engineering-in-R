# Author Kshitiz Sharma
# Analysis of equity using Fibonacci retracement stratey

rm(list=ls())
library(quantmod)

c=0
ticker <- c("AAPL")
Market_Data <- new.env()

sDate <- as.Date("2017-02-01")
a<-getSymbols(ticker, env = Market_Data, from=sDate)
Combined_Market_Data <- do.call(merge, eapply(Market_Data, Ad))
Combined_Market_Data <- data.frame(Combined_Market_Data )
value=as.Date(row.names(Combined_Market_Data))
plot(Combined_Market_Data$AAPL.Adjusted,type="l",xlab="Dates",ylab="Prices")



z=c(0.764,0.618, 0.381, 0.236, 0.50)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(value, Combined_Market_Data$AAPL.Adjusted,type="l",ylab="Prices")
par(new = TRUE)
plot(z, type = "n", axes = FALSE, bty = "n", xlab = "Time", ylab = "",ylim = c(0,1))
axis(side=4, at = pretty(range(z)))
mtext("Fibonacci ratios", side=4, line=3)
abline(h=0.764,col="blue")
abline(h=0.618,col="red")
abline(h=0.5,col="yellow")
abline(h=0.381,col="green")
abline(h=0.236,col="black")
title("Fibonacci Retracement, 0.764, 0.618, 0.5, 0.381, 0.236")




# Fibonacci Retracemen
a=1
b=1
fibo=c(1,1)
for(i in 3:50){
  fibo[i]=a+b
  temp=b
  b=fibo[i]
  a=temp
}
fibo


for(i in 2:49){
  print(fibo[i]/fibo[i+1])
}

for(i in 2:48){
  print(fibo[i]/fibo[i+2])
}

for(i in 2:47){
  print(fibo[i]/fibo[i+3])
}


# 0.618, 0.381, 0.236 
# Other series are 0.50 0.764








