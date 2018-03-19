# Author Kshitiz Sharma
# Analysis of equity using Fibonacci retracement stratey

rm(list=ls())
library(quantmod)

# Downloading equity Data
ticker <- c("AAPL")
Market_Data <- new.env()
sDate <- as.Date("2017-02-01")
a<-getSymbols(ticker, env = Market_Data, from=sDate)
Combined_Market_Data <- do.call(merge, eapply(Market_Data, Ad))
Combined_Market_Data <- data.frame(Combined_Market_Data )
value=as.Date(row.names(Combined_Market_Data))

prices = Combined_Market_Data$AAPL.Adjusted

# Plotting the data
z=c(0.764,0.618, 0.381, 0.236, 0.50)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(value, prices,type="l",ylab="Prices",xlab = "")
par(new = TRUE)
plot(z, type = "n", axes = FALSE, bty = "n", xlab = "Time", ylab = "",ylim = c(0,1))
axis(side=4, at = pretty(range(z)))
mtext("Fibonacci ratios", side=4, line=3)
abline(h=0.764,col="blue")
abline(h=0.618,col="red")
abline(h=0.5,col="yellow")
abline(h=0.381,col="green")
abline(h=0.236,col="black")
title("Fibonacci Retracement")

df <- data.frame(Date1=character(),Price1=double(),Date2=character(),Price2=double())
ratios=c()
t=0
for(i in 1:(length(prices)-1)){
  p1=as.double(prices[i])
  #print(p1)
  for(j in (i+1):length(prices)){
    p2=as.double(prices[j])
    #print(p2)
    ratio=(p1-p2)/p1
    ratios[t]=ratio
    t=t+1
    if((ratio > 0.61) & (ratio <  0.62)){
      print(ratio)
      temp = data.frame(Date1=value[i],Price1=price[i],Date2=value[j],Price2=price[j])
      df<-rbind(df,temp)
   }
  }
}

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








