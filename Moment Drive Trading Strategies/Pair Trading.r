# Author Kshitiz Sharma
# Analysis of equity using Fibonacci retracement stratey

rm(list=ls())
library(quantmod)

# Downloading equity Data
ticker <- c("XOM","CVX")
Market_Data <- new.env()
sDate <- as.Date("2015-02-01")
a<-getSymbols(ticker, env = Market_Data, from=sDate)
Combined_Market_Data <- do.call(merge, eapply(Market_Data, Ad))
Combined_Market_Data <- data.frame(Combined_Market_Data )
value=as.Date(row.names(Combined_Market_Data))

prices1 = Combined_Market_Data$XOM.Adjusted
prices2 = Combined_Market_Data$CVX.Adjusted


price_ratio=prices1/prices2
plot(price_ratio,type="o")
sd11=sd(price_ratio)

sd1.above=mean(price_ratio)+sd11
sd2.above=mean(price_ratio)+2*sd11
sd3.above=mean(price_ratio)+3*sd11

sd1.lower=mean(price_ratio)-sd11
sd2.lower=mean(price_ratio)-2*sd11
sd3.lower=mean(price_ratio)-3*sd11




correlation=c()
df <- data.frame(Date1=character(),Date2=character(),corr=double())
for(i in seq(from=1, to = length(prices1),by =7)){
  de=data.frame(Date1=value[i],Date2=value[i+6],corr=cor(prices1[i:(i+6)],prices2[i:(i+6)]))
  df=rbind(df,de)
}
df=head(df,-1)

plot(df$corr, type="b")

par(mar = c(5, 4, 4, 4) + 0.3)  
plot(value, prices1,type="l",ylab="Prices",xlab = "",col="red")
par(new = TRUE)
plot(prices2, type = "l", axes = FALSE, bty = "n", xlab = "Time",col="blue",ylab="")
axis(side=4, at = pretty(range(prices2)))


df2 = subset(df, corr < 0.7)

