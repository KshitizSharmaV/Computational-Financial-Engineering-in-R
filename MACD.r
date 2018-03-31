# MACD indicator
rm(list=ls())
prices = time.series.of.your.prices
# MACD indicator
prices <- prices
y.min <- min(prices)
y.max <- max(prices)
plot(ts(prices), ylim=c(y.min, y.max), ylab="Stock Price")

par(new=T)
bac.sma12 <- EMA(prices, n=12)
plot(ts(bac.sma12), ylab="", col = "steelblue", lwd=1,axes=F)

par(new=T)
bac.sma26 <- EMA(prices, n=26)
plot(ts(bac.sma26), ylab="", col = "red",lwd=1,axes=F)
legend("topright",
       legend=c("Stock Price", "EMA 12","EMA 26"),
       lty=c(1,1,1), col=c("black", "steelblue","red"),cex=0.55)


bac.MACD.t =  bac.sma12 - bac.sma26
signal_line = EMA(bac.MACD.t,n=9)

plot(signal_line,type="l",col="blue", ylab="Signal Line")
par(new=T)
plot(bac.MACD.t,type="l",lty=2,col="red",axes=F,ylab=NA)
axis(side = 4)
mtext(side = 4, line = 3, 'MACD Lline')
legend("topleft",
       legend=c("Signal Line", "MACD Line"),
       lty=c(1,2), pch=c(NA, NA), col=c("red3", "black"),cex=0.55)

v=0
total=0
for(i in 34:(length(signal_line)-1)){
  a=bac.MACD.t[i+1] - signal_line[i+1]
  b=bac.MACD.t[i] - signal_line[i]
  if((a*b)<0){
    short=0
    long=0
    if(a<0){
      short=1
    }else{
      long=1
    }
    short.buy.pos1=prices[i+1]
    for(j in (i+2):(length(signal_line)-1)){
      c=bac.MACD.t[j+1] - signal_line[j+1]
      d=bac.MACD.t[j] - signal_line[j]
      if((c*d)<0){
        short.buy.pos2=prices[j+1]
        print(short)
        print(long)
        print("Bought or sold at position 1:")
        print(short.buy.pos1)
        print("Bought or sold at position 2:")
        print(short.buy.pos2)
        print("Profit or Loss on Position:")
        if(short==1){
          profit.loss=short.buy.pos1 - short.buy.pos2
          
        }else{
          profit.loss=short.buy.pos2 - short.buy.pos1
        }
        print(profit.loss)  
        total=total+profit.loss
        print(j+1)
        print(i+1)
        i=j+3
        v=v+1
        print("########")
        break
      }
    }
  }
}
total
