# Assignment 3 FE 570
# Author Kshitiz Sharma

######################################################
# MACD indicator
rm(list=ls())

# MACD indicator

data=read.csv("sp500hst.csv")


data[,6] <- as.numeric(as.character( data[, 6] ))
sub.bac <- subset(data[,6], data$Ticker=='BAC');
y.min <- min(sub.bac)
y.max <- max(sub.bac)
plot(ts(sub.bac), ylim=c(y.min, y.max), ylab="Stock Price")

par(new=T)
bac.sma12 <- EMA(sub.bac, n=12)
plot(ts(bac.sma12), ylab="", col = "steelblue", lwd=1,axes=F)

par(new=T)
bac.sma26 <- EMA(sub.bac, n=26)
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
    short.buy.pos1=sub.bac[i+1]
    for(j in (i+2):(length(signal_line)-1)){
      c=bac.MACD.t[j+1] - signal_line[j+1]
      d=bac.MACD.t[j] - signal_line[j]
      if((c*d)<0){
        short.buy.pos2=sub.bac[j+1]
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
# Now because I am selling it once it crosess again the line, so how should I posution that.

######################################################
# RSI indicator


plot(sub.bac,type="l")

RSI.value=RSI(sub.bac,n=14)
plot(RSI.value,type="l", ylim=c(20,85))
par(new=TRUE)
abline(h=70,col="Blue")
par(new=TRUE)
abline(h=30,col="Blue")

# Building Strategy - 70 Mean overbought and 30 means over sold
# So we will sell if the prices go below 70 and buy if price goes below 30
i=15
profit.loss=0
while(i < length(RSI.value)){
  position_entered=0
  temp=1
  if(RSI.value[i]>70){
    position_entered=sub.bac[i]
    position_exited=0
    for(j in (i+1):length(RSI.value)){
      if(RSI.value[j]<70){
        print("######")
        position_exited=sub.bac[j]
        print(RSI.value[i])
        print(RSI.value[j])
        print("Short Position Entered :")
        print(position_entered)
        print("Position Exited :")
        print(position_exited)
        print("Profit Booked :") 
        print((position_entered - position_exited))
        profit.loss = profit.loss + position_entered - position_exited
        temp=j
        break
      }
    }
  }
  if(temp==1){
    i=i+1
  }else{
    i=temp
  }
}
profit.loss


i=15
profit.loss=0
while(i < length(RSI.value)){
  position_entered=0
  temp=1
  if(RSI.value[i]<30){
    position_entered=sub.bac[i]
    position_exited=0
    for(j in (i+1):length(RSI.value)){
      if(RSI.value[j]>30){
        print("######")
        position_exited=sub.bac[j]
        print(RSI.value[i])
        print(RSI.value[j])
        print("Short Position Entered :")
        print(position_entered)
        print("Position Exited :")
        print(position_exited)
        print("Profit Booked :") 
        print((position_entered - position_exited))
        profit.loss = profit.loss + position_exited - position_entered 
        temp=j
        break
      }
    }
  }
  if(temp==1){
    i=i+1
  }else{
    i=temp
  }
}
profit.loss






#sub.aa[144:170]
#[1] 14.27 14.44 14.40 14.24 14.70 14.73 15.03 14.74 14.87
#[10] 14.39 14.57 14.34 14.43 14.31 13.91 13.72 13.75 13.71
#[19] 13.85 14.11 14.05 13.44 13.57 13.72 13.43 13.15 12.58
#> RSI.value[144:170]
#[1] 70.81201 72.27207 65.24272 66.37480 68.69310 69.75677
#[7] 73.61794 74.86585 75.15787 73.32297 74.11690 74.23487
##[13] 81.03150 81.60420 56.87151 56.52665 59.44019 53.63372
#[19] 57.18264 55.25567 49.10010 41.50074 46.28595 53.20125
#[25] 47.27677 50.19942 44.43299



######################################################
# Head Shoulder Patterns

sub.aa <- subset(data[,6], data$Ticker=="AA")
y = sub.aa
# Using SM Library to smooth the prices using Kernel Regression estimation
library(sm)
t = 1:length(sub.aa)
# fit kernel regression with cross-validatio
h = h.select(t/1.3, y, method = 'cv')
temp = sm.regression(t, y, h=h, display = 'none')
# find estimated fit
mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y

# second step is to find local extrema, tops and bottoms, using first 
# derivative of the kernel regression estimator. 
temp = diff(sign(diff(mhat)))
# loc - location of extrema, loc.dir - direction of extrema
loc = which( temp != 0 ) + 1
loc.dir = -sign(temp[(loc - 1)])

patterns.price = data.frame(E1=double(),E2=double(),E3=double(),E4=double(),E5=double())
patterns.position = data.frame(E1=double(),E2=double(),E3=double(),E4=double(),E5=double())
for(i in 1:(length(loc)-5)){
  E1=sub.aa[loc[i]]
  E2=sub.aa[loc[i+1]]
  E3=sub.aa[loc[i+2]]
  E4=sub.aa[loc[i+3]]
  E5=sub.aa[loc[i+4]]
  
  t=0
  # Check for opposite signs, and change temp if found
  #for(j in 1:4){
  #  a=loc.dir[j+i]
  #  b=loc.dir[(j+1)+i]
  ##  if((a*b)>0){
  #    temp=1
  #  }
  #}
  
  if(t==0){
    avg.top = (E1 + E5) / 2
    avg.bot = (E2 + E4) / 2
    if(E3>E2 & E3 > E4){
      if(E3>E1 & E3>E5){
        print(E1)
        print(E2)
        print(E3)
        print(E4)
        print(E5)
        print("asas")
        if((abs(E1 - avg.top) < (1.5/100 * avg.top)) & (abs(E5 - avg.top) < (1.5/100 * avg.top))){
          if((abs(E2 - avg.bot) < (1.5/100 * avg.bot)) & ((abs(E4 - avg.bot) < 1.5/100 * avg.bot))){
            temp=data.frame(E1=E1,E2=E2,E3=E3,E4=E4,E5=E5)
            patterns.price=rbind(patterns.price,temp)
            
            temp=data.frame(E1=loc[i],E2=loc[i+1],E3=loc[i+2],E4=loc[i+3],E5=loc[i+4])
            patterns.position=rbind(patterns.position,temp)
            
          }
        }
      }
    }
  }
}
patterns.price
patterns.position


plot(1:length(sub.aa),sub.aa,type="l")
par(new=TRUE)
l=c(patterns.price[1,1],patterns.price[1,2],patterns.price[1,3],patterns.price[1,4],patterns.price[1,5])
f=c(patterns.position[1,1],patterns.position[1,2],patterns.position[1,3],patterns.position[1,4],patterns.position[1,5])
points(f,l,type="o",col="blue")
par(new=TRUE)
cutoff = (patterns.price[1,2]+patterns.price[1,4])/2
abline(h=cutoff,col="red")
par(new=TRUE)
patter_height = patterns.price[1,3] - cutoff
# The Pattern Height is 3.62



for(t in patterns.position[1,5]:length(sub.aa)){
  prices=sub.aa[t]
  if(prices<cutoff){
    print("Enter the short Position")
    price_shorted_at=sub.aa[t]
    break
  }
}
price_shorted_at
target_price = price_shorted_at - patter_height
target_price
# The Target Price is 8.96

abline(h=target_price,col="blue")










