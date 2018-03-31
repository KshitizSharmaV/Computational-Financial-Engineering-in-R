# RSI indicator
# Give your time series here 
prices = time_series

plot.2=plot(prices,type="l")

RSI.value=RSI(prices,n=14)
plot.1=plot(RSI.value,type="l", ylim=c(20,85))
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
    position_entered=prices[i]
    position_exited=0
    for(j in (i+1):length(RSI.value)){
      if(RSI.value[j]<70){
        print("######")
        position_exited=prices[j]
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
    position_entered=prices[i]
    position_exited=0
    for(j in (i+1):length(RSI.value)){
      if(RSI.value[j]>30){
        print("######")
        position_exited=prices[j]
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