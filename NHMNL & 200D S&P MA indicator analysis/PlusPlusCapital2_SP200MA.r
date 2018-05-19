# Step 6
# This analysis is from 15/03/91, since data was not available on Bloomberg
# I have built this sheet after cleaning all data 
rm(list=ls())
data=read.csv("SP200.csv")

five.SP=data$SP5
ten.SP =data$SP10
five.200.MA= data$MA5RANK
ten.200.MA= data$MA10RANK
Price.SP=  data$Price
dates.SP.200.MA= data$Dates

l=length(Price.SP)


# Step 1
calculate.return = function(days){
  # To Store the positions where we entered the trade
  position = c()
  # To Calculate profit and loss 
  profit.loss=c()
  # To keep track of out vectors length
  t=1
  for(i in 1:l){
    if(five.SP[i] < 0.10 || ten.SP[i]< 0.10){
      if(five.200.MA[i] > 0.95 || ten.200.MA[i]>0.95){
        position[t]=i+1
        profit.loss[t]=(Price.SP[i+days]/Price.SP[i+1]-1)*100
        t=t+1
      }
    }
  }
  position
  return(profit.loss)
}

returns.data=NULL
for(i in 2:101){
  # calling the calculate.return function to find the returns for days given
  returns.data=cbind(returns.data,calculate.return(i))
  #colnames(returns.data)=i
}
# This vector has 1 to 100 days of returns as asked for in vector 1
returns.data



# Step 2
# To find the occurence
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mean.returns=c()
sd.returns=c()
skew.returns=c()
ts.test=c()
occurence.returns=c()
for(i in 1:100){
  mean.returns[i]=mean(returns.data[,i])
  sd.returns[i]=sd(returns.data[,i])
  occurence.returns[i]=Mode(returns.data[,i])
  ts.test[i]=t.test(returns.data[,i])$statistic
  skew.returns[i]=skewness(returns.data[,i])
}
mean.returns
sd.returns
occurence.returns
skew.returns
ts.test



# Step 3
plot(c(1:100),mean.returns,type="l",xlab="Days",ylab="Mean returns")


# Step 4

# Drawing a new function 
calculate.return2 = function(days,check){
  # To Store the positions where we entered the trade
  position = c()
  # To Calculate profit and loss 
  profit.loss=c()
  # To keep track of out vectors length
  t=1
  for(i in 1:l){
    if(five.SP[i] < 0.10 || ten.SP[i]< 0.10){
      if(five.200.MA[i] > 0.95 || ten.200.MA[i]>0.95){
        position[t]=i+1
        profit.loss[t]=(Price.SP[i+days]/Price.SP[i+1]-1)*100
        t=t+1
      }
    }
  }
  complete.data=NULL
  # entry day
  if(check==0){
    temp=as.character(data$Dates)
    complete.data=cbind(complete.data,temp[position])  
  }else{
    complete.data=cbind(complete.data,dates.SP.200.MA[position])    
  }
  # entry price of day
  complete.data=cbind(complete.data,as.integer(Price.SP[position]))
  complete.data=cbind(complete.data,Price.SP[position+days])
  if(check==0){
    temp=as.character(data$Dates)
    complete.data=cbind(complete.data,temp[position+days])  
  }else{
    complete.data=cbind(complete.data,dates.SP.200.MA[position+days])
  }
  complete.data=cbind(complete.data,profit.loss)
  complete.data=cbind(complete.data,five.SP[position-1])
  complete.data=cbind(complete.data,ten.SP[position-1])
  complete.data=cbind(complete.data,five.200.MA[position-1])
  complete.data=cbind(complete.data,ten.200.MA[position-1])
  
  colnames(complete.data)=c("Date","Entry.price","Price.45.days.later","Date.Trade.45.later","Return","SP.5.ranks","SP.10.rank","200.MA.5d.rank","200.MA.10d.rank")
  complete.data=as.data.frame(complete.data)
  return(complete.data)
}

days=45
value=calculate.return2(days,1)
write.csv(calculate.return2(days,0),"Percentage_of_stock_above_200d_moving_average.csv")



# Step 5
t.test(value$Return)$statistic
# We are trying to find evidence of a significant between the population mean and a hypothesized value
# The greater the value of t which is 1.7049 in our case the greater evidnece against null hypothesis, that
# there is no significant differnece




