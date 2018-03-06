# Autor Kshitiz Sharma
# ksharma3@stevens.edu
rm(list=ls())
# Question 1
# Read the CSV file
file=read.csv("GE_2007-2017.csv")
l1=length(file$Adj.Close)

# Log.returns to store the log returns 
log.returns<-c()
# Run a for loop to save the log.returns
for(i in 2000:l1){
  return=log(file$Adj.Close[i]/file$Adj.Close[i-1])
  log.returns<-append(log.returns, return)  
}

#Measurin Volatilty 
# σt =   1  t (ri − r ̄)2 1/2.
r.mean= mean(log.returns)
l2=length(log.returns)
#22 is the last n observations in your data set to observer recent volatilities
n=22
Sum.return.mean=0
for(i in (l2-n+1):l2){
  Sum.return.mean= Sum.return.mean + (log.returns[i]-r.mean)^2
}
# Sigma t is the realized volatility or the historical volatility
Sigma.t = (Sum.return.mean/n)^(1/2)
## 0.01315459

#Part a
# Volaility for the Random Walks σˆt = σt−1
# to find this value we will again use n=22 and it will start from one position behind
# previos and one position before the end log.returns
Sum.return.mean=0
for(i in (l2-n):(l2-1)){
  Sum.return.mean= Sum.return.mean + (log.returns[i]-r.mean)^2
}
# Sigma t is the realized volatility or the historical volatility
Sigma.random.walk.t.minus.one = (Sum.return.mean/n)^(1/2)
## 0.01308929

#Part b
#Exponential smoothing average (often called exponential moving average or EMA):
#  σˆt = (1 − β)σt−1 + βσˆt−1, 0 < β < 1
beta=2/(n+1)
Sum.return.mean=0
for(i in (l2-n-1):(l2-2)){
  Sum.return.mean= Sum.return.mean + (log.returns[i]-r.mean)^2
}
# Sigma t is the realized volatility or the historical volatility
Sigma.random.walk.t.minu.one.cap = (Sum.return.mean/n)^(1/2)
#0.0130532
Sigma.exponential.smoothing.average = (1-beta)*Sigma.random.walk.t.minus.one +beta*Sigma.random.walk.t.minu.one.cap
## 0.01308615

#Part c
#Exponentially weighted moving average 
# σˆt =  βiσt−i/ βi,0 < β < 1
Sum.Beta=0
Final.Sum=0
for(i in 2:l2){
  Sum.return.mean=0
  for(j in (l2-i):l2){
    Sum.return.mean= Sum.return.mean + (log.returns[i]-r.mean)^2
  }
  # Sigma t is the realized volatility or the historical volatility
  Sigma.t = (Sum.return.mean/i)^(1/2)
  
  Beta=2/(i+1)
  Sum.Beta=Sum.Beta + Beta
  Final.Sum=Final.Sum+Beta*Sigma.t
}
Sigma.Exponentially.weighted.moving.average=Final.Sum/Sum.Beta
Sigma.Exponentially.weighted.moving.average
# 0.01335111

plot(log.returns)
