file=read.csv("GE_2007-2017.csv")


# Roll model
# mt = mt−1 + ut
# at − bt = 2c
# pt = mt + qt*c
# γ0 ≡ V ar(∆pt) = 2c2 + σu2
# γ1 ≡ Cov(∆pt−1, ∆pt) = −c2
prices <- file$Adj.Close
prices.diff<- diff(prices)
cov.value <- cov(prices.diff[1:length(prices.diff)-1],prices.diff[2:length(prices.diff)])
Roll.Spread.Spot=2*sqrt(-(cov.value))
# Bid Ask spread is $ 0.1030463
prices <- prices - Roll.Spread.Spot
# Measuring funamental Volatilty 
log.returns2<-c()
# Run a for loop to save the log.returns
for(i in 2:l1){
  return=log(prices[i]/prices[i-1])
  log.returns<-append(log.returns, return)  
}
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
Sigma.t2 = (Sum.return.mean/n)^(1/2)
# The Fundamental Volatility is 0.01326695











