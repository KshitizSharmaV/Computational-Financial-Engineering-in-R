rm(list=ls())
r = 0.03
sigma = 0.3
S0 = 100
K = 100
t = 5
M = 100000


# Value of Arithmetic Asian Call Option
N=t*252
M=20000
dt=1/252
Sum.value=0
ptm=proc.time()
for(i in 1:M){
  Sum=S0
  St=c(S0)
  for(j in 1:(N-1)){
    St[j+1]=St[j]*exp((r-sigma^2/2)*dt+sigma*sqrt(dt)*rnorm(1,0,1))
    Sum = Sum + St[j+1]
  }
  v=exp(-r*t)*max((Sum/(N+1)-K),0)
  #print(v)
  Sum.value = Sum.value+ v
}
proc.time()-ptm
print("Value of Arithmetic Asian Call Option is")
print(Sum.value/M)

Sum.value/M-1.96*sigma/sqrt(M)
Sum.value/M+1.96*sigma/sqrt(M)


# Value of Geometric Asian Call Option
M=30000
N=t*252
dt=1/252
Sum.value=0
ptm=proc.time()
for(i in 1:M){
  Sum=log(S0)
  St=c(S0)
  for(j in 1:(N-1)){
    St[j+1]=St[j]*exp((r-sigma^2/2)*dt+sigma*sqrt(dt)*rnorm(1,0,1))
    Sum = Sum + log(St[j+1])
  }
  value = exp(Sum/(N+1))
  v=exp(-r*t)*max(value-K,0)
  Sum.value = Sum.value+ v
}
ptm=proc.time()
print("Value of Geometric Asian Call Option is")
print(Sum.value/M)
# 14.64053