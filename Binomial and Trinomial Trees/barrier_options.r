# Barrier option

#Problem 1
barrier_european_up_and_out = function(K,T,S,sigma,r,H,N){
  # Set Coefficients Trigeorgis
  dt = T/N
  v = r-0.5*sigma^2
  dxu = sqrt(sigma^2*dt + (v*dt)^2)
  dxd = -dxu
  pu = 0.5 + 0.5*(v*dt/dxu)
  pd = 1-pu
  # Precompute Constants
  disc = exp(-r*dt)
  dpu=disc*pu
  dpd=disc*pd
  edxud=exp(dxu-dxd)
  edxd=exp(dxd)
  
  #intialize asset prices at maturity N
  St=c()
  St[1]=S*exp(N*dxd)
  for(i in 2:(N+1)){
    St[i]=St[i-1]*edxud
  }
  print(St)
  # Intalize Options Value At Maturity
  C.Value = c()
  for(i in 1:(N+1)){
    if(St[i]<H){
      C.Value[i] = max(0.0,St[i]-K)
    }
    else{
      C.Value[i] = 0.0
    }
  }
  print(C.Value)
  #Step Back Through The Tree
  
  for(i in N:1){
    C.Value2=c()
    for(j in 1:i){
      St[j]=St[j]/exp(dxd)
      if(St[j]<H){
        C.Value[j]=dpd*C.Value[j] + dpu*C.Value[j+1]
      }
      else{
        C.Value[j]=0
      }
      C.Value2[j]=C.Value[j]
    }
    print(C.Value2)
  }
}  
# European Call Option Up and In Options
barrier_european_up_and_out(10,0.3,10,0.2,0.01,11,1500)
# Option Price = 0.05578389

#Problem 2
# Black Scholes derivation for same Call Up and Out Options

rm(list=ls())
H=11
K=10
S=10
sigma=0.2
T=0.3
r=0.01
delta=0
blackscholes <- function(S, K, r, T, sigma){
  d1 <- (log(S/K)+ (r+sigma^2/2)*(T))/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  options_call <- S*pnorm(d1) - K*exp(-r*(T))*pnorm(d2)
  options_put <- K*exp(-r*(T))*pnorm(-d2) -S*pnorm(-d1)  
  options <- list("call"=options_call,"put"=options_put,"d1"=d1,"d2"=d2)
  return(options) 
}
cbs1=blackscholes(S,K,r,T,sigma)
cbs2=blackscholes(S,H,r,T,sigma)
cbs3=blackscholes((H^2/S),K,r,T,sigma)
cbs4=blackscholes((H^2/S),H,r,T,sigma)
v=(r - delta - (sigma)^2/2)
dbs1=(log(S/H)+v*T)/(sigma*sqrt(T))
dbs2=(log(H/S)+v*T)/(sigma*sqrt(T))

option.price = cbs1$call - cbs2$call - (H-K)*exp(-r*T)*pnorm(dbs1) - (H/S)^((2*v)/sigma^2)*(cbs3$call - cbs4$call - (H-K)*exp(-r*T)*pnorm(dbs2))
option.price
# Option Price 0.0530928

#Problem 3
# Up and In Call European Option 5.1 way

Call.up.in = (H/S)^((2*v)/sigma^2)*(cbs3$put - cbs4$put + (H-K)*exp(-r*T)*pnorm(-dbs2)) + cbs2$call +(H-K)*exp(-r*T)*pnorm(dbs1)
#0.3981948

# in-out parity 
# Call.up&out + Call.up&in = Call
Call.up.in = cbs1$call - option.price
# 0.3981948

# Problem 4
# American Up and In Call Options
barrier_american_up_and_in = function(K,T,S,sigma,r,H,N){
  # Set Coefficients Trigeorgis
  dt = T/N
  v = r-0.5*sigma^2
  dxu = sqrt(sigma^2*dt + (v*dt)^2)
  dxd = -dxu
  pu = 0.5 + 0.5*(v*dt/dxu)
  pd = 1-pu
  # Precompute Constants
  disc = exp(-r*dt)
  dpu=disc*pu
  dpd=disc*pd
  edxud=exp(dxu-dxd)
  edxd=exp(dxd)
  
  #intialize asset prices at maturity N
  St=c()
  St[1]=S*exp(N*dxd)
  for(i in 2:(N+1)){
    St[i]=St[i-1]*edxud
  }
  print(St)
  # Intalize Options Value At Maturity
  C.Value = c()
  for(i in 1:(N+1)){
    if(St[i]>H){
      C.Value[i] = max(0.0,St[i]-K)
    }
    else{
      C.Value[i] = 0.0
    }
  }
  print(C.Value)
  #Step Back Through The Tree
  
  for(i in N:1){
    C.Value2=c()
    for(j in 1:i){
      St[j]=St[j]/exp(dxd)
      if(St[j]>H){
        C.Value[j]=dpd*C.Value[j] + dpu*C.Value[j+1]
        C.Value[j]= max(C.Value[j],St[j]-K)  
      }
      else{
        C.Value[j]=0
      }
      C.Value2[j]=C.Value[j]
    }
    print(C.Value2)
  }
}  
# American Call Option Up and In Options
barrier_american_up_and_in(100,1,100,0.2,0.06,95,3)
