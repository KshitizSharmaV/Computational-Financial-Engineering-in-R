# Simulating the Heston model.
rm(list = ls())
S=100
Y = 0.010201
kappa = 6.21
theta = 0.019
sigma = 0.61
rho = -0.7
r = 0.0319
K =  100
t = 1
M = 1000000
N = 300
dt = t/N

# Euler Scheme
sum_ct1=0
sum_ct2=0
counter=0
Yt=0
St=0
ptm=proc.time()
cuonter=0
for(i in 1:M){
  # St=S
  lnSt = log(S)
  Yt=Y
  for(j in 1:N){
    e1=rnorm(1,0,1)
    e2=rnorm(1,0,1)
    W1=e1
    W2=rho*e1 + sqrt(1-rho^2)*e2
    lnSt = lnSt + (r - 0.5*Yt)*dt + sqrt(Yt*dt)*W2
    Yt = Yt + kappa*(theta - Yt)*dt+sigma*sqrt(Yt*dt)*W1
    Yt=abs(Yt)
  }
  St = exp(lnSt)
  Ct=max(0,St-K)
  sum_ct1= sum_ct1+Ct
  sum_ct2= sum_ct2+Ct*Ct
}
proc.time() - ptm
call.value = sum_ct1/M * exp(-r*t)
call.value
# 7.101
sd = sqrt((sum_ct2 - sum_ct1*sum_ct1/M) * exp(-2*r*t)/(M-1))
sd
se=sd/sqrt(M)
se

# Euler Milstein Scheme
sum_ct1=0
sum_ct2=0
counter=0
Yt=0
St=0
ptm=proc.time()
for(i in 1:M){
  St=S
  Yt=Y
  for(j in 1:N){
    e1=rnorm(1,0,1)
    e2=rnorm(1,0,1)
    W1=e1
    W2=rho*e1 + sqrt(1-rho^2)*e2
    St = St + r*St*dt + St*sqrt(dt*Yt)*W1 + (Yt*St*dt*(W1^2-1))/2
    Yt = Yt + kappa*(theta - Yt)*dt+sigma*sqrt(Yt*dt)*W2 + (sigma^2*dt*(W1^2-1))/4
    Yt=max(0,Yt)
    
    #print(Yt)
    #print(St)
  }
  if(St>100){
    counter=counter+1
  }
  Ct=max(0,St-K)
  sum_ct1= sum_ct1+Ct
  sum_ct2= sum_ct2+Ct*Ct
}
proc.time()-ptm
call.value = sum_ct1/M * exp(-r*t)
call.value
# 6.902057
sd = sqrt((sum_ct2 - sum_ct1*sum_ct1/M) * exp(-2*r*t)/(M-1))
sd
se=sd/sqrt(M)
se