# Autor Kshitiz Sharma
rm(list=ls())
library(quantmod)
# Question 1
###########################################
# Problem 1.a
# additive binomial tree to calculate the values of the
# European Call option and for the European Put option
additive_binomial_tree_european = function(K,T,S,sigma,r,N,flag){
  # Set Coefficients Trigeorgis
  dt = T/N
  v = r-0.5*sigma^2
  dxu = sqrt(sigma^2*dt + (v*dt)^2)
  dxd = -dxu
  pu = 0.5 + 0.5*(v*dt/dxu)
  pd = 1-pu
  # Precompute Constants
  dsc = exp(-r*dt)
  # Intialize Price at Maturity N
  #St=c()
  #St=append(St,S*exp(N*dxd))
  #for(i in 1:N){
  #  St=append(St,St[i-1]*exp(dxu-dxd))
  #}
  # Intialize Price at Maturity N
  St=c()
  St[1]=S*exp(N*dxd)
  for(i in 2:(N+1)){
    St[i]=St[i-1]*exp(dxu-dxd)
  }
  # Intalize Options Value At Maturity
  C.Value = c()
  if(flag==1){
    print("Call Eurpean Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,St[i]-K)
    }
  }else{
    print("Put Eurpean Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,K-St[i])
    }
  }
  #Step Back Through The Tree
  for(i in N:1){
    C.Value2<-c()
    for(j in 1:i){
      C.Value[j]= dsc * ( pu * C.Value[j+1] + pd*C.Value[j])
      C.Value2[j]=C.Value[j]
      }
    #print(C.Value2)
  }
  return(C.Value[1])
}
# Flag at the end of function is 1 for call
additive_binomial_tree_european(100,1, 100, 0.2, 0.06, 3,1)
# Flag at the end of function is 0 for put
additive_binomial_tree_european(100,1, 100, 0.2, 0.06, 3,0)

#######################
# Problem 1.b

# Give the year for which you want the Implied Volatility to be calculated
option.date = "2018"
# Give data from which you want the stock prices data to be downloaded
sDate <- as.Date("2018-01-01")
# Give the name of the ticker for which you want IV(Implied Volatility)
ticker <- c("GS")

# Manual Update
# Interst Rate
r=0.0142
# You will also need to put in the number of expiration days time.
time.left.days = c(9, 13, 19)
# Number of Maturites you want to calculate till
N=3

l = length(ticker)
Option.data <- new.env()
Market.Data=new.env()

# Downloading Options data
# you need to call the function below with a single ticker or use a for loop to interate over the 
# ticker list for calling below function
# Sample get.option.data("AAPL",sDate)
get.option.data <- function(ticker,expiry){
  Option.data <- getOptionChain(ticker, Exp = expiry)
  return(Option.data)
} 

# Downloading Tickes Past prices and todays prices
stock.prices<-getSymbols(ticker, env = Market.Data, from=sDate)
Combined.Market.Past.Prices <- do.call(merge, eapply(Market.Data, Ad))
Today.price <- data.frame(Prices=double())
for(i in 1:l){
  p=tail(Combined.Market.Past.Prices[,i],n=1)
  Today.price=cbind(Today.price,p)
}

Combined.Market.Past.Prices
Today.price = Today.price[[2]]

# BlackScholes which will be used by  Bisection Model
blackscholes <- function(S, K, r, T, sigma){
  d1 <- (log(S/K)+ (r+sigma^2/2)*(T))/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  options_call <- S*pnorm(d1) - K*exp(-r*(T))*pnorm(d2)
  options_put <- K*exp(-r*(T))*pnorm(-d2) -S*pnorm(-d1)  
  options <- list("call"=options_call,"put"=options_put,"d1"=d1,"d2"=d2)
  return(options) 
}

# Bisection Model For Calculating Implied Volatility
Bisection.1 <- function(input, price ,strike,bid,ask,time,flag){
  options <- blackscholes(price,strike,r,time,input)
  if(flag==0){
    answer <- options$call - (bid + ask)/2
  }else{
    answer <- options$put - (bid + ask)/2
  }
  return(answer)
}

Bisection.2 <- function(price,strike,bid,ask,time,flag){
  tolerance_level = 0.000001
  a =  0
  b =  10
  while((b-a) > tolerance_level){
    input <- (a+b)/2
    f_combined <- Bisection.1(input,price,strike,bid,ask,time,flag)
    f_a <- Bisection.1(a,price,strike,bid,ask,time,flag)
    f_b <- Bisection.1(b,price,strike,bid,ask,time,flag)
    if((f_combined*f_a)<0){
      b <- (a+b)/2
    }
    if((f_combined*f_b)<0){
      a <- (a+b)/2
    }
    if(f_combined*f_a >0 & f_combined*f_b > 0){
      return(404)
    }
    if(f_a==0){
      return(a)
    }
    if(f_b==0){
      return(b)
    }
  }
  return(a)
}

# Calculating implied Volatility for 3 strike prices from today 
# I will be making use of just one ticker GS dates remain same
# 2018-03-09, 2018-03-16, 2018-03-23

df <- data.frame(Expiry=character(),Strikes=double(),IVolatility=double(),stringsAsFactors = TRUE)
Call.strikes =c()
Put.strikes =c()
Bid.Call.Prices=c()
Ask.Call.Prices = c()
Bid.Put.Prices=c()
Ask.Put.Prices=c()
Call.iv =c()
Put.iv =c()
for(i in 1:l){
  k=0
  c=0
  t=0
  flag=0
  time_tracker=0
  At_money_sum=0
  In_Out_sum=0
  print(ticker[i])
  value = get.option.data(ticker[i],option.date)
  for(j in value){
    time_tracker=time_tracker+1
    flag=0
    for(m in j){      # Put - Call
      strike <- m$Strike
      bid <- m$Bid
      ask <- m$Ask
      Vol <- m$Vol
      for(p in 1:length(Vol)){
        if(!is.null(Vol[p])){
          if(Vol[p]!=0){
            v=Bisection.2(Today.price,strike[p],bid[p],ask[p],time.left.days[time_tracker]/252,flag)
            check = Today.price/strike[p]
            if((check > 0.90) & (check < 1.05)){
              if(v != 404){
                At_money_sum = At_money_sum + v
                k=k+1
                print(v)
                if(flag==0){
                  Call.strikes=append(Call.strikes,strike[p])
                  Call.iv=append(Call.iv,v)
                  Bid.Call.Prices=append(Bid.Call.Prices,bid[p])
                  Ask.Call.Prices=append(Ask.Call.Prices,ask[p])
                }else{
                  Put.strikes=append(Put.strikes,strike[p])
                  Put.iv=append(Put.iv,v)
                  Bid.Put.Prices=append(Bid.Put.Prices,bid[p])
                  Ask.Put.Prices=append(Ask.Put.Prices,ask[p])
                }
              }
              else{
                t=t+1
              }
            }
            else{
              if(v != 404){
                In_Out_sum = In_Out_sum + v
                c=c+1
              }
              else{
                t=t+1 
              }
            }
          }
          de<-data.frame(Expiry=(rownames(m)[p]),Strikes=strike[p],IVolatility=v)
          df<-rbind(df,de)
        }  
      }
      flag=1
    }
    Call.strikes=append(Call.strikes,0)
    Call.iv=append(Call.iv,0)
    Put.strikes=append(Put.strikes,0)
    Put.iv=append(Put.iv,0)
    if(time_tracker==N){
      break
    }
  }
  print("At The Money Implied Volatility")
  print(At_money_sum/k)
  print("Out and In The Money Implied Volatility")
  print(In_Out_sum/c)
  #print((In_Out_sum+At_money_sum)/(c+k))
  #print(k)
  #print(c)
  #print(t)
} 
write.csv(df,"ImpliedVolailityData.csv")


# Comes the main part of question Checking Black Scholes Value and Binomial trees values
t=1
European.call.price <-c()
for(i in 1:length(Call.strikes)){
  if(Call.strikes[i] == 0){
    t = t+1    
  }
  else{
    print(Call.strikes[i])
    value1=blackscholes(Today.price,Call.strikes[i],r,time.left.days[t]/252,Call.iv[i])
    print("From BlackScholes")
    print(value1$call)
    value2=additive_binomial_tree_european(Call.strikes[i],time.left.days[t]/252, Today.price,Call.iv[i],r,300,1)
    European.call.price=append(European.call.price,value2)
    print(value2)
    print("***")
  }
}




t=1
European.put.price <-c()
for(i in 1:length(Put.strikes)){
  if(Put.strikes[i] == 0){
    t = t+1    
  }
  else{
    print(Put.strikes[i])
    value1=blackscholes(Today.price,Put.strikes[i],r,time.left.days[t]/252,Put.iv[i])
    print("From BlackScholes")
    print(value1$put)
    value2=additive_binomial_tree_european(Put.strikes[i],time.left.days[t]/252, Today.price,Put.iv[i],r,300,0)
    European.put.price=append(European.put.price,value2)
    print(value2)
    print("***")
  }
}

#######################
# Problem 1.c

steps=c(50, 100, 150, 200, 250, 300, 350, 400)
for(j in 1:length(steps)){
  Error<-c()
  t=1
  for(i in 1:length(Call.strikes)){
    if(Call.strikes[i] == 0){
      t = t+1    
      Error=append(Error,0)
    }
    else{
      print(Call.strikes[i])
      value1=blackscholes(Today.price,Call.strikes[i],r,time.left.days[t]/252,Call.iv[i])
      print("From BlackScholes")
      print(value1$call)
      value2=additive_binomial_tree_european(Call.strikes[i],time.left.days[t]/252, Today.price,Call.iv[i],r,j,1)
      print(value2)
      Error=append(Error,abs(value1$call - value2))
      print("***")
    }
  }
  plot(Call.strikes,Error,xlim=range(250:300),main=steps[j])
}

#######################
# Problem 1.d

# additive binomial tree to calculate the American option, both Call and Put.
additive_binomial_tree_american = function(K,T,S,sigma,r,N,flag){
  # Set Coefficients Trigeorgis
  dt = T/N
  v = r-0.5*sigma^2
  dxu = sqrt(sigma^2*dt + (v*dt)^2)
  dxd = -dxu
  pu = 0.5 + 0.5*(v*dt/dxu)
  pd = 1-pu
  # Precompute Constants
  dsc = exp(-r*dt)
  # Intialize Price at Maturity N
  #St=c()
  #St=append(St,S*exp(N*dxd))
  #for(i in 1:N){
  #  St=append(St,St[i-1]*exp(dxu-dxd))
  #}
  # Intialize Price at Maturity N
  St=c()
  St[1]=S*exp(N*dxd)
  for(i in 2:(N+1)){
    St[i]=St[i-1]*exp(dxu-dxd)
  }
  # Intalize Options Value At Maturity
  C.Value = c()
  if(flag==1){
    print("Call American Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,St[i]-K)
    }
  }else{
    print("Put American Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,K-St[i])
    }
  }
  #print(St)
  #print(C.Value)
  #Step Back Through The Tree
  for(i in N:1){
    C.Value2=c()
    for(j in 1:i){
      St[j]=St[j]/exp(dxd)
      if(flag==1){
        C.Value[j]= max(dsc * ( pu * C.Value[j+1] + pd*C.Value[j]),St[j]-K)
      }
      else{
        C.Value[j]= max(dsc * ( pu * C.Value[j+1] + pd*C.Value[j]),K-St[j])  
      }
      C.Value2[j]=C.Value[j]
    }
    print(C.Value2)
  }
  return(C.Value[1])
}
# American Call Option Trigeorgis
additive_binomial_tree_american(100,1, 100, 0.2, 0.06, 3,1)
# American Put Option Trigeorgis
additive_binomial_tree_american(100,1, 100, 0.2, 0.06, 3,0)


# Comes the main part of question Checking Black Scholes Value and American Binomial trees values
bks1 <- c()
American.call.price <-c()
t=1
for(i in 1:length(Call.strikes)){
  if(Call.strikes[i] == 0){
    t = t+1    
  }
  else{
    print(Call.strikes[i])
    value1=blackscholes(Today.price,Call.strikes[i],r,time.left.days[t]/252,Call.iv[i])
    bks1= append(bks1,value1$call)
    print("From BlackScholes")
    print(value1$call)
    value2=additive_binomial_tree_american(Call.strikes[i],time.left.days[t]/252, Today.price,Call.iv[i],r,300,1)
    American.call.price=append(American.call.price,value2)
    print(value2)
    print("***")
  }
}

American.put.price=c()
bks2 <-c()
t=1
for(i in 1:length(Put.strikes)){
  if(Put.strikes[i] == 0){
    t = t+1    
  }
  else{
    print(Put.strikes[i])
    value1=blackscholes(Today.price,Put.strikes[i],r,time.left.days[t]/252,Put.iv[i])
    bks2= append(bks2,value1$put)
    print("From BlackScholes")
    print(value1$put)
    value2=additive_binomial_tree_american(Put.strikes[i],time.left.days[t]/252, Today.price,Put.iv[i],r,300,0)
    American.put.price=append(American.put.price,value2)
    print(value2)
    print("***")
  }
}

#######################
# Problem 1.e

df <- data.frame(Bids= Bid.Call.Prices,Ask=Ask.Call.Prices, blackscholes=bks1, European.Price=European.call.price, American.Price=American.call.price, Strikes = Call.strikes[Call.strikes!=0])
df2 <- data.frame(Bids= Bid.Put.Prices,Ask=Ask.Put.Prices, blackscholes=bks2, European.Price=European.put.price, American.Price=American.put.price, Strikes = Put.strikes[Put.strikes!=0])

#######################
# Problem 1.f
Call.iv2=c()
t=1
counter=0
for(p in 1:length(Call.strikes)){
      if(Call.strikes[p] == 0){
        t = t+1    
        counter=counter+1
      }
  else{
    v=Bisection.2(Today.price,Call.strikes[p],American.call.price[p-counter],American.call.price[p-counter],time.left.days[t]/252,0)
    Call.iv2=append(Call.iv2,v)
  }
}
print(Call.iv[Call.iv!=0])
Call.iv2


Put.iv2=c()
t=1
counter=0
for(p in 1:length(Put.strikes)){
  if(Put.strikes[p] == 0){
    t = t+1    
    counter=counter+1
  }
  else{
    v=Bisection.2(Today.price,Put.strikes[p],American.put.price[p-counter],American.put.price[p-counter],time.left.days[t]/252,1)
    Put.iv2=append(Put.iv2,v)
  }
}
print(Put.iv[Put.iv!=0])
Put.iv2

#######################
# Problem 1.g
# Trinomial tree to price an American Put option
# Since there are three possible movements in the stock price  in trinomial tree
# as compared to binomial it provides a better aproximation technique. 
# Because of trinomial tree more regular grid it's more flexible to work with 
# and it provides easy extension to timing varying drift and volatility paramters

trinomial_tree_american = function(K, T, S, sig, r, div, N, Nj, dx){
  Option.value <- matrix(, nrow = (2*Nj+1), ncol = (Nj+1))
    # Precompute Constants
  dt = T/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  pu = 0.5*dt*((sig/dx)^2 + nu/dx)
  pm = 1.0 - dt*(sig/dx)^2 - r*dt
  pd = 0.5*dt*((sig/dx)^2 - nu/dx)
  # Intialise asset prices at maturity
  St = c()
  St[2*Nj+1]=S*exp(-Nj*dx)
  for(j in (2*Nj):1){
      St[j]=St[j+1]*edx
  }
  print(St)
  #Intialize option value at maturity
  for(j in (2*Nj+1):1){
    Option.value[j,Nj+1]  = max(0,K-St[j])
  }
  #Step back through the lattice
  for(i in N:1){
    for(j in (2*Nj):2){
      Option.value[j,i]=pu*(Option.value[j+1,i+1]) + pm*(Option.value[j,i+1]) + pd*(Option.value[j-1,i+1])
      #Boundary Conditions
      Option.value[2*Nj+1,i]=Option.value[2*Nj,i] + (St[2*Nj]-St[2*Nj+1])
      Option.value[1,i]=Option.value[2,i]
      #print(Option.value)  
      # Apply early exercise Condition
      for(j in (2*Nj+1):1){
        #Option.value[j,Nj+1]=max(Option.value[j,Nj],K-St[j])
        Option.value[j,i]=max(Option.value[j,i],K-St[j])
      }
      #print(Option.value)
    }
  }
  print(Option.value)
}
trinomial_tree_american(100, 1, 100, 0.2, 0.06, 0.03, 3, 3, 0.2)
additive_binomial_tree_american(100,1,100,0.2,0.06,3,0)


####################################################
# Question 2
# Barrier option

#Problem 1
#Barrier European Up and Out Option in binomial tree
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
rm(list=ls())
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
barrier_american_up_and_in(100,1,100,0.2,0.06,90,3)

##########################
# Questoin 3
# Adapting The Binomial Model to The Varying Volatility

#Problem 1
table.varaibles<-data.frame(r.i=double(),sigma.i=double(), v.i=double(), t.i =double(), p.i=double())
delta.x=0.06
for(i in 0:11){
  r.i = 0.05*(1 + 0.01*i)
  sigma.i = 0.20*(1 + 0.005*i)
  v.i = r.i -sigma.i^2/2
  t.i = (-sigma.i^2 + sqrt(sigma.i^4 + 4*v.i^2*delta.x^2))/(2*v.i^2)
  p.i = 0.5 + (v.i*t.i)/(2*delta.x)
  temp<-data.frame(r.i, sigma.i,v.i, t.i, p.i)
  colnames(temp)<-c("r.i", "sigma.i","v.i", "t.i", "p.i")
  table.varaibles<-rbind(table.varaibles, temp)
}
table.varaibles

# Finding the optimum value
sigma.bar = sum(table.varaibles$sigma.i)/11
v.bar = sum(table.varaibles$v.i)/11
delta.t.bar = 1/11
delta.x=sqrt(sigma.bar^2 * delta.t.bar + v.bar^2 * delta.t.bar^2)
delta.x
#Optimum vaule is 0.0676661




