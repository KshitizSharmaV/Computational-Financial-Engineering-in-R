rm(list=ls())
library(quantmod)
library(tidyr)

# Question 1
# Declaratoin of variables for Equity and Options Data
c=0
ticker <- c("AAPL","SPY","^VIX")
Market_Data <- new.env()
Option_Data <- new.env()
temp2 <-new.env()
sDate <- as.Date("2017-02-01")
option_eDate <- "2018"
l <- length(ticker)
# Get Equity Data
a<-getSymbols(ticker, env = Market_Data, from=sDate)
Combined_Market_Data <- do.call(merge, eapply(Market_Data, Ad))
# Save the data in CSV format
write.zoo(Combined_Market_Data,"Combined_Market_Data.csv",index.name="Date",sep=",")

v=tail(Combined_Market_Data,1)
Prices3 <- c(v$AAPL.Adjusted,v$SPY.Adjusted,v$VIX.Adjusted)
Prices <- c(167.37, 269.59, 19.26)
# Get Options Data
get_option <- function(ticker,expiry){
  Option_Data <- getOptionChain(ticker, Exp = expiry)
  return(Option_Data)
}
for(i in 1:length(ticker)){
  Option_Data = get_option(ticker[i],option_eDate)
  for(i in Option_Data){
    temp <- do.call(rbind, lapply(Option_Data, function(x) do.call(rbind, x)))
  }
    temp2 <- rbind(temp2,temp)
}
write.csv(temp2, 'Combined_Option_Data.csv')


# Question 2
# Please refer Question 1

# Question 4
#Federal Funds Rate - 1.42%

# Questoin 3 
# Please refer the PDF attached with assignwment

# Question 5
blackscholes <- function(S, K, r, T, sigma){
  d1 <- (log(S/K)+ (r+sigma^2/2)*(T))/(sigma*sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  options_call <- S*pnorm(d1) - K*exp(-r*(T))*pnorm(d2)
  options_put <- K*exp(-r*(T))*pnorm(-d2) -S*pnorm(-d1)  
  options <- list("call"=options_call,"put"=options_put,"d1"=d1,"d2"=d2)
  return(options) 
}
# Prices for stocks & ETF AAPL, SPY
# Strike prices for stocks & ETF -  AAPL, SPY & VIX
Strikes <- c(170,270,25)
r <- .0142
# Trading days calculatoin(T - t)
# 2018-02-16, 2018-02-23, 2018-03-02, 2018-03-09, 2018-03-23, 2018-03-29, 2018-04-20, 2018-05-18, 2018-06-15, 2018-09-21, 2019-01-18, 2020-01-17
a=c(2,6,11,16,26,30,46, 66, 85, 109, 153)
# Calculating Volatility for the stock
Stocks_vol=c(l)
for(i in 1:l){
  table1 <- getSymbols(ticker[i], from = "2017-09-01", auto.assign = F)
  temp.log.return <- dailyReturn(table1, type = "log")
  Stocks_vol[i] <- sd(temp.log.return) * sqrt(252)
}
for(i in 1:l){
  options = blackscholes(Prices[i],Strikes[i],r,(a[2])/252,Stocks_vol[i])  
  print(ticker[i])
  print(options$call)
  print(options$put)
}

# Question 6
#Bisection Model

Bisection <- function(input, price ,strike,bid,ask,time){
  options <- blackscholes(price,strike,r,time,input)
  answer <- options$call - (bid + ask)/2
  return(answer)
}
Bisection2 <- function(price,strike,bid,ask,time){
  tolerance_level = 0.000001
  a =  0
  b =  10
  while((b-a) > tolerance_level){
    #print(b)
    #print(a)
    input <- (a+b)/2
    f_combined <- Bisection(input,price,strike,bid,ask,time)
    #print(f_combined)
    f_a <- Bisection(a,price,strike,bid,ask,time)
    #print(f_a)
    f_b <- Bisection(b,price,strike,bid,ask,time)
    #print(f_b)
    #print("f_combined*f_a")
    #print(f_combined*f_a)
    #print("f_combined*f_b")
    #print(f_combined*f_b)
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


for(i in 1:(length(ticker))){
  k=0
  c=0
  t=0
  time_tracker=0
  At_money_sum=0
  In_Out_sum=0
  print(ticker[i])
  value = get_option(ticker[i],option_eDate)
  for(j in value){
    time_tracker=time_tracker+1
    strike <- j$calls$Strike
    bid <- j$calls$Bid
    ask <- j$calls$Ask
    Vol <- j$calls$Vol
    for(p in 1:length(Vol)){
      if(!is.null(Vol[p])){
        if(Vol[p]!=0){
          v=Bisection2(Prices[i],strike[p],bid[p],ask[p],a[time_tracker]/252)
          check = Prices[i]/strike[p]
          if((check > 0.95) & (check < 1.05)){
            if(v != 404){
              At_money_sum = At_money_sum + v
              k=k+1
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
      }  
    }
    if(time_tracker==11){
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

 
  #print(i$puts$Strike)
  #print(i$calls$Bid)


# Question 7
# To calculate the implied volatility for every Maturity, Options Type and Stock

df <- data.frame(Expiry=character(),Strikes=double(),
                 IVolatility=double(),stringsAsFactors = TRUE)
ticker2 <- c("AMZN","SPY","^VIX")
Prices2 <- c(1451.05,269.59,19.26)
for(i in 1:(length(ticker2))){
  k=0
  c=0
  t=0
  time_tracker=0
  Avg_Implied_volatility=0
  At_money_sum=0
  In_Out_sum=0
  print(ticker2[i])
  value = get_option(ticker2[i],option_eDate)
  for(j in value){   
    time_tracker=time_tracker+1
    for(m in j){      # Put - Call
      strike <- m$Strike
      bid <- m$Bid
      ask <- m$Ask
      Vol <- m$Vol
      for(p in 1:length(Vol)){
        if(!is.null(Vol[p])){
          if(Vol[p]!=0){
            v=Bisection2(Prices2[i],strike[p],bid[p],ask[p],a[time_tracker]/252)
            check = Prices2[i]/strike[p]
            if((check > 0.95) & (check < 1.05)){
              if(v != 404){
                At_money_sum = At_money_sum + v
                k=k+1
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
    } #m
    if(time_tracker==11){
      break
    }
  } #j
  print((At_money_sum+In_Out_sum)/(c+k))
}  
write.csv(df,"ImpliedVolailityData.csv")


# Question 8
df <- data.frame(Expiry=character(),Strikes=double(),
                 Call=double(),Put=double(),stringsAsFactors = TRUE)
flag=0
for(i in 1:length(ticker)){
  value = get_option(ticker[i],option_eDate)
  t=1
  for(j in value){ #Each Expiry Dates
    print(a[t])
    for(m in j){ # Each Put and Call Options
      strike <- m$Strike
      last <- m$Last
      bid <- m$Bid
      ask <- m$Ask
      if(flag==0){
        for(p in 1:length(strike)){
          # By Put Call Parity
          Price_Of_Other_Option = last[p] + strike[p]*exp(-r*a[t]) - Prices[i]
          de<-data.frame(Expiry=(rownames(m)[p]),Strikes=strike[p],Call=last[p],Put=Price_Of_Other_Option)
          df<-rbind(df,de)
        }
        flag=1
      }
      else{
        for(p in 1:length(strike)){
          # By Put Call Parity
          Price_Of_Other_Option = last[p] - strike[p]*exp(-r*a[t]) + Prices[i]
          de<-data.frame(Expiry=(rownames(m)[p]),Strikes=strike[p],Call=Price_Of_Other_Option,Put=last[p])
          df<-rbind(df,de)
        }
        flag=0
      }
    }
    t=t+1
  }
}
write.csv(df,"PutCallParityData.csv")

# Question 9
## Graphs for Call Implied Volatilities
for(i in 1:(length(ticker))){
  Ivol <-c()
  t=0
  time_tracker=0
  value = get_option(ticker[i],option_eDate)
  print(ticker[i])
  for(m in value){    
    time_tracker=time_tracker+1
    Strikes <- c()
    strike <- m$call$Strike
      bid <- m$call$Bid
      ask <- m$call$Ask
      Vol <- m$call$Vol
      for(p in 1:length(Vol)){
        if(!is.null(Vol[p])){
        if(Vol[p]!=0){
          v=Bisection2(Prices[i],strike[p],bid[p],ask[p],a[time_tracker]/252)
          check = Prices[i]/strike[p]
          if((check > 0.85) & (check < 1.10)){
            if(v != 404){
              Strikes = append(Strikes, strike[p])  
              old=Strikes
              Ivol = append(Ivol, v)
            }
          }
        }
        }
      }
      if(is.null(Strikes)){
        Strikes=old
      }
      print(Strikes)
    t=t+1
    if(t == 3){
      print(Ivol)
      Iv_matrix=matrix(Ivol,nrow=length(Strikes),ncol=3)
      x<-data.frame(Iv_matrix)
      rownames(x)<- Strikes
      print(x)
      plot(Strikes,x$X1,type="l",col="red")
      lines(Strikes,x$X2,col="green")
      lines(Strikes,x$X3,col="blue")
      break
    }
  } #j
}  

## Graphs for Put Implied Volatilities
for(i in 1:(length(ticker))){
  Ivol <-c()
  t=0
  time_tracker=0
  value = get_option(ticker[i],option_eDate)
  print(ticker[i])
  for(m in value){    
    time_tracker=time_tracker+1
    Strikes <- c()
    strike <- m$put$Strike
    bid <- m$puts$Bid
    ask <- m$puts$Ask
    Vol <- m$puts$Vol
    for(p in 1:length(Vol)){
      if(!is.null(Vol[p])){
        if(Vol[p]!=0){
          v=Bisection2(Prices[i],strike[p],bid[p],ask[p],a[time_tracker]/252)
          check = Prices[i]/strike[p]
          if((check > 0.85) & (check < 1.10)){
            if(v != 404){
              Strikes = append(Strikes, strike[p])  
              old=Strikes
              Ivol = append(Ivol, v)
            }
          }
        }
      }
    }
    if(is.null(Strikes)){
      Strikes=old
    }
    print(Strikes)
    t=t+1
    if(t == 3){
      print(Ivol)
      Iv_matrix=matrix(Ivol,nrow=length(Strikes),ncol=3)
      x<-data.frame(Iv_matrix)
      rownames(x)<- Strikes
      print(x)
      plot(Strikes,x$X1,type="l",col="red")
      lines(Strikes,x$X2,col="green")
      lines(Strikes,x$X3,col="blue")
      break
    }
  } #j
}  



# Question 10
# Calculate the derivatives of the call option price with respect to S (Delta), 
# and σ (Vega) and the second derivative with respect to S (Gamma).
#1 Call Delta  = exp(-q * t) * N(d1), where q is dividend yield which in our case is 0
# t is time in year

# Feb 16 expiry/ On 5th, 2nd and 31

K = 155
C0= 8.7
C1 = 6.68
C2 = 4.7
S0= 167.78
S1 = 159.8483
S2 = 155.8546

t= 9/252
#Delta
delta =(C2 - C1)/(S2 - S1)
delta
value=blackscholes(S2,K,r,t,Stocks_vol[1])  
pnorm(value$d1)

# Gamma
gamma = exp(-value$d1^2/2) * (1/sqrt(2*3.14*t)) * (1/(S2*Stocks_vol[1]))
gamma
# S2,C2= S1,C1=S+∆S, Sº,Cº= S-∆S 
gamma =(C2 - 2*C1 + C0)/(S2-S1)^2
gamma

#Vega
vega = S2 * sqrt(t) * ( exp(-value$d1^2/2)/ sqrt(2 * 3.14) )
vega



#Quesstion 11
iv=read.csv("ImpliedVolailityData11thFeb.csv",header=TRUE,sep=",")
iv = iv[,-1]
iv=iv[iv$IVolatility!="404",]
   
t=0
for(i in 1:(length(ticker))){
  value = get_option(ticker[i],option_eDate)
  for(i in value){
         
  }
}

df <- data.frame(Expiry=character(),Strikes=double(),Call=double(),Put=double(),stringsAsFactors = TRUE)


for(i in 1:length(iv$Expiry)){
 if(grepl("AAPL180216C",iv$Expiry[i])){
   value = blackscholes(Prices[1],iv$Strikes[i],r,2/252,iv$IVolatility[i])
   de = data.frame(Expiry=iv$Expiry[i],Strikes =iv$Strikes[i], Call = value$call, Put= value$put)
   df=rbind(df,de)
   }
  if(grepl("SPY180216C",iv$Expiry[i])){
    value = blackscholes(Prices[2],iv$Strikes[i],r,2/252,iv$IVolatility[i])
    de = data.frame(Expiry=iv$Expiry[i],Strikes =iv$Strikes[i], Call = value$call, Put= value$put)
    df=rbind(df,de)
  }
  if(grepl("VIX180216C",iv$Expiry[i])){
    value = blackscholes(Prices[3],iv$Strikes[i],r,2/252,iv$IVolatility[i])
    de = data.frame(Expiry=iv$Expiry[i],Strikes =iv$Strikes[i], Call = value$call, Put= value$put)
    df=rbind(df,de)
  }
  
}




#Part 3
#Numerical Integration of real-valued functions

# Question 1
# Trapezoid Rule
a=10^6
N=100000000
Sum=0.0
for(n in 1:(N-1)){
  x = -a + n * (2 * a)/N
  s=sin(x)/x
  Sum=sum(Sum,s,na.rm = T)
}

h= (-a + 2 * (2 * a)/N) - (-a + 1 * (2 * a)/N)

Integrand_value = h*(1/2 + Sum + sin(N)/2)
print(Integrand_value)

#Simpson Rule
Sum=0
for(n in 1:(N-1)){
  x = -a + n * (2 * a)/N
  if(x==0){
    s=1  
  }
  s=sin(x)/x
  if(x==-a | x==a){
    s=s/3  
  }
  else if(x %% 2==0){
    s=4*s/3  
  }
  else{
    s=2*s/3  
  }
  Sum=sum(Sum,s,na.rm = T)
}
Integrand_value = h*Sum
print(Integrand_value)


# Question 2
Pi <- 3.14
truncation_error <- function(a,N){
  Sum=0.0
  for(n in 1:(N-1)){
    x = -a + n * (2 * a)/N
    s=sin(x)/x
    Sum=sum(Sum,s,na.rm = T)
  }
  
  h= (-a + 2 * (2 * a)/N) - (-a + 1 * (2 * a)/N)
  
  Integrand_value = h*(1/2 + Sum + sin(N)/2)
  print(Integrand_value)
  return(Integrand_value - Pi)
}
a <- c(10^5,10^6,10^7)
N <- c(10^6, 10^7, 10^8)
error <- c()
for(i in 1:3){
  value =  truncation_error(a[i],N[i])
  error <-  append(error, value)
}

trunc_error <- do.call(rbind, Map(data.frame, a=a, N=N,approx = error))
trunc_error

# Design For Simpson Rule
truncation_error2<-function(a,N){
  Sum=0
  for(n in 1:(N-1)){
    x = -a + n * (2 * a)/N
    if(x==0){
      s=1  
    }
    s=sin(x)/x
    if(x==-a | x==a){
      s=s/3  
    }
    else if(x %% 2==0){
      s=4*s/3  
    }
    else{
      s=2*s/3  
    }
    Sum=sum(Sum,s,na.rm = T)
  }
  return(Sum)
}
a <- c(10^5,10^6,10^7)
N <- c(10^6, 10^7, 10^8)
error <- c()
for(i in 1:3){
  value =  truncation_error2(a[i],N[i])
  error <-  append(error, value)
}
trunc_error <- do.call(rbind, Map(data.frame, a=a, N=N,approx = error))
trunc_error


# As the value of a and N increases we can more precise and precise data with less error

# Question 3
#Trapezoid Convergence
a=10^6 
N=10000000
e=10^-4
c=0
Sum=0
previous=0
for(i in 2:(N)){
  for(n in 1:(i-1)){
    x = -a + n * (2 * a)/n
    s=sin(x)/x
    Sum=sum(Sum,s,na.rm = T)
    h= (-a + 2 * (2 * a)/n) - (-a + 1 * (2 * a)/n)
    Integrand_value = h*(1/2 + Sum + sin(n+1)/2)
    if(abs(Integrand_value-previous) < e){
      break 
    }
    c=c+1
    previous=Integrand_value
  }
}

print("To reach convergence the algo needs ")
print(c)

# Simpson Convergence
Sum=0
previous=0
c=0
for(i in 2:N){
  for(n in 1:(i-1)){
  x = -a + n * (2 * a)/n
  h= (-a + 2 * (2 * a)/n) - (-a + 1 * (2 * a)/n)
  if(x==0){
    s=1  
  }
  s=sin(x)/x
  if(x==-a | x==a){
    s=s/3  
  }
  else if(x %% 2==0){
    s=4*s/3  
  }
  else{
    s=2*s/3  
  }
  Sum=sum(Sum,s,na.rm = T)
  Integrand_value = h*Sum
  if(abs(Integrand_value-previous) < e){
    break 
  }
  c=c+1
  previous=Integrand_value
  }
}
print("To reach convergence the algo needs ")
print(c)

# Questoin 4
# Tapezoid Rule
h=0.0002
e=10^-4
c=0
Sum=0
previous=0
for(x in seq(from=0, to=2, by=h)){
  s=1 + exp(-x*2) * cos(8*x^(2/3))
  if(x==0 | x==2){
    s=s/2  
  }
  Sum=sum(Sum,s,na.rm = T)
  Integrand_value = h*Sum
  if(abs(Integrand_value-previous) < e){
    break
  }
  else{
    c=c+1
  }
  previous=Integrand_value
}
print("Integral value is")
print(Integrand_value)
print("To reach convergence the algo needs ")
print(c)


#Simpson Rule
h=0.0002
e=10^-4
c=0
Sum=0
previous=0
for(x in seq(from=0, to=2, by=h)){
  s=1 + exp(-x*2) * cos(8*x^(2/3))
  if(x==0 | x==2){
    s=s/3  
  }
  else if(x %% 2==0){
    s=4*s/3  
  }
  else{
    s=2*s/3  
  }
  Sum=sum(Sum,s,na.rm = T)
  Integrand_value = h*Sum
  if(abs(Integrand_value-previous) < e){
    break
  }
  else{
    c=c+1
  }
  previous=Integrand_value
}
print("Integral value is")
print(Integrand_value)
print("To reach convergence the algo needs ")
print(c)




