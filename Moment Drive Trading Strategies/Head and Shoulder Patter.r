# Author Kshitiz Sharma
# Permission is granted to use this code hence forth 
# I don't assure any kind of dependency if the code generate error for you
# This code is foe head and shoulder patern only you need to tweak variable for inverse HS pattern

######################################################
# Head Shoulder Patterns
# Substitute over here your price put prices = your closing prices

# Prices is your time series for your closing prices
prices <- time.series
# Give your time series above

# Using SM Library to smooth the prices using Kernel Regression estimation
library(sm)
t = 1:length(prices)
# fit kernel regression with cross-validatio
# t/1.3 is for smaller bandwith, the smaller the badnwidth is more, number of maxims and minimas you will have
# Refer Here for more http://faculty.washington.edu/yenchic/17Sp_302/R12.pdf
h = h.select(t/1.3, prices, method = 'cv')

temp = sm.regression(t, prices, h=h, display = 'none')
# find estimated fit
mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$prices

# second step is to find local extrema, tops and bottoms, using first 
# derivative of the kernel regression estimator. 
temp = diff(sign(diff(mhat)))
# loc - location of extrema, loc.dir - direction of extrema
loc = which( temp != 0 ) + 1
loc.dir = -sign(temp[(loc - 1)])

patterns.price = data.frame(E1=double(),E2=double(),E3=double(),E4=double(),E5=double())
patterns.position = data.frame(E1=double(),E2=double(),E3=double(),E4=double(),E5=double())
for(i in 1:(length(loc)-5)){
  E1=prices[loc[i]]
  E2=prices[loc[i+1]]
  E3=prices[loc[i+2]]
  E4=prices[loc[i+3]]
  E5=prices[loc[i+4]]
  
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


plot(1:length(prices),prices,type="l")
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



for(t in patterns.position[1,5]:length(prices)){
  prices=prices[t]
  if(prices<cutoff){
    print("Enter the short Position")
    price_shorted_at=prices[t]
    break
  }
}

target_price = price_shorted_at - patter_height
target_price
# The Target Price is 8.96

abline(h=target_price,col="blue")










