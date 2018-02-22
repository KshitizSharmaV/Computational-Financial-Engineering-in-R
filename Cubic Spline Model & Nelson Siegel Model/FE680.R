
# Section 1.1
# Discount Curve Values
d <-c(0.9813,0.9592,0.9385,0.9161,0.8937,0.8587,0.8219,0.7960,0.7625,0.7276)
t<-c(1:10)
length(d)
# Zero Curve
z<-c()
for(i in 1:10){
  value<-(1/d[i])^(1/t[i])-1
  z<-append(z,value)
}
z*100

# Forward Curve
f <- c()
for(i in 2:10){
  value<-(d[i-1]/d[i])-1
  print(d[i])
  print(d[i-1])
  print(value)
  print("*****")
  f<-append(f,value)
}
f*100

# Section 1.2
# Calculating The present Value 
cf<-c(4,4,4,4,4,4,4,4,104)
pv1<-c()
pv_sum=0
temp=0
for(i in 1:9){
  temp=temp+(cf[i]*d[i])
  pv1=append(pv1,temp)
}
temp
pv1


# Section 1.3
f1 <- c(0.019,0.02304003,0.02205647,0.02445148,0.02506434,0.04075929,0.04477430,0.0325,0.0439,0.0479)
f1=f1*100
d2<-c()
temp=1
f2=(f1)+0.5
# New Discount Factor
for(i in 1:10){
  temp=temp*(1/(1+f2[i]/100))
  d2<-append(d2, temp)
}
d2


#New PV
pv2<-c()
temp=0
for(i in 1:9){
  temp=temp+(cf[i]*d2[i])
  pv2=append(pv2,temp)
}
temp
pv2

# Calculating DV01
#dv01= -(∆P/∆y)

dv01<-c()
temp=0
for(i in 1:9){
  temp = -(1/1000)*((pv2[i]-pv1[i])/(f2[i]-f1[i]))
  dv01=append(dv01,temp)
}
dv01*100


# Section 1.4
inc<-c(1,2,3)
for(j in 1:3){
  print("Increase in percetntage by")
  print(inc[j])
  d2<-c()
  temp=1
  f2=(f1)+inc[j]
  # New Discount Factor
  for(i in 1:10){
    temp=temp*(1/(1+f2[i]/100))
    d2<-append(d2, temp)
  }
  print("Discount Factor")
  print(d2)
  
  pv2<-c()
  temp=0
  for(i in 1:9){
    temp=temp+(cf[i]*d2[i])
    pv2=append(pv2,temp)
  }
  print("Present Value")
  print(pv2)
}


#Section 1.5
# Making use of cubic spline model to fund the yield for 1.5 years
t <- c(0,1,2,3,4,5,6,7,8,9,10)
y <- c(1.6, 1.9, 2.1, 2.2, 2.45,2.5, 2.55, 2.8, 2.85,3,3.15)
spl <- smooth.spline(y ~ t)
s<-predict(spl, 1.5)
# Yield Rate is 1.9688
Forwar_Price = 100*exp(.0196*1.5)

# Foward Price Of Bond Is 102.9836


#Section 1.6
#Duration of a bond

duration = - ((-102.9836 + 100)/(1.96888 - 1.6))/100
#0.08088267


#Answer 3
# Section 3 
t <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 30)
y <- c(0.17, 0.23, 0.30, 0.61, 0.90, 1.69, 2.42, 3.08, 4.36)
spl <- smooth.spline(y ~ t)
plot(spl, ylab = 'Yield', xlab = 'Years', main = ' Yield Curve')
lines(spl)
predict(spl, t)
new_t <- seq(from = 0.5, to = 30, by = 0.5)
s<-predict(spl, new_t)
spl <- smooth.spline(s$y ~ s$x)
plot(spl, ylab = 'Yield', xlab = 'Years', main = ' Yield Curve')
lines(spl)


# Answer 4
# Seciton 4

next_p <- c(0.4356, 0.2644, 0.2658, 0.4342, 0.0192, 0.4753, 0.3534, 0.1000, 0.2685, 0.4342, 0.2274, 0.1027, 0.2712, 0.4370, 0.4822, 0.2260, 0.4822, 0.2260, 0.2301, 0.4808, 0.4932, 0.4959, 0.2397, 0.4959, 0.2397, 0.4959, 0.2397, 0.2438, 0.4959)
maturity_t <- c(0.4356, 0.7644, 1.2658, 1.9342, 2.0192, 2.9753, 3.3534, 3.6000, 4.2685, 4.9342, 5.2274, 5.6027, 6.2712, 6.9370, 7.4822, 7.7260, 8.4822, 8.7260, 9.2301,9.9808, 25.4932, 26.4959, 26.7397, 27.4959, 27.7379, 28.4959, 28.7397, 29.2438, 29.9945)
coupon <- c(0.875, 0.875, 0.750, 0.625, 0.375, 0.750,1.5, 1.75, 2.125, 1.75, 4.5, 2.375, 2.750, 2.375, 3.5, 3.875, 2.750, 3.125, 3.375, 2.625, 4.5, 4.75, 5, 4.375, 4.5, 3.5, 4.25, 4.375, 3.875)
clean_price <- c(100.3, 100.48, 100.50, 100.31, 99.78, 100.16, 102.34, 103.08, 104.19, 102.06, 115.91, 104.36, 105.86, 102.97, 110.53, 113.09, 103.98, 106.5, 108.00, 101.19, 117.58, 122.28, 126.97, 115.19, 117.47, 98.98, 112.44, 114.67, 105.75)

# Calculate the Dirty Bond Price
Bond_Dirty_Price <- c()
Delta = 0.5
P=100
for(i in 1:29){
    value = clean_price[i]+ (Delta * (coupon[i]/100) * P) * (maturity_t[i]-next_p[i])/Delta
    Bond_Dirty_Price = append(Bond_Dirty_Price, value)
}
Bond_Dirty_Price

# Calculating the yield to maturity for each maturity

#Calculating r1
r <- c()
for(i in 1:29){
  Sum_of_coupons=0
  V=Bond_Dirty_Price[i]
  Cou=(Delta*(coupon[i]/100)*P)
  for(j in 1:(i-1)){
    if(i==1){
      Sum_of_coupons=0
    }  
    else{
      Sum_of_coupons = Sum_of_coupons + Cou/((1+r[j])^maturity_t[j])
    }
  }
  value =V-Sum_of_coupons
  r1 = ((Cou+P)/value)^(1/maturity_t[i])-1
  r[i]=r1
}
r=r*100
NS<-Nelson.Siegel(r,maturity_t)
NS

#length(next_p)
#length(maturity_t)
#length(coupon)
#length(clean_price)






















