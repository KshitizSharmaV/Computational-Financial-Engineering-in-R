rm(list=ls())

ibm = 0.40
tbill = 0.30
cyuan = 0.30

# Estimate the portfolio's current value P0
portfolio = 10000000
ibm = ibm*portfolio
tbill = tbill*portfolio
cyuan = cyuan*portfolio

no.share.ibm = ibm / 80
no.share.tbill = tbill/ 90000
no.yuan = cyuan * 6.1

# the portfolio has 

80*no.share.ibm + 90000*no.share.tbill +no.yuan/6.1



M=3000000
Xt=80
Yt=90000
Zt=6.1
dt =0.001
t=10/252
Xt.arr=c()
Yt.arr=c()
Zt.arr=c()


for(i in 1:M){
  t=dt
  Xt=80
  Yt=90000
  Zt=6.1
  for(j in 1:40){
    Xt= Xt+ 0.01*Xt*dt + 0.3*Xt*rnorm(1,0,1)*sqrt(dt)
    Yt= Yt + 100*(90000 + 1000*t - Yt)*dt +  sqrt(Yt)*rnorm(1,0,1)*sqrt(dt)
    Zt = Zt + 5*(6-Zt)*dt+ 0.01*sqrt(Zt*dt)*rnorm(1,0,1)
    t=t+dt
  }
  Xt.arr[i]=Xt
  Yt.arr[i]=Yt
  Zt.arr[i]=Zt
}

Xt.arr
Yt.arr
Zt.arr
Rt=c()

# Another Method
v1=0
for(i in 1:M){
  v1 = Xt.arr[i]*no.share.ibm + Yt.arr[i]*no.share.tbill + no.yuan/Zt.arr[i]
  Rt[i]=((v1-portfolio)/portfolio)*100
}
# The Return Is

Rt=sort(Rt)

qqplot(c(1:M),Rt)
index=M*0.01
Rt[index]
# -5.344098

# We may expect with 99% confidence that, our loss will not exceed more than
(portfolio * Rt[index])/100
# - 534409.8

data.file <- data.frame("IBM"=Xt.arr,"TBill"=Yt.arr,"USY"=Zt.arr)
corr.matrix=matrix(nrow=3,ncol=3)
# Build the portfolio's coerelated matrix using stock historical data.
for(i in seq(1,3,by=1)){
  for(j in seq(1,3,by=1)){
    corr.matrix[i,j]=cor(data.file[,i],data.file[,j])
  }
}
corr.matrix
# Create the Cholesky decomposition of the covariance matrix.
R=chol(corr.matrix)
R
price.file <- data.frame("IBM"=Xt.arr,"TBill"=Yt.arr,"USY"=Zt.arr)
for(i in 1:M){
  Xt=80
  Yt=90000
  Zt=6.1
  t=dt
  for(j in 1:40){
    
    # Generate a vector of n independent standard normal variates
    Z.values = rnorm(3,mean=0,sd=1)
    Z=matrix(Z.values,nrow=3,ncol = 1)
    Z
    #multiply the matrix resulting from the Cholesky decomposition with the vector of standard normal 
    #variates in order to get a vector of correlated variates.
    W=R %*% Z
    W=W*dt
    #print(W)
    
    Xt= Xt+0.01*Xt*dt + 0.3*Xt*W[1,1]*sqrt(dt)
    Yt= Yt + 100*(90000 + 1000*t - Yt)*dt +  sqrt(Yt)*W[1,1]*sqrt(dt)
    Zt = Zt + 5*(6-Zt)*dt+ 0.01*sqrt(Zt*dt)*W[1,1]
    t=t+dt
  }
  v1 = Xt.arr[i]*no.share.ibm + Yt.arr[i]*no.share.tbill + Zt.arr[i]*no.yuan
  Rt[i]=((v1-portfolio)/portfolio)*100
  #print(Rt)
  #Avg.return =Avg.return + Rt
}
Rt=sort(Rt)
qqplot(c(1:M),sort(Rt))
Rt[index]
# -5.333644
(portfolio * Rt[index])/100
#######
# part c 
# Finding the CVAR

value=0
for(i in 1:index){
  value = value + Rt[i]   
}
((value/index)*portfolio)/100
# -604778.2

# We may expect with 99% confidence that, our loss will not exceed more than

