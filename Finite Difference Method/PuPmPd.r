# Author Kshitiz Sharma
######## April 2018 ##############
# This code is made available mby author for learning purpose. The below implementations 
# are from book Implementing-Derivatives-Models (Les Clewlow and Chris Strickland)
# Anybody can use this code free of charge. The author of this code is not responsible
# for any harm or price calculation


# Graphing Pu, Pr, Pd for implicit FDM, You can do same for explicit just change 
# the line 28, 29 and 30 with the formula for Explicit 

pu.test=c()
pm.test=c()
pd.test=c()
sigma.test=c()

i=1
t=1
r=0.06
error=0.001
div = 0.03
for(sigma in seq(from=0.05, to=0.6,by=0.05)){
  sigma.test[i]=sigma
  dt = error/(1+3*sigma^2)
  dx=sigma*sqrt(3*dt)
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu.test[i] = -0.5*dt*((sigma/dx)^2 + nu/dx)
  pm.test[i] = 1.0 + dt*(sigma/dx)^2 + r*dt
  pd.test[i] = -0.5*dt*((sigma/dx)^2- nu/dx)
  i=i+1
}

plot(sigma.test,pu.test,col="red",type="l")
par(new=TRUE)
plot(pm.test,col="blue",type="l",xlab="",ylab="",axes=F)
axis(side = 4)
par(new=TRUE)
plot(pd.test,col="black",type="l",xlab="",ylab="",axes=F)