
# What is the value of a European swap option that gives the holder the right to enter into a 
# 3- year annual-pay swap in four years where a fixed rate of 5% is paid and LIBOR is received? 
# The swap principal is $10 million. Assume that the yield curve is flat at 5% per annum with
# annual compounding and the volatility of the swap rate is 30%.
rm(list=ls())
# Time for bond option execrise price
T=4
# Time for which swap will last
t=3
# Forward Swap rate
S.0=0.05
# Principal
L=10000000
# Rate in Swap Which Owne has to pay
S.K = 0.05
# volatility
sigma= 0.3

A.sum=0
for(i in seq(from=5,to=7,by=1)){
  print(i)
  A.sum =  A.sum + exp(-S.0 * i)
}
d.1 = (log(S.0/S.K) + sigma^2*T/2)/(sigma*sqrt(T))
d.2 = d.1 - sigma*sqrt(T)

# Since floating is being paid and we are reciving fixed LIBOR
# It's a call option on bond with a strike price at par
value.call = L * A.sum*(S.0*pnorm(d.1) - S.K* pnorm(d.2))
value.call
# 262271.2

value.put = L * A.sum*(S.K* pnorm(-d.2) - S.0*pnorm(-d.1))
value.put
# 262271.2



# Question 2
