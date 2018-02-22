# Black-Litterman Model
# Parameters needed
# E[R]=[(????) +P'?? P][(????) ??+P'?? Q]

# A - Price of Risk
# S - Variance- Cvovariance Matrix S
# w - Market Weights
# Pi- Implied Equilibrium Excress returns as vector
# Q - Views
# P - Link Matrix
# Omega - Uncertainity of views


############################### Part 1
# We are going to calclate value for risk aversion A
# A = (E(rm)-rf)/(variance)

rm(list=ls())
library(quantmod)
library(quandl)

#Downloading SP500 and calculateing log returns
getSymbols('^GSPC',from ="2010-01-01",to="2018-01-01")
SP500=data.frame(GSPC['2010::'])
SP500<- monthlyReturn(SP500, type = "log")

# 3 Months treasury bills as risk freee interest rates
getSymbols('DGS3MO', src='FRED',from ="2010-01-01",to="2017-12-01")
data=DGS3MO['2010::']
Rate=data.frame(to.monthly(data))
rf=Rate/100
rf=rf$data.Close

#Calculate Market Excess return
Market_excess_returns=(SP500$monthly.returns - rf)
#Calculate average value i.e (E(rm)-rf))
Avg.Market_excess=mean(Market_excess_returns)

#Calculate market Variance
v=var(SP500$monthly.returns)[1]

A= Avg.Market_excess/v
#A 5.873085 has + ve return per unit risk

##############################Part 2
# Calculation of Variance-Covariance Matrix
# Make A portfolio of 6 companies - 'AAPL','','GS','TSLA','AMZN','KO'
ticker=c('AAPL','GOOGL','GS','JPM','AMZN','KO')

monthly.log.return = NULL
for(i in 1:6){
    table1 = getSymbols(ticker[i],from="2010-01-01",auto.assign = F)
    temp.log.return=monthlyReturn(table1,type="log")
    monthly.log.return=cbind(monthly.log.return,temp.log.return)
}
colnames(monthly.log.return) = ticker
monthly.log.return = as.data.frame(monthly.log.return)

cov.table = cov(monthly.log.return)
cov.matrix = data.matrix(cov.table)


##############################Part 3

# Calculating Market Capitalization of each stock we took
market.cap=c(881.561,722.913,97.686,370.687,561.419,192.966)
market.weights=market.cap/sum(market.cap)
market.weights.matrix=matrix(market.weights,nrow=6,ncol=1)


##############################Part 4
# Pi Implied Equilibrium Excress returns as vector
# Pi = ASw

# Black-Litterman Function says that we don't need to maximise investors market utility function
# because weights are aready observed in the market. So we took market weights and went one step 
# back and reverse the problem by calculating Implied Equlibrium excess return

# Note for developer cov.matrix and market.weights.matrix are matrix and multiplied by %*%
Pi=A * (cov.matrix %*% market.weights.matrix)

# Implied Equilibrium Excress returns as vector
#        [,1]
# AAPL  0.015290783
# GOOGL 0.015067183
# GS    0.012574669
# JPM   0.011417493
# AMZN  0.015850409
# KO    0.005206992


##############################Part 4
# Building views and link Matricies
# Q is the views which are expressed, these views are relatives
# View 1 - Apple will beat MSFT by 10%
# View 2 - GS will beat JPM by 15%
# View 3 - AMZN will beat KO by 30%
Q=matrix(c(0.10,0.15,0.30),nrow=3,ncol=1)

# P is the link Matrix
P=matrix(c(1,-1,0,0,0,0,0,0,1,-1,0,0,0,0,0,0,1,-1),nrow=3,ncol=6,byrow=T)

##############################Part 5

# Omega which is uncertanity of views
tao=1
Omega = tao *((P %*% cov.matrix)  %*% t(P))
#              [,1]          [,2]          [,3]
#[1,]  0.0059677819 -0.0002419764 -0.0004714597
#[2,] -0.0002419764  0.0017297293  0.0002619576
#[3,] -0.0004714597  0.0002619576  0.0050632394

##############################Part 6
# Final Calculation for Black Litteran Model
# We will calculate Black Litterman in two parts equation

Black.Litterman.weights.ensure =  solve(solve(tao*cov.matrix)+(t(P)%*%solve(Omega))%*%P)
Black.Litterman.weights.average = solve(tao*cov.matrix)%*%Pi + (t(P)%*%solve(Omega))%*%Q


BLreturn = Black.Litterman.weights.ensure %*% Black.Litterman.weights.average

print(BLreturn)








