# Calculating CAPM return 
# r(equity) = rf + beta * (rm - rf)
# rf - risk free rate of return
# rm - market return
# beta - Beta is a measure of the volatility, or systematic risk, of a security or a portfolio 
# in comparison to the market as a whole. Beta is used in the capital asset pricing model (CAPM), 
# which calculates the expected return of an asset based on its beta and expected market returns.


# Part 1 Calculating the stock log returns
rm(list=ls())
#*# Enter your stocks symbol
ticker=c('AAPL','GOOGL','GS','JPM','AMZN','KO')

#*# from_date
from.date= "2015-01-01"


monthly.log.return = NULL
n=length(ticker)
for(i in 1:n){
    #*# to date
    table1 = getSymbols(ticker[i],from=from.date,to="2018-01-01",auto.assign = F)
    temp.log.return=monthlyReturn(table1,type="log")
    monthly.log.return=cbind(monthly.log.return,temp.log.return)
}
colnames(monthly.log.return) = ticker
monthly.log.return = as.data.frame(monthly.log.return)

# Part 2 Calculating the market returna and downloading 3 month T-bill

#Downloading SP500 and calculateing log returns
#*# to date
getSymbols('^GSPC',from =from.date,to="2018-01-01")
#*# change the year after which you want data for
SP500=data.frame(GSPC['2015::'])
SP500<- monthlyReturn(SP500, type = "log")
SP500.mean=mean(SP500)

# 3 Months treasury bills as risk freee interest rates
#*# to date
getSymbols('DGS3MO', src='FRED',from =from.date,to="2017-12-01")
#*# change the year after which you want data for
data=DGS3MO['2015::']
Rate=data.frame(to.monthly(data))
rf=Rate/100
rf.mean=mean(rf$data.Close)

# Part 2 Calculating Beta
# Beta = covariance(market return, stock return)/var(market return)

Market.Var= var(SP500)[1][1]
beta.stocks = NULL

for(i in 1:n){
    b= cov(monthly.log.return[i],SP500)/Market.Var
    beta.stocks=c(beta.stocks,b)
}

# Part 2 Calculating CAPM Return
CAPM.Returns= rf.mean + beta.stocks*(SP500.mean - rf.mean)

print("Beta for Stocks")
print(beta.stocks)

print("CAPM returns")
print(CAPM.Returns)

# graoh Just for Apple Stocks
data=as.vector(SP500[,1])
data2=as.vector(monthly.log.return[,1])
plot(data,data2,xlab="SP 500", ylab="Apple log returns",pch=19)
abline(lm(data2~data), col="red") # regression line (y~x) 



