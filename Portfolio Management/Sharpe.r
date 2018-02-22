rm(list=ls())
library(PortfolioAnalytics)
library(quantmod)
library(xts)
library(tseries)

monthly_stock_returns <- function(ticker, start_year) {
  # Download the data from Yahoo finance
  symbol <- getSymbols(ticker, src = 'yahoo', auto.assign = FALSE, warnings = FALSE) 
  # Tranform it to monthly returns using the periodReturn function from quantmod
  data <- periodReturn(symbol, period = 'daily', subset=paste(start_year, "::", sep = ""), 
                       type = 'log')
  
  # Let's rename the column of returns to something intuitive because the column name is what
  # will eventually be displayed on the time series graph.
  colnames(data) <- as.character(ticker)
  
  # We want to be able to work with the xts objects that result from this function 
  # so let's explicitly put them to the global environment with an easy to use 
  # name, the stock ticker.
  assign(ticker, data, .GlobalEnv)
}

year <- "2017-06-06"

#rf = .001183
rf= 0.0000563


monthly_stock_returns('AAPL', year)
monthly_stock_returns('JPM', year)
monthly_stock_returns('AMZN', year)
monthly_stock_returns('DVA', year)
monthly_stock_returns('IBM', year)
monthly_stock_returns('KO', year)
BRK=monthly_stock_returns('BRK-B', year)
monthly_stock_returns('BAC', year)
monthly_stock_returns('BA', year)

tickers=c("AAPL", "JPM", "AMZN", "DVA", "IBM","KO","BRK","BAC","BA")
# Merge the 3 monthly return xts objects into 1 xts object.
merged_returns <- merge.xts(AAPL, JPM, AMZN, DVA, IBM, KO, BRK, BAC, BA)

# Before we combine these into a portfolio, graph the individual returns and 
# see if anything jumps out as unusual. It looks like something happened to  
# Google in March of 2014, but that something didn't affect JP Morgan or Amazon.
dygraph(merged_returns) %>% 
  dyAxis("y", label = "%") %>% 
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) 

#fund.names <- colnames(merged_returns)
#pspec <- portfolio.spec(assets=fund.names)
#print.default(pspec)
#pspec <- add.constraint(portfolio=pspec,type="weight_sum",min_sum=1,max_sum=1)
#pspec <- add.constraint(portfolio=pspec, type="box",min=0.05,max=0.4)
#pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=9,max_pos_short=0)
#pspec <- add.constraint(portfolio=pspec, type="return", return_target=0.07)
#summary(pspec)
#maxret <- add.objective(portfolio=pspec, type="return", name="mean")
#pt_maxret <- optimize.portfolio(R=merged_returns, portfolio=maxret,optimize_method="ROI", trace=TRUE)



eq_weights <- c( .75, .10, .10, .10, .75, .75, .20, .50, .10)

means <- apply(merged_returns, 2, "mean")
sds <- apply(merged_returns, 2, "sd")
returns <- merged_returns


plot(sds,means)
text(sds, means, labels = colnames(merged_returns), cex = 0.007)
abline(h = 0, lty = 3)



opt <- portfolio.optim(returns)
pf_weights <- opt$pw
pf_weights2 <- pf_weights*100
names(pf_weights) <- colnames(returns)
pf_weights*100



# Select optimum weights opt_weights
#opt_weights <- pf_weights[pf_weights >= 0.01] #How is 0.01 determined?

# Barplot of opt_weights
barplot(pf_weights)


max_weights1 <- rep(0.20, ncol(returns))

pf_mean <- portfolio.optim(returns, reshigh = max_weights1, pm = 1.1 * mean(returns))
pf_weights3<-pf_mean$pw
names(pf_weights3) <- colnames(returns)
barplot(pf_weights3)



opt2 <- portfolio.optim(returns, reshigh = max_weights1, pm = 1.07 * mean(returns))
pf_weights4<-opt2$pw
names(pf_weights4) <- colnames(returns)
barplot(pf_weights4)




