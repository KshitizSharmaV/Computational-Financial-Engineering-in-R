library(quantmod)
library(tseries)
library(chron)
library(timeDate)

ticker <- c("JPM", "WFC", "BAC", "C", "GS", "V", "AIG", "MS", "CME", "DFS")

daily.log.return <- NULL

for(i in 1:10){
  table1 <- getSymbols(ticker[i], from = "2014-01-01", auto.assign = F)
  temp.log.return <- dailyReturn(table1, type = "log")
  daily.log.return <- cbind(daily.log.return, temp.log.return)
}

colnames(daily.log.return) <- ticker

monthly.simple.return <- NULL

for(i in 1:10){
  table1 <- getSymbols(ticker[i], from = "2014-01-01", auto.assign = F)
  temp.simple.return <- monthlyReturn(table1, type = "arithmetic")
  monthly.simple.return <- cbind(monthly.simple.return, temp.simple.return)
  
}

monthly.simple.return <- as.data.frame(monthly.simple.return)

jarque.bera.test(monthly.simple.return[,1])
# Since the P-value is 0.5656, this indicates this equity is normal distributed
jarque.bera.test(monthly.simple.return[,2])
jarque.bera.test(monthly.simple.return[,3])
jarque.bera.test(monthly.simple.return[,4])
jarque.bera.test(monthly.simple.return[,5])
jarque.bera.test(monthly.simple.return[,6])
jarque.bera.test(monthly.simple.return[,7])
jarque.bera.test(monthly.simple.return[,8])
jarque.bera.test(monthly.simple.return[,9])
jarque.bera.test(monthly.simple.return[,10])

cor.table <- cor(daily.log.return)

for(i in 1:10){
  cor.table[i,i] <- 0
}

which.max(cor.table)
#equity BAC and C has the highest correlation

timeline1 <- getSymbols("BAC", from = "2014-01-01", auto.assign = F)
timeline1 <- as.data.frame(timeline1)

write.csv(timeline1, row.names=T, "temp.csv")

timeline2 <- read.csv("temp.csv")

timeline3 <- as.Date(timeline2$X)
nrow(daily.log.return)

plot(timeline3, daily.log.return[,3], type = "l")
lines(timeline3, daily.log.return[,4], type = "l", col = "red")
