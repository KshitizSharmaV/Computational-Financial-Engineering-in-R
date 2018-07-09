rm(list=ls())
library(e1071)      
data=data.frame(read.csv("Implied Volatility.csv"))

thresold = .90
  
for(i in 1:6){
  var=paste0("Date.",i,sep="")
  var.number= which(names(data)%in%c(var))
  dates=data[var.number]
  iv=data[var.number+1]
  temp=c()
  for(j in 1:nrow(dates)){
    temp[j]=as.integer(substr(dates[j,],start = 1,stop = 4))
  }
  value=match(as.integer(2001),temp)
  
  dates=dates[value:nrow(dates),]
  iv=iv[value:nrow(iv),]
  
  new.data.frame=as.data.frame(cbind(dates, iv))
  
  new.data.frame=new.data.frame[!is.na(new.data.frame[, 1]),]
  
  perc.rank <- function(x) trunc(rank(x))/length(x)
  
  new.data.frame=within(new.data.frame,xr<-perc.rank(iv))
  
  frame.change.volatility=c(1:100)
  frame.change.volatility=as.data.frame(frame.change.volatility)
  colnames(frame.change.volatility)=c("Names")
  
  # Gives the data of current + 100 days change in IV 
  j=1  
  while(j<nrow(new.data.frame)){
    if(new.data.frame$xr[j]>thresold){
      temp.change.volatility=c()
      for(k in 1:100){
        temp.change.volatility[k]=(new.data.frame$iv[k+j]-new.data.frame$iv[j])/new.data.frame$iv[j]*100
      } # for k loop
      frame.change.volatility[, ncol(frame.change.volatility) + 1]=temp.change.volatility
      names(frame.change.volatility)[ncol(frame.change.volatility)] <- paste0("Change_", j)
      j=j+4 
    } # if
    j=j+1
  } # for j loop
  
  
  summary.stats=data.frame(Mean=double(),Standard.Deviation=double(),Skewness=double(),Kurtosis=double(),T.Test=double(),Winners=integer(),Losers=integer())
  # Summary Stats 
  for(j in 2:ncol(frame.change.volatility)){
    val=frame.change.volatility[,j]
    if(!is.na(val)[100]){
      m.t=mean(val)
      sd.t=sd(val)
      sk.t=skewness(val)
      ku.t=kurtosis(val)
      t.t=t.test(val)
      
      if(thresold==0.90){
        winners.t=sum(val<0)
        losers.t=sum(val>=0)
      }
      if(thresold==0.10){
        winners.t=sum(val>0)
        losers.t=sum(val<=0)
      }
      
      temp=data.frame(m.t,sd.t,sk.t,ku.t,t.t$p.value,winners.t,losers.t)
      colnames(temp)=c("Mean","Standard.Deviation","Skewness","Kurtosis","T.Test","Winners","Loserss")
      summary.stats=rbind(summary.stats,temp)    
    }
  }
  
  if(i==1){write.csv(summary.stats,"S&P500.csv")}else if(i==2){write.csv(summary.stats,"TY.csv")}else if(i==3){write.csv(summary.stats,"Crude.csv")}else if(i==4){write.csv(summary.stats,"Gold.csv")}else if(i==5){write.csv(summary.stats,"Euro.csv")}else if(i==6){write.csv(summary.stats,"Corn.csv")}
  
}

#write.csv(frame.change.volatility,"Answer.csv")
