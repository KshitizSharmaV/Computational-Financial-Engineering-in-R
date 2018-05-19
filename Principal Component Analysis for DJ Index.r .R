library(quantmod)
stockData <- new.env()
lookup.symb=c("AAPL","AXP","BA", "CAT","CSCO","CVX","KO","DWDP","XOM","GE","GS","HD",
              "IBM","INTC","JNJ","JPM","MCD","MMM","MRK","MSFT","NKE","PFE","PG","TRV","UNH","UTX",
              "V","VZ","WMT","DIS")
getSymbols(lookup.symb, from="2013-05-12", env=stockData, src="yahoo")
ReturnMatrix=NULL


for(i in 1:length(lookup.symb))
{
  tmp <- get(lookup.symb[i], pos=stockData)   # get data from stockData environment  
  ReturnMatrix=cbind(ReturnMatrix,   (Cl(tmp)-Op(tmp)) / Op(tmp)   )
  colnames(ReturnMatrix)[i]=lookup.symb[i]
}

t=1260
Yit=NULL
sigma.i=c()
for(i in 1:length(lookup.symb)){
  tmp <- get(lookup.symb[i], pos=ReturnMatrix)  
  R.bar <- sum(tmp)/t
  print(tmp)
  print(R.bar)
  sigma.i[i]<- sqrt(sum((tmp-R.bar)^2)/t)
  Yit=cbind(Yit,(tmp-R.bar)/sigma.i[i])
  colnames(Yit)[i]=lookup.symb[i]
}

# Part 2
Cit = cor(Yit)/t
Cit

# Part 3  
Prin.Comp = prcomp(Cit, scale = T) 
summary(Prin.Comp)

names(Prin.Comp)

#The rotation contains the directions of the principal components (eigenvectors)
eigen.vectors=Prin.Comp$rotation
eigen.vectors
# the eigenvalues
eigen.values=Prin.Comp$sdev^2 
eigen.values
plot(eigen.values,type="l")

sum(eigen.values[1:5])/sum(eigen.values)

# Part 4
V=eigen.vectors[,1]
Factors=c()

for(j in 1:t){
  tmp <- ReturnMatrix[j,]
  Sum=0
  for(i in 1:length(lookup.symb)){
    Sum=Sum + (V[i]/sigma.i[i])*tmp[,i]
  }
  Factors[j]= Sum/sqrt(eigen.values[1])
}
Factors

mean(Factors)
# 0.03300924

sd(Factors)
# 0.8462316

# Part 5
DIA <- new.env()
getSymbols("DIA", from="2013-05-12", env=DIA, src="yahoo")
temp=get("DIA",pos=DIA)
temp$DIA.Close
DIA.return = (temp$DIA.Close-temp$DIA.Open) / temp$DIA.Open
DIA.standardized.return = (DIA.return - mean(DIA.return))/sd(DIA.return)

plot(Factors,type="p")
plot(DIA.standardized.return,type="p")
linearmod <- lm(Factors ~ DIA.standardized.return)
summary(linearmod)
# Multiple R-squared:  0.7349 

# Part 6

Final.k.t=NULL
f.names = c("F1t","Ft2","F3t","Ft4","Ft5")
for(f in 1:5){
  V=eigen.vectors[,f]
  Factors=c()
  for(j in 1:t){
    tmp <- ReturnMatrix[j,]
    Sum=0
    for(i in 1:length(lookup.symb)){
      Sum=Sum + V[i]/sigma.i[i]*tmp[,i]
    }
    Factors[j]= Sum/sqrt(eigen.values[f])
  }
  Final.k.t = cbind(Final.k.t,Factors)
  colnames(Final.k.t)[f]=f.names[f]
}

Final.k.t

# Finding standardized returns for Equities
equity.sd.returns=NULL
for(i in 1:length(lookup.symb)){
  tmp <- get(lookup.symb[i], pos=ReturnMatrix)  
  check = (tmp - mean(tmp))/sd(tmp)
  equity.sd.returns=cbind(equity.sd.returns,check)
  colnames(equity.sd.returns)[i]=lookup.symb[i]
}
equity.sd.returns

# Run a regression with 5 factors and obtain parameters in Beta sk
coefficients.table=NULL
for(i in 1:length(lookup.symb)){
  v=lm(formula = equity.sd.returns[,i] ~ Final.k.t[,1] + Final.k.t[,2]+ Final.k.t[,3]+ Final.k.t[,4]+ Final.k.t[,5])
  coefficients.table=cbind(coefficients.table,v$coefficients)  
  colnames(coefficients.table)[i]=lookup.symb[i]
}
coefficients.table

# Factor model for stock returns:
Next.Ten.Day.Return = NULL
for(i in 1:length(lookup.symb)){
  check=c()
  tmp <- get(lookup.symb[i], pos=ReturnMatrix)
  for(j in 1:10){
    F.k=rt(5,3.5)
    G.s=rt(1,3.5)
    sample= coefficients.table[,i][2]*F.k[1]+coefficients.table[,i][3]*F.k[2]+coefficients.table[,i][4]*F.k[3]+coefficients.table[,i][5]*F.k[4]+coefficients.table[,i][6]*F.k[5]
    check[j]= mean(tmp)+sd(tmp)*sample+sd(tmp)*sqrt(1-sum(coefficients.table[,i][-1]^2))*G.s
  }
  Next.Ten.Day.Return = cbind(Next.Ten.Day.Return,check)
  colnames(Next.Ten.Day.Return)[i]=lookup.symb[i]
}
sum(Next.Ten.Day.Return)

# Portfolio returs for next 10 days
Portfolio=c()
for(i in 1:10){
  Portfolio[i]=sum(Next.Ten.Day.Return[i,])
}

# Equally weighted portfolio value, Run Stimulation 100000 times
Return.For.Var=c()
stimulations=100000
Portfolio=c()
for(h in 1:stimulations){
  Next.Ten.Day.Return = NULL
  for(i in 1:length(lookup.symb)){
    check=c()
    tmp <- get(lookup.symb[i], pos=ReturnMatrix)
    for(j in 1:5){
      F.k=rt(5,3.5)
      G.s=rt(1,3.5)
      sample= coefficients.table[,i][2]*F.k[1]+coefficients.table[,i][3]*F.k[2]+coefficients.table[,i][4]*F.k[3]+coefficients.table[,i][5]*F.k[4]+coefficients.table[,i][6]*F.k[5]
      check[j]= mean(tmp)+sd(tmp)*sample+sd(tmp)*sqrt(1-sum(coefficients.table[,i][-1]^2))*G.s
    }
    Next.Ten.Day.Return = cbind(Next.Ten.Day.Return,check)
    colnames(Next.Ten.Day.Return)[i]=lookup.symb[i]
  }
  Portfolio[h]=sum(Next.Ten.Day.Return)
}
Rt=sort(Portfolio)
Rt[stimulations*0.01]
qqnorm(Rt)
Rt.inverse[index]