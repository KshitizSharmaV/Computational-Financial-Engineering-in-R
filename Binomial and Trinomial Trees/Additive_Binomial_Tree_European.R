# additive binomial tree to calculate the values of the
# European Call option and for the European Put option
additive_binomial_tree_european = function(K,T,S,sigma,r,N,flag){
  # Set Coefficients Trigeorgis
  dt = T/N
  v = r-0.5*sigma^2
  dxu = sqrt(sigma^2*dt + (v*dt)^2)
  dxd = -dxu
  pu = 0.5 + 0.5*(v*dt/dxu)
  pd = 1-pu
  # Precompute Constants
  dsc = exp(-r*dt)
  # Intialize Price at Maturity N
  #St=c()
  #St=append(St,S*exp(N*dxd))
  #for(i in 1:N){
  #  St=append(St,St[i-1]*exp(dxu-dxd))
  #}
  # Intialize Price at Maturity N
  St=c()
  St[1]=S*exp(N*dxd)
  for(i in 2:(N+1)){
    St[i]=St[i-1]*exp(dxu-dxd)
  }
  # Intalize Options Value At Maturity
  C.Value = c()
  if(flag==1){
    print("Call Eurpean Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,St[i]-K)
    }
  }else{
    print("Put Eurpean Option with Trigeorgis")
    for(i in 1:(N+1)){
      C.Value[i]= max(0.0,K-St[i])
    }
  }
  print(St)
  print(C.Value)
  #Step Back Through The Tree
  for(i in N:1){
    for(j in 1:i){
      C.Value[j]= dsc * ( pu * C.Value[j+1] + pd*C.Value[j])
    }
    print(C.Value)
  }
}
# Flag at the end of function is 1 for call
additive_binomial_tree_european(100,1, 100, 0.2, 0.06, 3,1)
# Flag at the end of function is 0 for put
additive_binomial_tree_european(100,1, 100, 0.2, 0.06, 3,0)
