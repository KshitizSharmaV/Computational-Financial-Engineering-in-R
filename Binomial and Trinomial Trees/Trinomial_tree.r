# Trinomial tree to price an American Put option
# Since there are three possible movements in the stock price  in trinomial tree
# as compared to binomial it provides a better aproximation technique. 
# Because of trinomial tree more regular grid it's more flexible to work with 
# and it provides easy extension to timing varying drift and volatility paramters
trinomial_tree_american_put = function(K, T, S, sig, r, div, N, Nj, dx){
  # Precompute Constants
  dt = T/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  pu = 0.5*dt*((sig/dx)^2 + nu/dx)
  pm = 1.0 - dt*(sig/dx)^2 - r*dt
  pd = 0.5*dt*((sig/dx)^2 - nu/dx)
  # Intialise asset prices at maturity
  St = c()
  St[2*Nj+1]=S*exp(-Nj*dx)
  for(j in (2*Nj):1){
    St[j]=St[j+1]*edx
  }
  print(St)
  #Intialize option value at maturity
  for(j in (2*Nj+1):1){
    Option.value[j,Nj+1]  = max(0,K-St[j])
  }
  #Step back through the lattice
  for(i in N:1){
    for(j in (2*Nj):2){
      Option.value[j,i]=pu*(Option.value[j+1,i+1]) + pm*(Option.value[j,i+1]) + pd*(Option.value[j-1,i+1])
      #Boundary Conditions
      Option.value[2*Nj+1,i]=Option.value[2*Nj,i] + (St[2*Nj]-St[2*Nj+1])
      Option.value[1,i]=Option.value[2,i]
      #print(Option.value)  
      # Apply early exercise Condition
      for(j in (2*Nj+1):1){
        #Option.value[j,Nj+1]=max(Option.value[j,Nj],K-St[j])
        Option.value[j,i]=max(Option.value[j,i],K-St[j])
      }
      #print(Option.value)
    }
  }
  print(Option.value)
}
trinomial_tree_american_put(100, 1, 100, 0.2, 0.06, 0.03, 3, 3, 0.2)





trinomial_tree_american_call = function(K, T, S, sig, r, div, N, Nj, dx){
  # Precompute Constants
  dt = T/N
  nu = r - div - 0.5 * sig^2
  edx = exp(dx)
  pu = 0.5*dt*((sig/dx)^2 + nu/dx)
  pm = 1.0 - dt*(sig/dx)^2 - r*dt
  pd = 0.5*dt*((sig/dx)^2 - nu/dx)
  # Intialise asset prices at maturity
  St = c()
  St[2*Nj+1]=S*exp(-Nj*dx)
  for(j in (2*Nj):1){
    St[j]=St[j+1]*edx
  }
  print(St)
  #Intialize option value at maturity
  for(j in (2*Nj+1):1){
    Option.value[j,Nj+1]  = max(0,St[j]-K)
  }
  #Step back through the lattice
  for(i in N:1){
    for(j in (2*Nj):2){
      Option.value[j,i]=pu*(Option.value[j+1,i+1]) + pm*(Option.value[j,i+1]) + pd*(Option.value[j-1,i+1])
      #Boundary Conditions
      Option.value[2*Nj+1,i]=Option.value[2*Nj,i] + (St[2*Nj]-St[2*Nj+1])
      Option.value[1,i]=Option.value[2,i]
      #print(Option.value)  
      # Apply early exercise Condition
      for(j in (2*Nj+1):1){
        #Option.value[j,Nj+1]=max(Option.value[j,Nj],K-St[j])
        Option.value[j,i]=max(Option.value[j,i],St[j]-K)
      }
      #print(Option.value)
    }
  }
  print(Option.value)
}
trinomial_tree_american_call(100, 1, 100, 0.2, 0.06, 0.03, 3, 3, 0.2)