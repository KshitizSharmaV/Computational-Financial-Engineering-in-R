# Author Kshitiz Sharma
######## April 2018 ##############
# This code is made available mby author for learning purpose. The below implementations 
# are from book Implementing-Derivatives-Models (Les Clewlow and Chris Strickland)
# Anybody can use this code free of charge. The author of this code is not responsible
# for any harm or price calculation


# Explicit Finite Difference Method for european call option
finite.difference.method.call = function(K,t,S,sigma,r,div,N,Nj,dx){
  
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = 0.5*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 - dt*(sigma/dx)^2 - r*dt
  pd = 0.5*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(St[j]-K))
  }
  
  #Step Back Through Lattice
  for(j in seq(from=(loc.col-1),to=1,by=-1)){
    for(i in seq(from=2,to=(loc.row-1),by=1)){
      C[i,j]=pu*C[i+1,j+1] + pm*C[i,j+1]+pd*C[i-1,j+1]
    }
    #Boundary Condition
    C[1,j]=C[2,j]
    C[loc.row,j]=C[(loc.row-1),j]+(St[loc.row]-St[loc.row-1])
  }
  print(C)
}

# Explicit Fintie Differnece Method for european put option
finite.difference.method.put = function(K,t,S,sigma,r,div,N,Nj,dx){
  
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = 0.5*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 - dt*(sigma/dx)^2 - r*dt
  pd = 0.5*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(K-St[j]))
  }
  
  #Step Back Through Lattice
  for(j in seq(from=(loc.col-1),to=1,by=-1)){
    for(i in seq(from=2,to=(loc.row-1),by=1)){
      C[i,j]=pu*C[i+1,j+1] + pm*C[i,j+1]+pd*C[i-1,j+1]
    }
    #Boundary Condition
    C[1,j]=C[2,j] + (St[2]-St[1])
    C[loc.row,j]=C[(loc.row-1),j]
    
  }
  print(C)
}


# Implicit Finite Difference Method European Put option
Implicit.finite.difference.method.put = function(K,t,S,sigma,r,div,N,Nj,dx){
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = -0.5*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 + dt*(sigma/dx)^2 + r*dt
  pd = -0.5*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(K-St[j]))
  }
  #print(C)
  
  # Compute derivative boundary condition
  lambda_l = -1*(St[2]-St[1])
  lambda_u = 0.0
  # Step Back Through Lattice
  for(j in seq(from=(loc.col-1),to=1,by=-1)){
    C=solve_implicit_tridiagonal_system.put(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,j)
    # Apply Early Exercise Condition
    #for(i in seq(from=1,to=loc.row,by=1)){
    #  C[i,j]=max(C[i,j],K-St[i])
    #}
  }
  print(C)
}
solve_implicit_tridiagonal_system.put = function(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,i){
  #Substisiute boundary condition at j = 1 and j = 2
  pmp=c()
  pp=c()
  pmp[2] = pm + pd
  pp[2] = C[2,loc.col] + pd*lambda_l
  
  #eliminate upper diagonal
  for(j in 3:(loc.row-1)){
    pmp[j] = pm -pu*pd/pmp[j-1]
    pp[j] = C[j,i+1] - pp[j-1]*pd/pmp[j-1]
  }
  #print(pmp)
  #print(pp)
  # use boundary condition at j = loc.row and equation at j = loc.row-1
  C[loc.row,i] = (pp[loc.row-1]+pmp[loc.row-1]*lambda_u)/(pu + pmp[loc.row-1])
  C[(loc.row-1),i] = C[loc.row,i] - lambda_u
  #print(C)
  # back- substituion
  for(j in (loc.row-2):2){
    C[j,i]=(pp[j]-pu*C[j+1,i])/pmp[j]
  }
  C[1,i]= C[2,i]-lambda_l
  return(C)
}

# Implicit Finite Differnece Method European Call option
Implicit.finite.difference.method.call = function(K,t,S,sigma,r,div,N,Nj,dx){
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = -0.5*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 + dt*(sigma/dx)^2 + r*dt
  pd = -0.5*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(St[j]-K))
  }
  #print(C)
  # Compute derivative boundary condition
  lambda_l = 0.0
  lambda_u = (St[loc.row]-St[loc.row-1])
  # Step Back Through Lattice
  for(j in seq(from=(loc.col-1),to=1,by=-1)){
    C=solve_implicit_tridiagonal_system.call(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,j)
    #print(C)
    # Apply Early Exercise Condition
    #for(i in seq(from=1,to=loc.row,by=1)){
    #  C[i,j]=max(C[i,j],St[i]-K)
    #}
    #print(C)
  }
  print(C)
}
solve_implicit_tridiagonal_system.call = function(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,i){
  #Substisiute boundary condition at j = 1 and j = 2
  pmp=c()
  pp=c()
  pmp[2] = pm + pd
  pp[2] = C[2,loc.col] + pd*lambda_l
  
  #eliminate upper diagonal
  for(j in 3:(loc.row-1)){
    pmp[j] = pm -pu*pd/pmp[j-1]
    pp[j] = C[j,i+1] - pp[j-1]*pd/pmp[j-1]
  }
  #print(pmp)
  #print(pp)
  # use boundary condition at j = loc.row and equation at j = loc.row-1
  C[loc.row,i] = (pp[loc.row-1]+pmp[loc.row-1]*lambda_u)/(pu + pmp[loc.row-1])
  C[(loc.row-1),i] = C[loc.row,i] - lambda_u
  # back- substituion
  for(j in (loc.row-2):2){
    C[j,i]=(pp[j]-pu*C[j+1,i])/pmp[j]
  }
  C[1,i]= C[2,i]-lambda_l
  return(C)
}


# Crank-Nicolson Finite Difference method for European Put
Crank.Nicolson.Finite.Difference.Method.Put = function(K,t,S,sigma,r,div,N,Nj,dx){
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = -0.25*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 + 0.5*dt*(sigma/dx)^2 + 0.5*r*dt
  pd = -0.25*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(K-St[j]))
  }
  #print(C)
  # Compute derivative boundary condition
  lambda_l = -1*(St[2]-St[1])
  lambda_u = 0.0
  $# Step Back Through Lattice
    for(j in seq(from=(loc.col-1),to=1,by=-1)){
      print("**")
      C=solve_Crank_Nicolson_tridiagonal_system.put(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,j)
      # Apply Early Exercise Condition
      #for(i in seq(from=1,to=loc.row,by=1)){
      #  C[i,j]=max(C[i,j],K-St[i])
      #}
      #print(C)
    }
  print(C)
}
solve_Crank_Nicolson_tridiagonal_system.put = function(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,i){
  #Substisiute boundary condition at j = 1 and j = 2
  pmp=c()
  pp=c()
  pmp[2] = pm + pd
  pp[2] = -pu*C[3,loc.col] - (pm-2)*C[2,loc.col] - pd*C[1,loc.col]+ pd*lambda_l
  
  #eliminate upper diagonal
  for(j in 3:(loc.row-1)){
    pmp[j] = pm -pu*pd/pmp[j-1]
    pp[j] = -pu*C[j+1,i+1] -(pm-2)*C[j,i+1]-pd*C[j-1,i+1] - pp[j-1]*pd/pmp[j-1]
  }
  print(pmp)
  print(pp)
  # use boundary condition at j = loc.row and equation at j = loc.row-1
  C[loc.row,i] = (pp[loc.row-1]+pmp[loc.row-1]*lambda_u)/(pu + pmp[loc.row-1])
  C[(loc.row-1),i] = C[loc.row,i] - lambda_u
  print(C)
  # back- substituion
  for(j in (loc.row-2):2){
    C[j,i]=(pp[j]-pu*C[j+1,i])/pmp[j]
  }
  C[1,i]= C[2,i]-lambda_l
  return(C)
}

# Crank-Nicolson Finite Difference method for European Call
Crank.Nicolson.Finite.Difference.Method.Call = function(K,t,S,sigma,r,div,N,Nj,dx){
  # Precompute Constants
  dt = t/N
  nu = r - div - 0.5 *sigma^2
  edx = exp(dx)
  pu = -0.25*dt*((sigma/dx)^2 + nu/dx)
  pm = 1.0 + 0.5*dt*(sigma/dx)^2 + 0.5*r*dt
  pd = -0.25*dt*((sigma/dx)^2- nu/dx)
  
  # Intialise asset Prices at Maturity
  loc.row=2*Nj+1
  loc.col=N+1
  St=c()
  St[1] = S*exp(-Nj*dx)
  for(j in seq(from=2,to=loc.row,by=1)){
    St[j] = St[j-1]*edx
  }
  print(St) 
  
  # Intialize option value at maturity
  C=matrix(0,nrow =loc.row ,ncol=loc.col)
  
  for(j in seq(from=1,to=loc.row,by=1)){
    C[j,loc.col] = max(0,(St[j]-K))
  }
  #print(C)
  # Compute derivative boundary condition
  lambda_l = 0.0
  lambda_u = (St[loc.row]-St[loc.row-1])
  # Step Back Through Lattice
  for(j in seq(from=(loc.col-1),to=1,by=-1)){
    print("**")
    C=solve_Crank_Nicolson_tridiagonal_system.call(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,j)
    # Apply Early Exercise Condition
    #for(i in seq(from=1,to=loc.row,by=1)){
    #  C[i,j]=max(C[i,j],St[i-K])
    #}
    print(C)
  }
  print(C)
}
solve_Crank_Nicolson_tridiagonal_system.call = function(C,pu,pm,pd,lambda_l,lambda_u,loc.col,loc.row,i){
  #Substisiute boundary condition at j = 1 and j = 2
  pmp=c()
  pp=c()
  pmp[2] = pm + pd
  pp[2] = -pu*C[3,loc.col] - (pm-2)*C[2,loc.col] - pd*C[1,loc.col]+ pd*lambda_l
  
  #eliminate upper diagonal
  for(j in 3:(loc.row-1)){
    pmp[j] = pm -pu*pd/pmp[j-1]
    pp[j] = -pu*C[j+1,i+1] -(pm-2)*C[j,i+1]-pd*C[j-1,i+1] - pp[j-1]*pd/pmp[j-1]
  }
  print(pmp)
  print(pp)
  # use boundary condition at j = loc.row and equation at j = loc.row-1
  C[loc.row,i] = (pp[loc.row-1]+pmp[loc.row-1]*lambda_u)/(pu + pmp[loc.row-1])
  C[(loc.row-1),i] = C[loc.row,i] - lambda_u
  print(C)
  # back- substituion
  for(j in (loc.row-2):2){
    C[j,i]=(pp[j]-pu*C[j+1,i])/pmp[j]
  }
  C[1,i]= C[2,i]-lambda_l
  return(C)
}



# Calls
temp=finite.difference.method.call(100,1,100,0.2,0.06, 0.03,1188,60,0.0125)
write.csv(temp,file="check1.csv")
# Value is 8.542
v=Implicit.finite.difference.method.call(100,1,100,0.2,0.06,0.03,2000,33,0.02236)
write.csv(v,file="check2.csv")
# Value is 7.8852
v=Crank.Nicolson.Finite.Difference.Method.Call(100,1,100,0.2,0.06,0.03,31.62,33.04,0.0222)
write.csv(v,file="check6.csv")
# 7.755



# Puts
v=finite.difference.method.put(100,1,100,0.2,0.06,0.03,1188,60,0.0125)
write.csv(v,file="check3.csv")
# Value is 6.762728
v=Implicit.finite.difference.method.put(100,1,100,0.2,0.06,0.03,2000,33,0.02236)
write.csv(v,file="check4.csv")
# Value is 7.163173
v=Crank.Nicolson.Finite.Difference.Method.Put(100,1,100,0.2,0.06,0.03,31.62,33.04,0.0222)
write.csv(v,file="check5.csv")



