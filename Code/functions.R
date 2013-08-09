sourceCpp('Code/createNzRcpp.cpp')

gibbSampleOld<-function(K,N,phi,w,z,alpha,iter,forward=TRUE){
  if (forward){
    itervec <- 1:N
  } else {
    itervec <- N:1
  }
  Nz<-createNzRcpp(NzNull=numeric(K),z=z)
  sumAlpha <-sum(alpha)
  prob<-numeric(K)
  
  for (sweep in 1:iter){
    for (n in itervec){
      Nz[z[n]]<-Nz[z[n]]-1
      for (k in 1:K){
        prob[k] <- phi[k,w[n]] * (Nz[k]+alpha[k])/(N-1+sumAlpha)
      }
      prob<-prob/sum(prob)
      z[n]<-sample(1:K,1,prob=prob)
      Nz[z[n]]<-Nz[z[n]]+1
    }
  }
  return(list(z,Nz))
}

gibbSampleNew<-function(K,N,phi,w,z,alpha,iter,forward=TRUE,mode=TRUE){
  if (forward){
    itervec <- 1:N
  } else {
    itervec <- N:1
  }
  Nz<-createNzRcpp(NzNull=numeric(K),z=z)

  res <- gibbsZRcpp(Nz=Nz,z=z,w=w,alpha=alpha,phi=phi,itervec=itervec,iter=iter,mode=mode)
  
  return(list(z,Nz))
}

gibbICM<-function(K,N,phi,w,z,alpha,iter){
  Nz<-numeric(K)
  for (i in 1:length(z)){
    Nz[z[i]] <- Nz[z[i]] + 1
  }
  sumAlpha <-sum(alpha)
  prob<-numeric(K)
  
  for (sweep in 1:iter){
    oldz <- z
    for (n in 1:N){
      Nz[z[n]]<-Nz[z[n]]-1
      for (k in 1:K){
        prob[k] <- phi[k,w[n]] * (Nz[k]+alpha[k])/(N-1+sumAlpha)
      }
      prob<-prob/sum(prob)
      z[n]<-which.max(prob)
      Nz[z[n]]<-Nz[z[n]]+1
    }
    if(all(z == oldz)){
      message("ICM stopped before iterations ran out.")
      break
    }
  }
  return(list(z,Nz))
}

logTprob<-function(zto,zfrom,Nz,phi,w,alpha){
  Nd <- length(w)
  lp <- 0
  for (n in 1:N){
    Nz[zfrom[n]] <- Nz[zfrom[n]]-1
    for (k in 1:K){
      prob[k] <- phi[k,w[n]] * (Nz[k]+alpha[k])/(N-1+sumAlpha)
    }
    prob<-prob/sum(prob)
    lp <- lp + log(prob[zto[n]]) 
    Nz[zto[n]] <- Nz[zto[n]] + 1
  }
  return(lp)
}
