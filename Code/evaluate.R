evaluate<- function(LDAobject,newdata,method="Chib"){
  # Assertions
  if (class(LDAobject)!="LDA_Gibbs"){
    stop("LDAobject is not a LDA_Gibbs object")
  }
  if (class(newdata)[1]!="DocumentTermMatrix"){
    stop("newdata is not a DocumentTermMatrix object")
  }
  
  # Get results from the LDAobject
  phi<-posterior(LDAobject)[[1]]
  alpha<-LDAobject@alpha * (numeric(LDAobject@k)+1)
  K <- LDAobject@k
  D <- dim(newdata)[1]
  logEvidenceD <- numeric(D)
  
  # Iterations
  zBurnin <- 20 # The number of burnins befor zstar is calculated 3 is used in Wallach et al. (2009)
  zStariter <- 15 # Number of maximum iterations "iterative conditional modes", stops if z_{n}==z_{n-1} 12 used in Wallach et al. (2009)
  Siter <- 1000 # S 1000 used in
  
  # Do the calculations for each document doc<-1
  for (doc in 1:D){
    # Create parameters for the document
    wDoc <- newdata[doc,]
    Nd <- sum(wDoc$v)
    w <- rep(wDoc$j, wDoc$v)
    z <- sample(1:k,size=Nd,replace=TRUE) # Initializaton of z
    # Initialize the gibbsampler to produce an z-vector for the document
    z <- gibbSample(K=K,N=Nd,phi=phi,w=w,z=z,alpha=alpha,iter=zBurnin,mode=FALSE)[[1]]

    if (method=="Chib"){
      # Calculate zstar 
      #Find local optimim to use as z^*, "iterative conditional modes"
      gibbzstar<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=z,alpha=alpha,iter=zStariter,mode=TRUE)  
      zstar<-gibbzstar[[1]]
      # Start the algoritm
      logTvals <- numeric(Siter)
      # Draw starting position s
      ss <- sample(1:Siter,1)
      # Draw z^s
      gibbzs<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=FALSE,mode=FALSE)
      logTvals[ss]<-logTprob(zto=zstar,zfrom=gibbzs[[1]],Nz=gibbzs[[2]],phi=phi,w=w,alpha=alpha)

      # Draw forward part 
      for (step in (ss+1):Siter){ #All m�ste bygga p� zstar och fram�t - g�r en Rcpploop f�r denna och nedan
        gibbzz<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=TRUE,mode=FALSE)
        logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
      }
      # Draw backward part
      for (step in (ss-1):1){
        gibbzz<-gibbSample(K=K,N=Nd,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=FALSE)
        logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
      }
      # Calculate estimate log P(w|phi,alpha) (log evidence)
      log_pz = sum(lgamma(gibbzstar[[2]]+alpha)) + lgamma(sum(alpha)) - sum(lgamma(alpha)) - lgamma(Nd+sum(alpha))
      log_w_given_z<-0
      for (t in 1:Nd){
        log_w_given_z <- log_w_given_z + log(phi[zstar[t],w[t]])
      }
      log_evidence <- log_pz + log_w_given_z - (log(sum(exp(logTvals))) - log(Siter))
    }
    # Save results in 
    logEvidenceD[doc]<-log_evidence
  }
  return(sum(logEvidenceD))
}

