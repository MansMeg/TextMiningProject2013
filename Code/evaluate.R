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
  logEvidence <- NA
  
  # Create progress bar
  pb <- txtProgressBar(min = 0, max = D, style = 3)
  
  if (method == "Chib"){
    # Iterations
    zBurnin <- 20 # The number of burnins befor zstar is calculated 3 is used in Wallach et al. (2009)
    zStariter <- 15 # Number of maximum iterations "iterative conditional modes", stops if z_{n}==z_{n-1} 12 used in Wallach et al. (2009)
    Siter <- 1000 # S 1000 used in
    
    debug<-FALSE
    if(debug){
      debuglist<<-list()
    }
    # Do the calculations for each document this can be parallized using mclapply doc<-1
    for (doc in 1:D){
      # Save Seed for debugging
      if(debug){debuglist[[doc]]<<-.Random.seed}
      
      # Create parameters for the document
      wDoc <- newdata[doc,]
      Nd <- sum(wDoc$v)
      w <- rep(wDoc$j, wDoc$v)
      z <- sample(1:K,size=Nd,replace=TRUE) # Initializaton of z
      # Initialize the gibbsampler to produce an z-vector for the document
      z <- gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=zBurnin,forward=TRUE,allz=FALSE)[[2]]
  
      # Calculate zstar 
      # Find local optimim to use as z^* using "iterative conditional modes" as is done by Wallach et al. (2009)
      gibbzstar<-icmZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=zStariter,forward=TRUE)
      zstar<-gibbzstar[[2]]
      # Start the algoritm
      logTvals <- numeric(Siter)
      # Draw starting position s
      ss <- sample(1:Siter,1)
      # Draw z^s
      gibbzs<-gibbsZRcpp(z=zstar,w=w,alpha=alpha,phi=phi,iter=1,forward=FALSE,allz=FALSE)
      logTvals[ss]<-logTprobRcpp(zto=zstar,zfrom=gibbzs[[2]],Nz=gibbzs[[1]],phi=phi,w=w,alpha=alpha)
      
      # Draw forward part 
      if(ss<Siter){
        logTvals[(ss+1):Siter]<-chibIterateRcpp(zstart=gibbzs[[2]],zstar=zstar,w=w,alpha=alpha,phi=phi,iter=(Siter-ss),forward=TRUE)
      }
      # Draw backward part
      if(ss>1){
        logTvals[(ss-1):1]<-chibIterateRcpp(zstart=gibbzs[[2]],zstar=zstar,w=w,alpha=alpha,phi=phi,iter=(ss-1),forward=FALSE)
      }
      # Calculate estimate log P(w|phi,alpha) (log evidence)
      log_pz = sum(lgamma(gibbzstar[[1]]+alpha)) + lgamma(sum(alpha)) - sum(lgamma(alpha)) - lgamma(Nd+sum(alpha))
      log_w_given_z <- sum(sapply(1:Nd,FUN=function(t){log(phi[zstar[t],w[t]])}))
      log_evidence <- log_pz + log_w_given_z - (log(sum(exp(logTvals))) - log(Siter))
      
      # Save results in 
      logEvidenceD[doc]<-log_evidence
      
      # Updating progress bar
      setTxtProgressBar(pb, doc)
      }
    
    logEvidence<-sum(logEvidenceD)
  }  
  
  # Harmonic mean
  if (method == "Harmonic"){
    zBurnin <- 1000 # S 1000 used in
    Siter <- 1000 # S 1000 used in

    for (doc in 1:D){ #doc<-1
      # Create parameters for the document
      wDoc <- newdata[doc,]
      Nd <- sum(wDoc$v)
      w <- rep(wDoc$j, wDoc$v)
      z <- sample(1:K,size=Nd,replace=TRUE) # Initializaton of z      
      z <- gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=zBurnin,forward=TRUE,allz=FALSE)[[2]]
      # Initialize the gibbsampler to produce an z-matrix for the document
      zMatrix <- gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=Siter,forward=TRUE,allz=TRUE)[[2]]
      # Calculate sum(log(p(w|z,phi)) for each
      logProbS<-apply(zMatrix,1,logProbDoc,w=w,phi=phi)
      # Save evidence
      logEvidenceD[doc]<- -(matrixStats::logSumExp(lx=-logProbS)-log(length(logProbS)))     
      # Updating progress bar
      setTxtProgressBar(pb, doc)      
    }
    logEvidence<-sum(logEvidenceD)
  }
  
  # Closing progress bar
  close(pb)
  return(logEvidence)
}

# To do, Use RArmadillo for sample function, handle log(sum(exp(logTvals)))
                                                 
