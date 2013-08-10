LDAobject<-best_model
newdata <- riksdagDTMclean[holdoutIndex,]
doc<-1

phi<-posterior(LDAobject)[[1]]
alpha<-LDAobject@alpha * (numeric(LDAobject@k)+1)
K <- LDAobject@k
D <- dim(newdata)[1]
logEvidenceD <- numeric(D)
zBurnin <- 20 # The number of burnins befor zstar is calculated 3 is used in Wallach et al. (2009)
zStariter <- 15 # Number of maximum iterations "iterative conditional modes", stops if z_{n}==z_{n-1} 12 used in Wallach et al. (2009)
Siter <- 1000 # S 1000 used in
wDoc <- newdata[doc,]
Nd <- sum(wDoc$v)
w <- rep(wDoc$j, wDoc$v)
sourceCpp('Code/createNzRcpp.cpp')

zstart <- sample(1:k,size=Nd,replace=TRUE) # Initializaton of z
N<-length(zstart)
itervec <- 1:N
mode<-TRUE
iter<-1000
Nz<-createNzRcpp(NzNull=numeric(K),z=zstart)

test<-gibbsZRcpp(Nz=Nz,z=zstart,w=w,alpha=alpha,phi=phi,itervec=itervec,iter=iter,mode=TRUE)
test2<-gibbsZRcpp(Nz=Nz,z=zstart,w=w,alpha=alpha,phi=phi,itervec=itervec,iter=iter,mode=FALSE)

