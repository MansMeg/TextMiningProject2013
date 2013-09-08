LDAobject=estmodobj
newdata=riksdagDTMclean[holdoutIndex,]

LDAobject<-estmodobj
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

set.seed(10)
test1<-gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=1,forward=TRUE)[[2]]
set.seed(10)
test2<-getPart(z=z,w=w,alpha=alpha,phi=phi,iter=1,forward=TRUE)
test1==test2

test<-gibbsZRcpp(Nz=Nz,z=zstart,w=w,alpha=alpha,phi=phi,itervec=itervec,iter=iter,mode=TRUE)
test2<-gibbsZRcpp(Nz=Nz,z=zstart,w=w,alpha=alpha,phi=phi,itervec=itervec,iter=iter,mode=FALSE)

test<-gibbsZRcpp(z=z,w=w,alpha=alpha,phi=phi,iter=1000,forward=TRUE)

zend<-test$z
myNz<-test$Nz

calcNzRcpp(K=max(zend),zend)
test <- icmZRcpp(z=zstart,w=w,alpha=alpha,phi=phi,iter=1000,forward=TRUE)

testprob1 <- logTprobR(zto=zstart,zfrom=zend,Nz=myNz,w=w,alpha=alpha,phi=phi)
testprob2 <- logTprobRcpp(zto=zstart,zfrom=zend,Nz=myNz,w=w,alpha=alpha,phi=phi)
test[1,]/test2[1,]
testprob1==testprob2

Nalpha1[1]
Nalpha2[1]

deb<-logTprobR(zto=zend,zfrom=zstart,Nz=myNz,w=w,alpha=alpha,phi=phi)
checkout<-deb[[3]]


K<-3
myphi<-matrix(c(0.4,0.9,0.0000001,0.6,0.1,(1-0.0000001)),nrow=K,dimnames=list(1:K,1:2))
myw<-c(1,2,2,1,2,1)
myalpha<-c(1,1,1)
myzstart<-c(1,3,3,3,3,1)
myzend<-c(1,2,1,1,2,2)
mynewNz<-calcNzRcpp(K=K,z=myzstart)
mynewNz[myw[1]] = mynewNz[myw[1]]-1
myprob<-numeric(2)
  myprob[1] <- myphi[1,1] * (mynewNz[1]+alpha[1])
  myprob[2] <- myphi[2,1] * (mynewNz[2]+alpha[2])
sum(myprob)

logTprobRcpp(zto=myzstart,zfrom=myzend,Nz=mynewNz,w=myw,alpha=myalpha,phi=myphi)
logTprobR(zto=myzstart,zfrom=myzend,Nz=mynewNz,w=myw,alpha=myalpha,phi=myphi)

probmat1<-test
probmat2<-test2

test[1]
test2[1]


all(phimat1==phimat2)
# TRUE

set.seed(10)
testing1<-chibIterateRcpp(zstart=z,w=w,alpha=alpha,phi=phi,iter=1,forward=TRUE)
set.seed(10)
testing2<-chibIterateRcpp(zstart=z,zstar=zstar,w=w,alpha=alpha,phi=phi,iter=1000,forward=TRUE)
hist(testing2)

logTvals[(ss - 1):1]<-numeric(0)
