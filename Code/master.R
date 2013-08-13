# install packages
if(FALSE){
  install.packages("topicmodels")
  install.packages("tm")
  install.packages("stringr")
  install.packages("knitr")
  install.packages("SnowballC")
  install.packages("xtable")
}


# Load nessecary libraries
library(tm)
library(topicmodels)
library(stringr)
library("slam")
library("SnowballC")
library(Rcpp)

# Program parameters
project_path <- "/Users/mansmagnusson/Desktop/Text Mining/Project/TextMiningProject2013/"
text_files_path <- "Textfiler/"
setwd(project_path)

# Create tm corpus object
if (FALSE){
  temp_files_path <- "Tempfiler/"
  tempfiles <- dir(paste(project_path,text_files_path,sep=""))[1:1000]
  file.copy(paste(project_path,text_files_path,tempfiles,sep=""),
            paste(project_path,temp_files_path,tempfiles,sep=""))
  text_files_path<-temp_files_path
}


# Create corpus
createCorpus <- FALSE
if (createCorpus){
riksdagCorp <- Corpus(DirSource(paste(project_path,text_files_path,sep="")), language="se")

# Pre-processing
## Convert to Lower Case
riksdagCorp <- tm_map(riksdagCorp, str_replace_all,pattern="\n",replacement=" ")
## Remove Stopwords
# riksdagCorp <- tm_map(riksdagCorp, removeWords, stopwords("swedish"))
## Remove Punctuations
riksdagCorp <- tm_map(riksdagCorp, removePunctuation)
## Remove Numbers
riksdagCorp <- tm_map(riksdagCorp, removeNumbers)
## Eliminating Extra White Spaces
riksdagCorp <- tm_map(riksdagCorp, stripWhitespace)
save(riksdagCorp,file="riksdagsCorpus.Rdata")
## Stemming
riksdagCorpStemmed <- tm_map(riksdagCorp, stemDocument,language="swedish")

# To do: Numbers to NUMBER token
# På sikt kolla att behålla viss punctuation ex. (s) och nummer ersätts med NUMBER
save(riksdagCorpStemmed,file="riksdagsCorpusStemmed.Rdata")
}

if (!createCorpus){
  load(file="Data/riksdagsCorpus.Rdata")
}



# Create term matrix
createDTM<-FALSE
if (createDTM){
  riksdagDTM <- DocumentTermMatrix(riksdagCorp, control = list(minWordLength = 3))
  save(riksdagDTM,file="riksdagsDTM.Rdata")
}
if (!createDTM){
  load(file="Data/riksdagsDTM.Rdata")
}

# Clean the DTM
term_tfidf <- tapply(riksdagDTM$v/row_sums(riksdagDTM)[riksdagDTM$i], riksdagDTM$j, mean) * log2(nDocs(riksdagDTM)/col_sums(riksdagDTM > 0))
summary(term_tfidf)
# riksdagDTMclean <-riksdagDTM[,term_tfidf>= 0.03]
# riksdagDTMclean <-riksdagDTM[,order(term_tfidf)][,1:4000]
which(row_sums(riksdagDTMclean)==0)

riksdagCorp[["GY09109-28.txt"]]
riksdagCorp[["GZ09108-88.txt"]]
riksdagDTMclean <- riksdagDTMclean[row_sums(riksdagDTMclean)>0,]
save(riksdagDTMclean,file="riksdagsDTMclean.Rdata")
#str(riksdagDTMclean)

  
# test LDA
# load("Data/riksdagsDTMcleansmall.Rdata")

minK <- 10
maxK <- 200
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],100)
bin_search<-10
resphi <- 2 - (1 + sqrt(5))/2
loglikDF <- data.frame(k=c(minK,maxK,round((maxK-minK)/(1+phi)),rep(NA,bin_search)),loglik=rep(NA,bin_search+3))
load(file="Data/loglik.Rdata")

for (i in 1:(bin_search+2){
  row <- min(which(is.na(loglikDF$loglik)))
  if(is.na(loglikDF$k[row])){
    maxLogLik<-loglikDF$k[which.max(loglikDF$loglik)]
    sortedVec<-sort(loglikDF$k)
    index <- which((sortedVec)==maxLogLik)
    abc<-sortedVec[c(index-1,index,index+1)]
    if (abc[3] - abc[2] > abc[2] - abc[1]){
      loglikDF$k[row] <- round(abc[2] + resphi*(abc[3] - abc[2]))
    } else {
      loglikDF$k[row] <- round(abc[2] - resphi*(abc[2] - abc[1]))
    }
  }
  if (loglikDF$k[row] %in% loglikDF$k[-row]) break

  k <- loglikDF$k[row]

  control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                            verbose = 1, prefix = "testfil", save = 0, keep = 0,
                            seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                            iter = 50, burnin = 200, thin = 1)
  best_model <- LDA(riksdagDTMclean, k = k, method = "Gibbs", control = control_LDA_Gibbs)

  save(loglikDF,file="Data/loglik.Rdata")
  gc()
  }
}




# Produce final LDA model

# Det verkar som att thin är detsamma som iter 
k <- 15
set.seed(k)
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],100)
control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                               verbose = 1, prefix = "testfil", save = 0, keep = 0,
                               seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                               iter = 50, burnin = 200, thin = 10)
best_model <- LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)

# Jämför old och new functions

# implementera sample() från armadillo för att speeda up än mer.

resvec<-numeric(20)
for (test in 1:20){
  resvec[test]<-evaluate(best_model,riksdagDTMclean[1:2,])
}
hist(resvec)

str(best_model)
terms(test, 10)

# Create a gibbs sampler for samling new document topics for a given
LDAobject<-best_model



str(riksdagDTMclean)

# evaluate(LDAobject,newdata,method="chib")
# Creating the data used for gibbs and Chib-algo
phi<-posterior(LDAobject)[[1]]
alpha<-LDAobject@alpha * (numeric(LDAobject@k)+1)
wDoc <- riksdagDTMclean[1,]
wTriplet<-matrix(c(wDoc$i,wDoc$j,wDoc$v),ncol=3)
K <- LDAobject@k
N <- sum(wTriplet[,3])
w <- rep(wTriplet[,2], wTriplet[,3])
z <- sample(1:k,size=N,replace=TRUE)
siter <- 100
# Draw gibbs samples for the unknown vector
z<-gibbSample(K=K,N=N,phi=phi,w=w,z=z,alpha=alpha,iter=1000)[[1]]
#% Find local optimim to use as z^*, "iterative conditional modes"
gibbzstar<-gibbICM(K=K,N=N,phi=phi,w=w,z=z,alpha=alpha,iter=100)
zstar<-gibbzstar[[1]]
# Draw starting position s
ss <- sample(1:siter,1)
logTvals <- numeric(siter)
# Draw z^s
gibbzs<-gibbSample(K=K,N=N,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=FALSE)
logTvals[ss]<-logTprob(zto=zstar,zfrom=gibbzs[[1]],Nz=gibbzs[[2]],phi=phi,w=w,alpha=alpha)
# Draw forward part
for (step in (ss+1):siter){
  gibbzz<-gibbSample(K=K,N=N,phi=phi,w=w,z=zstar,alpha=alpha,iter=1)
  logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
}
# Draw backward part
for (step in (ss-1):1){
  gibbzz<-gibbSample(K=K,N=N,phi=phi,w=w,z=zstar,alpha=alpha,iter=1,forward=FALSE)
  logTvals[step]<-logTprob(zto=zstar,zfrom=gibbzz[[1]],Nz=gibbzz[[2]],phi=phi,w=w,alpha=alpha)
}
# Calculate estimate log P(w|phi,alpha) (log evidence)
Nkstar <- gibbzstar[[2]]
log_pz = sum(lgamma(Nkstar+alpha)) + lgamma(sum(alpha)) - sum(lgamma(alpha)) - lgamma(N+sum(alpha))
log_w_given_z<-0
for (t in 1:N){
  log_w_given_z <- log_w_given_z + log(phi[zstar[t],w[t]])
}
log_evidence <- log_pz + log_w_given_z - (log(sum(exp(logTvals))) - log(siter))

# sourceCpp("Code/gibbsHeldout.cpp")
# gibbsHeldout(w=wTriplet,phi=phi,alpha=alpha,iter=10,thin=10,burnin=10)

# To do 1: Implement Rstan LDA model, only for 1000 anföranden.
# Implement function/class in Python to go from txt documents to sparse format together with readmefile.
# The output format should be possibly stan format and triplet format (for sparse matrices).


# Remove stopwords and words < 5



sample.int(k,prob=1:20)

# To do 2: Implement regression


matIndex(mat=diag(3),0,1)

