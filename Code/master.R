# install packages
if(FALSE){
  install.packages("topicmodels")
  install.packages("tm")
  install.packages("stringr")
  install.packages("knitr")
  install.packages("SnowballC")
  install.packages("xtable")
  install.packages("matrixStats")
}

# Load nessecary libraries
library(tm)
library(topicmodels)
library(stringr)
library("slam")
library("SnowballC")
library(Rcpp)
library(parallel)
library(matrixStats)

# Program parameters
project_path <- "/Users/mansmagnusson/Desktop/Text Mining/Project/TextMiningProject2013/"
text_files_path <- "Textfiler/"
setwd(project_path)

# Load evaluation functions
Rcpp::sourceCpp('Code/evaluateRcpp.cpp')
source('Code/evaluate.R', echo=TRUE)

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
cleanDTM<-FALSE
if (cleanDTM){
  term_tfidf <- tapply(riksdagDTM$v/row_sums(riksdagDTM)[riksdagDTM$i], riksdagDTM$j, mean) * log2(nDocs(riksdagDTM)/col_sums(riksdagDTM > 0))
  summary(term_tfidf)
  # riksdagDTMclean <-riksdagDTM[,term_tfidf>= 0.03]
  riksdagDTMclean <-riksdagDTM[,order(term_tfidf)][,1:10000]
  noInformationDoc<-which(row_sums(riksdagDTMclean)==0)
  riksdagDTMclean <- riksdagDTMclean[row_sums(riksdagDTMclean)>0,]
  save(riksdagDTMclean,file="riksdagsDTMclean.Rdata")
}
if (!cleanDTM){
  load("riksdagsDTMclean.Rdata")
}

# Test LDA
# load("Data/riksdagsDTMcleansmall.Rdata")

minK <- 10
maxK <- 200
set.seed(20130801)
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],100)



# Burning in models
models<-c(1:4*5,3:6*10,4:5*20)
for (i in 1:length(models)){
  if(!file.exists(paste("Models/LDAmod",models[i],".Rdata",sep=""))){
    k<-models[i]
    control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,best=TRUE,
                               verbose = 1, prefix = "testfil", save = 0, keep = 1,
                               seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                               iter = 1, burnin = 2000, thin = 1)
    modelobj <- LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)# Produce final LDA model     
    save(modelobj,file=paste("Models/LDAmod",models[i],".Rdata",sep=""))
    plot(modelobj@logLiks[(length(modelobj@logLiks)-500):length(modelobj@logLiks)],type="l",main=paste("LDA model K =",models[i]))
  }
}

# Estimating models
savedModels<-dir(path="Models/")
k<-as.numeric(str_extract(savedModels,pattern="[0-9]+"))
estimatedModels<-dir(path="Results/")[str_detect(string=dir(path="Results/"),pattern="finmod")]

control_LDA_Gibbs <- list(iter = 200, burnin = 0, thin = 1,verbose = 1,best=TRUE)
for (i in 1:length(savedModels)){ # i<-1
  if(!paste("finmod",k[i],".Rdata",sep="")%in%estimatedModels){
    load(paste("Models/",savedModels[i],sep=""))
    cat("Estimating model k =",k[i],"...\n")
    estmodobj <- LDA(riksdagDTMclean[-holdoutIndex,], method = "Gibbs", model=modelobj, control = control_LDA_Gibbs) 
    gc()
    save(estmodobj,file=paste("Results/finmod",k[i],".Rdata",sep=""))
    rm(modelobj)
    rm(estmodobj)
  }
}


# Evaluating models
estimatedModels<-dir(path="Results/")[str_detect(string=dir(path="Results/"),pattern="finmod")]
k<-as.numeric(str_extract(estimatedModels,pattern="[0-9]+"))
if(!exists("Result/evaluate.Rdata")){
  evaluateDF <- data.frame(k=k,pW=rep(NA,length(k)))
}else{
  load("Results/evaluate.Rdata")
}

for (model in 1:length(estimatedModels)){ # model<-1
  if(is.na(evaluateDF[k[model]==evaluateDF$k,2])){
    load(paste("Results/",estimatedModels[model],sep=""))
    evaluateDF[k[model]==evaluateDF$k,2]<-evaluate(LDAobject=estmodobj,newdata=riksdagDTMclean[holdoutIndex,],method="Harmonic")
    save(evaluateDF,file="Results/evaluate.Rdata")  
  }
}


# Beräkna perplexity, likelihood och (om tid finnes) harmonic mean




bin_search<-10
resphi <- 2 - (1 + sqrt(5))/2
pWDF <- data.frame(k=c(minK,maxK,round((maxK-minK)*resphi),rep(NA,bin_search)),pW=rep(NA,bin_search+3))
#load(file="Data/loglik.Rdata")
load(file="Data/pWDF.Rdata")

for (i in 1:(bin_search+2){
  row <- min(which(is.na(pWDF$pW)))
  if(is.na(pWDF$k[row])){
    maxpW<-pWDF$k[which.max(pWDF$pW)]
    sortedVec<-sort(pWDF$k)
    index <- which((sortedVec)==maxpW)
    abc<-sortedVec[c(index-1,index,index+1)]
    if (abc[3] - abc[2] > abc[2] - abc[1]){
      pWDF$k[row] <- round(abc[2] + resphi*(abc[3] - abc[2]))
    } else {
      pWDF$k[row] <- round(abc[2] - resphi*(abc[2] - abc[1]))
    }
  }
  if (pWDF$k[row] %in% pWDF$k[-row]) break
  
  k <- pWDF$k[row]
  
  control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                            verbose = 1, prefix = "testfil", save = 0, keep = 0,
                            seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                            iter = 1, burnin = 1000, thin = 1)
  best_model <- LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)
  pWDF$pW[row]<-evaluate(best_model,riksdagDTMclean[holdoutIndex,])
  
  rm(debuglist)
  save(pWDF,file="Data/pWDF.Rdata")
  gc()
}}

plot(modelobj@logLiks[700:1000],type="l")

test<-model_list[[1]]

plot(test@fitted[[2]]@logLiks)

# Det verkar som att thin är detsamma som iter 
k <- 100
set.seed(k)
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],10)
control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                          verbose = 1, prefix = "testfil", save = 0, keep = 0,
                          seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                          iter = 50, burnin = 200, thin = 10)
best_model <- LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)

rm(debuglist)
