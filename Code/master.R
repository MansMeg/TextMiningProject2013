# install packages
if(FALSE){
  install.packages("Rcpp")
  install.packages('inline')
  install.packages("topicmodels")
  install.packages("tm")
  install.packages("stringr")
  install.packages("knitr")
  install.packages("SnowballC")
  install.packages("xtable")
  install.packages("matrixStats")
  install.packages("coda")
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
library(coda)

# Program parameters
if(Sys.info()[1]=="Linux"){
  project_path <- "/home/rstudio/TextMiningProject2013"
}else{
  project_path <- file.path("/Users",Sys.info()[7],"Dropbox/Projekt/Text Mining/Project/TextMiningProject2013")
}
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
  riksdagCorpBasic <- Corpus(DirSource(paste(project_path,text_files_path,sep="")), language="se")
  riksdagCorp <- riksdagCorpBasic

  # Pre-processing
  # Lexicalize and divide swedish words
  
  ## Convert to Lower Case
  riksdagCorp <- tm_map(riksdagCorp, tolower)  
  ## Replacing \n with whitespace (to remove it).  
  riksdagCorp <- tm_map(riksdagCorp, str_replace_all,pattern="\n",replacement=" ")
  # riksdagCorp <- tm_map(riksdagCorp, removeWords, stopwords("swedish"))
  ## Remove Punctuations
  riksdagCorp <- tm_map(riksdagCorp, removePunctuation)
  ## Remove Numbers
  riksdagCorp <- tm_map(riksdagCorp, removeNumbers)
  ## Eliminating Extra White Spaces
  riksdagCorp <- tm_map(riksdagCorp, stripWhitespace)
  save(riksdagCorp,file="Data/riksdagsCorpus.Rdata")
  save(riksdagCorpBasic,file="Data/riksdagsCorpusBasic.Rdata")
  ## Stemming
  riksdagCorpStemmed <- tm_map(riksdagCorp, stemDocument,language="swedish")
  
  # To do: Numbers to NUMBER token
  # P? sikt kolla att beh?lla viss punctuation ex. (s) och nummer ers?tts med NUMBER
  save(riksdagCorpStemmed,file="Data/riksdagsCorpusStemmed.Rdata")
}
if (!createCorpus){
  load(file="Data/riksdagsCorpus.Rdata")
  load(file="Data/riksdagsCorpusStemmed.Rdata")
}

createMetadata<-FALSE
if(createMetadata){
  anfMetadata<-read.csv(file="Data/AnfMetadata2010.csv")
  anfMetadata<-rbind(anfMetadata,read.csv(file="Data/AnfMetadata2011.csv"))
  anfMetadata<-rbind(anfMetadata,read.csv(file="Data/AnfMetadata2012.csv"))
  save(anfMetadata,file="Data/anfMetadata.Rdata")
}
if(!createMetadata){
  load(file="Data/anfMetadata.Rdata")
}  

# Create term matrix
createDTM<-FALSE
if (createDTM){
  controlList <- list(minWordLength = 3,local = c(5,Inf)) # Detta verkar inte funka
  riksdagDTM <- DocumentTermMatrix(riksdagCorp, control = controlList)
  riksdagDTMStemmed <- DocumentTermMatrix(riksdagCorpStemmed, control = controlList))
  save(riksdagDTM,file="Data/riksdagsDTM.Rdata")
  save(riksdagDTMStemmed,file="Data/riksdagsDTMStemmed.Rdata")
}
if (!createDTM){
  load(file="Data/riksdagsDTM.Rdata")
  riksdagDTM<-riksdagDTM[,col_sums(x=riksdagDTM)>5]
  #load(file="Data/riksdagsDTMStemmed.Rdata")    
}

# Clean the DTM
cleanDTM<-FALSE
if (cleanDTM){
  term_tfidf <- tapply(riksdagDTM$v/row_sums(riksdagDTM)[riksdagDTM$i], riksdagDTM$j, mean) * log2(nDocs(riksdagDTM)/col_sums(riksdagDTM > 0))
  # riksdagDTMclean <-riksdagDTM[,term_tfidf>= 0.03]
  riksdagDTMclean <-riksdagDTM[,order(term_tfidf,decreasing=TRUE)[1:25000]]
  noInformationDoc<-which(row_sums(riksdagDTMclean)==0)
  length(noInformationDoc)
  # inspect(riksdagCorp[noInformationDoc[1:5]])
  riksdagDTMclean <- riksdagDTMclean[row_sums(riksdagDTMclean)>0,]
  save(riksdagDTMclean,file="Data/riksdagsDTMclean.Rdata")
}
if (!cleanDTM){
  load("Data/riksdagsDTMclean.Rdata")
  dim(riksdagDTMclean)
}
set.seed(20130801)
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],100)


# Burning in models i<-10
mR<-5 # No of burn-in-chain to asses convergence
burninNo<-2000
chooseLastNo<-1000
models<-c(50*1:5)
alpha<-0.01

for (i in 1:length(models)){ # j<-i<-k<-1
  if(!file.exists(paste("Models/LDAmod",models[i],".Rdata",sep=""))){
    k<-models[i]
    modelobjList<-list()
    mcmcList<-mcmc.list()
    
    control_LDA_Gibbs <- list(alpha = alpha, estimate.beta = TRUE, best=TRUE,
                               verbose = 0, prefix = "testfil", save = 0, keep = 1,
                               seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                               iter = 1, burnin = burninNo, thin = 1)
    for (l in 1:mR){
      cat("Running LDA model",l,"of",mR,"with K =",k,"Iter =",burninNo,"and alpha =",control_LDA_Gibbs$alpha,"\nStarting",as.character(Sys.time()),"...\n")
      control_LDA_Gibbs$seed<-as.integer(Sys.time())
      modelobjList[[l]]<-LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)
      mcmcList[[l]]<-as.mcmc(modelobjList[[l]]@logLiks[(burninNo-chooseLastNo):burninNo])
      gc()
    }
    print(gelman.diag(mcmcList))
    save(modelobjList,mcmcList,file=paste("Models/LDAmod",models[i],".Rdata",sep=""))
    plot(mcmcList)    
    rm(modelobjList)
  }
}


# Restart models
savedModels<-dir(path="Models/")
fileDF<-data.frame(k=as.numeric(str_extract(savedModels,pattern="[0-9]+")),
                   restartNo=as.numeric(str_extract(str_extract(savedModels,pattern="restart[0-9]+"),"[0-9]+")),
                   fileName=savedModels,stringsAsFactors=F)
fileDF$restartNo[is.na(fileDF$restartNo)]<-0
fileDF<-fileDF[order(fileDF$k,fileDF$restartNo,decreasing=T),]
fileDF<-fileDF[!duplicated(fileDF$k),]
fileDF<-fileDF[order(fileDF$k),]

# fileDF<-fileDF[1:2,]

restartBurnin<-1200
restartChooseLastNo<-1000

for (i in 1:dim(fileDF)[1]){#i<-1
  load(file.path("Models",fileDF[i,3]))
  if(fileDF[i,2]==0){
    mcmcListOfList<-list(mcmcList)
  }
  noOfChains<-length(modelobjList)
  startobjList<-modelobjList
  modelobjList<-list()
  mcmcList<-mcmc.list()

  control_LDA_Gibbs <- list(alpha = alpha, estimate.beta = TRUE, best=TRUE,
                            verbose = 0, prefix = "testfil", save = 0, keep = 1,
                            seed = as.integer(Sys.time()), nstart = 1,
                            iter = 1, burnin = restartBurnin, thin = 1)

  for (l in 1:noOfChains){
    cat("ReRunning LDA model",l,"of",noOfChains,"with K =",startobjList[[l]]@k,"Iter =",
        restartBurnin,"and alpha =",alpha,"\nStarting",as.character(Sys.time()),"...\n")
    control_LDA_Gibbs$seed <- as.integer(Sys.time())

    modelobjList[[l]]<-LDA(riksdagDTMclean[-holdoutIndex,], 
                           method = "Gibbs", 
                           model=startobjList[[l]], 
                           control=control_LDA_Gibbs)
    mcmcList[[l]]<-as.mcmc(modelobjList[[l]]@logLiks[(restartBurnin-restartChooseLastNo):restartBurnin])
    gc()
  }
  plot(mcmcList)
  gelman.diag(mcmcList)
  mcmcListOfList[[length(mcmcListOfList)+1]]<-mcmcList
  save(modelobjList,mcmcListOfList,file=paste("Models/LDAmod",models[i],"restart",(length(mcmcListOfList)-1),".Rdata",sep=""))
  rm(modelobjList,mcmcListOfList)
  gc()
}



for (l in 1:mR){
  cat("Running LDA model",l,"of",mR,"with K =",k,"Iter =",burninNo,"and alpha =",control_LDA_Gibbs$alpha,"\nStarting",as.character(Sys.time()),"...\n")
  control_LDA_Gibbs$seed<-as.integer(Sys.time())
  modelobjList[[l]]<-LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)
  mcmcList[[l]]<-as.mcmc(modelobjList[[l]]@logLiks[(burninNo-chooseLastNo):burninNo])
  gc()
}

i<-1
save(modelobjList,mcmcList,restartNo,
     file=paste("Models/LDAmod",k[i],"restart",restartNo,".Rdata",sep=""))





# Email me when sim is done. 
# if restarting doesn't work
# Restart model and save them as restart1...restartn o.s.v.

# Restarting models
burninNo<-7500
load("Models/LDAmod50alpha0.01.Rdata")
startModelList<-modelobjList
modelobjList<-list()
mcmcList<-mcmc.list()
control_LDA_Gibbs <- list(alpha = startModelList[[1]]@alpha, estimate.beta = TRUE, best=TRUE,
                          verbose = 0, prefix = "testfil", save = 0, keep = 1,
                          seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                          iter = 1, burnin = burninNo, thin = 1)
for (l in 1:length(startModelList)){
  cat("Restarting LDA model",l,"with K =",startModelList[[l]]@k,"Iter =",burninNo," and alpha =",startModelList[[l]]@alpha,", starting",as.character(Sys.time()),"...\n")
  control_LDA_Gibbs$seed<-as.integer(Sys.time())
  modelobjList[[l]]<-LDA(riksdagDTMclean[-holdoutIndex,], method = "Gibbs",model=startModelList[[l]], control = control_LDA_Gibbs)
  mcmcList[[l]]<-as.mcmc(modelobjList[[l]]@logLiks[(burninNo-chooseLastNo):burninNo])
  gc()
}



plot(mcmcList)
save(modelobjList,mcmcList,file=paste("Models/LDAmod",modelobjList[[1]]@k,"alpha",modelobjList[[1]]@alpha,".Rdata",sep=""))


# Estimating models
savedModels<-dir(path="Models/")
k<-as.numeric(str_extract(savedModels,pattern="[0-9]+"))
estimatedModels<-dir(path="Results/")[str_detect(string=dir(path="Results/"),pattern="finmod")]

control_LDA_Gibbs <- list(iter = 200, burnin = 0, thin = 1,best=TRUE)
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


# Inaktivt minne start: 126 mB

# Evaluating models
estimatedModels<-dir(path="Results/")[str_detect(string=dir(path="Results/"),pattern="finmod")]
k<-as.numeric(str_extract(estimatedModels,pattern="[0-9]+"))
if(!exists("Results/evaluate.Rdata")){
  evaluateDF <- data.frame(k=k,pWharmonic=rep(NA,length(k)),pWchib=rep(NA,length(k)),logLik=rep(NA,length(k)))
}else{
  load("Results/evaluate.Rdata")
}

for (i in 1:1){
  evaluateDF <- data.frame(k=k,pWharmonic=rep(NA,length(k)),pWchib=rep(NA,length(k)),logLik=rep(NA,length(k)))
  for (model in 1:length(estimatedModels)){ # model<-1
    if(is.na(evaluateDF[k[model]==evaluateDF$k,2])){
      print(estimatedModels[model])
      load(paste("Results/",estimatedModels[model],sep=""))
      evaluateDF$pWharmonic[k[model]==evaluateDF$k]<-evaluate(LDAobject=estmodobj,newdata=riksdagDTMclean[holdoutIndex,],method="Harmonic")
      evaluateDF$pWchib[k[model]==evaluateDF$k]<-evaluate(LDAobject=estmodobj,newdata=riksdagDTMclean[holdoutIndex,],method="Chib")
      evaluateDF$loglik[k[model]==evaluateDF$k]<-estmodobj@loglikelihood
      rm(estmodobj)
    }
  }
  evaluateDF<-evaluateDF[order(evaluateDF$k),]
  rownames(evaluateDF)<-NULL
  save(evaluateDF,file=paste("Results/evaluate",i,".Rdata",sep="")) 
  print(paste("Results/evaluate",i,".Rdata has been saved",sep=""))
}


eval<-5
j<-1
plotVar<-"pWchib"
load(paste("Results/evaluate",j,".Rdata",sep=""))
plot(evaluateDF$k,evaluateDF[,plotVar],xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="blue")
for (j in 2:eval){
  load(paste("Results/evaluate",j,".Rdata",sep=""))
  evaluateDF<-evaluateDF[order(evaluateDF$k),]
  lines(evaluateDF$k,evaluateDF[,plotVar],type="b",col="blue")  
}


evaluateDF0h<-evaluateDF[order(evaluateDF$k),]
load("Results/evaluateHarmonic.Rdata")
load("Results/evaluateChib.Rdata")

limit<-0
plot(evaluateDF0h$k,evaluateDF0h$pW,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="blue")
lines(evaluateDF2$k,evaluateDF2$pWharmonic,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="blue")
lines(evaluateDF1$k,evaluateDF1$pWharmonic,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="blue")

plot(evaluateDF0c$k,evaluateDF0c$pW,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="red")
lines(evaluateDF2$k,evaluateDF2$pWchib,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="red")
lines(evaluateDF1$k,evaluateDF1$pWchib,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="red")

plot(evaluateDF2$k,evaluateDF2$loglik,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="green")
lines(evaluateDF1$k,evaluateDF1$loglik,xlab="K",ylab="logP(w|Phi,alpha)",type="b",col="green")



# Analysies
load("Results/finmod100.Rdata")

# Most comon topic
modPosterior<-posterior(estmodobj)
test<-colMeans(modPosterior$topics)
which.max(test)
hist(test)

modPosterior$terms[52,][order(modPosterior$terms[52,],decreasing=TRUE)][1:20]
# Ber?kna perplexity, likelihood och (om tid finnes) harmonic mean




reinfeldt<-str_detect(string=anfMetadata$talare[-noInformationDoc][-holdoutIndex],pattern="Statsminister FREDRIK REINFELDT")
meanDebateReinfeldt<-colMeans(modPosterior$topics[reinfeldt,])
head(meanDebateReinfeldt[order(meanDebateReinfeldt,decreasing=TRUE)],10)
hist(meanDebateReinfeldt)
modPosterior$terms[70,][order(modPosterior$terms[70,],decreasing=TRUE)][1:20]
modPosterior$terms[15,][order(modPosterior$terms[15,],decreasing=TRUE)][1:20]


borg<-str_detect(string=as.character(anfMetadata$talare)[-noInformationDoc][-holdoutIndex],pattern="Finansminister ANDERS BORG")
meanDebateBorg<-colMeans(modPosterior$topics[borg,])
head(meanDebateBorg[order(meanDebateBorg,decreasing=TRUE)],10)
modPosterior$terms[52,][order(modPosterior$terms[52,],decreasing=TRUE)][1:20]
modPosterior$terms[75,][order(modPosterior$terms[75,],decreasing=TRUE)][1:20]

sd<-anfMetadata$parti[-noInformationDoc][-holdoutIndex]=="SD"
meanDebateSD<-colMeans(modPosterior$topics[sd,])
hist(meanDebateSD)
modPosterior$topics

mod1<-glm(sd~modPosterior$topics,family="binomial")
which(coefficients(mod1)==max(coefficients(mod1),na.rm=TRUE))


hist(exp(predict(mod1)) / (1 + exp(predict(mod1))) )
predict(mod1)

modPosterior$terms[44,][order(modPosterior$terms[44,],decreasing=TRUE)][1:20]

test<-as.data.frame(table(anfMetadata$talare))



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

# Det verkar som att thin ?r detsamma som iter 
k <- 100
set.seed(k)
holdoutIndex<-sample(1:dim(riksdagDTMclean)[1],10)
control_LDA_Gibbs <- list(alpha = 50/k, estimate.beta = TRUE,
                          verbose = 1, prefix = "testfil", save = 0, keep = 0,
                          seed = as.integer(Sys.time()), nstart = 1, delta = 0.1,
                          iter = 50, burnin = 200, thin = 10)
best_model <- LDA(riksdagDTMclean[-holdoutIndex,], k = k, method = "Gibbs", control = control_LDA_Gibbs)

rm(debuglist)
