savedModels<-dir(path="Models/")
fileDF<-data.frame(k=as.numeric(str_extract(savedModels,pattern="[0-9]+")),
                   restartNo=as.numeric(str_extract(str_extract(savedModels,pattern="restart[0-9]+"),"[0-9]+")),
                   fileName=savedModels,stringsAsFactors=F)
fileDF$restartNo[is.na(fileDF$restartNo)]<-0
fileDF<-fileDF[order(fileDF$k,fileDF$restartNo,decreasing=T),]
#fileDF<-fileDF[!duplicated(fileDF$k),]
fileDF<-fileDF[order(fileDF$k),]
row.names(fileDF)<-NULL

inspectNo<-2
rm(mcmcListOfList,modelobjList)
load(file.path("Models",fileDF[inspectNo,3]))
modelobjList[[1]]
length(mcmcListOfList)
fileDF[inspectNo,3]
plot(mcmcListOfList[[length(mcmcListOfList)]])
gelman.diag(mcmcListOfList[[length(mcmcListOfList)]])

tempMcmcList<-mcmc.list()
for (i in 1:length(mcmcListOfList[[1]])){
  mcmcChain<-numeric(0)
  for (j in 1:length(mcmcListOfList)){
    mcmcChain<-c(mcmcChain,mcmcListOfList[[j]][[i]])
  }
  tempMcmcList[[i]]<-as.mcmc(mcmcChain)
}
plot(tempMcmcList)

