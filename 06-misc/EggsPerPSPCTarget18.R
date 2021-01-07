#source("05-results/compare-models/models-select.R")
setwd("c:/models/wgbast")
library(coda)
library(xlsx)

# Time
######################
Year<-c(1996:2019) # Smolt years
Nyears<-length(Year) 
Nstocks<-17

model<-"2019"

path<-"H:/Biom/FullLifeHistoryModel/2018/prg/output/"
######################


#chains<-chains_new
RiskLevel<-75
a<-array(NA, dim=c(Nstocks,1000))
b<-array(NA, dim=c(Nstocks,1000))
R2<-array(NA, dim=c(Nstocks,1000))
for(r in 1:Nstocks){
  a[r,]<-chains[,paste0("alphaSR[",r,"]")]#[[1]]
  b[r,]<-chains[,paste0("betaSR[",r,"]")]#[[1]]
  R2[r,]<-chains[,paste0("R0[33,",r,"]")]*RiskLevel/100 # R0 in assessment year -1
  #R2[r,]<-chains[,paste0("R0[33,",r,"]")][[1]]*RiskLevel/100 # R0 in assessment year -1
}

Estar<-array(NA, dim=c(Nstocks,1000))
for(i in 1:1000){
for(r in 1:Nstocks){
  Estar[r,i]<-((a[r,i]*R2[r,i])/(1-b[r,i]*R2[r,i]))/1000 # eggs in millions
}
}

tmp<-array(NA, dim=c(Nstocks,4))
for(r in 1:Nstocks){
  tmp[r,]<-quantile(Estar[r,], probs=c(0.5,0.75,0.9,0.95))
}
colnames(tmp)<-c("50%","75%","90%","95%")
rownames(tmp)<-c("Torne","Simo","Kalix","Rane"
                ,"Pite","Aby","Byske","Rickle","Savaran"
                ,"Ume","Ore","Lodge","Ljungan","Morrum"
                ,"Eman", "Kage", "Testeboån")
tmp

write.xlsx(tmp,paste0("05-results/EggsPerPSPC_",model,"_",RiskLevel,"level",".xlsx"), row.names=T)



