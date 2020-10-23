# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Statistics for salmon returning to Bothnian Bay
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*



rm(list=ls(all=TRUE))
library(coda)
#library(xlsx)
library(writexl)

source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r") #Henni

#PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
#PathOut<-"H:/Biom/Scenarios/2018/prg/" # output



################################################################################
#! #############################################################################
# Version of the estimation model
Model<-"2020_updated"

# Time
LastHistYear<-2019
LastPredYear<-2032
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
Nstocks<-17


# Scenarios
#! Effort 
EffScen<-1

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")

File
load(File)

#! #############################################################################
################################################################################

dim(RiverCatchW)
dim(spW_age)

ascWtot<-CatchRiverTotW<-array(NA, dim=c(Nstocks,Nyears, 1000))
ascW<-array(NA, dim=c(6,Nstocks,Nyears, 1000))
for(r in 1:Nstocks){
  for(y in 1:Nyears){
    for(s in 1:1000){
      CatchRiverTotW[r,y,s]<-sum(RiverCatchW[2:6, r,y,s])
    for(a in 1:6){
      ascW[a,r,y,s]<-RiverCatchW[a,r,y,s]+spW_age[r,y,a,s]
    }
    ascWtot[r,y,s]<-sum(ascW[2:6,r,y,s])
      
}}}

dim(May1stW)
dim(MigrW)


dim(RiverCatchR)
dim(May1stR)
dim(MigrR)

ascRtot<-CatchRiverTotR<-array(NA, dim=c(4,Nyears, 1000))
ascR<-array(NA, dim=c(6,4,Nyears, 1000))
for(u in 1:4){
  for(y in 1:Nyears){
    for(s in 1:1000){
      CatchRiverTotR[u,y,s]<-sum(RiverCatchR[2:6, u,y,s])
      for(a in 1:6){
        ascR[a,u,y,s]<-RiverCatchR[a,u,y,s]+spR_age[u,y,a,s]
      }
      ascRtot[u,y,s]<-sum(ascR[2:6,u,y,s])
}}}

stats<-function(var){
q5<-q50<-q95<-c()

for(i in 1:dim(var)[1]){
  tmp<-summary(as.mcmc(var[i,]), quantiles=c(0.05,0.5,0.95))
  q5[i]<-tmp$quantiles[1]
  q50[i]<-tmp$quantiles[2]
  q95[i]<-tmp$quantiles[3]
}
#res<-cbind(year[1]:year[length(year)],q50,q5,q95)
res<-cbind(q50,q5,q95)
return(res)
}

stats(ascRtot[1,,])

dfR<-as.data.frame(cbind(stats(ascRtot[1,,]),stats(ascRtot[2,,]), stats(ascRtot[3,,]),stats(ascRtot[4,,])))
write_xlsx(dfR,path=paste0(PathScen,"AscRiverReared.xlsx"))

dfW<-as.data.frame(cbind(stats(ascWtot[1,,]),stats(ascWtot[2,,]), stats(ascWtot[3,,]),stats(ascWtot[4,,]),
                         stats(ascWtot[5,,]),stats(ascWtot[6,,]), stats(ascWtot[7,,]),stats(ascWtot[8,,]),
                         stats(ascWtot[9,,]),stats(ascWtot[10,,]), stats(ascWtot[11,,]),stats(ascWtot[12,,]),
                         stats(ascWtot[13,,]),stats(ascWtot[14,,]), stats(ascWtot[15,,]),stats(ascWtot[16,,]),
                         stats(ascWtot[17,,])))
write_xlsx(dfW,path=paste0(PathScen,"AscRiverWild.xlsx"))

# Valituloksia Atsolle:

# Number of smolts
stock<-1
for(stock in 1:2){
  dim(SmoltW)
  dim(SmoltW[stock,,])
  smolts<-cbind(c(year[1]:year[length(year)]),stats(SmoltW[stock,,])[,1])
  
  
  dim(May1stW)
  dim(MigrW)
  migrW<-may1stW<-array(NA, dim=c(6, Nstocks,Nyears,1000))
  for(r in 1:Nstocks){
    for(y in 1:Nyears){
      for(s in 1:1000){
        for(a in 1:6){
      migrW[a,r,y,s]<-MigrW[a,y,r,1,s] # second last index has 2 slots but only the first contains stuff     
      may1stW[a,r,y,s]<-May1stW[a,y,r,1,s]  # second last index has 2 slots but only the first contains stuff     
        }}}}        
  
  May1stW[2,,1,1,1:2] 
  
  tmp<-cbind(stats(may1stW[2,stock,2:Nyears,])[,1],stats(may1stW[3,stock,2:Nyears,])[,1],stats(may1stW[4,stock,2:Nyears,])[,1],
             stats(may1stW[5,stock,2:Nyears,])[,1],stats(may1stW[6,stock,2:Nyears,])[,1])
  tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  may1st<-tmp2
  
  tmp<-cbind(stats(migrW[2,stock,2:Nyears,])[,1],stats(migrW[3,stock,2:Nyears,])[,1],stats(migrW[4,stock,2:Nyears,])[,1],
         stats(migrW[5,stock,2:Nyears,])[,1],stats(migrW[6,stock,2:Nyears,])[,1])
  tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  migr<-tmp2
  
  dim(WCTNCtot)
  dim(WCTNCtot[2,,stock,])
  tmp<-cbind(stats(WCTNCtot[2,,stock,])[,1],stats(WCTNCtot[3,,stock,])[,1],stats(WCTNCtot[4,,stock,])[,1],
  stats(WCTNCtot[5,,stock,])[,1],stats(WCTNCtot[6,,stock,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  CC<-tmp2
  
  tmp<-cbind(stats(ascW[2,stock,,])[,1],stats(ascW[3,stock,,])[,1],stats(ascW[4,stock,,])[,1],
        stats(ascW[5,stock,,])[,1],stats(ascW[6,stock,,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  asc<-tmp2
  
  dim(spW_age)
  
  tmp<-cbind(stats(spW_age[stock,,2,])[,1],stats(spW_age[stock,,3,])[,1],stats(spW_age[stock,,4,])[,1],
  stats(spW_age[stock,,5,])[,1],stats(spW_age[stock,,6,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  spw<-tmp2
  
  stats(SpawnerW[stock,,])[,1:2]
  
  smolts
  may1st
  migr
  CC
  asc
  spw
  
  
  tbl<-as.data.frame(cbind(
  smolts,
  rbind(rep(NA,6),may1st),
  rbind(rep(NA,6),migr),
  CC,
  asc,
  spw
  ))
  if(stock==1){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Torne.xlsx"))}
  if(stock==2){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Simo.xlsx"))}
}

