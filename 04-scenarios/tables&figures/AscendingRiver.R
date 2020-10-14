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
EffScen<-5

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

dim(SmoltW)
dim(SmoltW[1,,])
stats(SmoltW[1,,])[,1:2]


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

tmp<-cbind(stats(may1stW[2,1,2:Nyears,])[,2],stats(may1stW[3,1,2:Nyears,])[,2],stats(may1stW[4,1,2:Nyears,])[,2],
           stats(may1stW[5,1,2:Nyears,])[,2],stats(may1stW[6,1,2:Nyears,])[,2])
tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
colnames(tmp2)<-c("year",2:6)
migrTorne<-tmp2

tmp<-cbind(stats(migrW[2,1,2:Nyears,])[,2],stats(migrW[3,1,2:Nyears,])[,2],stats(migrW[4,1,2:Nyears,])[,2],
       stats(migrW[5,1,2:Nyears,])[,2],stats(migrW[6,1,2:Nyears,])[,2])
tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
colnames(tmp2)<-c("year",2:6)
may1stTorne<-tmp2

dim(spW_age)


stats(SpawnerW[1,,])[,1:2]


