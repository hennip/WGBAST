# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Makes a table for a certain year from the scenario results.
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
  
rm(list=ls(all=TRUE))


# Scenarios
#! Effort 
EffScen<-5 

#for(EffScen in 1:6){
source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")

File
load(File)
#! #############################################################################
################################################################################


C_OLL<-array(NA, c(Nyears,1000))
C_CTN<-array(NA, c(Nyears,1000))
CalC_tot<-array(NA, c(Nyears,1000))
for(i in 1:1000){
  for(y in 1:Nyears){
    C_OLL[y,i]<-sum(WOLLCtot[2:6,y,1:Nstocks,i])+sum(ROLLCtot[2:6,y,1:4,i])    
    C_CTN[y,i]<-sum(WCTNCtot[2:6,y,1:Nstocks,i])+sum(RCTNCtot[2:6,y,1:4,i])
    
    if(y>1){
      CalC_tot[y,i]<-C_OLL[y-1,i]+C_CTN[y,i]
    }
  }
}

cbind(c(1992:LastPredYear),c(1:Nyears))

# ??????????????????????????????????????
stats<-function(dat,v){
  sumDat<-summary(as.mcmc(dat[v,]))

  med<-sumDat$quantiles[3]
  high<-sumDat$quantiles[5]
  low<-sumDat$quantiles[1]

  return(rbind(med, low, high))
}
# ??????????????????????????????????????


SpawnerTOT<-array(NA, dim=c(Nyears,1000))
for(y in 1:Nyears){
  for(s in 1:1000){
    SpawnerTOT[y,s]<- sum(SpawnerW[1:Nstocks,y,s])
  }
}

dim(PFAW)

PFA_T<-array(NA, dim=c(Nyears,1000))
PFA_MSW<-array(NA, dim=c(Nyears,1000))
PFA_G<-array(NA, dim=c(Nyears,1000))
for(y in 1:Nyears){
for(s in 1:1000){
  PFA_T[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s],na.rm=T)+sum(PFAR[1:6,y,1:4,1,s])
  PFA_G[y,s]<-sum(PFAW[1,y,1:Nstocks,1,s],na.rm=T)+sum(PFAR[1,y,1:4,1,s])
  PFA_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s],na.rm=T)+sum(PFAR[2:6,y,1:4,1,s])
}}



cbind(c(1992:LastPredYear),c(1:Nyears))
# ===============================================
# calendar year 2021 is year 30 for trapnetting and 
# year 29 for offshore fisheries
yCTN<-30 
yOLL<-29

# Save relevant calculations
df<-cbind(
rbind("TotCatch", stats(CalC_tot, yCTN)) ,
rbind("River", stats(CatchRiver, yCTN)) ,
rbind("SpawnerTot", stats(SpawnerTOT, yCTN)),

rbind("PFA", stats(PFA_T, yOLL)),
rbind("PFA grilse", stats(PFA_G, yOLL)),
rbind("PFA MSW", stats(PFA_MSW, yOLL))#,       
#rbind("CalC/PFA1",stats(CalC_tot, yCTN)[1]/stats(PFA,yCTN))       
)

df<-as.data.frame(df)

#stats(CalC_tot, yCTN)[1]/stats(PFA,yCTN)

write_xlsx(df, paste0(PathScen,"ScenTable2018_EScen",EffScen,"_",Model,".xlsx"))
#}



