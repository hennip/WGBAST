# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Statistics for number of salmon at different points of time/space
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*



rm(list=ls(all=TRUE))
library(coda)
#library(xlsx)
library(writexl)

source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r") #Henni

##############################################################################
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

##############################################################################

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


# Number ascending to river = river catch + number of spawners
dim(RiverCatchW)
dim(spW_age)

ascWtot<-ascW2tot<-CatchRiverTotW<-array(NA, dim=c(Nstocks,Nyears, 1000))
ascW2<-array(NA, dim=c(6,Nstocks,Nyears, 1000)) # approximated number ascending
for(r in 1:Nstocks){
  for(y in 1:Nyears){
    for(s in 1:1000){
      CatchRiverTotW[r,y,s]<-sum(RiverCatchW[2:6, r,y,s])
    for(a in 1:6){
      ascW2[a,r,y,s]<-RiverCatchW[a,r,y,s]+spW_age[r,y,a,s]
    }
    ascWtot[r,y,s]<-sum(ascW[2:6,y,r,s]) # approximated number ascending
    ascW2tot[r,y,s]<-sum(ascW2[2:6,r,y,s])# approximated number ascending
    
}}}


stock<-1
tmp<-cbind(stats(ascW[2,,stock,])[,1],stats(ascW[3,,stock,])[,1],stats(ascW[4,,stock,])[,1],
           stats(ascW[5,,stock,])[,1],stats(ascW[6,,stock,])[,1])
ascTorne1<-cbind(c(year[1]:year[length(year)]),tmp)
colnames(ascTorne1)<-c("year",2:6)

tmp<-cbind(stats(ascW2[2,stock,,])[,1],stats(ascW2[3,stock,,])[,1],stats(ascW2[4,stock,,])[,1],
           stats(ascW2[5,stock,,])[,1],stats(ascW2[6,stock,,])[,1])
ascTorne2<-cbind(c(year[1]:year[length(year)]),tmp)
colnames(ascTorne2)<-c("year",2:6)


x1<-stats(ascWtot[stock,,])
x2<-stats(ascW2tot[stock,,])
x1-x2


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



stats(ascRtot[1,,])

# dfR<-as.data.frame(cbind(stats(ascRtot[1,,]),stats(ascRtot[2,,]), stats(ascRtot[3,,]),stats(ascRtot[4,,])))
# write_xlsx(dfR,path=paste0(PathScen,"AscRiverReared.xlsx"))
# 
# dfW<-as.data.frame(cbind(stats(ascWtot[1,,]),stats(ascWtot[2,,]), stats(ascWtot[3,,]),stats(ascWtot[4,,]),
#                          stats(ascWtot[5,,]),stats(ascWtot[6,,]), stats(ascWtot[7,,]),stats(ascWtot[8,,]),
#                          stats(ascWtot[9,,]),stats(ascWtot[10,,]), stats(ascWtot[11,,]),stats(ascWtot[12,,]),
#                          stats(ascWtot[13,,]),stats(ascWtot[14,,]), stats(ascWtot[15,,]),stats(ascWtot[16,,]),
#                          stats(ascWtot[17,,])))
# write_xlsx(dfW,path=paste0(PathScen,"AscRiverWild.xlsx"))

# Välituloksia Atsolle, mediaanit:

# Number of smolts
stock<-1
smolts<-may1st<-migr<-CC<-asc<-spw<-coastM<-list()
for(stock in 1:4){
  dim(SmoltW)
  dim(SmoltW[stock,,])
  smolts[[stock]]<-cbind(c(year[1]:year[length(year)]),stats(SmoltW[stock,,])[,1])
  
  
  dim(May1stW)
  dim(MigrW)
  coastalMW<-migrW<-may1stW<-array(NA, dim=c(6, Nstocks,Nyears,1000))
  #array(NA, dim=c(Nstocks,Nyears,1000))
    for(r in 1:Nstocks){
    for(y in 1:Nyears){
      for(s in 1:1000){
        for(a in 1:6){
      migrW[a,r,y,s]<-MigrW[a,y,r,1,s] # second last index has 2 slots but only the first contains stuff     
      may1stW[a,r,y,s]<-May1stW[a,y,r,1,s]  # second last index has 2 slots but only the first contains stuff     
      coastalMW[a,r,y,s]<-migrW[a,r,y,s]-ascW[a,y,r,s]-WCTNCtot[a,y,r,s]
        }
      #coastalMW[r,y,s]<-sum(migrW[,r,y,s],na.rm=T)-sum(ascW[,r,y,s],na.rm = T)-sum(WCTNCtot[,y,r,s], na.rm = T)
      
      }}}        
  
  dim(coast_MW)
  
    tmp<-cbind(stats(may1stW[2,stock,2:Nyears,])[,1],stats(may1stW[3,stock,2:Nyears,])[,1],stats(may1stW[4,stock,2:Nyears,])[,1],
             stats(may1stW[5,stock,2:Nyears,])[,1],stats(may1stW[6,stock,2:Nyears,])[,1])
  tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  may1st[[stock]]<-tmp2
  
  tmp<-cbind(stats(migrW[2,stock,2:Nyears,])[,1],stats(migrW[3,stock,2:Nyears,])[,1],stats(migrW[4,stock,2:Nyears,])[,1],
         stats(migrW[5,stock,2:Nyears,])[,1],stats(migrW[6,stock,2:Nyears,])[,1])
  tmp2<-cbind(c((year[1]+1):year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  migr[[stock]]<-tmp2
  
  dim(WCTNCtot)
  dim(WCTNCtot[2,,stock,])
  tmp<-cbind(stats(WCTNCtot[2,,stock,])[,1],stats(WCTNCtot[3,,stock,])[,1],stats(WCTNCtot[4,,stock,])[,1],
  stats(WCTNCtot[5,,stock,])[,1],stats(WCTNCtot[6,,stock,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  CC[[stock]]<-tmp2
  
  dim(coastalMW)
  stats(coastalMW[2,1,2:Nyears,])
  dim(coastalMW[2,,stock,])
  tmp<-cbind(stats(WCTNCtot[2,,stock,])[,1],stats(WCTNCtot[3,,stock,])[,1],stats(WCTNCtot[4,,stock,])[,1],
             stats(WCTNCtot[5,,stock,])[,1],stats(WCTNCtot[6,,stock,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  CM[[stock]]<-tmp2
  
  tmp<-cbind(stats(ascW[2,,stock,])[,1],stats(ascW[3,,stock,])[,1],stats(ascW[4,,stock,])[,1],
        stats(ascW[5,,stock,])[,1],stats(ascW[6,,stock,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  asc[[stock]]<-tmp2
  
  dim(spW_age)
  
  tmp<-cbind(stats(spW_age[stock,,2,])[,1],stats(spW_age[stock,,3,])[,1],stats(spW_age[stock,,4,])[,1],
  stats(spW_age[stock,,5,])[,1],stats(spW_age[stock,,6,])[,1])
  tmp2<-cbind(c(year[1]:year[length(year)]),tmp)
  colnames(tmp2)<-c("year",2:6)
  spw[[stock]]<-tmp2
  
  stats(SpawnerW[stock,,])[,1:2]
  
  # smolts
  # may1st
  # migr
  # CC
  # asc
  # spw
  
  
  tbl<-as.data.frame(cbind(
  smolts[[stock]],
  rbind(rep(NA,6),may1st[[stock]]),
  rbind(rep(NA,6),migr[[stock]]),
  CC[[stock]],
  asc[[stock]],
  spw[[stock]]
  ))
  if(stock==1){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Torne.xlsx"))}
#  if(stock==2){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Simo.xlsx"))}
}

par(mfrow=c(2,1))
for(stock in 1:2){
  tmp<-stats(coastalMW[stock,2:Nyears,])
  plot(1993:2032,tmp[,1],  type="l", main=ifelse(stock==1,"Torne","Simo")) #ylim=c(-100,100)
}

#cbind(1993:2032,tmp)


# AU1

# Smolts
dim(SmoltW)
dim(SmoltR)

AU1smolts<-array(NA, dim=c(Nyears, 1000))
  for(y in 1:Nyears){
    for(s in 1:1000){
      AU1smolts[y,s]<-sum(SmoltW[1:4,y,s])
    }}

AU1smoltsW<-cbind(c(year[1]:year[length(year)]),stats(AU1smolts)[,1])
AU1smoltsR<-cbind(c(year[1]:year[length(year)]),stats(SmoltR[1,,])[,1])

# Adults wild

AU1spW<-AU1ascW<-AU1ctnCW<-AU1migrW<-AU1may1stW<-array(NA, dim=c(6,Nyears,1000))
for(y in 1:Nyears){
  for(s in 1:1000){
    for(a in 1:6){
      AU1migrW[a,y,s]<-sum(MigrW[a,y,1:4,1,s]) # second last index has 2 slots but only the first contains stuff     
      AU1may1stW[a,y,s]<-sum(May1stW[a,y,1:4,1,s])  # second last index has 2 slots but only the first contains stuff     
      AU1ctnCW[a,y,s]<-sum(WCTNCtot[a,y,1:4,s])
      AU1ascW[a,y,s]<-sum(ascW[a,1:4,y,s])
      AU1spW[a,y,s]<-sum(spW_age[1:4,y,a,s])
    }}} 

dim(AU1migrW)
dim(AU1may1stW)
dim(AU1ctnCW)
dim(AU1ascW)
dim(AU1spW)
AU1migrW[,1:10,1]
AU1ctnCW[,1:10,1]
AU1ascW[,1:10,1]

tmp1<-list(AU1may1stW, AU1migrW, AU1ctnCW, AU1ascW, AU1spW)
for(i in 1:5){
 #i<-1
  tmp<-tmp1[[i]]
  
  if(i<3){# 1992 missing in migr & may1st:
    tmp2<-cbind(stats(tmp[2,2:Nyears,])[,1],stats(tmp[3,2:Nyears,])[,1],
             stats(tmp[4,2:Nyears,])[,1],stats(tmp[5,2:Nyears,])[,1],
             stats(tmp[6,2:Nyears,])[,1])
    tmp3<-rbind(rep(NA,6),cbind(c((year[1]+1):year[length(year)]),tmp2))
  }else{
    tmp2<-cbind(stats(tmp[2,,])[,1],stats(tmp[3,,])[,1],
                stats(tmp[4,,])[,1],stats(tmp[5,,])[,1],
                stats(tmp[6,,])[,1])
    tmp3<-cbind(c(year[1]:year[length(year)]),tmp2)
  }
  colnames(tmp3)<-c("year",2:6)
  if(i==1){AU1May1stW<-tmp3}
  if(i==2){AU1MigrW<-tmp3}
  if(i==3){AU1CtnCW<-tmp3}
  if(i==4){AU1AscW<-tmp3}
  if(i==5){AU1SpW<-tmp3}
  
}

tbl<-as.data.frame(cbind(
  AU1smoltsW,AU1May1stW,AU1MigrW,
  AU1CtnCW, AU1AscW,AU1SpW))

write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_AU1_wild.xlsx"))


# Adults reared


AU1spR<-array(NA, dim=c(6,Nyears,1000))
for(y in 1:Nyears){
  for(s in 1:1000){
    for(a in 1:6){
       AU1spR[a,y,s]<-spR_age[1,y,a,s]
    }}} 

tmp1<-list(MigrR[,,1,1,], May1stR[,,1,1,], RCTNCtot[,,1,], ascR[,1,,], AU1spR)
for(i in 1:5){
  tmp<-tmp1[[i]]
  if(i<3){# 1992 missing in migr & may1st:
    tmp2<-cbind(stats(tmp[2,2:Nyears,])[,1],stats(tmp[3,2:Nyears,])[,1],
                stats(tmp[4,2:Nyears,])[,1],stats(tmp[5,2:Nyears,])[,1],
                stats(tmp[6,2:Nyears,])[,1])
    tmp3<-rbind(rep(NA,6),cbind(c((year[1]+1):year[length(year)]),tmp2))
  }else{
    tmp2<-cbind(stats(tmp[2,,])[,1],stats(tmp[3,,])[,1],
                stats(tmp[4,,])[,1],stats(tmp[5,,])[,1],
                stats(tmp[6,,])[,1])
    tmp3<-cbind(c(year[1]:year[length(year)]),tmp2)
  }
  colnames(tmp3)<-c("year",2:6)
  if(i==1){AU1May1stR<-tmp3}
  if(i==2){AU1MigrR<-tmp3}
  if(i==3){AU1CtnCR<-tmp3}
  if(i==4){AU1AscR<-tmp3}
  if(i==5){AU1SpR<-tmp3}
  
}

tbl<-as.data.frame(cbind(
  AU1smoltsR,AU1May1stR,AU1MigrR,
  AU1CtnCR, AU1AscR,AU1SpR))

write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_AU1_reared.xlsx"))



dim(MigrR)
dim(May1stR)
dim(RCTNCtot)
dim(ascR)
dim(spR_age)



 smolts[[stock]],
  rbind(rep(NA,6),may1st[[stock]]),
  rbind(rep(NA,6),migr[[stock]]),
  CC[[stock]],
  asc[[stock]],
  spw[[stock]]
))
#  if(stock==1){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Torne.xlsx"))}
#  if(stock==2){write_xlsx(tbl,path=paste0(PathScen,"Snapshots_medians_Simo.xlsx"))}
}
