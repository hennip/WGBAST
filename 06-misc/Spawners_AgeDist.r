#rm(list=ls(all=TRUE))

# Skip == T when running 05-results/workflow-extra-output 
if(exists("skip")==F){
  
source("../run-this-first-wgbast.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location

Model<-"2024_JAGS_Mps" # Assessment model version
nsim<-1000
LastHistYear<-2023
ymax<-350
LastPredYear<-LastHistYear+ymax


EffScen<-14

#Load the file containing stats
File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

#ScenProj_2024_JAGS_Mps_EScen1_RCzero23-35
File
load(File)
}

#! #############################################################################
################################################################################

Years<-c(1992:LastPredYear)
length(Years)

Nyears<-yBreak<-length(Years)
sims<-c(1:nsim)
Nstocks<-17; Nunits<-4 

#! #############################################################################
################################################################################

RiverNames<-c("Torne","Simo","Kalix","Rane"
,"Pite","Aby","Byske","Rickle","Savaran"
,"Ume","Ore","Lodge","Ljungan","Morrum"
,"Eman", "Kage", "Testeb")

dim(spW_age)

# medians
AgeW<-array(NA, dim=c(Nyears,6,Nstocks), dimnames=list(Years,1:6,RiverNames))
AgeR<-array(NA, dim=c(Nyears,6,4), dimnames=list(Years,1:6,c(1:4)))
for(y in 1:Nyears){
for(a in 1:6){
  for(r in 1:Nstocks){
    AgeW[y,a,r]<-median(spW_age[r,y,a,1:1000])
  }
  for(u in 1:4){
    AgeR[y,a,u]<-median(spR_age[u,y,a,1:1000])
  }
}
}


AgePropsW<-array(NA, dim=c(Nyears,6,Nstocks), dimnames=list(Years,1:6,RiverNames))
AgeTotW<-array(NA, dim=c(Nyears,Nstocks))
AgePropsR<-array(NA, dim=c(Nyears,6,4), dimnames=list(Years,1:6,c(1:4)))
AgeTotR<-array(NA, dim=c(Nyears,4))
for(y in 1:Nyears){
  for(r in 1:Nstocks){
    AgeTotW[y,r]<-sum(AgeW[y,1:6,r])
    for(a in 1:6){
      AgePropsW[y,a,r]<-AgeW[y,a,r]/AgeTotW[y,r]
    }
  }
  for(u in 1:4){
    AgeTotR[y,u]<-sum(AgeR[y,1:6,u])
    for(a in 1:6){
      AgePropsR[y,a,u]<-AgeR[y,a,u]/AgeTotR[y,u]
    }
  }
}
round(AgePropsW,2)
round(AgePropsR,2)

for(i in 1:Nstocks){
  write_xlsx(as.data.frame(cbind(Years,AgePropsW[,,i])), paste0(PathOut_Scen, "AgePropsW_",RiverNames[i],".xlsx"))
}
for(i in 1:4){
  write_xlsx(as.data.frame(cbind(Years,AgePropsR[,,i])), paste0(PathOut_Scen, "AgePropsR_AU",i,".xlsx"))
}




# vuodet 2002-2012 = 11-21
Torne<-array(NA, dim=c(Nyears,6))
for(y in 1:Nyears){
for(a in 1:6){
Torne[y,a]<-median(spW_age[1,y,a,1:1000])
}
}

TorneTot<-vector()
TorneProp<-array(NA, dim=c(Nyears,6))
for(y in 1:Nyears){
TorneTot[y]<-sum(Torne[y,1:6])
  for(a in 1:6){
    TorneProp[y,a]<-Torne[y,a]/TorneTot[y]
  }
}
cbind(round(TorneProp,2),Years )


# Proportion that survives to spawn per smolt cohort

propW<-array(NA, dim=c(36, Nstocks))
propR<-array(NA, dim=c(36, Nstocks))
for(y in 1:36){
  for(r in 1:Nstocks){
    propW[y,r]<-median((spW_age[r,y+1,2,]+spW_age[r,y+2,3,]+spW_age[r,y+3,4,]+spW_age[r,y+4,5,]+
                 spW_age[r,y+5,6,])/SmoltW[r,y,])
  }
  for(u in 1:4){
    propR[y,u]<-median((spR_age[u,y+1,2,]+spR_age[u,y+2,3,]+spR_age[u,y+3,4,]+spR_age[u,y+4,5,]+
                           spR_age[u,y+5,6,])/SmoltR[u,y,])
  }
}
rownames(propW)<-c(1992:(1992+35))
rownames(propR)<-c(1992:(1992+35))
propW<-round(propW,3)
propR<-round(propR,3)

cbind(propW[,1],propR[,1],propW[,1]/propR[,1])
        
        
