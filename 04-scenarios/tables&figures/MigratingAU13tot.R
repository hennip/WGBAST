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

#Number of migrating per wild river
# 
# dim(MigrW)
# 
# for(i in 1:Nyears){
#   tmp<-summary(sum(MigrW[2:6,y,r,,]), quantiles=c(0.05,0.5,0.95))
# }  
# #Number of migrating per reared AU
# 
# dim(MigrR)
# 

##############


dim(Migr_AU13tot)

mu<-c();sd<-c()
q5<-c(); q50<-c(); q95<-c()

for(i in 1:Nyears){
  tmp<-summary(as.mcmc(Migr_AU13tot[i,]), quantiles=c(0.05,0.5,0.95))
  mu[i]<-tmp$statistics[1]
  sd[i]<-tmp$statistics[2]
  q5[i]<-tmp$quantiles[1]
  q50[i]<-tmp$quantiles[2]
  q95[i]<-tmp$quantiles[3]
}

res<-as.data.frame(cbind(year,mu, sd, q5, q50, q95))
  write_xlsx(res,path=paste0(PathOut,"migratingAU13.xlsx"))

# Reared grilse proportions, AU 1-3

# Fourth index (1) is migrating individuals
MigrRAU13_Age<-array(NA, dim=c(6,Nyears,1000))
for(a in 1:6){
  for(y in 1:Nyears){
  for(i in 1:1000){
    MigrRAU13_Age[a,y,i]<-  sum(MigrR[a,y,1:3,1,i], na.rm=T)
  }
}
}
GrilseProp<-array(NA, dim=c(Nyears, 1000))
for(y in 1:Nyears){
  for(i in 1:1000){
    GrilseProp[y,i]<-MigrRAU13_Age[2,y,i]/sum(MigrRAU13_Age[2:6,y,i])
    }}

Q5<-c()
Q50<-c()
Q95<-c()

for(y in 2:Nyears){
  tmp<-summary(as.mcmc(GrilseProp[y,]), quantiles=c(0.05,0.5,0.95))
  Q5[y]<-tmp$quantiles[1]
  Q50[y]<-tmp$quantiles[2]
  Q95[y]<-tmp$quantiles[3]
  }

res<-cbind(c(1992:(1992+Nyears-1)),Q5, Q50, Q95)
write.xlsx(res,paste0(PathOut,"migratingAU13R_GrilseProp.xlsx"))



