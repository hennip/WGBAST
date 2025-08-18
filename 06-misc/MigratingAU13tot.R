# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Statistics for salmon returning to Bothnian Bay
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#rm(list=ls(all=TRUE))



# Skip == T when running 05-results/workflow-extra-output 
if(exists("skip")==F){

source("../run-this-first-wgbast.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location
  
Model<-"2024_JAGS_Mps" # Assessment model version
nsim<-1000
LastHistYear<-2023
ymax<-350
LastPredYear<-LastHistYear+ymax
Years<-c(1992:LastPredYear)
length(Years)

Nyears<-yBreak<-length(Years)
sims<-c(1:nsim)
Nstocks<-17; Nunits<-4 


EffScen<-14

#Load the file containing stats
File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

File
load(File)
}
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

stats<-function(var){
  mu<-sd<-q5<-q50<-q95<-c()
  for(i in 2:Nyears){
    tmp<-summary(as.mcmc(var[i,]), quantiles=c(0.05,0.5,0.95))
    mu[i]<-tmp$statistics[1]
    sd[i]<-tmp$statistics[2]
    q5[i]<-tmp$quantiles[1]
    q50[i]<-tmp$quantiles[2]
    q95[i]<-tmp$quantiles[3]
  }
  x<-cbind(Years,mu, sd, q5, q50, q95)
  x
}


dim(Migr_AU13tot)
dim(Migr_AU13W)


res<-stats(Migr_AU13tot)
write_xlsx(as.data.frame(res),path=paste0(PathOut_Scen,"migratingAU1-3.xlsx"))

res<-stats(Migr_AU13W)
write_xlsx(as.data.frame(res),path=paste0(PathOut_Scen,"migratingAU1-3_wild.xlsx"))
  
res<-stats(Migr_AU13R)
write_xlsx(as.data.frame(res),path=paste0(PathOut_Scen,"migratingAU1-3_reared.xlsx"))
  
res<-stats(Migr_Tornio)
write_xlsx(as.data.frame(res),path=paste0(PathOut_Scen,"migratingAU1-3_torne.xlsx"))

res<-stats(Migr_Simo)
write_xlsx(as.data.frame(res),path=paste0(PathOut_Scen,"migratingAU1-3_simo.xlsx"))

# Torne
  
  mu<-sd<-q5<-q50<-q95<-c()
  for(i in 2:Nyears){
    tmp<-summary(as.mcmc(Migr_Tornio[i,]), quantiles=c(0.05,0.5,0.95))
    mu[i]<-tmp$statistics[1]
    sd[i]<-tmp$statistics[2]
    q5[i]<-tmp$quantiles[1]
    q50[i]<-tmp$quantiles[2]
    q95[i]<-tmp$quantiles[3]
  }
  
  res<-as.data.frame(cbind(Years,mu, sd, q5, q50, q95))
  write_xlsx(res,path=paste0(PathOut_Scen,"migratingAU1-3_Torne.xlsx")) 
  
  
  
  
  
  
# Reared grilse proportions, AU 1-3

# Fourth index (1) is migrating individuals
MigrRAU13_Age<-array(NA, dim=c(6,Nyears,1000))
for(a in 1:6){
  for(y in 1:Nyears){
  for(i in 1:1000){
    #MigrRAU13_Age[a,y,i]<-  sum(MigrR[a,y,1:3,1,i], na.rm=T)
    MigrRAU13_Age[a,y,i]<-  sum(MatR_1[a,y,1:3,i], na.rm=T)
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

res<-cbind(c(1992:(1992+Nyears-1)),Q5, Q50, Q95) %>% as.data.frame
write_xlsx(res,paste0(PathOut_Scen,"migratingAU1-3R_GrilseProp.xlsx"))



