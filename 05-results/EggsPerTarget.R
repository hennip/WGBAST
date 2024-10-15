library(coda)
library(writexl)
source("run-this-first.R")

# Time
######################
Year<-c(1996:2024) # Smolt years
Nyears<-length(Year) 
Nstocks<-17
nsim<-1000

model<-"2024"


dim(chains)
cbind(c(1:length(c(1987:2025))),c(1987:2025))

load(paste0(pathMain,"WGBAST_shared/scen/2024/output/ScenProj_2024_JAGS_Mps_EScen1_RCzero23-35.RData"))
dim(BHalpha)
dim(BHbeta)

load(paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/ref_pts_2024_JAGS_Mps.RData"))
dim(Smolt_lim)
dim(Smolt_MSY)

load(paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/ref_pts_2024_2024_JAGS_Mps_eggs.RData"))
#[1] "comp.inds" "Eggs_lim"  "Eggs_MSY"  "Eggs0"     "MSY"       "Smolt_lim"
#[7] "Smolt_MSY"
dim(Eggs_MSY)

load(paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/eqm_distns_Mps.RData"))
#[1] "R0_all" "S0_all"

Eggs_tbl<-function(Eggs){
df<-array(NA, dim=c(Nstocks,3))
for(r in 1:Nstocks){
  df[r,]<-quantile(Eggs[r,], probs=c(0.5,0.75,0.9))
}
colnames(df)<-c("50%","75%","90%")
rows<-c("Torne","Simo","Kalix","Rane"
                ,"Pite","Aby","Byske","Rickle","Savaran"
                ,"Ume","Ore","Lodge","Ljungan","Morrum"
                ,"Eman", "Kage", "Testeboan")
df<-as.data.frame(cbind(rows,df))
df
}

write_xlsx(Eggs_tbl(Eggs_lim),paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/Eggslim_",model,".xlsx"))
write_xlsx(Eggs_tbl(Eggs_MSY),paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/EggsMSY_",model,".xlsx"))
write_xlsx(Eggs_tbl(Eggs0),paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/Eggs0_",model,".xlsx"))


write_xlsx(Eggs_tbl(Elim),paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/Eggslim2_",model,".xlsx"))
write_xlsx(Eggs_tbl(Emsy),paste0(pathMain,"WGBAST_shared/scen/2024/Ref pts/EggsMSY2_",model,".xlsx"))







E<-((BHalpha[1,1]*Smolt_lim[1,1])/(1-BHbeta[1,1]*Smolt_lim[1,1]))/1000 # eggs in millions


Elim<-array(NA, dim=c(Nstocks,nsim))
Emsy<-array(NA, dim=c(Nstocks,nsim))
for(i in 1:nsim){
  for(r in 1:Nstocks){
    Elim[r,i]<-((BHalpha[i,r]*Smolt_lim[r,i])/(1-BHbeta[i,r]*Smolt_lim[r,i]))/1000 # eggs in millions
    Emsy[r,i]<-((BHalpha[i,r]*Smolt_MSY[r,i])/(1-BHbeta[i,r]*Smolt_MSY[r,i]))/1000 # eggs in millions
  }
}



RiskLevel<-75
 R2<-array(NA, dim=c(Nstocks,nsim))
 # for(r in 1:Nstocks){
 # #r<-1
 #   tmp<-array(NA,dim=c(nsim,length(10:33)))
 #   for(i in 10:33){ # years 1996-2019
 #     tmp[,i-9]<-chains[,paste0("R0[",i,",",r,"]")]*RiskLevel/100 # R0 in assessment year -1
 #   }
 # R2[r,]<-apply(tmp,1,mean)
 # }
R2<-R0_all*RiskLevel/100

#summary(as.mcmc(R2[1,]))

Estar<-array(NA, dim=c(Nstocks,nsim))
for(i in 1:nsim){
for(r in 1:Nstocks){
  Estar[r,i]<-((a[r,i]*R2[r,i])/(1-b[r,i]*R2[r,i]))/1000 # eggs in millions
}
}

df<-array(NA, dim=c(Nstocks,4))
for(r in 1:Nstocks){
  df[r,]<-quantile(Estar[r,], probs=c(0.5,0.75,0.9,0.95))
}
colnames(df)<-c("50%","75%","90%","95%")
rownames(df)<-c("Torne","Simo","Kalix","Rane"
                ,"Pite","Aby","Byske","Rickle","Savaran"
                ,"Ume","Ore","Lodge","Ljungan","Morrum"
                ,"Eman", "Kage", "Testeboan")
df<-as.data.frame(df)

write_xlsx(df,paste0("05-results/EggsPerPSPC_",model,"_",RiskLevel,"level",".xlsx"))



