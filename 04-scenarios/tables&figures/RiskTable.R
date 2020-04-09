
rm(list=ls(all=TRUE))

source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")

# Time
cbind(c(1992:2032),c(1:41))
compyear<-28 # 2019
refyear<-35 # 2026


# Scenarios
#! Effort 
for(EffScen in 1:8){
#EffScen<-1

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")

File
load(File)


#Calculate the chance of some statistic (vector) being above some reference point
risk<-function(x,ref) {sum(ifelse(x>ref,1,0))/length(x)}


#sum(ifelse(frac[r,]>1,1,0))/length(frac[r,])

# Calculate probability for smolt production to be higher 
# on next generation compared to last observed year

dim(SmoltW)
frac<-array(NA, dim=c(Nstocks,1000))
for(r in 1:13){
  frac[r,]<-SmoltW[r,refyear,]/SmoltW[r,compyear,]# 2020/2013, AU 1-3
}
for(r in 14:15){
  frac[r,]<-SmoltW[r,refyear-1,]/SmoltW[r,compyear,]# 2019/2013, AU 4
}
for(r in 16:17){
  frac[r,]<-SmoltW[r,refyear,]/SmoltW[r,compyear,]# 2020/2013, AU 1-3
}

ProbIncrease<-c()
for(r in 1:Nstocks){
  ProbIncrease[r]<-risk(frac[r,],1)
}
cbind(1:Nstocks,ProbIncrease)

#summary(as.mcmc(SmoltW[r,refyear,]))
#summary(as.mcmc(SmoltW[r,22,]))
#summary(as.mcmc((SmoltW[r,22,]-SmoltW[r,refyear,])/SmoltW[r,22,]))
#summary(as.mcmc((SmoltW[r,refyear,]-SmoltW[r,22,])/SmoltW[r,22,]))

# Calculate probability to reach 50/75 % of the PSPC in the next generation

target_R0<-array(NA, dim=c(1000,Nstocks))
for(r in 1:Nstocks){
  for(i in 1:1000){
    target_R0[i,r]<-mean(R0[(yBreak-4):yBreak,r,i]) # average R0 over last 5 years
  }
}

#Store risk values
Prob50<-array(NA, dim=c(Nstocks,Nyears))
Prob75<-array(NA, dim=c(Nstocks,Nyears))
for(i in 1:length(year)){
  for(r in 1:Nstocks){
    Prob50[r,i]<-risk(SmoltW[r,i,],0.50*target_R0[,r])
    Prob75[r,i]<-risk(SmoltW[r,i,],0.75*target_R0[,r])
  }
}

#For the year 2019
Rivers50<-c(Prob50[1:13,refyear],Prob50[14:15,refyear-1],Prob50[16,refyear],Prob50[17,refyear])
Rivers75<-c(Prob75[1:13,refyear],Prob75[14:15,refyear-1],Prob75[16,refyear],Prob75[17,refyear])
  
df<-as.data.frame(cbind(Rivers50,Rivers75,ProbIncrease))

write_xlsx(df,paste0(PathScen,"RiskByRivers_EScen",EffScen,"_",Model,".xlsx"))

}



