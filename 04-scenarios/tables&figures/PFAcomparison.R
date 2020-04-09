
rm(list=ls(all=TRUE))


source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")

EffScen<-1

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")

File
load(File)


# ===============================================================================

dim(PFAW)
PFAW2<-array(NA, dim=c(6,Nyears,1000))
PFAW2_MSW<-array(NA, dim=c(Nyears,1000))
PFAR2<-array(NA, dim=c(6,Nyears,1000))
PFAR2_MSW<-array(NA, dim=c(Nyears,1000))
PFA<-array(NA, dim=c(6,Nyears,1000))
PFA_MSW<-array(NA, dim=c(Nyears,1000))
PFAall<-array(NA, dim=c(Nyears,1000))
PFAWall<-array(NA, dim=c(Nyears,1000))
for(y in 1:Nyears){
for(s in 1:1000){
  PFAWall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)
  PFAall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[1:6,y,1:4,1,s])
  PFA_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[2:6,y,1:4,1,s])
  PFAR2_MSW[y,s]<-sum(PFAR[2:6,y,1:4,1,s])
  PFAW2_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)

for(a in 1:6){
  PFAW2[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)
  PFAR2[a,y,s]<-sum(PFAR[a,y,1:4,1,s])
  PFA[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[a,y,1:4,1,s])
}
}}

# ?????????????????????????????????????????????????????????????????
# Old estimates
#model<-"2019"
#File2<-paste0(PathScen,"ScenProj_",model,"_MpsMED_EScen",EffScen,".RData")
File2<-paste0(PathScen,"ScenProj_2019_LR_MpsMED_EScen1.RData")
load(File2)


dim(PFAW)
PFAW2old<-array(NA, dim=c(6,Nyears,1000))
PFAW2_MSWold<-array(NA, dim=c(Nyears,1000))
PFAR2old<-array(NA, dim=c(6,Nyears,1000))
PFAR2_MSWold<-array(NA, dim=c(Nyears,1000))
PFAold<-array(NA, dim=c(6,Nyears,1000))
PFA_MSWold<-array(NA, dim=c(Nyears,1000))
PFAallold<-array(NA, dim=c(Nyears,1000))
PFAWallold<-array(NA, dim=c(Nyears,1000))
for(y in 1:(Nyears-2)){
for(s in 1:1000){
  PFAWallold[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)
  PFAallold[y,s]<-sum(PFAW[1:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[1:6,y,1:4,1,s])
  PFA_MSWold[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[2:6,y,1:4,1,s])
  PFAR2_MSWold[y,s]<-sum(PFAR[2:6,y,1:4,1,s])
  PFAW2_MSWold[y,s]<-sum(PFAW[2:6,y,1:Nstocks,1,s], na.rm=T)

for(a in 1:6){
  PFAW2old[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)
  PFAR2old[a,y,s]<-sum(PFAR[a,y,1:4,1,s])
  PFAold[a,y,s]<-sum(PFAW[a,y,1:Nstocks,1,s], na.rm=T)+sum(PFAR[a,y,1:4,1,s])
}
}}



# OBS! One year shift in the time-series! Take care that
# highest abundances hit the correct years!
windows(record = T)
par(mfrow=c(2,2))    
par(mar=c(3,5,3,1)+0.1)

# 1SW wild
med1<-c();low1<-c();high1<-c()
med2<-c();low2<-c();high2<-c()
for(y in 1:(Nyears-2)){
  tmp1<-summary(as.mcmc(PFAW2[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med1[y]<-tmp1$quantiles[3]
  low1[y]<-tmp1$quantiles[1]
  high1[y]<-tmp1$quantiles[5]

  tmp2<-summary(as.mcmc(PFAW2old[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med2[y]<-tmp2$quantiles[3]
  low2[y]<-tmp2$quantiles[1]
  high2[y]<-tmp2$quantiles[5]
}
#cbind(year,med2)
plot(year[1:(Nyears-2)]+0.8, med2, pch=16, ylim=c(0,2000), col="green",
main=paste(sep="","1SW wild, scen ",EffScen), xlim=c(1992,2027.3),
xlab="Year", ylab="Abundance (in 1000's)")
segments(year[1:(Nyears-2)]+0.8,low2, year[1:(Nyears-2)]+0.8,high2, col="green")

points(year[1:(Nyears-2)]+1, med1, pch=16)
segments(year[1:(Nyears-2)]+1,low1, year[1:(Nyears-2)]+1,high1)

# 1SW wild + reared
med1<-c();low1<-c();high1<-c()
med2<-c();low2<-c();high2<-c()
for(y in 1:(Nyears-2)){
  tmp1<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med1[y]<-tmp1$quantiles[3]
  low1[y]<-tmp1$quantiles[1]
  high1[y]<-tmp1$quantiles[5]

  tmp2<-summary(as.mcmc(PFAold[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med2[y]<-tmp2$quantiles[3]
  low2[y]<-tmp2$quantiles[1]
  high2[y]<-tmp2$quantiles[5]
}

plot(year[1:(Nyears-2)]+0.8, med2, pch=16, ylim=c(0,3000), col="green", xlim=c(1992,2027.3),
main=paste(sep="","1SW wild & reared, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year[1:(Nyears-2)]+0.8,low2, year[1:(Nyears-2)]+0.8,high2, col="green")

points(year[1:(Nyears-2)]+1, med1, pch=16)
segments(year[1:(Nyears-2)]+1,low1, year[1:(Nyears-2)]+1,high1)

# MSW wild

med1<-c();low1<-c();high1<-c()
med2<-c();low2<-c();high2<-c()
for(y in 1:(Nyears-2)){
  tmp1<-summary(as.mcmc(PFAW2_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med1[y]<-tmp1$quantiles[3]
  low1[y]<-tmp1$quantiles[1]
  high1[y]<-tmp1$quantiles[5]

  tmp2<-summary(as.mcmc(PFAW2_MSWold[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med2[y]<-tmp2$quantiles[3]
  low2[y]<-tmp2$quantiles[1]
  high2[y]<-tmp2$quantiles[5]
}
plot(year[1:(Nyears-2)]+0.8, med2, pch=16, ylim=c(0,2500), col="green",xlim=c(1992,2027.3),
main=paste(sep="","MSW wild, scen ",EffScen), 
xlab="Year", ylab="Abundance (in 1000's)")
segments(year[1:(Nyears-2)]+0.8,low2, year[1:(Nyears-2)]+0.8,high2, col="green")

points(year[1:(Nyears-2)]+1, med1, pch=16)
segments(year[1:(Nyears-2)]+1,low1, year[1:(Nyears-2)]+1,high1)


# MSW wild + reared

med1<-c();low1<-c();high1<-c()
med2<-c();low2<-c();high2<-c()

for(y in 1:(Nyears-2)){
  tmp1<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med1[y]<-tmp1$quantiles[3]
  low1[y]<-tmp1$quantiles[1]
  high1[y]<-tmp1$quantiles[5]

  tmp2<-summary(as.mcmc(PFA_MSWold[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med2[y]<-tmp2$quantiles[3]
  low2[y]<-tmp2$quantiles[1]
  high2[y]<-tmp2$quantiles[5]
}
plot(year[1:(Nyears-2)]+0.8, med2, pch=16, ylim=c(0,3000), col="green", xlim=c(1992,2027.3),
main=paste(sep="","MSW wild & reared, scen ",EffScen), xlab="Year", ylab="Abundance (in 1000's)")
segments(year[1:(Nyears-2)]+0.8,low2, year[1:(Nyears-2)]+0.8,high2, col="green")

points(year[1:(Nyears-2)]+1, med1, pch=16)
segments(year[1:(Nyears-2)]+1,low1, year[1:(Nyears-2)]+1,high1)

# Total PFA: 
par(mfrow=c(1,1))

med1<-c();low1<-c();high1<-c()
med2<-c();low2<-c();high2<-c()

for(y in 1:(Nyears-2)){
  tmp1<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med1[y]<-tmp1$quantiles[3]
  low1[y]<-tmp1$quantiles[1]
  high1[y]<-tmp1$quantiles[5]
  
  tmp2<-summary(as.mcmc(PFAallold[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
  med2[y]<-tmp2$quantiles[3]
  low2[y]<-tmp2$quantiles[1]
  high2[y]<-tmp2$quantiles[5]
}
plot(year[1:(Nyears-2)]+0.8, med2, pch=16, ylim=c(0,4500), col="green", xlim=c(1992,2027.3),
     main=paste(sep="","Total PFA, scen ",EffScen), xlab="Year", ylab="Abundance (in 1000's)")
segments(year[1:(Nyears-2)]+0.8,low2, year[1:(Nyears-2)]+0.8,high2, col="green")
points(year[1:(Nyears-2)]+1, med1, pch=16)
segments(year[1:(Nyears-2)]+1,low1, year[1:(Nyears-2)]+1,high1)
legend("topleft", c("2020","2019"), pch=16, lty=1, col=c(1,"green"))


 # To check the stats:

# Total PFA: 
med<-c();low<-c();high<-c()
for(y in 1:Nyears){
  med[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
 
cbind(year+1,med, low, high)
 
 
# Wild PFA: 
med<-c();low<-c();high<-c()
for(y in 1:Nyears){
  med[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFAWall[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}
 
cbind(year+1,med, low, high)

# Grilse total PFA
med<-c();low<-c();high<-c()
for(y in 1:Nyears){
  med[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFA[1,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}

cbind(year+1,med, low, high)

# MSW total PFA
med<-c();low<-c();high<-c()
for(y in 1:Nyears){
  med[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[3]
  low[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[1]
  high[y]<-summary(as.mcmc(PFA_MSW[y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))$quantiles[5]
}

cbind(year+1,med, low, high)
 

 

