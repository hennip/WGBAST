# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Maturation per age class
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ 


# Maturation is the same in all scenarios
#! Effort
EffScen<-scen_Matur

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")
#ScenProj_2023_EScen1_RCzero23-35
File
load(File)


# ===============================================================================
par(mfrow=c(2,2))
par(mar=c(4,5,3,1)+0.1)

medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
#dim(MatRateW)
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MatRateW[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MatRateW[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MatRateW[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MatRateR[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MatRateR[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MatRateR[2,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="Grilse", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)
legend("topleft",c("wild","reared"), pch=c(21,21), col=c("black","red"),
       pt.bg=c("black","red"),lty=c(1,1))


medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MatRateW[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MatRateW[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MatRateW[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MatRateR[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MatRateR[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MatRateR[3,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="2SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)


medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MatRateW[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MatRateW[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MatRateW[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MatRateR[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MatRateR[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MatRateR[4,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="3SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)


#cbind(year,medW, lowW, highW)


medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MatRateW[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MatRateW[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MatRateW[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MatRateR[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MatRateR[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MatRateR[5,y,1,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="4SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)
