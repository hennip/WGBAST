# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Maturation per age class
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ 
  
rm(list=ls(all=TRUE))


source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")

# Maturation is the same in all scenarios
#! Effort 
EffScen<-5

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")

File
load(File)


# ===============================================================================
#windows()
tiff(paste0(PathScen,"F4323_maturation.tiff"),  width=1600, height=1600, res=200)

par(mfrow=c(2,2))
par(mar=c(4,5,3,1)+0.1)

medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MaturationW[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MaturationW[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MaturationW[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MaturationR[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MaturationR[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MaturationR[2,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="Grilse", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)
legend("topleft",c("wild","reared"), pch=c(21,21), col=c("black","red"),
pt.bg=c("black","red"),lty=c(1,1))

#cbind(year,medW, lowW, highW)
summary(as.mcmc(MaturationW[2,23,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95)) # 2014
summary(as.mcmc(MaturationR[2,23,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95)) # 2014


medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MaturationW[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MaturationW[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MaturationW[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MaturationR[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MaturationR[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MaturationR[3,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="2SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)

#cbind(year,medW, lowW, highW)

medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MaturationW[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MaturationW[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MaturationW[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MaturationR[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MaturationR[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MaturationR[4,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="3SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)
 

#cbind(year,medW, lowW, highW)


medW<-c();lowW<-c();highW<-c()
medR<-c();lowR<-c();highR<-c()
for(y in 1:length(year)){
  medW[y]<-summary(as.mcmc(MaturationW[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowW[y]<-summary(as.mcmc(MaturationW[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highW[y]<-summary(as.mcmc(MaturationW[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
  medR[y]<-summary(as.mcmc(MaturationR[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[3]
  lowR[y]<-summary(as.mcmc(MaturationR[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[1]
  highR[y]<-summary(as.mcmc(MaturationR[5,y,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))$quantiles[5]
}

plot(year-0.2, medR,pch=16,col="red", ylim=c(0,1), main="4SW", ylab="Proportion mature", xlab="Year")
segments(year-0.2,lowR, year-0.2,highR, col="red")
points(year, medW,pch=16)
segments(year,lowW, year,highW)

#cbind(year,medW, lowW, highW)

dev.off()
