##! #################################################
## Scenarios for Mps and M74, F 4.3.2.2
##! #################################################


# Maturation is the same in all scenarios
#! Effort 
EffScen<-scen_MPS

#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

File


load(File)

# ===============================================================================

par(mfrow=c(2,1))
par(mar=c(4,5,3,1)+0.1)

year <- c(1992:LastPredYear)

datW <- exp(-Mps_All)
datR <- exp(-Mps_AllR)
medW<-lowW<-highW<-MeanW<-c()
medR<-lowR<-highR<-MeanR<-c()
for(i in 1:dim(datW)[1]){
  dat_sumW<-summary(as.mcmc(datW[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  lowW[i]<-dat_sumW$quantiles[1]
  medW[i]<-dat_sumW$quantiles[3]
  highW[i]<-dat_sumW$quantiles[5]
  MeanW[i]<-dat_sumW$statistics[1]
  
  dat_sumR<-summary(as.mcmc(datR[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  lowR[i]<-dat_sumR$quantiles[1]
  medR[i]<-dat_sumR$quantiles[3]
  highR[i]<-dat_sumR$quantiles[5]
  MeanR[i]<-dat_sumR$statistics[1]
}

plot(year-0.3,medR[1:length(year)], type="p", pch=19, col="red", ylim=c(0,0.6),
     main="Post-smolt survival", ylab = "Rate", xlab="Year")
segments(year-0.3, lowR[1:length(year)], year-0.3, highR[1:length(year)], col="red")
points(year,medW[1:length(year)], type="p", pch=19)
segments(year, lowW[1:length(year)], year, highW[1:length(year)])
legend("topright",c("wild","reared"), pch=c(19,19), col=c("black","red"),lty=c(1,1))


apu<-c()
for( i in 1:1000){
  apu[i]<-sum(exp(-Mps_All)[17:20,i])/4
}
summary(as.mcmc(apu))


dat <- 1-M74_All
med<-vector(); low<-vector(); high<-vector()
for(i in 1:dim(dat)[1]){
  dat_sum<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  low[i]<-dat_sum$quantiles[1]
  med[i]<-dat_sum$quantiles[3]
  high[i]<-dat_sum$quantiles[5]
}

plot(year,med[1:length(year)], type="p", pch=19, ylim=c(0,1),main="M74 survival", ylab = "Rate",
     xlab="Year")
segments(year, low[1:length(year)], year, high[1:length(year)])
#med

p <- recordPlot();p

