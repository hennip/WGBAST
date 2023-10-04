
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.1.1, K graphs

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


nstocks<-17

source("05-results/flhm-figures/rmd_prior_data/make_JAGS_data_2023.R")
proj_years<-0
AUS<-4


tend<-years
yend<-1987+years-1

e_delay<-c(rep(4,times=13),3,3,4,3)
stocks1<-which(e_delay==4)
stocks2<-which(e_delay==3)

######################################################################################################

v<-as.matrix(chainsP)




# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


par(mfrow=c(2,3))
par(mar=c(3,4,4,2))

plot(density(na.omit(d[,"K[1]"]),n=100000), xlim=c(0,5000), lwd=2, ylim=c(0,0.003), main="Tornionjoki, Unit 1", xlab="")
lines(density(exp(rnorm(10000,M_K[1],sqrt(1/tau_K[1]))),bw=100),lwd=2,col="grey")
abline(v=exp(M_K[1]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[1]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[2]"]),n=100000), xlim=c(0,200), lwd=2, ylim=c(0,0.04), main="Simojoki, Unit 1")
lines(density(exp(rnorm(10000,M_K[2],sqrt(1/tau_K[2]))),bw=7),lwd=2,col="grey")
abline(v=exp(M_K[2]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[2]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[3]"]),n=100000), xlim=c(0,2500), lwd=2, ylim=c(0,0.005), main="Kalixälven, Unit 1")
lines(density(exp(rnorm(10000,M_K[3],sqrt(1/tau_K[3]))),bw=20),lwd=2,col="grey")
abline(v=exp(M_K[3]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[3]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[4]"]),n=100000),  xlim=c(0,250), lwd=2, ylim=c(0,0.025), main="Råneälven, Unit 1")
lines(density(exp(rnorm(10000,M_K[4],sqrt(1/tau_K[4]))),bw=10),lwd=2,col="grey")
abline(v=exp(M_K[4]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[4]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[5]"]),n=100000), xlim=c(0,200), lwd=2, ylim=c(0,0.2), main="Piteälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[5],sqrt(1/tau_K[2]))),bw=3),lwd=2,col="grey")
abline(v=exp(M_K[5]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[5]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[6]"]),n=100000),xlim=c(0,80), lwd=2, ylim=c(0,0.13), main="Åbyälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[6],sqrt(1/tau_K[6]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[6]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[6]"])),lty=2,lwd=2)

p1 <- recordPlot()
p1

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#windows()
par(mfrow=c(2,3))
par(mar=c(3,4,4,2))
plot(density(na.omit(d[ ,"K[7]"]),n=100000),xlim=c(0,500), lwd=2, ylim=c(0,0.015), main="Byskeälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[7],sqrt(1/tau_K[7]))),bw=15),lwd=2,col="grey")
abline(v=exp(M_K[7]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[7]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[8]"]),n=100000),xlim=c(0,30), lwd=2, ylim=c(0,0.15), main="Rickleån, Unit 2")
lines(density(exp(rnorm(10000,M_K[8],sqrt(1/tau_K[8]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[8]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[8]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[9]"]),n=100000), xlim=c(0,40), lwd=2, ylim=c(0,0.15), main="Sävarån, Unit 2")
lines(density(exp(rnorm(10000,M_K[9],sqrt(1/tau_K[9]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[9]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[9]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[10]"]),n=100000),xlim=c(0,700), lwd=2, ylim=c(0,0.015), main="Ume/Vindelälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[10],sqrt(1/tau_K[10]))),bw=30),lwd=2,col="grey")
abline(v=exp(M_K[10]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[10]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[11]"]),n=100000), xlim=c(0,150), lwd=2, ylim=c(0,0.03), main="Öreälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[11],sqrt(1/tau_K[11]))),bw=2),lwd=2,col="grey")
abline(v=exp(M_K[11]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[11]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[12]"]),n=100000),xlim=c(0,200), lwd=2, ylim=c(0,0.02), main="Lögdeälven, Unit 2")
lines(density(exp(rnorm(10000,M_K[12],sqrt(1/tau_K[12]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[12]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[12]"])),lty=2,lwd=2)

p2 <- recordPlot()
p2
 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
par(mfrow=c(2,3))
par(mar=c(3,4,4,2))

plot(density(na.omit(d[ ,"K[16]"]),n=100000),xlim=c(0,200),ylim=c(0,0.03),main="Kågeälven, Unit 2", lwd=2)
lines(density(exp(rnorm(10000,M_K[16],sqrt(1/tau_K[16]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[16]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[16]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[13]"]),n=100000),xlim=c(0,12), lwd=2, ylim=c(0,0.4),   main="Ljungan, Unit 3")
lines(density(exp(rnorm(10000,M_K[13],sqrt(1/tau_K[13]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[13]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[13]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[17]"]),n=100000),xlim=c(0,12), lwd=2, ylim=c(0,0.8),   main="Testeboån, Unit 3")
lines(density(exp(rnorm(10000,M_K[17],sqrt(1/tau_K[17]))),bw=0.6),lwd=2,col="grey")
abline(v=exp(M_K[17]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[17]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[15]"]),n=100000),xlim=c(0,50), lwd=2, ylim=c(0,0.1), main="Emån, Unit 4")
lines(density(exp(rnorm(10000,M_K[15],sqrt(1/tau_K[15]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[15]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[15]"])),lty=2,lwd=2)

plot(density(na.omit(d[ ,"K[14]"]),n=100000),xlim=c(0,150), lwd=2, ylim=c(0,0.08), main="Mörrumsån, Unit 4")
lines(density(exp(rnorm(10000,M_K[14],sqrt(1/tau_K[14]))),bw=5),lwd=2,col="grey")
abline(v=exp(M_K[14]),lty=2,col="grey",lwd=2)
abline(v=median(na.omit(d[,"K[14]"])),lty=2,lwd=2)

p3 <- recordPlot()
p3
 
