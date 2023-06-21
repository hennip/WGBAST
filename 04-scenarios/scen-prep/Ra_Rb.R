# First read in the chains from the assessment model (JAGS)

load(file=paste0(PathOutput_FLHM,"FLHM_2023_rivHR_data2023_thin350.RData"))
chains<-as.mcmc(run) #pool 2 chains together

Ra<-chains[,"Ra"]
Rb<-chains[,"Rb"]

par(mfrow=c(1,2))
traceplot(Ra)
traceplot(Rb)

#Ra<-as.data.frame(Ra[[1]])[,1]
#Rb<-as.data.frame(Rb[[1]])[,1]
if(nchains==1){
Ra<-as.data.frame(Ra)[,1]
Rb<-as.data.frame(Rb)[,1]
}

plot(Ra,Rb)

# 1000 samples from distribution:
RMPS<-rbeta(length(Ra),Ra,Rb)
plot(density(RMPS))


mean(RMPS)
sd(RMPS)
quantile(RMPS,p=c(0.025,0.5,0.975))

par(mfrow=c(1,2))
plot(density(RMPS))

rmu<-mean(RMPS)
rmu
reta<-(rmu*(1-rmu)-(sd(RMPS)^2))/(sd(RMPS)^2)
reta

ra<-rmu*reta
rb<-(1-rmu)*reta

samp<-rbeta(1000, ra, rb)
plot(density(as.mcmc(samp)))
quantile(samp,p=c(0.025,0.5,0.975))

#! Input these to Inputs-file
# Rmu
rmu
#Reta
1/reta # difference in the parameterisation





