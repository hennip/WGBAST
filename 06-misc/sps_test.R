
require(rjags)
model<-"
model{

#for(i in 1:5){
#  for(s in 1:2){
#    x[i,s]~dbin((1-sps[i,s]),n[s])
#}}

  # for(s in 1:2){
  #   for(i in 1:5){
  #    Mps[i,s]<- -log(sps[i,s])
  #    logit(sps[i,s])<-logit_sps[i,s]
  #   }
  # 
  #   logit_sps[1,s]~dnorm(0,0.39) 
  #   mean_sps[s]~dnorm(0,0.39) 
  #   for(i in 2:5){
  #     logit_sps[i,s]<-mean_sps[s]+eps[i]#~dnorm(mu_sps[i],tau_sps)
  #   }
  # }
  # 
  # for(i in 1:5){
  #   eps[i+1]~dnorm(eps[i]*phi, (1/(1-pow(phi,2)*pow(sd_eps,2))))
  # }  
  # eps[1]~dunif(-10,10)
  # #phi~dunif(-1,1)
  # phi~dunif(-0.6,0.6)
  # sd_eps~dunif(0.01,1.6)


early_MpsW~dlnorm(0.23,19)T(0.5,5)
early_sps<-exp(-early_MpsW)
l_early<-log(early_sps/(1-early_sps))
#logit_sps[1]<-log(early_sps/(1-early_sps))

  for(i in 1:30){
   Mps[i]<- -log(sps[i])
   logit(sps[i])<-logit_sps[i]
  }

  logit_sps[1]~dnorm(-0.95,5.88) # corresponds to early_MpsW~dlnorm(0.23,19)T(0.5,5)->early_surv->logit
  #mean_sps[1]~dnorm(-0.95,5.88)
  
   for(r in 1:2){
     mean_sps[r]~dnorm(mu_mean_sps, 1/pow(sd_mean_sps,2))
   }
   mu_mean_sps~dnorm(-0.95,25)
   sd_mean_sps~dunif(0.001,0.5) #between stocks variation in mean sps

  for(i in 2:30){
    logit_sps[i]<-mean_sps[1]+eps[i]#~dnorm(mu_sps[i],tau_sps)
    logit_sps_mean[i]<-mu_mean_sps+eps[i]
  }

  for(i in 1:30){
    eps[i+1]~dnorm(eps[i]*phi,1/((1-pow(phi,2))*pow(sd_eps,2)))
  }  
  eps[1]~dnorm(-0.95,5.88) # gives early_surv for each year if phi near 1, sd_eps near 0 and mean_sps 0 
  phi~dunif(-1,1)
  sd_eps~dunif(0.001,0.4) # Chosen so that a priori annual survivals are roughly at early_sps level




}
"

#data=list(x=array(c(rep(1,5),rep(10,5)), dim=c(5,2)),n=c(100,100))
jm=jags.model(textConnection(model),#data,
              n.chains=2)
chains=coda.samples(jm,c("sps","phi", "sd_eps",
                         "mean_sps",
                         "early_sps",
                         "l_early",
                         "eps"#,"m"
                         ),n.iter=10000)

#autocorr.plot(chains)
summary(chains)
plot(chains[,"early_sps"])
plot(chains[,"phi"])
plot(chains[,"sd_eps"])
plot(chains[,"eps[2]"])
plot(chains[,"sps[2]"])
plot(chains[,"sps[3]"])


plot(chains[,"eps[3]"])

plot(chains[,"eps[4]"])
plot(chains[,"eps[5]"])


# Analyse simulated data, parameterisation = 2
data=list(x=x,m=2)
jm=jags.model(textConnection(model),data,n.chains=1)
chains=coda.samples(jm,c("mu","sd","p[20]"),n.iter=10000)
par(mfrow=c(2,2))

autocorr.plot(chains)

# Analyse simulated data, parameterisation = 3
data=list(x=x,m=3)
jm=jags.model(textConnection(model),data,n.chains=1)
chains=coda.samples(jm,c("mu","sd","p[20]"),n.iter=10000)

par(mfrow=c(2,2))

autocorr.plot(chains) 
