
require(rjags)
model<-"
model{

  for(i in 1:5){
   surv[i]<-exp(-m[i])
  }
  
  m[1]~dlnorm(0.23,19)T(0.5,5)
  mean_m<-0#~dlnorm(0.23,19)T(0.5,5) 
  
  for(i in 2:5){
    #mu_m[i] <- log(phi*m[i-1]+(1-phi)*mean_m) - 0.5 / tau # mean reverting AR(1)
    #m[i]~dlnorm(mu_m[i],tau)
    mu_m[i] <- phi*m[i-1]+(1-phi)*mean_m # mean reverting AR(1)
    m[i]~dnorm(mu_m[i],tau)T(0.5,5)
  }

tau<-1/((1-pow(phi,2))*(sd*sd))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd~dunif(0.001,2)
#sd~dunif(0.01,1.6)
phi~dunif(0,1) #positive autocorrelation


}
"

#data=list(x=x,m=1)
jm=jags.model(textConnection(model),#data,
              n.chains=1)
chains=coda.samples(jm,c("surv"#,"m"
                         ),n.iter=10000)

#autocorr.plot(chains)
summary(chains)
plot(chains)


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
