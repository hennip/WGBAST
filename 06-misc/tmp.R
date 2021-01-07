"
#early_MpsW~dlnorm(0.23,19)T(0.5,5)
#for(i in 1:4){
#    mu_MpsW[i] <- log(early_MpsW)
#}
#for(i in 5:(m+proj_years)){
#    mu_MpsW[i] <- log(mean(MpsW[(i-4):(i-1)])) - 0.5 / tau_MpsW # 4-year MA
#}    



# Annual Mps survival
for(i in 1:(m+proj_years)){
  for(s in 1:stocks){
    MpsW[i,s]<-
  }
}


for(s in 1:stocks){
for(i in 1:(m+proj_years)){
  logit(SpsW[i,s])<-logitSpsW[i,s]
  
  logitSpsW[i,s]~dnorm(muSpsW[i,s],tauSpsW)
  mu_SpsW[i] <- phi_Sps[i]*logitSpsW[i-1,s]+mean_SpsW[s] # mean reverting AR(1)
}
}


  MpsW[1,s]~dlnorm(0.23,19)T(0.5,5)
  mean_MpsW[s]~dlnorm(0.23,19)T(0.5,5) 
  
  for(i in 2:(m+proj_years)){
    #mu_MpsW[i,s] <- log(phi_mps*MpsW[i-1,s]+(1-phi_mps)*mean_mps[s]) - 0.5 / tau_MpsW # mean reverting AR(1)
    mu_MpsW[i,s] <- phi_mps*MpsW[i-1,s]+(1-phi_mps)*mean_MpsW[s] # mean reverting AR(1)
    MpsW[i,s]~dnorm(mu_MpsW[i,s],tau_MpsW)T(0.5,5) #Obs! Not logN as previously
    
  }
}

tau_MpsW<-1/((1-pow(phi_mps,2))*(sd_mps*sd_mps))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd_mps~dunif(0.01,1.6)
phi_Sps~dunif(-1,1) 


 logit(HtW[i,2])<-logitHtW2[i+1] 
 
for(i in 1:(m+4)){ # i: calendar year
  logitHtW2[i+1]~dnorm(mu_trW[i+1],tau_tr)
  mu_trW[i+1]<-phi_tr*logitHtW2[i]+(1-phi_tr)*mean_trW # mean reverting AR(1)
  
  #logitHtR2[i+1]~dnorm(mu_trR[i+1],tau_tr)
  # mu_trR[i+1]<-phi_tr*logitHtR2[i]+(1-phi_tr)*mean_trR
  
}

tau_tr<-1/((1-pow(phi_tr,2))*(sd_tr*sd_tr))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd_tr~dunif(0.01,1.6)
phi_tr~dunif(0,1) #positive autocorrelation
mean_trW~dnorm(0,0.39) # implies uniform[0,1] prior for mean harvest rate
#mean_trR~dnorm(0,0.39) 
logitHtW2[1]~dnorm(0,0.39) 
#logitHtR2[1]~dnorm(0,0.39)


Reff_mu~dbeta(0.9,1.8)
Reff_eta~dunif(0,0.5) 
Ra<-Reff_mu/Reff_eta
Rb<-(1-Reff_mu)/Reff_eta   

tau_MpsW <- 1/log(((CV_MpsW)* (CV_MpsW)) + 1)
CV_MpsW ~dbeta(1,1)

for(i in 1:(m+proj_years)){
#for(s in 1:stocks){
#  MpsW[i,s]

  
  MpsW[i]~dlnorm(mu_MpsW[i],tau_MpsW)     ## Post-smolt M for wild salmon < post-smolt M for reared salmon
  #survMpsW[i]<-exp(-MpsW[i])
  
  RMps[i]~dbeta(Ra,Rb) 
  ReffectMps[i] <- (RMps[i] * 1.5)+1                 # hatchery-reared effect between 1 and 2.5
  
  MpsR[i] <- MpsW[i] * ReffectMps[i]
  #survMpsR[i]<-exp(-MpsR[i])
  
  surv[i,1,1,1]<-exp(-MpsW[i]/Tstep) 		          #survmortW
  


"