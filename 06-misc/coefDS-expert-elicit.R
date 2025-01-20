source("../run-this-first-wgbast.R")


M1<-"model{

x~dlnorm(M,T)

M<-log(mu)-0.5/T
T<-1/log(cv*cv+1)

#mu<-1.05
#cv<-0.15
mu<-1.0375
cv<-0.023

y~dlnorm(0.0365,1891)



}"


# data=list(
#   N_SE=length(dfSE$yy),yy=dfSE$yy, stockSE=dfSE$stock
# ) 

var_names=c("x", "y")
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M1,
                 monitor= var_names,#data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
chains<-as.mcmc(run0)
summary(chains, quantiles=c(0.05, 0.5, 0.95))


M2<-"model{
  
  #coefDS[i]~dlnorm(M_coefDS,T_coefDS)
  x~dlnorm(M_coefDS,T_coefDS)
  M_coefDS<-log(mu_coefDS)-0.5/T_coefDS
  T_coefDS<-1/log(cv_coefDS*cv_coefDS+1)
  mu_coefDS~dlnorm(0.0365,20000)
  cv_coefDS~dunif(0.01,0.03)
  

#y~dlnorm(0.0365,1891)
  
  
  
  
}"

var_names=c("x", "y", "mu_coefDS", "cv_coefDS")
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M2,
                 monitor= var_names,#data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
chains<-as.mcmc(run0)
summary(chains, quantiles=c(0.05, 0.5, 0.95))

################################################################################

#   Ekspertti 1
#   mediaani 1.5 90%PI 1.1-2.2

#   Ekspertti 2
#   mediaani 1.0375 90%PI 1-1.075

#   Ekspertti 3
#   mediaani 1.025 90%PI 1-1.05



#jos kaikki yhdisteään, tulevan lognormaalin mu =1.205355 ja cv = 0.2229662
#jos vain juha ja erkki mu = 1.031237 ja cv = 0.01970493

M3<-"model{

#E1~dgamma(25,16.5)
E1~dlnorm(log(1.5)-0.5/TE1, TE1)
TE1<-1/log(cvE1*cvE1+1)
cvE1<-0.2

E2~dnorm(1.0375, 1/(0.02279838*0.02279838))
E3~dnorm(1.025, 1/(0.01519892*0.01519892))


X1~dlnorm(log(muX1)-0.5/TX1,TX1)
muX1<-1.205355
TX1<-1/log(0.2229662*0.2229662+1)

X2~dlnorm(log(muX2)-0.5/TX2,TX2)
muX2<-1.031237
TX2<-1/log(0.01970493*0.01970493+1)

  
  #coefDS[i]~dlnorm(M_coefDS,T_coefDS)
  
  X<-x+1
  x~dlnorm(M_coefDS,T_coefDS)
  M_coefDS<-log(mu_coefDS)-0.5/T_coefDS
  T_coefDS<-1/log(cv_coefDS*cv_coefDS+1)
  
  #mu_coefDS~dlnorm(log(1.205355)-0.5/Tmu,Tmu)
  #Tmu<-1/log(0.01970493*0.01970493+1)
 mu_coefDS~dlnorm(log(1.205355-1)-0.5/Tmu,Tmu)
  Tmu<-1/log(0.4*0.4+1)
  cv_coefDS~dunif(0.01,5)
  
  
  # mu_coefDS~dlnorm(log(0.24)-0.5/Tmu,Tmu)
  # Tmu<-1/log(3.26*3.26+1)
  # cv_coefDS~dunif(0.01,1)
  # 
  
  tmp<-mu_coefDS+1
  
  
  
  

}"

var_names=c(#"E1", "E2", "E3", 
            #"X1", 
            #"X2",
  "tmp",
  "X",
            "x", 
            "mu_coefDS", "cv_coefDS")
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M3,
                 monitor= var_names,#data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
chains<-as.mcmc(run0)
summary(run0)
summary(chains, quantiles=c(0.05, 0.5, 0.95))
plot(run0)

#   Ekspertti 1
#   mediaani 1.5 90%PI 1.1-2.2

#   Ekspertti 2
#   mediaani 1.0375 90%PI 1-1.075

#   Ekspertti 3
#   mediaani 1.025 90%PI 1-1.05

#jos kaikki yhdisteään, tulevan lognormaalin mu =1.205355 ja cv = 0.2229662
#jos vain juha ja erkki mu = 1.031237 ja cv = 0.01970493





################################################################################
# Koska hierarkkiset priorit vaikuttavat aiheuttavan numeerisia ongelmia 
# (kEdc kaataa mallin extend-jagsilla alkuarvoistaessa)
# tehdään kompromissina yksi coef-priori jolla mennään v 25 assessmentti
# Kokeillaan hierarkkisten priorien palauttamista kun ollaan siirrytty Nimbleen

# Tavoite tyyliin (X yo hierarkkisen perusteella)
#jos kaikki yhdisteään, tulevan lognormaalin mu =1.205355 ja cv = 0.2229662

M3<-"model{

  X<-x+1
  x~dlnorm(-2.029014,1.1211) # CoefDS
  # x~dlnorm(log(1.205355-1)-0.5/Tmu,Tmu)
  # Tmu<-1/log(1.2*1.2+1)

}"

var_names=c(#"E1", "E2", "E3", 
  "X",
  "x")
#inits=list(p=array(0.01,dim=c(1754,2)))

run0 <- run.jags(M3,
                 monitor= var_names,#data=data, #inits = inits,
                 n.chains = 2, method = 'parallel', thin=10, burnin =10000,
                 modules = "mix",keep.jags.files=F,sample =1000, adapt = 1000,
                 progress.bar=TRUE)
chains<-as.mcmc(run0)
summary(run0)
summary(chains, quantiles=c(0.05, 0.5, 0.95))


plot(run0)

