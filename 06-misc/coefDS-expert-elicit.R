source("../run-this-first-wgbast.R")

#jos kaikki yhdisteään, tulevan lognormaalin mu =1.205355 ja cv = 0.2229662
#jos vain juha ja erkki mu = 1.031237 ja cv = 0.01970493

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

