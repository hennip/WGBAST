
# AR(1) model for Mps

# Load input
source("04-scenarios/scen-prep/Mps_cor.r")

M1<-"
model{
	
  # input from the model
  # Mps_obs: Medians of the instantaneous mortalities 
  # cv_obs: Cv's of the instantaneous mortalities
  for(i in 1:m){
    Mps_obs[i]~dlnorm(logM[i],T_obs[i])
    T_obs[i]<-1/log(cv_obs[i]*cv_obs[i]+1)
  }
  
  # 1st time step
  x[1]~dnorm(0,0.1)
  M[1]<--log(S[1])
  logit(S[1])<-x[1]
  logM[1]<-log(M[1])
  
  # next time steps
  for(i in 2:100){
    x[i]<-x[i-1]*w+e[i] # AR(1)
    e[i]~dnorm(c,tau)
    M[i]<--log(S[i])
    logit(S[i])<-x[i]
    logM[i]<-log(M[i])
  }
  
  means~dbeta(2,2)	# mean
  mu<-log(means/(1-means)) # mean on logit-scale
  c<-mu*(1-w)
  sigma2~dunif(0,1)
  sigma2e<-(1-w*w)*sigma2 # variance of the error term
  tau<-1/sigma2e
  w~dunif(0,1) # autocorrelation coefficient
  
  pred_averageMean<-mean(x[32:35]) #2018-2021, 4 year average without last year which is more uncertain
  #pred_averageLow<-x[19] # 19= 2005
  #pred_averageLast<-x[25] # 25= 2011
  #pred_average<-mu

}"
Mname<-"model_Mps.txt"
cat(M1,file=str_c("04-scenarios/scen-prep/", Mname))

data<-list(
  m=36, # until 2022
  Mps_obs=MpsMed,
  cv_obs=CV
)


var_names<-c(
  "pred_averageMean",
  "sigma2","w"
)

run<-run.jags(str_c("04-scenarios/scen-prep/", Mname),monitor=var_names, data=data, 
              n.chains=2,sample=50000, burnin=5000,
              method='parallel', modules = "mix", keep.jags.files = "test")

plot(run)

summary(run)
#                   Lower95     Median   Upper95       Mean        SD Mode        MCerr MC%ofSD
# pred_averageMean -2.124480 -1.8487300 -1.578670 -1.8506799 0.1386260   NA 0.0009802338     0.7
# sigma2            0.149894  0.4929745  0.977323  0.5198728 0.2438262   NA 0.0065509660     2.7
# w                 0.850742  0.9467970  0.990956  0.9357366 0.0420903   NA 0.0021132214     5.0
# SSeff        AC.10     psrf
# pred_averageMean 20000 -0.003144212 1.000212
# sigma2            1385  0.550420654 1.000524
# w                  397  0.925315357 1.005598

# In Input-file:
# mu<--1.849    # median survival 2018-2021, 14% (S=exp(mu)/(1+exp(mu)))
# w<-0.94      
# sigma2<-0.52


