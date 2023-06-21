
# Save M74 output (coda files) from Full life history model and 
# read those in 

#summary(chains[ ,regexpr("M74",varnames(chains))>0])$statistics

source("00-basics/run-this-first.R")
source("00-basics/packages.R")

load(file=paste0(PathOutput_FLHM,"FLHM_2023_rivHR_data2023_thin350.RData"))
chains<-as.mcmc(run) #pool 2 chains together
nchains<-1


years<-c(1992:2022)
Nyears<-length(years)
Nstocks<-17

mean_M74<-matrix(nrow=Nyears,ncol=Nstocks)
sd_M74<-matrix(nrow=Nyears,ncol=Nstocks)
for(i in 1:Nyears){  
  for(r in 1:Nstocks){
    if(nchains==1){
      M74<-chains[,str_c("M74[",i+5,",",r,"]")]
    }#length(M74)  
    if(nchains==2){
      M74<-chains[,str_c("M74[",i+5,",",r,"]")][[1]]
    }#length(M74)  
    logit_M74<-log(M74/(1-M74))
    mean_M74[i,r]<-mean(logit_M74)
    sd_M74[i,r]<-sd(logit_M74)
  }
}


M1<-"
model{
  means~dbeta(2,2)
  mu<-log(means/(1-means))	
  c<-mu*(1-w)
  sigma2~dunif(0,10)
  sigma2e<-(1-w*w)*sigma2
  tau<-1/sigma2e
  w~dunif(0,1)
  
  for(r in 1:Nstocks){
    x[1,r]~dnorm(0,0.001)

    for(i in 2:(Nyears+9)){
      x[i,r]<-x[i-1,r]*w+e[i,r]
      e[i,r]~dnorm(c,tau)
      logit(S[i,r])<-x[i,r]
    }

    for(i in 1:Nyears){ #1992->  :history
      M74_obs[i,r]~dnorm(x[i,r],T_obs[i,r])
      T_obs[i,r]<-1/pow(sd_obs[i,r],2)
    }
  }
	for(i in 1:(Nyears+9)){
		meanx[i]<-mean(x[i,1:Nstocks])
  }
  pred_averageMean[1]<-mean(meanx[1:8]) # 1992:1999
  pred_averageMean[2]<-mean(meanx[1:31]) # 1992:2022

}
"
Mname<-"model_M74.txt"
cat(M1,file=str_c("04-scenarios/scen-prep/", Mname))

data<-list(
  Nyears=Nyears,
  Nstocks=Nstocks,
  M74_obs=mean_M74,
  sd_obs=sd_M74
)


var_names<-c(
  "pred_averageMean",
  "mu",
  "sigma2","w"
)

run<-run.jags(str_c("04-scenarios/scen-prep/", Mname),monitor=var_names, data=data, 
              n.chains=2,sample=20000, burnin=10000,
              method='parallel', modules = "mix", keep.jags.files = F)
run2<-extend.jags(run, combine=T, sample=20000)

plot(run2)

summary(run2)

#                     Lower95     Median   Upper95       Mean         SD Mode        MCerr
# pred_averageMean[1] -0.641232 -0.5475315 -0.447816 -0.5475646 0.04930765   NA 0.0006929293
# pred_averageMean[2] -2.240520 -2.1384050 -2.042620 -2.1392529 0.05056175   NA 0.0010244040
# mu                  -3.373850 -3.0422200 -2.743120 -3.0498690 0.16165157   NA 0.0039732615
# sigma2               0.018614  0.2558350  0.523035  0.2718181 0.13575977   NA 0.0064516388
# w                    0.820735  0.8596395  0.893577  0.8584709 0.01853542   NA 0.0007143689
# MC%ofSD SSeff      AC.10      psrf
# pred_averageMean[1]     1.4  5063 0.07484829 1.0000275
# pred_averageMean[2]     2.0  2436 0.20592795 0.9999789
# mu                      2.5  1655 0.46071714 1.0009299
# sigma2                  4.8   443 0.88526879 1.0016934
# w                       3.9   673 0.84244275 1.0022568

# In inputs-file: (means)
## Stable mean on logit scale
#  muM74<- -2.139 

## Marginal variance  on logit scale
#  sigma2M74<-0.272

## AutoC coefficient  on logit scale
#  wM74<- 0.858

