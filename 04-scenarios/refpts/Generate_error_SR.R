
assess_year<-2023

Nstocks<-17
nYears<-329

load(file=paste0(PathSim,"FLHM_2023_rivHR_data2023_thin350.RData"))
#chains<-as.mcmc.list(run)
chains<-run$mcmc
d<-as.matrix(chains)
keep.sample<-seq(4,4000,by=4)
d<-d[keep.sample,]
nsim<-dim(d)[1]

tauSR<-d[,grep("tau_SR",colnames(d))]

errorSR<-array(exp(rnorm(nsim*nYears*Nstocks, mean= -0.5/tauSR,sd = sqrt(1/tauSR))),dim=c(nsim,nYears,Nstocks))    #mean is 0       
save(errorSR,file=paste0("SR_devs_1000_",assess_year,".RData"))


