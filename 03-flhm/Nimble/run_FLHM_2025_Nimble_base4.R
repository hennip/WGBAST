
#river migration survival by stock/AU - currently wild stocks only (check) 
#Only Ume/Vindel currently has reduced spawner survival in some years
#Note that if extra mortality is added for wild stocks 1-4 in years with tagging data, 
#TxWs need to move into a loop where s is stock index, not AU - do something about this - use average surv_migr over stocks in each AU?
#In this version river is survrR or survrW and surv are re-numbered
#Now HrW instead of HrR for reared spawners (Torne and Simo) in population dynamics (tagged and untagged)
#Parr added to NrRsp not NrW (wild)

#flexible indexing not complete!!
# library(abind)
# library(coda)
# library(nimble)
# library(nimbleHMC)
# library(parallel)
# library(reshape2)
# library(extraDistr)
source("../run-this-first-wgbast.R")

parallel<-T
CR<-T


assessment_year<-2025
years<-length(seq(1987:assessment_year))
proj_years<-0
maxage<-6
rstocks<-2 #Lule, Dal
stock_indices<-c(1:17) #17
#NB if run with Torne and Simo they should have positions 1 and 2 because of exceptions for these rivers
stocks<-length(stock_indices)
allstocks<-17 #number of stocks in data files
AUS<-4
trolling<-1

##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixalven"
#4 "Ranealven"
#5 "Pitealven"
#6 "Abyalven"
#7 "Byskealven"
#8 "Ricklean"
#9 "Savaran"
#10 "Vindelalven"
#11 "Orealven"
#12 "Logdealven"
#13 "Ljungan"
#14 "Morrumsan"
#15 "Eman"
#16 "Kagealven"
#17 "Testeboan"


#setwd("C:/WGBAST15/WGBAST_2025")
#source("flhm/paths_FLHM.R") #windows


modelName<-"FLHM_Nimble_2025_CR_base4"      

CR<-T
source("00-basics/plotfunctions.r")
source(paste0(PathModel_FLHM,"make_JAGS_data_",assessment_year,".R"))

source("03-flhm/Nimble/make_inits_Nimble.R")

source(paste0(PathModel_Nimble,modelName,".R"))

if(parallel==T){
  parr_code<-paste0("Parallel_code_Nimble_",assessment_year,".R")
  modelfile<-paste0(PathModel_Nimble,modelName,".R")
  source(paste0(PathModel_Nimble,parr_code))
}


WGBASTData<-list(SmoltWobs=as.matrix(exp(mu_SmoltW[,stock_indices])),cr_ObsW=cr_ObsW,cl_ObsW=cl_ObsW,cdo_ObsW=cdo_ObsW,cdc_ObsW=cdc_ObsW,cc_ObsW=cc_ObsW,cr_ObsRsp=cr_ObsRsp,cr_ObsR=cr_ObsR,cc_ObsR=cc_ObsR,cl_ObsR=cl_ObsR,cdo_ObsR=cdo_ObsR,cdc_ObsR=cdc_ObsR,ncr_ObsTot=cat_r,ncc_ObsTot=cat_c,nco_ObsTot=cat_o,nct_ObsTot=cat_t,log_WpropObs=log(WpropObs),log_RpropObs=log(1-WpropObs),sp_count=as.matrix(sp_count[,stock_indices]),TrapTot=TrapTot,NLuleRec=NLuleRec,WGrilse=as.matrix(WGrilse[,stock_indices]),WMSW=as.matrix(WMSW[,stock_indices]),ladder_count=as.matrix(ladder_count[,stock_indices]),MSWprop=as.matrix(MSWprop[,stock_indices]))
                           

WGBASTConsts<-list(Tstep=12,AUS=AUS,m=years,stocks=stocks,rstocks=rstocks,AUR=c(2,3),AU=AU[stock_indices],SR_unit=SR_unit[stock_indices],
                   M_R0=M_K[stock_indices],tau_R0=tau_K[stock_indices],mu_Parr=as.matrix(mu_Parr[,stock_indices]),tau_Parr=as.matrix(tau_Parr[,stock_indices]),
                   Smolt_Rsp=as.matrix(Smolt_Rsp[,stock_indices]),mu_SmoltW=as.matrix(mu_SmoltW[,stock_indices]),tau_SmoltW=as.matrix(tau_SmoltW[,stock_indices]),
                   Er=Er,El=El,Edo=Edo,Edc=Edc,Ectn=Ectn,Ecgn=Ecgn,sealMort=sealMort,rel_W=rel_W1,rel_R=rel_R1,rel_Rsp=rel_Rsp1,reportcAdj=reportcAdj,
                   muTemp=muTemp,tauTemp=tauTemp,mon_Ec=c(6,2,2,2,2,2),mon_Edc=rep(1,times=6),
                   mon_E=array(c(1,rep(2,times=5),rep(2,times=6),rep(8,times=6),rep(1,times=6),1,rep(2,times=5),rep(1,times=6)),dim=c(6,6)),
                  ureport_r=unrep$coef_r,ureport_c=unrep$coef_c,ureport_o=unrep$coef_o,PropCR=cat_ratio[,1],PropCW=cat_ratio[,2],mu_sp_alpha=mu_sp_alpha[stock_indices],
                   mu_sp_beta=mu_sp_beta[stock_indices],CV_sp_alpha=CV_sp_alpha[stock_indices],CV_sp_beta=CV_sp_beta[stock_indices],SmoltRdata=SmoltRdata,sd_wr=sd_wr,N_sp_count=as.matrix(N_sp_count[,stock_indices]),
                   RProp=RProp,CatchR=CatchR,NLuleRel=NLuleRel,yLule=yLule,Grilse_all=as.matrix(Grilse_all[,stock_indices]),MSW_all=as.matrix(MSW_all[,stock_indices]),alpha_ladder=as.matrix(alpha_ladder[,stock_indices]),
                   beta_ladder=as.matrix(beta_ladder[,stock_indices]),au1_stocks=au1_stocks,au2_stocks=au2_stocks,au3_stocks=au3_stocks,au4_stocks=au4_stocks,mu_mu_a=c(-2.784,-2.784),tau_mu_a=c(2.388,2.388),
                   mu_sd_a=c(-0.2653,-0.2653),tau_sd_a=c(3.1529,3.1529),smolt_year=smolt_year,e_delay=e_delay,rivHR=as.matrix(rivHR[,stock_indices]), maxvar = 0.5 / 12,prop_fem=prop_fem,alpha_migr=alpha_migr,beta_migr=beta_migr,M74_mu=as.matrix(M74_mu[,stock_indices]),M74_tau=as.matrix(M74_tau[,stock_indices]),alpha_rel=alpha_rel,beta_rel=beta_rel,mu_mu_sp=mu_mu_sp[stock_indices],tau_mu_sp=tau_mu_sp[stock_indices],
mu_CV_sp=mu_CV_sp[stock_indices],tau_CV_sp=tau_CV_sp[stock_indices])
                   

WGBASTInits<-make.inits()

rm(d, chains)

print(paste0(modelName,"_data", assessment_year))

  parnames<-c("coefDS","tau_MpsW","MpsW","MpsR","mu_MpsW","NspWtot","SmoltR",
            "SmoltW","EPR","EPR_M74","alphaSR","betaSR","tau_SR","z","K","R0",
            "nco_ObsTotX","ncr_ObsTotX","ncc_ObsTotX",
            "sp_countX","LW","LR","M74",
            "Eggstot","Eggstot_M74","MW","MR","fec","NccW","NccR","probMSW",
            "NladderW_tot", "Usmolt","pTrap","NrRtot","delta","bL","tauL",
            "LReffect","cL","mucL","taucL","Wprop","tauCR","tauCC","tauCO","tauCT",
            "p.ladder","p.detect",
            "HrW","HrR","HdoW","HdoR","HdcW","HdcR","HlW","HlR","HcW","HcR",
            
            "HtW", "HtR","phi_tr","mean_trW", "nct_ObsTotX", "sd_tr", # Params of trolling
            "phi_ql", "mean_qlW","sd_ql", # AR(1) params of ql
            "phi_qd", "mean_qdW","sd_qd", #AR(1) params of qd
            "phi_tr", "mean_trW","sd_tr", #AR(1) params of trolling
            "NrAll_tot","NspWtot","Tretain","muCC",
            "muCO","muCR","nc_oll_Tot","nc_odn_Tot",
            "qcgnR","qcgnW","qctnR","qctnW","qdR","qdW","qrR","qrW","qlR","qlW",
            "reportc","reportd","reportl","reportrR","reportrW","rrR",
            "surv_migr","logit_qlW","early_MpsW","mu_MpsW","tau_MpsW","mu_qlW","tau_ql","nco_W",
            "nco_R","p.mort","p.rel","nctW_rel","nctW_Tot","CV_ladder",#"mu_CVSR","tau_CVSR",
            "mu_spawn","CV_spawn")    
######################################################################################3


##parallel
 if(parallel==T){
 ptm <- proc.time()
 this_cluster <- makeCluster(4,outfile="")
 chain_output <- parLapply(cl = this_cluster, X = 1:2, 
                           fun = run_wgbastCode, 
                           wdata = WGBASTData,wconsts=WGBASTConsts,winits=WGBASTInits,
                           wmonitor=parnames,mfile=modelfile)
 
 ## It's good practice to close the cluster when you're done with it.
 stopCluster(this_cluster)
 
 proc.time() - ptm
 }else if(parallel==F){
  
  #nimbleOptions(enableDerivs = TRUE)
  wgbastModel<-nimbleModel(code = WGBASTCode, constants = WGBASTConsts,
  data = WGBASTData, inits=WGBASTInits,calculate=FALSE) # ,buildDerivs = TRUE
  #wgbastModel$calculate()
  
  stnodes <- wgbastModel$getNodeNames(stochOnly = TRUE, includeData = FALSE)
  allvars<-wgbastModel$getVarNames(nodes = stnodes)
  mvars<-allvars[!(grepl("lifted",allvars))]  
  
  #set.seed(794648) 
  wgbastModel$simulate(wgbastModel$getDependencies(stnodes))
  wgbastModel$calculate()
  
  for(i in 1:length(mvars)){
  print(paste0(mvars[i]," ",wgbastModel$calculate(mvars[i]) ))
  }
  


    
  wgbastConf <- configureMCMC(wgbastModel, print=TRUE, useConjugacy = FALSE, monitors = parnames) 
  wMCMC <- buildMCMC(wgbastConf) # uncompiled R code 

  
  Cwgbast <- compileNimble(wgbastModel,get_mu_tau,showCompilerOutput = TRUE)
 
  CwgbastMCMC <- compileNimble(wMCMC, project = wgbastModel)
  

  
ptm<-proc.time()
CwgbastMCMC$run(1000, time = TRUE, nburnin=500, thin=1)  #2000 took 2800s
print(proc.time()-ptm)   #2043 without slice sampler MW MR
print("done") 
}

save(chain_output,file="Nimble_base4_2025.RData")

v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)

chains<-mcmc.list(list(v1,v2)) 

samples <- as.matrix(CwgbastMCMC$mvSamples)
chains<-as.mcmc(samples)
traceplot(chains[,"MW"])

