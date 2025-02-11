
#river migration survival by stock/AU - currently wild stocks only (check) 
#Only Ume/Vindel currently has reduced spawner survival in some years
#Note that if extra mortality is added for wild stocks 1-4 in years with tagging data, 
#TxWs need to move into a loop where s is stock index, not AU - do something about this - use average surv_migr over stocks in each AU?
#In this version river is survrR or survrW and surv are re-numbered

#Now HrW instead of HrR for reared spawners (Torne and Simo) in population dynamics (tagged and untagged)
#Parr added to NrRsp not NrW (wild)

#flexible indexing not complete!!

# Packages are called in run-this-first.r (calls packages.r) 
#library(R2jags)
# library(runjags)
# library(rjags) 

# load.module("mix")
# load.module("glm")
source("../run-this-first-wgbast.R")


assessment_year<-2024
years<-length(seq(1987:assessment_year))

proj_years<-0
maxage<-6
rstocks<-2 #Lule, Dal
stock_indices<-c(1:17)
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

#source("/home/henni/WGBAST/03-histmodel/paths_FLHM.R") #AMD
#source("03-histmodel/paths_FLHM.R") #windows
#setwd("C:/WGBAST15/WGBAST_2024")
#source("flhm/paths_FLHM.R") #windows

#modelName<-"FLHM_JAGS_2024_orig" # Same model structure as in 2023 assessment
#modelName<-"FLHM_JAGS_2024_CR"   #variant with trolling C&R 
#modelName<-"FLHM_JAGS_2024_CR_coefDSEE"   #variant with a prior for CoefDS
modelName<-"FLHM_JAGS_2024_CR_river_catch1_coefDSEE" 

CR<-ifelse(grepl("CR",modelName),T,F) #boolean to read correct version of catch data file
SimoMSW<-ifelse(grepl("coefDShierEE",modelName),T,F) #boolean to read correct version of catch data file

#source(paste0(PathFunctions,"plotfunctions.r")) # Called in run-this-first.r
source(paste0(PathModel_FLHM,"make_JAGS_data_",assessment_year,".R"))



source(paste0(PathModel_FLHM,modelName,".R"))
#runName<-paste0(modelName,"_",startyear,"-",endyear)


runName<-modelName
print(runName)

datalist<-list(
  Tstep=12,AUS=AUS,m=years,proj_years=proj_years,stocks=stocks,rstocks=rstocks,AUR=c(2,3),
  AU=AU[stock_indices],SR_unit=SR_unit[stock_indices],M_R0=M_K[stock_indices],tau_R0=tau_K[stock_indices],
  M74_alpha=as.matrix(M74_alpha[,stock_indices]),M74_beta=as.matrix(M74_beta[,stock_indices]),
  mu_Parr=as.matrix(mu_Parr[,stock_indices]),
  tau_Parr=as.matrix(tau_Parr[,stock_indices]),
  Smolt_Rsp=as.matrix(Smolt_Rsp[,stock_indices]),
  mu_SmoltW=as.matrix(mu_SmoltW[,stock_indices]),
  tau_SmoltW=as.matrix(tau_SmoltW[,stock_indices]),
  Er=Er,El=El,Edo=Edo,Edc=Edc,Ectn=Ectn,Ecgn=Ecgn,sealMort=sealMort,
  rel_W=rel_W1,rel_R=rel_R1,rel_Rsp=rel_Rsp1,reportcAdj=reportcAdj,
  muTemp=muTemp,tauTemp=tauTemp,mon_Ec=c(6,2,2,2,2,2),
  mon_Edc=rep(1,times=6),
  mon_E=array(c(1,rep(2,times=5),rep(2,times=6),rep(8,times=6),rep(1,times=6),1,rep(2,times=5),rep(1,times=6)),dim=c(6,6)), 
  avail_r=which(stock_indices %in% avail_r),avail_dc=which(stock_indices %in% avail_dc),
  ureport_r=unrep$coef_r,ureport_c=unrep$coef_c,ureport_o=unrep$coef_o,
  PropCR=cat_ratio[,1],PropCW=cat_ratio[,2],
  SmoltWobs=as.matrix(exp(mu_SmoltW[,stock_indices])),
  SmoltRdata=SmoltRdata,
  mu_sp_alpha=mu_sp_alpha[stock_indices],mu_sp_beta=mu_sp_beta[stock_indices],
  CV_sp_alpha=CV_sp_alpha[stock_indices],CV_sp_beta=CV_sp_beta[stock_indices],
  cr_ObsW=cr_ObsW,cl_ObsW=cl_ObsW,cdo_ObsW=cdo_ObsW,cdc_ObsW=cdc_ObsW,cc_ObsW=cc_ObsW,
  cr_ObsRsp=cr_ObsRsp,cr_ObsR=cr_ObsR,cc_ObsR=cc_ObsR,cl_ObsR=cl_ObsR,cdo_ObsR=cdo_ObsR,
  cdc_ObsR=cdc_ObsR,#ncr_ObsTot=cat_r,
  ncc_ObsTot=cat_c,nco_ObsTot=cat_o,
  ncrW_ObsTot=ncrW_ObsTot+0.001,ncrR_ObsTot=cat_r,
  
  nct_ObsTot=cat_t, #trolling catch
  sd_wr=sd_wr,
  #WpropObs=WpropObs,
  log_WpropObs=log(WpropObs), log_RpropObs=log(1-WpropObs),
  sp_count=as.matrix(sp_count[,stock_indices]),
  N_sp_count=as.matrix(N_sp_count[,stock_indices]),
  MSWprop=as.matrix(MSWprop[,stock_indices]),
  RProp=RProp,TrapTot=TrapTot,CatchR=CatchR,
  NLuleRel=NLuleRel,NLuleRec=NLuleRec,yLule=yLule,
  WGrilse=as.matrix(WGrilse[,stock_indices]),
  WMSW=as.matrix(WMSW[,stock_indices]),
  Grilse_all=as.matrix(Grilse_all[,stock_indices]),
  MSW_all=as.matrix(MSW_all[,stock_indices]),
  alpha_ladder=as.matrix(alpha_ladder[,stock_indices]),
  beta_ladder=as.matrix(beta_ladder[,stock_indices]),
  ladder_count=as.matrix(ladder_count[,stock_indices]),
  #au1_stocks=au1_stocks,au2_stocks=au2_stocks,au3_stocks=au3_stocks,
  #au4_stocks=au4_stocks,
  prop_fem=prop_fem,alpha_migr=alpha_migr,
  beta_migr=beta_migr,smolt_year=smolt_year,e_delay=e_delay,
  rivHR=as.matrix(rivHR[,stock_indices]),
  alpha_rel=alpha_rel,beta_rel=beta_rel,
  au1_stocks=au1_stocks,au2_stocks=au2_stocks,au3_stocks=au3_stocks,au4_stocks=au4_stocks)  





alpha_detect<-c(18,18,rep(10,times=15))
beta_detect<-c(2,2,rep(10,times=15))

iql<-rep(0.001,years+proj_years+4)
iqd<-array(c(rep(NA,(years+proj_years+5)),rep(0.01,(years+proj_years+5)*2)), dim=c((years+proj_years+5),3))

iqcW<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),1),
            dim=c(years+proj_years+3,4,1))
iqcR<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),3),
            dim=c(years+proj_years+3,4,3))

inits.fn<-function() {
  list(fec=c(exp(8),exp(9),exp(9.5),exp(9.5),exp(9.7)),
       K=rlnorm(length(stock_indices),M_K[stock_indices]*1.1,0.50),
       p.detect=array(rbeta((years+proj_years+5)*length(stock_indices),rep(alpha_detect[stock_indices],each=years+proj_years+5),rep(beta_detect[stock_indices],each=years+proj_years+5)),dim=c(years+proj_years+5,stocks)),
       MpsW=rlnorm(years+proj_years,-1.2,0.3),
       logit_qlW=log(iql/(1-iql)),     
       logit_qdW=log(iqd/(1-iqd)),     
       MW=rlnorm(1,-2.3,0.15),MR=rlnorm(1,-2,0.15),
       early_MpsW=rlnorm(1,0.23,0.15),cv_SR=rlnorm(1,-1.61,0.10),
       CV_MpsW=rbeta(1,30,70),Reff_mu=rbeta(1,15,35),Reff_eta=rbeta(1,10,10)*0.5,SCRW=rbeta(1,3,17),
       CV_HrW=runif(1,0.01,0.99),logit_deltaHRW=rnorm(1,0.85,0.5),mu_HrW=rnorm(stocks,-1.3862944 ,0.5), 
       HrW_autoc=runif(stocks, 0.1,0.99)) 																									 
  
}




parnames<-c("coefDS", #"mu_coefDS", "cv_coefDS",
            "tau_MpsW","MpsW","MpsR","mu_MpsW","NspWtot","SmoltR",
            "SmoltW","EPR","EPR_M74","alphaSR","betaSR","tau_SR","z","K","R0",
            "nco_ObsTotX","ncr_ObsTotX","ncc_ObsTotX",
            "sp_countX","LW","LR","M74",
            "Eggstot","Eggstot_M74","IBSFC","MW","MR","fec","NccW","NccR","probMSW",
            "NladderW_tot", "Usmolt","pTrap","NrRtot","delta","bL","tauL",
            "LReffect","cL","mucL","taucL","Wprop","tauCR","tauCC","tauCO",
            "p.ladder","p.detect","Ra","Rb",
            "HrW","HrR","HdoW","HdoR","HdcW","HdcR","HlW","HlR","HcW","HcR",
            
            "HtW", "HtR","phi_tr","mean_trW","mean_trR", "nct_ObsTotX", "sd_tr", # Params of trolling
            "phi_ql", "mean_qlW","mean_qlR","sd_ql", # AR(1) params of ql
            "phi_qd", "mean_qdW","mean_qdR","sd_qd", "eff_qdW", "eff_qdR", #AR(1) params of qd
            "mean_qctnW", "mean_qctnR", "phi_qctn", "sd_qctn", "eff_qctn",
            "mean_qcgnW", "mean_qcgnR", "phi_qcgn", "sd_qcgn", "eff_qcgn",
            
            "NrAll_tot","NspWtot","Tretain","muCC",
            "muCO","muCR","nc_oll_Tot","nc_odn_Tot",
            "qcgnR","qcgnW","qctnR","qctnW","qdR","qdW","qrR","qrW","qlR","qlW",
            "reportc","reportd","reportl","reportrR","reportrW","rrR",
            "surv_migr","p.mort","p.rel","nctW_rel","nctW_Tot")



initsall<-list(inits.fn(),inits.fn())


print(paste0(runName,"_data", assessment_year))

##Quick test
##cat(WGBAST_model,file="wgbast_model.txt")
##jm<-jags.model("wgbast_model.txt",n.adapt=100,
##data=datalist,inits=inits.fn())
##
##chains<-coda.samples(jm,variable.names=parnames,n.iter=100,thin=10)
##v<-as.matrix(chains)

# Burn-in
t01<-Sys.time();print(t01)
run0 <- run.jags(WGBAST_model, monitor= parnames,
                 data=datalist,inits = initsall,
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10, adapt = 10000,
                 keep.jags.files=paste0(runName, assessment_year),
                 progress.bar=TRUE, jags.refresh=100)
t02<-Sys.time();print(t02)
print("run0 done");print(difftime(t02,t01))
print("--------------------------------------------------")

t1<-Sys.time();print(t1)
run1 <- extend.jags(run0, combine=F, sample=1000, thin=100, keep.jags.files=T)
t2<-Sys.time();print(t2)
print("run1 done"); print(difftime(t2,t1))
print("--------------------------------------------------")
run<-run1
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData"))


t3<-Sys.time();print(t3)
run2 <- extend.jags(run1, combine=T, sample=1000, thin=100, keep.jags.files=T)
t4<-Sys.time();print(t4)
print("run2 done");print(difftime(t4,t3))
print("--------------------------------------------------")
run<-run2
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData"))

t5<-Sys.time();print(t5)
run3 <- extend.jags(run2, combine=T, sample=1000, thin=100, keep.jags.files=T)
t6<-Sys.time();print(t6)
print("run3 done");print(difftime(t6,t5))
print("--------------------------------------------------")

run<-run3
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData"))

t7<-Sys.time();print(t7)
run4 <- extend.jags(run3, combine=T, sample=1000, thin=100, keep.jags.files=T)
t8<-Sys.time();print(t8)
print("run4 done");print(difftime(t8,t7))
print("--------------------------------------------------")

run<-run4
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData"))

t9<-Sys.time();print(t9)
run5 <- extend.jags(run4, combine=T, sample=1000, thin=100, keep.jags.files=T)
t10<-Sys.time();print(t10)
print("run5 done");print(difftime(t9,t10))
print("--------------------------------------------------")

run<-run5
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData"))

t11<-Sys.time();print(t11)
run6 <- extend.jags(run5, combine=T, sample=1000, thin=100, keep.jags.files=T)
t12<-Sys.time();print(t12)
print("run6 done");print(difftime(t11,t12))
print("--------------------------------------------------")

run<-run6
save(run, file=paste0(PathOut,runName, "_data",assessment_year,".RData")) 			   
