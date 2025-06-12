
# Data list
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
  ncr_ObsTot=cat_r,
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
  mu_mu_sp=mu_mu_sp[stock_indices],tau_mu_sp=tau_mu_sp[stock_indices],
  mu_CV_sp=mu_CV_sp[stock_indices],tau_CV_sp=tau_CV_sp[stock_indices]
#  au1_stocks=au1_stocks,au2_stocks=au2_stocks,au3_stocks=au3_stocks,au4_stocks=au4_stocks
  )  


# Initial values
alpha_detect<-c(18,18,rep(10,times=15))
beta_detect<-c(2,2,rep(10,times=15))

iql<-rep(0.001,years+proj_years+4)
iqd<-array(c(rep(NA,(years+proj_years+5)),rep(0.01,(years+proj_years+5)*2)), dim=c((years+proj_years+5),3))

iqcW<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),1),
            dim=c(years+proj_years+3,4,1))
iqcR<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),3),
            dim=c(years+proj_years+3,4,3))

logit_mu_spawn<-rnorm(stocks,mu_mu_sp,sqrt(1/(tau_mu_sp*5)))

logit_CV_spawn<-rnorm(stocks,mu_CV_sp,sqrt(1/(tau_CV_sp*5)))

logit_mu_spawn[1]<-runif(1,0.85,3) #lower bound 0.70
logit_CV_spawn[1]<-runif(1,-3,-1.75) 

logit_mu_spawn[3]<-runif(1,0,3) #lower bound 0.70
logit_CV_spawn[3]<-runif(1,-3,-0.4) 


mu_a<-c(rnorm(1,-2.784,0.50),rnorm(1,-2.784,0.50))
sd_a<-c(rlnorm(1,-0.2653,0.20),rlnorm(1,-0.2653,0.20))


inits.fn<-function() {
  list(fec=c(exp(8),exp(9),exp(9.5),exp(9.5),exp(9.7)),
       K=rlnorm(length(stock_indices),M_K[stock_indices]*1.1,0.50),
       logit_pdetect=matrix(rnorm((years+5)*stocks,logit_mu_spawn,rep(0.20,times=stocks)),nrow=(years+5),byrow=T),    
       
       
       
       MpsW=rlnorm(years+proj_years,-1.2,0.3),
       a_slope=rnorm(stocks,mu_a[SR_unit[1:stocks]],sd_a[SR_unit[1:stocks]]),
       logit_qlW=log(iql/(1-iql)),     
       logit_qdW=log(iqd/(1-iqd)),     
       MW=rlnorm(1,-2.3,0.15),MR=rlnorm(1,-2,0.15),
       early_MpsW=rlnorm(1,0.23,0.15),
       CV_MpsW=rbeta(1,30,70),Reff_mu=rbeta(1,15,35),Reff_eta=rbeta(1,10,10)*0.5,CV_ladder=rlnorm(1,-3,0.20),
       cv_SR=rlnorm(1,-1.5,0.50))  #,CV_ladder=rlnorm(1,-3,0.20)
}

# Parameters to monitor

parnames<-c(
  "coefDS", #"mu_coefDS", "cv_coefDS",
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
  "surv_migr","p.mort","p.rel","nctW_rel","nctW_Tot","CV_ladder")

