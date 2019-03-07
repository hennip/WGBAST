##NOT RUN
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 SCRIPT TO READ CODA FILES WITH THE MCMC OUTPUT CHAINS INTO R  
#              This script currently uses 1000 simulation of the MCMC chains. 
#              It reads in the simulations from the estimation model;
#              all the values come from the estimation model including smolts,
#              and adults at sea, etc, except spawners and catches - these two 
#              are calculated based on all the other values. Also acenarios 
#              for the future for Mps and M74 are generated here.

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               

library(coda)
library(abind)  #for M74 
library(reshape) #for seal M

library(runjags)

# Becky:
# ===============
#  PathSim<-"C:/WGBAST15/JAGS final 2018/" # results from the simulation model and output from scenarios
#  PathData<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2018/Scenarios/Data/" # extra input files 
#  PathScen<-"C:/WGBAST15/2018_scenarios/" # scenario results 

# Henni:
# ===============
PathSim<-"H:/FLR/WGBAST18/" # results from the simulation model and output from scenarios
PathData<-"C:/Users/412hpulkkin/Dropbox/WGBAST/JAGS/data_2018/" # extra input files 
PathScen<-"H:/FLR/WGBAST18/Scenarios/" # scenario results 
# ===============

#Model<-"2019"
Model<-"Testebo"
select_case<-2 #new SR parameterisation 

# Fetch JAGS model
#load(file="H:/FLR/WGBAST18/newSR_final2018-04-22.RData")
load(file="O:/FLR/WGBAST18/dev_test_Testeboan2019-02-18.RData")

d<-as.matrix(chains)

# RiverNames not needed in input
#RiverNames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
#             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage")

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks

set.seed(12345) #set the random number seed
################################################################################
# ##############################################################################
# Set the 'choice' for Mps scenario according to the preferred mean in the
# autocorrelation analysis
choice<-"MED"   # corresponds to Mps during 2008-2011 period

# Set the last year for historic part and the last year for predictions:
LastHistYear<-2017    
LastPredYear<-2032
ymax<-32  #last year for JAGS model inputs

################################################################################
################################################################################

#HistYears indicate the historic part of the model.
HistYears<-c(1992:LastHistYear)
#Define a year that separates historic part from future part
yBreak<-length(HistYears)
#Age 0 are the smolts, Age 1 are the 1SW salmon, Age 2-5 are the MSW salmon
ages<-c(0,5)
ages<-c(ages[],ages[2]-ages[1]+1)
#Time
years<-c(1992:LastPredYear)
nYears<-length(years)
years<-c(c(1992,LastPredYear),nYears)

# Adjust units 5 and 6
# =====================
PropCR<-read.table(paste0(PathData, "PropAU16.txt"),header=T)[,1]
PropCW<-read.table(paste0(PathData, "PropAU16.txt"),header=T)[,2]

# Repeat the last for the future years 
PropCW<-c(PropCW, rep(PropCW[length(PropCW)],(years[3]-yBreak)))
PropCR<-c(PropCR, rep(PropCR[length(PropCR)],(years[3]-yBreak)))
# =====================


# The model includes 4 different assessment units with wild rivers. 
# Unit 1 includes the rivers Torne, Simo, Kalix and Rane
# Unit 2 includes the rivers Pite, Aby, Byske, Rickle, Savaran, Ume, Ore, Logde and Kage 
# Unit 3 includes Ljungan and Unit 4 includes the rivers Morrum and Eman
units<-c(1,4)
units<-c(units[],units[2]-units[1]+1)

Mps_All<-array(NA,dim= c(years[3],1000))
Mps_AllR<-array(NA,dim= c(years[3],1000))
M74_All<-array(NA,dim= c(years[3],1000))
bL<-array(NA,dim= c(4,100))
tauL<-array(NA,dim= c(4,100))
LReff<-array(NA,dim= c(4,100))
delta<-array(NA,dim= c(4,100))
Etot_tmp<-array(0,dim=c(years[3],Nstocks,100))

#set up seal M multipliers by year and AU...
# Seal predation is assumed to be fixed for all simulations 
seals<-as.matrix(read.table(paste0(PathData, "scenarios_Fseal.txt")))
F_seal<-array(1,dim=c(years[3],6,4))

for(i in 1:years[3]){
  for(u in 1:3){
    F_seal[i,2:6,u]<-rep(seals[i],times=5)
  }
}

#set up sex ratios   
Ume_prop_fem<-as.matrix(read.table(paste0(PathData,"MSW_prop_fem_Ume_Vindel.txt"),row.names=1))

Ume_prop_fem<-Ume_prop_fem[6:(yBreak+6),1]

prop_fem<-array(0,dim=c(nYears,6,Nstocks))
#> dim(prop_fem)
#[1] 41  6 16
prop_fem_tmp<-c(0.06,0.73,0.73,0.89,0.89)
for(y in 1:nYears){
  for(r in 1:9){
    prop_fem[y,2:6,r]<-prop_fem_tmp
  }
  prop_fem[y,1:2,10]<-c(0,0.06)
  if(y<(yBreak+2)){ #data until 2018
    prop_fem[y,3:6,10]<-rep(Ume_prop_fem[y],4)}
  else{ 
    prop_fem[y,3:6,10]<-rep(mean(Ume_prop_fem[(yBreak-1):(yBreak+1)]),4)}   #average of last 3 years
  for(r in 11:17){ # Added Testebo!
    prop_fem[y,2:6,r]<-prop_fem_tmp
  }
}
#prop_fem[,,1]
#prop_fem[,,10]


# WE'RE BACK IN THE LOOP...
# ==============================================================================
# save the data for each of the pieces of the MCMC chain
# the reason for breaking it up into the chunks of 100 iterations each is that
# R was running out of memory when attempting to store all 1000 sim in the 
# multiplicity of arrays used   

for(loop in 1:10){
  #loop<-1
  # We will write the simulation results to the sim folder using 10 Rdata-files 
  # under these names 
  if(choice=="MED"){  
    BH_dataFile<-
      paste0(PathScen, "ScenHist_JAGSmodel", Model,"_",loop,".RData") # name to separate historical part from future projections?
  }
  
  #Sims stores the numbers for the simulation in the MCMC chain
  sims<-c(1+100*(loop-1),100*loop)   #1st and last indices of sims
  sims<-c(sims[],sims[2]-sims[1]+1)   #add number of sims as 3rd member of sims
  
  ##############################################################
  # CREATE R OBJECTS TO BE PASSED ON TO THE OPERATING MODEL (OM)
  # tau_SR is the process uncertainty in stock-recruit relationship 
  
  precisionBH<-d[sims[1]:sims[2],grep("tau_SR",colnames(d))]
  
  # Set up matrices to later input the stock-recruit parameters
  
  BH_alpha<-array(NA,dim=c(sims[3],Nstocks))
  BH_alpha[,stock_indices]<-d[sims[1]:sims[2],grep("alphaSR",colnames(d))]
  
  BH_beta<-array(NA,dim=c(sims[3],Nstocks))
  BH_beta[,stock_indices]<-d[sims[1]:sims[2],grep("betaSR",colnames(d))]
  
  if(select_case == 1){
    R_zero<-array(NA,dim=c(sims[3],Nstocks))
    BH_z<-array(NA,dim=c(sims[3],Nstocks))
    
    R_zero[,stock_indices]<-d[sims[1]:sims[2],grep("R0",colnames(d))]
    BH_z[,stock_indices]<-d[sims[1]:sims[2],grep("z",colnames(d))]
  } else if(select_case ==2){
    R_zero<-array(NA,dim=c(sims[3],years[3],Nstocks))
    BH_z<-array(NA,dim=c(sims[3],years[3],Nstocks))
    
    for(s in 1:length(stock_indices)){
      for(y in 1:yBreak){ 
        tempr<-paste0("R0[",y+years[1]-1987,",",s,"]")
        tempz<-paste0("z[",y+years[1]-1987,",",s,"]")
        R_zero[,y,stock_indices[s]]<-d[sims[1]:sims[2],grep(tempr,colnames(d),fixed=TRUE)]
        BH_z[,y,stock_indices[s]]<-d[sims[1]:sims[2],grep(tempz,colnames(d),fixed=TRUE)]
      }
      for (y in (yBreak+1):years[3]){ 
        tempr<-paste0("R0[",ymax,",",s,"]")
        tempz<-paste0("z[",ymax,",",s,"]")
        R_zero[,y,stock_indices[s]]<-d[sims[1]:sims[2],grep(tempr,colnames(d),fixed=TRUE)]
        BH_z[,y,stock_indices[s]]<-d[sims[1]:sims[2],grep(tempz,colnames(d),fixed=TRUE)]
      }
    }
  }
  
  # Initialize the basic quants to hold information about wild and reared stocks
  # Age relates to the age at sea, units relate to the 4 assessment units
  # in the case of reared fish and rivers in the case of wild, season allows
  # to differentiate between the salmon at sea and the salmon that are migrating
  # back to the river for spawning,
  
  #################
  # Quants: ages, years, rivers (AU's in case of reared),
  # spawning/ feeding migration, not used, simulations
  ################
  iniAgeQuantW<-array(NA, dim=c(ages[3],years[3],Nstocks,2,sims[3]))
  iniAgeQuantR<-array(NA, dim=c(ages[3],years[3],4,2,sims[3]))
  #           1    2     3     4                   5
  #indices  age year stock mature(2)/immature(1)  sim
  
  # Create quants to store values
  #! Note that these parameters contain only 100 iterations at time
  #! and thus should not be used as final results!
  #! (instead, other parameters containing full 1000 sample are combined)
  WsalmStock<-iniAgeQuantW; RsalmStock<-iniAgeQuantR
  WsalmNatMort<-iniAgeQuantW; RsalmNatMort<-iniAgeQuantR
  WsalmMatRate<-iniAgeQuantW; RsalmMatRate<-iniAgeQuantR
  
  PFAtmpW<-iniAgeQuantW
  PFAtmpR<-iniAgeQuantR
  
  WOLL_HRtmp<-iniAgeQuantW;  WODN_HRtmp<-iniAgeQuantW
  WCDN_HRtmp<-iniAgeQuantW;  WCGN_HRtmp<-iniAgeQuantW
  WCTN_HRtmp<-iniAgeQuantW;  WRF_HRtmp<-iniAgeQuantW
  
  ROLL_HRtmp<-iniAgeQuantR;  RODN_HRtmp<-iniAgeQuantR
  RCDN_HRtmp<-iniAgeQuantR;  RCGN_HRtmp<-iniAgeQuantR
  RCTN_HRtmp<-iniAgeQuantR;  RRF_HRtmp<-iniAgeQuantR
  
  WOLL_Ctmp<-iniAgeQuantW;  WODN_Ctmp<-iniAgeQuantW
  WCDN_Ctmp<-iniAgeQuantW;  WCGN_Ctmp<-iniAgeQuantW
  WCTN_Ctmp<-iniAgeQuantW;  WRF_Ctmp<-iniAgeQuantW
  
  ROLL_Ctmp<-iniAgeQuantR;  RODN_Ctmp<-iniAgeQuantR
  RCDN_Ctmp<-iniAgeQuantR;  RCGN_Ctmp<-iniAgeQuantR
  RCTN_Ctmp<-iniAgeQuantR;  RRF_Ctmp<-iniAgeQuantR
  
  p.ladder<-array(NA, dim=c(years[3],Nstocks,sims[3]))
  surv_migr<-array(NA, dim=c(years[3],Nstocks,sims[3]))

  # 1st index: age (G/MSW) 2nd index: AU
  qctnW<-array(NA, dim=c(2,4,sims[3]))
  qctnR<-array(NA, dim=c(2,4,sims[3]))
  qcgnW<-array(NA, dim=c(2,4,sims[3]))
  qcgnR<-array(NA, dim=c(2,4,sims[3]))
  qdW<-array(NA, dim=c(2,sims[3]))
  qdR<-array(NA, dim=c(2,sims[3]))
  qlW<-array(NA, dim=c(2,sims[3]))
  qlR<-array(NA, dim=c(2,sims[3]))
  
  #Input probability to find the ladder (~1 for all rivers except Ume)
  
  for(y in 1:(yBreak)){  #yBreak+3 =29 =2020
    for(s in 1:length(stock_indices)){   
      # Add five years because the MCMC files start from 1992 but 
      # the WGBAST model starts from 1987
      x<-paste0("p.ladder[",y+years[1]-1987,",",s,"]")
      x1<-paste0("surv_migr[",y+years[1]-1987,",",s,"]")      
      p.ladder[y,stock_indices[s],]<-d[sims[1]:sims[2],grep(x,colnames(d),fixed=TRUE)]
      surv_migr[y,stock_indices[s],]<-d[sims[1]:sims[2],grep(x1,colnames(d),fixed=TRUE)]
    } 
  }
  for(y in (yBreak+1):(nYears)){ 
    for(s in 1:length(stock_indices)){   
      p.ladder[y,stock_indices[s],]<- apply(p.ladder[(yBreak-2):yBreak,stock_indices[s],],2,mean)
      surv_migr[y,stock_indices[s],]<- apply(surv_migr[(yBreak-2):yBreak,stock_indices[s],],2,mean)
    } 
  }
  
  #Input wild smolts
  for(y in 1:(yBreak+3)){  #yBreak+3 =29 =2020
    for(s in 1:length(stock_indices)){   
      # Add five years because the MCMC files start from 1992 but 
      # the WGBAST model starts from 1987
      #e.g. the SmoltWW[6,] files corresponds to 1992
      x<-paste0("SmoltWW[",y+years[1]-1987,",",s,"]")   #years[1]=1992  so this is y+5
      WsalmStock[1,y,stock_indices[s],1,]<-d[sims[1]:sims[2],grep(x,colnames(d),fixed=TRUE)]
      
    } 
  }
  
  #Input reared smolts 
  for(y in 1:yBreak){
    for(u in  1:4){ 
      x<-paste0("SmoltR[",y+years[1]-1987,",",u,"]")   #years[1]=1992  so this is y+5
      RsalmStock[1,y,u,1,]<-d[sims[1]:sims[2],grep(x,colnames(d),fixed=TRUE)]
    }
  }
  
  #FUTURE RELEASES
  # Number of released smolts is assumed to be the same as last year (yBreak)
  RsalmStock[1,(yBreak+1):years[3],1,1,]<-RsalmStock[1,(yBreak),1,1,]
  RsalmStock[1,(yBreak+1):years[3],2,1,]<-RsalmStock[1,(yBreak),2,1,]
  RsalmStock[1,(yBreak+1):years[3],3,1,]<-RsalmStock[1,(yBreak),3,1,]
  RsalmStock[1,(yBreak+1):years[3],4,1,]<-RsalmStock[1,(yBreak),4,1,] 
  
  #Estimated numbers-at-age starting in May 1992 
  # Loop over the different age groups. 
  # Grilse or 1SW salmon from the 2nd age group (a=2), 
  # 2SW salmon form the 3rd age group (a=3), 
  # 3SW salmon from the 4th age group (a=4), 
  # 4SW salmon from the 5th age group (a=5) and 
  # 5SW salmon form the 6th age group (a=6)
  for(y in 1:yBreak){
    for(a in 2:6){
      for(s in 1:length(stock_indices)){   
        xa<-paste0("NccW[",(y+6-a),",",a,",",s,"]")         #check 2nd index NccW here
        #checked 10/01, OK: e.g. y=1, a=2 NccW[5,2] 2 yr olds in 1992
        #y=1, a=3 NccW[4,3] 3 yr olds in 1992
        #y=2, a=3 NccW[5,3] 3 yr olds in 1993 etc.
        WsalmStock[a,y,stock_indices[s],1,]<-d[sims[1]:sims[2],grep(xa,colnames(d),fixed=TRUE)]
      }
      for(v in 1:4){
        #Read and input the reared salmon for the different units (season=1)
        xar<-paste0("NccR[",(y+6-a),",",a,",",v,"]")      
        RsalmStock[a,y,v,1,]<-d[sims[1]:sims[2],grep(xar,colnames(d),fixed=TRUE)]
      }  
    }
  }
  
  #Instantaneous post-smolt nat mort for age=1
  for(y in 1:yBreak){  
    xR<-paste0("MpsR[",y+years[1]-1987,"]")
    xW<-paste0("^MpsW\\[",y+years[1]-1987,"\\]")  
    
    for(u in 1:4){
      RsalmNatMort[1,y,u,1,]<-d[sims[1]:sims[2],grep(xR,colnames(d),fixed=TRUE)]
      RsalmNatMort[1,y,u,2,]<-d[sims[1]:sims[2],grep(xR,colnames(d),fixed=TRUE)]
    }
    for(r in 1:Nstocks){
      WsalmNatMort[1,y,r,1,]<-d[sims[1]:sims[2],grep(xW,colnames(d))] 
      WsalmNatMort[1,y,r,2,]<-d[sims[1]:sims[2],grep(xW,colnames(d))]   
    }
  }
  for(y in 1:years[3]){  #MR MW
    for(u in 1:4){
      RsalmNatMort[2:6,y,u,1,]<-d[sims[1]:sims[2],grep("MR",colnames(d),fixed=TRUE)]
      RsalmNatMort[2:6,y,u,2,]<-d[sims[1]:sims[2],grep("MR",colnames(d),fixed=TRUE)]
    }
    for(r in 1:Nstocks){
      WsalmNatMort[2:6,y,r,1,]<-d[sims[1]:sims[2],grep("MW",colnames(d),fixed=TRUE)]
      WsalmNatMort[2:6,y,r,2,]<-d[sims[1]:sims[2],grep("MW",colnames(d),fixed=TRUE)]
    }
  }
  
  # Homing rates or maturation rates
  # The maturation rates are stored at the 1 slot (season=1)
  # Homing rate or maturation rate for smolts (age=1)
  WsalmMatRate[1,,,1,]<-0
  RsalmMatRate[1,,,1,]<-0
  # Homing rate or maturation rate for 1SW to 4SW salmon (age=2 to 5)
  
  # hierarchical parameters for maturation
  mucL<-d[sims[1]:sims[2],grep("mucL",colnames(d),fixed=TRUE)]
  taucL<-d[sims[1]:sims[2],grep("taucL",colnames(d),fixed=TRUE)]
  
  for(a in 1:4){
    tempb<-paste0("bL[",a,"]")
    tempt<-paste0("tauL[",a,"]")
    bL[a,]<-d[sims[1]:sims[2],grep(tempb,colnames(d),fixed=TRUE)]
    tauL[a,]<-d[sims[1]:sims[2],grep(tempt,colnames(d),fixed=TRUE)]
  }
  for(a in 1:3){
    templ<-paste0("LReffect[",a,"]")
    tempd<-paste0("delta[",a,"]")
    LReff[a,]<-d[sims[1]:sims[2],grep(templ,colnames(d),fixed=TRUE)]
    delta[a,]<-d[sims[1]:sims[2],grep(tempd,colnames(d),fixed=TRUE)]
  }
  LReff[4,]<-LReff[3,]
  delta[4,]<-delta[3,]
  
  # History  
  for(a in 2:5){     
    for(y in 1:yBreak){ 
      lR<-paste0("LR[",y+years[1]-1987,",",(a-1),"]")
      lW<-paste0("LW[",y+years[1]-1987,",",(a-1),"]")
      
      for(r in 1:Nstocks){
        WsalmMatRate[a,y,r,1,]<-d[sims[1]:sims[2],grep(lW,colnames(d),fixed=TRUE)]
      }
      for(u in 1:4){
        RsalmMatRate[a,y,u,1,]<-d[sims[1]:sims[2],grep(lR,colnames(d),fixed=TRUE)]     #reared salmon
      }   
    }
  }
  
  # Future
  for(y in (yBreak+1):years[3]){ 
    for(s in 1:sims[3]){
      cL<-rnorm(1,mean=mucL[s],sd=sqrt(1/taucL[s]))
      lw<-c()
      lr<-c()
      
      # Temperature estimates updated 22/3/2018
      if(y==(yBreak+1)){ # 2018
        Temp<-rnorm(1,mean=5.22,sd=0.391)
      }
      if(y>(yBreak+1)){ #2019->
        Temp<-rnorm(1,mean=4.185,sd=1.214)
      }
      
      for(a in 2:5){
        lw[a]<-rnorm(1,mean=cL+bL[a-1,s]+delta[a-1,s]*Temp,
                     sd=sqrt(1/tauL[a-1,s]))
        lr[a]<-rnorm(1,mean=cL+bL[a-1,s]+LReff[a-1,s]+delta[a-1,s]*Temp,
                     sd=sqrt(1/tauL[a-1,s]))
        
        WsalmMatRate[a,y,,1,s]<-exp(lw[a])/(1+exp(lw[a]))
        RsalmMatRate[a,y,,1,s]<-exp(lr[a])/(1+exp(lr[a])) 
      }
    }    
  }
  
  # Homing rate or maturation rate for older than 5SW salmon 
  WsalmMatRate[6,,,1,]<-1
  RsalmMatRate[6,,,1,]<-1
  
  # Fecundity expressed in terms of the number of eggs produced by each female
  # Fecundity is stored at the 2 slot (season=2)
  # Fecundity of smolts (age=1)
  WsalmMatRate[1,,,2,]<-0
  # Fecundity for 1SW to 3SW salmon (age=2 to 4)
  for(a in 2:6){ 
    fW<-paste(sep="","fec[",(a-1),"]") 
    for(st in 1:Nstocks){
      for(y in 1:years[3]){
        WsalmMatRate[a,y,st,2,]<-d[sims[1]:sims[2],grep(fW,colnames(d),fixed=TRUE)]
      }
    }
  }
  
  ################################################################
  #FUTURE M74 AND POST-SMOLT MORTALITY
  ################################################################
  
  #! AUTOCORRELATION ANALYSIS, will be updated annually
  #! Last update 23/04/2018
  ## Stable mean on logit scale
  
  #! Average mortality during 4 year period
  if(choice=="MED") {mu<--1.632}    # median survival 2013-2016, 19.5%
  w<-0.61      
  sigma2<-0.851
  
  ####
  ## code for autoregressive M74 process
  #! Last update 23/03/2018
  ####
  ## AutoC coefficient  on logit scale
  wM74<- 0.824
  
  ## Stable mean on logit scale
  muM74<- -1.89     # 85% survival
  
  ## Marginal variance  on logit scale
  sigma2M74<-0.305
  ####
  
  M74<-as.matrix(sims[1]:sims[2])
  
  #Use (yBreak -1) if want to discard the last years's estimate (year yBreak has no M74 data)
  for(y in 1:(yBreak-1)){ 
    x<-paste0(PathData,"M74/M74[" ,y+years[1]-1987,"]CODAchain1.txt")
    i<-read.table(x)
    M74<-cbind(M74,i[sims[1]:sims[2],2])
  }
  
  M74<-M74[,2:(yBreak)] # correct accordingly to previous for-loop! +1 if going until yBreak
  
  #=============
  # M74
  #=============
  #  for(y in (yBreak+1):years[3]){
  for(y in yBreak:years[3]){
    #y<-yBreak+2
    #y<-years[3]
    #dim(M74)
    autocorM74<-as.vector(M74[,y-1])
    autocorM74<-log(autocorM74/(1-autocorM74))
    
    for(i in 1:sims[3]){
      autocorM74[i]<-wM74*autocorM74[i]+
        rnorm(1,muM74*(1-wM74), sqrt(sigma2M74*(1-wM74*wM74))) 
      # Auto Regr process with lag 1
      
      autocorM74[i]<-exp(autocorM74[i])/(1+exp(autocorM74[i]))
      # Transforming to M74 mortality (proportion)
      
      autocorM74[i]<-min(0.8,autocorM74[i])
      j<-sims[1]-1+i
      M74_All[y,j]<-autocorM74[i]
    }
    M74<-cbind(M74,autocorM74)
    
  }#END SCENARIOS FOR M74
  
  #=============
  # Mps scenarios for the future
  #=============
  
  # These are the parameters of a beta distribution which adjust 
  # MpsR depending on MpsW
  #! Update these annually using script Ra_Rb.r
  #! Updated 23/04/2018
  Rmu<-0.13
  Reta<-0.259
  
  for(y in yBreak:years[3]){
    for(i in 1:sims[3]){
      
      #Generate scenarios for MpsW using autocorrelation analysis
      x<-as.numeric(log(exp(-WsalmNatMort[1,y-1,1,1,i])/
                          (1-exp(-WsalmNatMort[1,y-1,1,1,i]))))
      x<-w*x+rnorm(1,mu*(1-w),sqrt(sigma2*(1-w*w)))
      
      WsalmNatMort[1,y,1,1,i] <- -log((exp(x)/(1+exp(x))))
      WsalmNatMort[1,y,,,i] <- min(5.3,WsalmNatMort[1,y,1,1,i])
      
      #Adjust MpsR based on above
      Ra<-Rmu/Reta
      Rb<-(1-Rmu)/Reta
      RMps<-rbeta(1,Ra,Rb)
      ReffectMps<-RMps*1.5+1
      
      RsalmNatMort[1,y,,,i] <- ReffectMps*WsalmNatMort[1,y,1,1,i]
      
      j<-sims[1]-1+i
      Mps_All[y,j]<-WsalmNatMort[1,y,1,1,i]
      Mps_AllR[y,j]<-RsalmNatMort[1,y,1,1,i]
    }
    
  }#END SCENARIOS FOR  MPS
  
  
  ################################################################
  # Effort and catchability
  ################################################################
  # Initialise the array which will hold the effort information by ICES unit 
  # and by country. The unit will contain the different ICES units i.e. ICES 
  # units 22-29 which correspond to the offshore areas in the Baltic Main Basin, 
  # ICES unit 30 which corresponds to the Bothnian Sea and ICES unit 31 which 
  # corresponds to the Bothnian Bay. The seasons contain the information on the 
  # fishing fleet: OLL= offshore longline, ODN = offshore driftnet, 
  # CDN = coastal driftnet, CTN = coastal trapnet, CGN = Coastal gillnet or 
  # other gear and RF= river fishery. The areas correspond to the main fishing 
  # nations around the Baltic Sea i.e. Finland, Sweden and Denmark. The other 
  # nations are all combined in Other. The dominant fishing nation in the 
  # Other category is Poland. The effort is assumed to be known without error 
  # and therefore no MCMC chains are thus far available.
  
  EffortICES <- array(NA, dim= c(1, years[3],3,5,5,sims[3]))
  
  dimnames(EffortICES) <- list(age="All", year=years[1]:years[2],
                               unit=c("ICES 22-29","ICES 30","ICES 31"),
                               season=c("OLL","ODN","CDN","CTN","CGN"), 
                               area = c("Finland","Sweden","Denmark","Poland","Trolling"), # "Other" changed to "Poland", "Trolling" added
                               iter=1:sims[3]) 
  
  # Input effort data according to ICES unit and country
  # Because some countries do not participate to certain fisheries, all 
  # effort slots are first filled with zeros
  EffortICES[]<-0
  
  # Input effort data for the offshore driftnet fishery for the different 
  # countries from 1992 till 2007
  temp <-read.table(paste0(PathData,"/EffortODN_ICES.txt"))
  dimtemp<-dim(temp)[1]
  EffortICES[,1:yBreak,1,"ODN","Finland",]<-temp[6:dimtemp,1]
  EffortICES[,1:yBreak,1,"ODN","Sweden",]<-temp[6:dimtemp,2]
  EffortICES[,1:yBreak,1,"ODN","Denmark",]<-temp[6:dimtemp,3]
  EffortICES[,1:yBreak,1,"ODN","Poland",]<-temp[6:dimtemp,4]+temp[6:dimtemp,5] # PL + LV
  
  # Input effort data for the offshore longline fishery for the different 
  # countries from 1992 till 2007
  temp <-read.table(paste0(PathData,"/EffortOLL_ICES.txt"))
  EffortICES[,1:yBreak,1,"OLL","Finland",]<-temp[6:dimtemp,1]
  EffortICES[,1:yBreak,1,"OLL","Sweden",]<-temp[6:dimtemp,2]
  EffortICES[,1:yBreak,1,"OLL","Denmark",]<-temp[6:dimtemp,3]
  EffortICES[,1:yBreak,1,"OLL","Poland",]<-temp[6:dimtemp,4]
  EffortICES[,1:yBreak,1,"OLL","Trolling",]<-temp[6:dimtemp,5]
  
  # Input effort data for the coastal driftnet fishery from 1992 till 2007. 
  # Finland is the only country participating to this fishery
  temp <-read.table(paste0(PathData,"/EffortCDN_ICES.txt"))
  EffortICES[,1:yBreak,1,"CDN","Finland",]<-temp[6:dimtemp,1] 
  
  # Input effort data for the coastal trapnet fishery from 1992 till 2007. 
  # Finland and Sweden are the only countries participating to this fishery in 
  # the Gulf of Bothnia within ICES units 30 and 31
  temp <-read.table(paste0(PathData,"/EffortCTN_ICES.txt"))
  EffortICES[,1:yBreak,"ICES 30","CTN","Finland",]<-temp[6:dimtemp,1]
  EffortICES[,1:yBreak,"ICES 30","CTN","Sweden",]<-temp[6:dimtemp,2]
  EffortICES[,1:yBreak,"ICES 31","CTN","Finland",]<-temp[6:dimtemp,3]
  EffortICES[,1:yBreak,"ICES 31","CTN","Sweden",]<-temp[6:dimtemp,4]
  
  # Input effort data for the coastal gillnet fishery from 1992 till 2007. 
  # Finland and Sweden are the only countries participating to this fishery in 
  # the Gulf of Bothnia within ICES units 30 and 31
  temp <-read.table(paste0(PathData,"/EffortCGN_ICES.txt"))
  EffortICES[,1:yBreak,"ICES 30","CGN","Finland",]<-temp[6:dimtemp,1]
  EffortICES[,1:yBreak,"ICES 30","CGN","Sweden",]<-temp[6:dimtemp,2]
  EffortICES[,1:yBreak,"ICES 31","CGN","Finland",]<-temp[6:dimtemp,3]
  EffortICES[,1:yBreak,"ICES 31","CGN","Sweden",]<-temp[6:dimtemp,4]
  #Input effort data for the fisheries affecting wild salmon are the same as 
  # for those affected reared salmon
  
  #EffortICES[,,1,"OLL",,1] #to check that these are fine
  #EffortICES[,,2:3,"CTN",,1]
  
  #A separate array will hold the information by assessment unit. 
  EffortAssesUnit <- array(NA, dim = c(1,years[3],4,5,sims[3]))
  
  dimnames(EffortAssesUnit) <- list(age="All", year=years[1]:years[2], 
                                    unit=units[1]:units[2], season=c("OLL","ODN","CDN","CTN","CGN"),
                                    iter=1:sims[3])
  
  # Input effort data according to assessment unit. The effort according to 
  # assessment unit are calculated from the effort by country and ICES units
  # All effort slots are first filled with zeros.
  EffortAssesUnit[]<-0
  
  #Loop over the 4 assessment units
  for(u in 1:4){
    # The offshore longline effort consists of the effort from all 
    # countries in ICES units 22-29
    EffortAssesUnit[,1:yBreak,u,"OLL",]<-
      EffortICES[,1:yBreak,1,"OLL","Finland",]+
      EffortICES[,1:yBreak,1,"OLL","Sweden",]+ 
      EffortICES[,1:yBreak,1,"OLL","Denmark",]+
      EffortICES[,1:yBreak,1,"OLL","Poland",]+
      EffortICES[,1:yBreak,1,"OLL","Trolling",]
    
    # The offshore driftnet effort consists of the effort from all 
    # countries in ICES units 22-29
    EffortAssesUnit[,1:yBreak,u,"ODN",]<-
      EffortICES[,1:yBreak,1,"ODN","Finland",]+
      EffortICES[,1:yBreak,1,"ODN","Sweden",]+
      EffortICES[,1:yBreak,1,"ODN","Denmark",]+
      EffortICES[,1:yBreak,1,"ODN","Poland",]       
    
    #The coastal driftnet effort consists of the effort from Finland 
    # in ICES units 22-29 (unit=1)
    EffortAssesUnit[,1:yBreak,u,"CDN",]<-
      EffortICES[,1:yBreak,1,"CDN","Finland",]
  }
  rm(u)
  
  # Salmon from assessment unit 4 (u=4) are not affected by the 
  # coastal driftnet fishery
  EffortAssesUnit[,,4,"CDN",]<-0
  
  # Salmon from unit 1 are affected by the Finnish coastal trapnet fishery 
  # in ICES units 30 and 31
  EffortAssesUnit[,1:yBreak,1,"CTN",]<-
    EffortICES[,1:yBreak,"ICES 30","CTN","Finland",]+
    EffortICES[,1:yBreak,"ICES 31","CTN","Finland",]+ 
    0.45*EffortICES[,1:yBreak,"ICES 31","CTN","Sweden",] 
  
  # Salmon from unit 2 are affected by the Finnish coastal trapnet fishery 
  # in ICES units 30 and by the
  #Swedish coastal trapnet fishery in ICES unit 31
  EffortAssesUnit[,1:yBreak,2,"CTN",]<-
    EffortICES[,1:yBreak,"ICES 30","CTN","Finland",]+
    0.55*EffortICES[,1:yBreak,"ICES 31","CTN","Sweden",]
  
  # Salmon from unit 3 are affected by the Finnish coastal trapnet fishery 
  # in ICES units 30 and by the
  #Swedish coastal trapnet fishery in ICES unit 30
  EffortAssesUnit[,1:yBreak,3,"CTN",]<-
    EffortICES[,1:yBreak,"ICES 30","CTN","Finland",]+
    EffortICES[,1:yBreak,"ICES 30","CTN","Sweden",]
  
  # Salmon from unit 1 are affected by the Finnish coastal gillnet fishery 
  # in ICES units 30 and 31
  EffortAssesUnit[,1:yBreak,1,"CGN",]<-
    EffortICES[,1:yBreak,"ICES 30","CGN","Finland",]+
    EffortICES[,1:yBreak,"ICES 31","CGN","Finland",]
  
  # Salmon from unit 2 are affected by the Finnish coastal gillnet fishery 
  # in ICES units 30 and by the
  #Swedish coastal gillnet fishery in ICES unit 31
  EffortAssesUnit[,1:yBreak,2,"CGN",]<-
    EffortICES[,1:yBreak,"ICES 30","CGN","Finland",]+
    EffortICES[,1:yBreak,"ICES 31","CGN","Sweden",]
  
  # Salmon from unit 3 are affected by the Finnish coastal gillnet fishery 
  # in ICES units 30 and by the
  # Swedish coastal gillnet fishery in ICES unit 30
  EffortAssesUnit[,1:yBreak,3,"CGN",]<-
    EffortICES[,1:yBreak,"ICES 30","CGN","Finland",]+
    EffortICES[,1:yBreak,"ICES 30","CGN","Sweden",]
  
  #Read catchability coefficients for grilse/MSW for offshore fisheries 
  for(a in 1:2){ # grilse/MSW
    qdw<-paste0("qdW[",(a+1),"]")
    qdr<-paste0("qdR[",(a+1),"]")   
    qlw<-paste0("qlW[",(a+1),"]")   
    qlr<-paste0("qlR[",(a+1),"]")        
    
    qdW[a,]<-d[sims[1]:sims[2],grep(qdw,colnames(d),fixed=TRUE)]
    qdR[a,]<-d[sims[1]:sims[2],grep(qdr,colnames(d),fixed=TRUE)]
    qlW[a,]<-d[sims[1]:sims[2],grep(qlw,colnames(d),fixed=TRUE)]
    qlR[a,]<-d[sims[1]:sims[2],grep(qlr,colnames(d),fixed=TRUE)]
  }
  
  # Read catchability coefficients for grilse/MSW from stocks 
  # of different assessment units in different coastal fisheries
  for(u in 1:3){
    for(a in 1:2){ # grilse/MSW
      qctnw<-paste0("qctnW[",(a+1),",",u,"]")
      qctnr<-paste0("qctnR[",(a+1),",",u,"]")  
      qcgnw<-paste0("qcgnW[",(a+1),",",u,"]")  
      qcgnr<-paste0("qcgnR[",(a+1),",",u,"]") 
      
      qctnW[a,u,]<-d[sims[1]:sims[2],grep(qctnw,colnames(d),fixed=TRUE)]
      qctnR[a,u,]<-d[sims[1]:sims[2],grep(qctnr,colnames(d),fixed=TRUE)]
      qcgnW[a,u,]<-d[sims[1]:sims[2],grep(qcgnw,colnames(d),fixed=TRUE)]
      qcgnR[a,u,]<-d[sims[1]:sims[2],grep(qcgnr,colnames(d),fixed=TRUE)]
    }
  }
  
  qctnW[,4,]<-0
  qctnR[,4,]<-0
  qcgnW[,4,]<-0
  qcgnR[,4,]<-0
  
  #Input harvest rate for river fishery by year and age 
  for(y in 1:yBreak){    
    for(a in 1:6){
      # y+5 because historical model begins from 1987 but scenarios from 1992
      x<-paste0("HrW[",(y+5),",",a,"]")
      xR<-paste0("HrR[",(y+5),",",a,"]")
      for(r in 1:Nstocks){
        WRF_HRtmp[a,y,r,2,]<-d[sims[1]:sims[2],grep(x,colnames(d),fixed=TRUE)]
      }
      for(u in 1:4){
        RRF_HRtmp[a,y,u,2,]<-d[sims[1]:sims[2],grep(xR,colnames(d),fixed=TRUE)]
      }
    }
  }
  
  #!POPULATION MODEL STARTS HERE  
  #!##########################################################################
  
  #Historic time loop                  
  for(y in 1:yBreak){
    #y<-1
    tempW<- WsalmStock
    tempR<- RsalmStock
    
    tempW[,y,,1,]<- WsalmStock[,y,,1,]
    tempR[,y,,1,]<- RsalmStock[,y,,1,]
    
    # Smolts/ Post-smolts
    WOLL_HRtmp[1,y,1:Nstocks,1,]<-0
    ROLL_HRtmp[1,y,1:4,1,]<-0
    WODN_HRtmp[1,y,1:Nstocks,1,]<-0
    RODN_HRtmp[1,y,1:4,1,]<-0
    
    WCDN_HRtmp[1,y,1:Nstocks,2,]<-0
    WCTN_HRtmp[1,y,1:Nstocks,2,]<-0
    WCGN_HRtmp[1,y,1:Nstocks,2,]<-0
    RCDN_HRtmp[1,y,1:4,2,]<-0
    RCTN_HRtmp[1,y,1:4,2,]<-0
    RCGN_HRtmp[1,y,1:4,2,]<-0
    
    # Note that LL and DN harvest rates are the same for all stocks
    # but are given per stock and unit to keep the index structure the same  
    for(r in 1:Nstocks){
      WOLL_HRtmp[2,y,r,1,]<-   1-exp(-qlW[1,]* EffortAssesUnit[,y,1,"OLL",])
      WOLL_HRtmp[3:6,y,r,1,]<- 1-exp(-qlW[2,]* EffortAssesUnit[,y,1,"OLL",])
      WODN_HRtmp[2,y,r,1,]<-   1-exp(-qdW[1,]* EffortAssesUnit[,y,1,"ODN",])
      WODN_HRtmp[3:6,y,r,1,]<- 1-exp(-qdW[2,]* EffortAssesUnit[,y,1,"ODN",])     
    }
    for(u in 1:4){
      ROLL_HRtmp[2,y,u,1,]<-   1-exp(-qlR[1,]* EffortAssesUnit[,y,1,"OLL",])
      ROLL_HRtmp[3:6,y,u,1,]<- 1-exp(-qlR[2,]* EffortAssesUnit[,y,1,"OLL",])    
      RODN_HRtmp[2,y,u,1,]<-   1-exp(-qdR[1,]* EffortAssesUnit[,y,1,"ODN",])
      RODN_HRtmp[3:6,y,u,1,]<- 1-exp(-qdR[2,]* EffortAssesUnit[,y,1,"ODN",])
    }
    
    # in coastal fisheries, effort differs per unit
    # Effort is 0 in AU4
    for(u in 1:4){
      RCDN_HRtmp[2,y,u,2,]<-  1-exp(-qdR[1,]* EffortAssesUnit[,y,1,"CDN",])
      RCDN_HRtmp[3:6,y,u,2,]<-1-exp(-qdR[2,]* EffortAssesUnit[,y,1,"CDN",])
      RCTN_HRtmp[2,y,u,2,]<-  1-exp(-qctnR[1,u,]* EffortAssesUnit[,y,u,"CTN",])
      RCTN_HRtmp[3:6,y,u,2,]<-1-exp(-qctnR[2,u,]* EffortAssesUnit[,y,u,"CTN",])
      RCGN_HRtmp[2,y,u,2,]<-  1-exp(-qcgnR[1,u,]* EffortAssesUnit[,y,u,"CGN",])
      RCGN_HRtmp[3:6,y,u,2,]<-1-exp(-qcgnR[2,u,]* EffortAssesUnit[,y,u,"CGN",])
    }
    
    for(r in 1:Nstocks){
      WCDN_HRtmp[2,y,r,2,]<-   1-exp(-qdW[1,]*EffortAssesUnit[,y,AU[r],"CDN",])
      WCDN_HRtmp[3:6,y,r,2,]<- 1-exp(-qdW[2,]*EffortAssesUnit[,y,AU[r],"CDN",])
      
      WCTN_HRtmp[2,y,r,2,]<-   1-exp(-qctnW[1,AU[r],]*EffortAssesUnit[,y,AU[r],"CTN",])
      WCTN_HRtmp[3:6,y,r,2,]<- 1-exp(-qctnW[2,AU[r],]*EffortAssesUnit[,y,AU[r],"CTN",])
      
      WCGN_HRtmp[2,y,r,2,]<-   1-exp(-qcgnW[1,AU[r],]*EffortAssesUnit[,y,AU[r],"CGN",])
      WCGN_HRtmp[3:6,y,r,2,]<- 1-exp(-qcgnW[2,AU[r],]*EffortAssesUnit[,y,AU[r],"CGN",])
    }
    
    
    # On May 1st, split the number of salmon at sea into the number of salmon at 
    # sea that have matured and will spawn this year and the number of salmon at 
    # sea that have not yet matured and will spend at least another year at sea. 
    # Because smolts spend 1 year at sea before migrating, the values for 
    # WsalmNatMortat and RsalmNatMortat with age=1 is equal to 0.
    
    # On May 1st, the number of mature salmon that will migrate and spawn this 
    # year is determined by
    WsalmStock[,y,,2,]<-WsalmStock[,y,,1,]*WsalmMatRate[,y,,1,]
    RsalmStock[,y,,2,]<-RsalmStock[,y,,1,]*RsalmMatRate[,y,,1,]
    
    # On May 1st, the number of salmon that have not yet matured this year and 
    # that will stay at least another year at sea is determined by
    WsalmStock[,y,,1,]<-WsalmStock[,y,,1,]*(1-WsalmMatRate[,y,,1,])
    RsalmStock[,y,,1,]<-RsalmStock[,y,,1,]*(1-RsalmMatRate[,y,,1,])        
    
    # On June 1st, the number of migrating fish caught by the coastal driftnet 
    # fishery at the ?land Island is determined by
    # Effort is the same for unit 1:3 and salmon from unit 4 is not caught
    # Catches for unit four are zero, because catchability for unit 4 is zero
    WCDN_Ctmp[,y,,2,]<-WsalmStock[,y,,2,]*exp(-(WsalmNatMort[,y,,2,]/12))*WCDN_HRtmp[,y,,2,]
    RCDN_Ctmp[,y,,2,]<-RsalmStock[,y,,2,]*exp(-(RsalmNatMort[,y,,2,]/12))*RCDN_HRtmp[,y,,2,]
    
    # The number of migrating fish by age in June after the coastal driftnet 
    # fishery at the ?land Island is given by
    WsalmStock[,y,,2,]<-WsalmStock[,y,,2,]*exp(-(WsalmNatMort[,y,,2,]/12))-WCDN_Ctmp[,y,,2,]
    RsalmStock[,y,,2,]<-RsalmStock[,y,,2,]*exp(-(RsalmNatMort[,y,,2,]/12))-RCDN_Ctmp[,y,,2,]
    
    # The coastal trapnet and gillnet fisheries are assumed to only affect the 
    # stocks of assessment unit 1, 2 and 3 and not of assessment unit 4 (Mprrum & Eman). 
    
    for(a in 1:6){
      #a<-1
      for(r in 1:Nstocks){
        
        # On July 1st, the number of migrating fish caught by the coastal trapnet 
        # fishery is determined by
        WCTN_Ctmp[a,y,r,2,]<-WsalmStock[a,y,r,2,]*
          exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]/12))*WCTN_HRtmp[a,y,r,2,]
        
        # The number of migrating fish by age in July after the coastal 
        # trapnet fishery is given by
        WsalmStock[a,y,r,2,]<-WsalmStock[a,y,r,2,]*
          exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]/12))-WCTN_Ctmp[a,y,r,2,]
      }  
      
      for(u in 1:4){
        #u<-1
        RCTN_Ctmp[a,y,u,2,]<-RsalmStock[a,y,u,2,]*
          exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]/12))*RCTN_HRtmp[a,y,u,2,]
        
        RsalmStock[a,y,u,2,]<-RsalmStock[a,y,u,2,]*
          exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]/12))-RCTN_Ctmp[a,y,u,2,]
      }
      
      # On August 1st, the number of migrating fish caught by the coastal gillnet 
      # fishery is determined by
      
      for(r in 1:Nstocks){
        WCGN_Ctmp[a,y,r,2,]<-WsalmStock[a,y,r,2,]*
          exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]/12))*WCGN_HRtmp[a,y,r,2,]
        
        # The number of migrating fish by age in August after the coastal gillnet 
        # fishery is given by
        WsalmStock[a,y,r,2,]<-(WsalmStock[a,y,r,2,]*
                                 exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]/12))-WCGN_Ctmp[a,y,r,2,])*
          ifelse(a==1,1,p.ladder[y,r,])
      } 
      
      for(u in 1:4){
        RCGN_Ctmp[a,y,u,2,]<-RsalmStock[a,y,u,2,]*
          exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]/12))*RCGN_HRtmp[a,y,u,2,]  
        
        RsalmStock[a,y,u,2,]<-RsalmStock[a,y,u,2,]*
          exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]/12))-RCGN_Ctmp[a,y,u,2,]
      }
    }
    
    # On October 1st, the number of migrating fish caught by the 
    # river fishery is determined by
    #WRF_Ctmp[,y,,2,]<-WsalmStock[,y,,2,]*exp(-(WsalmNatMort[,y,,2,]/6))*WRF_HRtmp[,y,,2,]
    #RRF_Ctmp[,y,,2,]<-RsalmStock[,y,,2,]*exp(-(RsalmNatMort[,y,,2,]/6))*RRF_HRtmp[,y,,2,]
    
    WRF_Ctmp[,y,,2,]<-WsalmStock[,y,,2,]*WRF_HRtmp[,y,,2,]
    RRF_Ctmp[,y,,2,]<-RsalmStock[,y,,2,]*RRF_HRtmp[,y,,2,]
    
    #The number of migrating fish by age in October after the river fishery 
    # indicate the number of spawners in the river. The number of spawners 
    # are saved within the FLStock object as the number at age of migrating fish 
    for(a in 1:6){
      WsalmStock[a,y,,2,]<-(WsalmStock[a,y,,2,]-WRF_Ctmp[a,y,,2,])*exp(-(WsalmNatMort[a,y,,2,]/6))*
        ifelse(a==1,1,surv_migr[y,r,])
    }
    
    RsalmStock[,y,,2,]<-(RsalmStock[,y,,2,]-RRF_Ctmp[,y,,2,])*exp(-(RsalmNatMort[,y,,2,]/6))
    
    # Pre-fishery abundances on Sept 1.
    # remove 6 months of natural mortality for the next calendar year
    
    # 1 SW salmon
    for(a in 1:1){
      
      # On January 1st, the number of fish at sea caught by the offshore driftnet 
      # fishery is determined by
      # 8/12 = May-Dec
      for(r in 1:Nstocks){
        PFAtmpW[a,y,r,1,]<-PropCW[y-a+6]*WsalmStock[a,y,r,1,]*
                          exp(-((WsalmNatMort[a,y,r,1,]*(11/12))+
                                  (WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12)+
                                  (WsalmNatMort[2,y,r,1,]*(4/12))))
        
        WODN_Ctmp[a,y,r,1,]<-WsalmStock[a,y,r,1,]*exp(-(WsalmNatMort[a,y,r,1,]*(7/12)))*
          exp(-(WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12))*WODN_HRtmp[a,y,r,1,]
        
        # The number of fish at sea by age in January after the offshore driftnet 
        # fishery. Post-smolts are assumed not to be affected by the offshore 
        # driftnet fishery with catches being 0 but they are affected by a high 
        # post-smolt mortality rate 
        WsalmStock[a,y,r,1,]<-WsalmStock[a,y,r,1,]*exp(-(WsalmNatMort[a,y,r,1,]*(7/12)))*
          exp(-(WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12))-WODN_Ctmp[a,y,r,1,]
      }
      for(u in 1:4){
        PFAtmpR[a,y,u,1,]<-PropCR[y-a+6]*RsalmStock[a,y,u,1,]*
                          exp(-((RsalmNatMort[a,y,u,1,]*(11/12))+
                                  (RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12)+
                                  (RsalmNatMort[2,y,u,1,]*(4/12))))
        
        RODN_Ctmp[a,y,u,1,]<-RsalmStock[a,y,u,1,]*exp(-(RsalmNatMort[a,y,u,1,]*(7/12)))*
          exp(-(RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12))*RODN_HRtmp[a,y,u,1,]
        
        RsalmStock[a,y,u,1,]<-RsalmStock[a,y,u,1,]*exp(-(RsalmNatMort[a,y,u,1,]*(7/12)))*
          exp(-(RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12))-RODN_Ctmp[a,y,u,1,]
      }
    }
    
    for(a in 2:6){
      PFAtmpW[a,y,,1,]<-PropCW[y-a+6]*WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*((8+6)/12)))
      PFAtmpR[a,y,,1,]<-PropCR[y-a+6]*RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*((8+6)/12)))
      
      # On January 1st, the number of fish at sea caught by the offshore driftnet 
      # fishery is determined by
      # 8/12 = May-Dec
      WODN_Ctmp[a,y,,1,]<-WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*(8/12)))*WODN_HRtmp[a,y,,1,] 
      RODN_Ctmp[a,y,,1,]<-RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*(8/12)))*RODN_HRtmp[a,y,,1,]
      
      # The number of fish at sea by age in January after the offshore driftnet 
      # fishery. Post-smolts are assumed not to be affected by the offshore 
      # driftnet fishery with catches being 0 but they are affected by a high 
      # post-smolt mortality rate 
      WsalmStock[a,y,,1,]<-WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*(8/12)))-WODN_Ctmp[a,y,,1,]
      RsalmStock[a,y,,1,]<-RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*(8/12)))-RODN_Ctmp[a,y,,1,]
    }
    
    # On February 1st, the number of fish at sea caught by the offshore longline 
    # fishery is determined by
    WOLL_Ctmp[,y,,1,]<-WsalmStock[,y,,1,]*exp(-(WsalmNatMort[,y,,1,]/12))*WOLL_HRtmp[,y,,1,]  
    ROLL_Ctmp[,y,,1,]<-RsalmStock[,y,,1,]*exp(-(RsalmNatMort[,y,,1,]/12))*ROLL_HRtmp[,y,,1,]
    
    # The number of fish at sea by age in February after the offshore 
    # longline fishery. Post-smolt are assumed not to be affected by the 
    # offshore longline fishery with catches being 0 but they are affected 
    # by a high post-smolt mortality rate
    WsalmStock[,y,,1,]<-WsalmStock[,y,,1,]*exp(-(WsalmNatMort[,y,,1,]/12))-WOLL_Ctmp[,y,,1,] 
    RsalmStock[,y,,1,]<-RsalmStock[,y,,1,]*exp(-(RsalmNatMort[,y,,1,]/12))-ROLL_Ctmp[,y,,1,]
    
    #just here to check egg prodn
    for(s in 1:sims[3]){
      for(r in 1:Nstocks){ 
        if(y>=3){
          X<- WsalmStock[,(y-2),r,2,s]
          W<- WsalmMatRate[,(y-2),r,2,s] 
          E<-as.vector(X*W)
          #SexRatio<-c(0,0.06,0.73,0.73,0.89,0.89) 
          #E<-E*SexRatio
          E<-E*prop_fem[(y-2),,r]
          E<-sum(E,na.rm=T)
          E<-E*(1-M74[s,(y-2)])      #formerly no M74 here 
          Etot_tmp[(y-2),r,s]<-E
        }
      }      
      j<-sims[1]-1+s  
      Mps_All[y,j]<-WsalmNatMort[1,y,1,1,s]
      Mps_AllR[y,j]<-RsalmNatMort[1,y,1,1,s]
      M74_All[y,j]<-M74[s,y]
    }
    
    if(y>(yBreak-1)){  
      # The number of fish at sea by age on May 1st in the next year. 
      # It is assumed that the post-smolt mortality affects the post-smolt 
      # for 1 full year, starting from May 1st till May 1st the next year.
      # The oldest salmon die.
      WsalmStock[2:6,(y+1),,1,]<-WsalmStock[1:5,y,,1,]*exp(-(WsalmNatMort[1:5,y,,1,]*3/12))
      RsalmStock[2:6,(y+1),,1,]<-RsalmStock[1:5,y,,1,]*exp(-(RsalmNatMort[1:5,y,,1,]*3/12))
    }
    
    WsalmStock[,y,,1,]<-tempW[,y,,1,]
    RsalmStock[,y,,1,]<-tempR[,y,,1,]
    
  }#end historic year loop 
  
  #!POPULATION MODEL ENDS HERE  
  #!##########################################################################
  
  BS_data <- c(
    "PropCR","PropCW",
    "PFAtmpW","PFAtmpR",
    "WsalmStock","RsalmStock","WsalmNatMort","RsalmNatMort",
    "WsalmMatRate","RsalmMatRate","F_seal","R_zero","BH_alpha","BH_beta","M74",
    "precisionBH", "BH_z","EffortICES", "EffortAssesUnit",
    "WOLL_HRtmp","WODN_HRtmp","WCDN_HRtmp","WCGN_HRtmp","WCTN_HRtmp","WRF_HRtmp", 
    "ROLL_HRtmp","RODN_HRtmp","RCDN_HRtmp","RCGN_HRtmp","RCTN_HRtmp","RRF_HRtmp", 
    "WOLL_Ctmp", "WODN_Ctmp", "WCDN_Ctmp", "WCGN_Ctmp", "WCTN_Ctmp", "WRF_Ctmp",
    "ROLL_Ctmp", "RODN_Ctmp", "RCDN_Ctmp", "RCGN_Ctmp", "RCTN_Ctmp", "RRF_Ctmp", 
    "yBreak", "sims", 
    "qlW", "qlR", "qdR","qdW",
    "qctnW","qctnR", "qcgnW", "qcgnR",
    "Mps_All","Mps_AllR","Etot_tmp","M74_All","prop_fem","p.ladder","surv_migr")
  
  save(list = BS_data, file = BH_dataFile)
  
} # END OF THE SIMULATION LOOP WHICH READS 100 ITER AT A TIME AND 
# RECORDS PARAM AND HIST VALUES...
# ==============================================================================

