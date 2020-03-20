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

# Henni:
source("C:/Users/412hpulkkin/Documents/Projects/WGBAST/04-scenarios/paths_scens_HP.r")

#Give a model name
Model<-"2019_LR_EPR"
select_case<-2 #new SR parameterisation 

# Fetch JAGS model
load(file=paste0(PathSim,"FLHM_results_2019_extended2019-04-11.RData"))
#load(file=paste0(PathSim,"FLHM_results_2019_R0_corrected_2019-09-18.RData"))
#chains<-chains_new                       
d<-as.matrix(chains)

#RiverNames<-c("Torne", "Simo","Kalix","R?ne","Pite","?by","Byske","Rickle","S?var?n",
#             "Ume","?re","L?gde","Ljungan","M?rrum","Em?n","K?ge","Test")

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
LastHistYear<-2018    
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

pmat<-array(0,dim=c(years[3],6,100))  
pimm<-array(0,dim=c(years[3],6,100))  
simm<-array(0,dim=c(years[3],6,Nstocks,100))   
smat<-array(0,dim=c(years[3],6,Nstocks,100)) 
EPR<-array(0,dim=c(years[3],Nstocks,100)) 
EPR_M74<-array(0,dim=c(years[3],Nstocks,100))  

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
  for(r in 11:17){
    prop_fem[y,2:6,r]<-prop_fem_tmp
  }
}
#prop_fem[,,1]
#prop_fem[,,10]

source(paste0(PathFiles,"ProjEffort_loops.r"))


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
  "Mps_All","Mps_AllR","Etot_tmp","M74_All","prop_fem","p.ladder","surv_migr",
  "pmat","pimm","smat","simm","EPR","EPR_M74","K")
