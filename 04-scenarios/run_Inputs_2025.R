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
rm(list=ls(all=T))

source("../run-this-first-wgbast.R") 

# ONCE EVERYTHING WORKS FOR EVERYBODY, THIS BIT CAN BE REMOVED
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# library(coda)
# library(abind)  #for M74 
# library(reshape2) #for seal M
# 
# library(runjags)
# 
# #!! update Mps, Ra/Rb, M74, TEMP! Check OLL effort 0 for all (Swe, Fin, Den, Pol) in 2023
# 
# # # Becky:
#    PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
#    PathData<-"C:/WGBAST15/WGBAST_2024/data_2025/" # extra input files 
#    PathScen<-"C:/WGBAST15/2025_scenarios/" # scenario results 
#    PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Scenarios/"

# 
# PathSim<-PathOut_FLHM # simulations for scens read from where those were stored from FLHM
# PathData_Scen<-PathData_FLHM# currently the same as for FLHM data
# PathFiles<-"04-scenarios/" # Relative path, could be removed and used directly 
# PathOut_Scen<-"../../WGBAST_shared/scen/2025/" # scenario results written here. NOTE! Better not to write directly to shared folder, only copy-paste there final files

# Henni:
#source(paste0("/04-scenarios/paths_scens.r"))
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#Give a model name
assess_year<-2025
Model<-paste0(assess_year,"_JAGS_base4")
#Model<-paste0(assess_year,"_Nimble")

# Fetch model
# This remains to be configured, it's 4000 iter now
load(file=paste0(PathSim,"chain_cleaned_2025_base4.RData"))
Nimble<-grepl("Nimble",Model)

if(Nimble){
v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)
chains<-mcmc.list(list(v1,v2)) 
}else{
#chains<-run$mcmc #JAGS
chains<-chains_new
}

d<-as.matrix(chains)
#keep.sample<-seq(7,7000,by=7)  #keep 1000 samples for scens
#keep.sample<-seq(3,6000,by=3)  #keep 2000 samples for scens
d<-d[1001:2000,]
nsim<-dim(d)[1]

# nctW_rel[i] <-  nctW_Tot[i]*p.rel[i] 
pm<-d[,grep("p.mort",colnames(d))]
pr<-d[,grep("p.rel[36]",colnames(d),fixed=T)] 
ntW<-d[,grep("nctW_Tot[36]",colnames(d),fixed=T)] 
#nt<-d[,grep("nct_Tot[36]",colnames(d),fixed=T)]
median(ntW*(1-pr)+ntW*pr*pm) #2.03 selected chain results                     #these go to runProjEffort...
#median(nt-ntW)  #reared

                                      #2024 assess ind36=model year 2022/cal year 2023
#RiverNames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
#             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
e_delay<-c(rep(4,times=12),3,3,3,4,3)    #Ljungan from 4 to 3 2024

set.seed(12345) #set the random number seed
################################################################################

# Set the last year for historic part and the last year for predictions:
LastHistYear<-assess_year-1
ymax<-350        

LastPredYear<-LastHistYear+ymax
ymaxBH<-length(c(1987:LastHistYear))

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

#source(file=paste0(PathFiles,"Generate_error_SR.r"))

# Update Mps scenario params
# =====================
#! Last update 08/04/2024
mu_mps<- -2.28 # median survival 2020-2023 13%
w_mps<-0.95
sigma2_mps<-0.55

# These are the parameters of a beta distribution which adjust MpsR depending on MpsW
#! Update annually using script Ra_Rb.r
#! Updated 08/04/2024
Rmu<-0.26
Reta<-0.15

# Update M74 scenario params
# =====================
#updated 08/04/2024
mu_m74<- -2.203   #prev -2.139
w_m74<-0.869
sigma2_m74<-0.395 #previously 1.735, a bit surprising difference...

# Update sea surface temperature params
# =====================
#updated Apr-24
#mean_Temp1<-5.078; sd_Temp1<-0.1268 # Assessment year -1 : use only if full stock assessement is not updated (like in 2020)
mean_Temp2<-4.258; sd_Temp2<-0.1753 # Assessment year   	mu[33,4] 4.258	0.1753	
mean_Temp3<-4.219; sd_Temp3<-1.034 # Future    	mu[34,4]	4.219	1.034

# Adjust units 5 and 6
# =====================
PropCR<-read.table(paste0(PathData_Scen, "PropAU16.txt"),header=T)[,1]
PropCW<-read.table(paste0(PathData_Scen, "PropAU16.txt"),header=T)[,2]

# Repeat the last for the future years 
# 2020 assessment: This may be +1 in length, might not bother but in case discrepancy then change
PropCW<-c(PropCW, rep(PropCW[length(PropCW)],(years[3]-yBreak)))
PropCR<-c(PropCR, rep(PropCR[length(PropCR)],(years[3]-yBreak)))
# =====================

# The model includes 4 different assessment units with wild rivers. 
# Unit 1 includes Torne, Simo, Kalix and Rane
# Unit 2 includes Pite, Aby, Byske, Rickle, Savaran, Ume, Ore, Logde and Kage 
# Unit 3 includes Ljungan and Testeboan 
# Unit 4 includes Morrum and Eman
units<-c(1,4)
units<-c(units[],units[2]-units[1]+1)

Mps_All<-array(NA,dim= c(years[3],nsim))
Mps_AllR<-array(NA,dim= c(years[3],nsim))
M74_All<-array(NA,dim= c(years[3],nsim))
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
seals<-as.matrix(read.table(paste0(PathData_Scen, "scenarios_Fseal.txt")))
F_seal<-array(1,dim=c(years[3],6,4))


for(i in 1: dim(seals)[1]){
  for(u in 1:3){
    F_seal[i,2:6,u]<-rep(seals[i],times=5)
  }
}
for(i in (dim(seals)[1]+1):years[3]){
  for(u in 1:3){
    F_seal[i,2:6,u]<-rep(seals[dim(seals)[1]],times=5)
  }
}

#set up sex ratios   
Ume_prop_fem<-as.matrix(read.table(paste0(PathData_Scen,"MSW_prop_fem_Ume_Vindel.txt"),row.names=1))

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
    prop_fem[y,3:6,10]<-rep(mean(Ume_prop_fem[(yBreak-2):(yBreak)]),4)}   #average of last 3 years
  # 2020 assessment: use the following but then change back to above!
    # This because yBreak is the same as prev assessment but data has updated
     #prop_fem[y,3:6,10]<-rep(mean(Ume_prop_fem[(yBreak):(yBreak+2)]),4)}   #average of last 3 years
for(r in 11:17){
    prop_fem[y,2:6,r]<-prop_fem_tmp
  }
}
#prop_fem[,,1]
#prop_fem[,,10]


rivHRall<-as.matrix(read.table(paste0(PathData_Scen,"rivHR.txt"),header=T,row.names=1))
colnames(rivHRall)<-NULL      
rownames(rivHRall)<-NULL 

rivHR<-rivHRall[6:dim(rivHRall)[1], ]  #1992 ->

source("04-scenarios/Inputs_loops_2025.r")


