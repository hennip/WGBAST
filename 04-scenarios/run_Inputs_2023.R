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

source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location


#Give a model name
Model<-"2023"

# Fetch JAGS model
# This remains to be configured, it's 4000 iter now
load(file=paste0(PathSim,"FLHM_2023_rivHR_data2023_thin350.RData"))
#chains<-as.mcmc.list(run)
chains<-as.mcmc(run)


d<-as.matrix(chains)
keep.sample<-seq(4,4000,by=4)
d<-d[keep.sample,]
nsim<-dim(d)[1]

#RiverNames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
#             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks

set.seed(12345) #set the random number seed
################################################################################

# Set the last year for historic part and the last year for predictions:
LastHistYear<-2022
ymax<-320


#128
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

# Update Mps scenario params
# =====================
#! Last update 21/03/2023
mu_mps<- -1.849 # median survival 2016-2019, 16%
w_mps<-0.94
sigma2_mps<-0.52

# mu<--1.764    # median survival 2018-2021, 17% (=exp(mu))
# w<-0.94      
# sigma2<-0.514

# These are the parameters of a beta distribution which adjust MpsR depending on MpsW
#! Update annually using script Ra_Rb.r
#! Updated 21/03/2023
Rmu<-0.247
Reta<-0.215

# Update M74 scenario params
# =====================
#updated 21/03/2023
mu_m74<- -2.139
w_m74<-0.858
sigma2_m74<-0.272 #previously 1.735, a bit surprising difference...

# Update sea surface temperature params
# =====================
#updated 23/03/2021
#mean_Temp1<-5.078; sd_Temp1<-0.1268 # Assessment year -1 : use only if full stock assessement is not updated (like in 2020)
mean_Temp2<-5.62; sd_Temp2<-0.221 # Assessment year
mean_Temp3<-4.29; sd_Temp3<-1.031 # Future

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
    prop_fem[y,3:6,10]<-rep(mean(Ume_prop_fem[(yBreak-1):(yBreak+1)]),4)}   #average of last 3 years
  # 2020 assessment: use the following but then change back to above!
    # This because yBreak is the same as prev assessment but data has updated
     #prop_fem[y,3:6,10]<-rep(mean(Ume_prop_fem[(yBreak):(yBreak+2)]),4)}   #average of last 3 years
for(r in 11:17){
    prop_fem[y,2:6,r]<-prop_fem_tmp
  }
}
#prop_fem[,,1]
#prop_fem[,,10]

source(paste0(PathFiles,"Inputs_loops_2023.R"))


