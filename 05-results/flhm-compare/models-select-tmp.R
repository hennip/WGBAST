
# Note! Comparison codes assume now each chains-variable to have only 1 chain
# If there's 2, combine them together to make things run smooth

# For gelman diagnostics to work, have 2-chain version of the model2


library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

# setwd("C:/R/WGBAST/")
source("00-functions/tidy-functions_2chains.r")
source("C:/tmp/path-main.r")

nstocks<-17

# Choose data
pathData<-paste0(pathMain,"WGBAST_shared/data_2022/")

Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
Rivername_long<-read.table(str_c(pathData, "rivernames.txt"))[,1]

# Model 1
# =================

# # Piece 1: 1987-2007 of 2022 run
# load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_1987-2007_data2022_data2022.RData")); trolling1<-T;Mname1<-"2022 base, piece 1"
# chains1<-as.mcmc.list(run)
# nchains1<-2
# nsims1<-ifelse(nchains1==1,
#                length(chains1[,"MW"]),
#                length(chains1[,"MW"][[1]])*2)
# fix1<-1 # wprop needs years -1 for some reason...
# 
# # 2021 assessment data
# YearsB<-c(1987:2007)
# Years2B<-c(1992:2007)


# # 2021 assessment model version (final)
# load(file=paste0(pathMain,"WGBAST_shared/flhm/2021/chains_cleaned_2503.RData")); trolling1<-T;Mname1<-"2021 assessment model"
# chains1<-chains_new#as.mcmc.list(run)
# #summary(chains1[,"MW"])
# nchains1<-1
# nsims1<-ifelse(nchains1==1,
#              length(chains1[,"MW"]),
#              length(chains1[,"MW"][[1]])*2)
# fix<-0
# 
# 
# # 2021 assessment data
#   YearsB<-c(1987:2020)
#   Years2B<-c(1992:2020)

#Cleaned version 2022 (no stucked chains)
# load(file=paste0(pathMain,"WGBAST_shared/flhm/2022/FLHM_2022_results_cleaned.RData")); trolling1<-T;Mname1<-"2022 base model, cleaned"
# chains1<-chains_new
# summary(chains1[,"MW"])
# 
# nchains1<-1
# nsims1<-ifelse(nchains1==1,
#                length(chains1[,"MW"]),
#                length(chains1[,"MW"][[1]])*2)
# fix1<-0



load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_BetaApprox_data2022.RData")); trolling1<-T;Mname1<-"BetaApprox"
chains1<-chainsGR<-as.mcmc.list(run)
chains1<-chainsGR<-window(chains1, start=50000)
summary(chains1[,"MW"])
#sims<-500
nchains1<-2
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-0



 # 2022 assessment data
   YearsB<-c(1987:2021)
   Years2B<-c(1992:2021)

  
  
    
# Model 2:
# =================

# # Piece 1: 1987-2007 of 2022 run
# load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_1987-2007_data2022_data2022.RData")); trolling2<-T;Mname2<-"2022 base, piece 1"
# chains<-chainsGR<-as.mcmc.list(run)
# nchains2<-2
# nsims2<-ifelse(nchains2==1,
#                length(chains[,"MW"]),
#                length(chains[,"MW"][[1]])*2)
# fix2<-1 # wprop needs years -1 for some reason...


  # Cleaned version (no stucked chains)
  # load(file=paste0(pathMain,"WGBAST_shared/flhm/2022/FLHM_2022_results_cleaned.RData")); trolling2<-T;Mname2<-"2022 base model, cleaned"
  # chains<-chainsGR<-chains_new
  # summary(chains[,"MW"])
  # 
  # nchains2<-1
  # nsims2<-ifelse(nchains2==1,
  #                length(chains[,"MW"]),
  #                length(chains[,"MW"][[1]])*2)
  # fix2<-0
  # 
  # 
  
  #load(file=paste0(pathMain,"WGBAST_shared/flhm/2022/FLHM_2022_orig_data2022.RData")); trolling2<-T;Mname2<-"2022 base"
  #load(file=paste0(pathMain,"WGBAST_shared/flhm/2022/FLHM_2022_rivHR_data2022.RData")); trolling2<-T;Mname2<-"2022 river HR"
  #chainsGR<-as.mcmc.list(run) # for convergence diagnostics only
  #chains<-as.mcmc(run)
  #summary(chains[,"MW"])

   # Wprop corrected: stucked Wprop at 2011 & 2016 after 150k + 500*350  
#load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_corrected_data2022.RData")); trolling2<-T;Mname2<-"2021 assessment model"

#load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wproptest_data2022.RData")); trolling2<-T;Mname2<-"2021 assessment model"
#load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_corrected_bwr100_data2022.RData")); trolling2<-T;Mname2<-"2021 assessment model"

   #load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_corrected_bwr10_2_data2022.RData")); trolling2<-T;Mname2<-"2021 assessment model"
   #load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_corrected_bwr10_2_3010_data2022.RData")); trolling2<-T;Mname2<-"2021 assessment model" # TOSI MONESSA WPROP KETJUSSA ONGELMIA!!!
   #chains<-chainsGR<-as.mcmc.list(run)
   #chains<-chainsGR<-window(chains, start=130000)
   
   
   
   
load(file=paste0(pathMain,"output/wgbast/flhm/2022/FLHM_2022_orig_Wprop_BetaApprox_1_data2022.RData")); trolling2<-T;Mname2<-"BetaApprox, small changes (Becky)"
chains<-chainsGR<-as.mcmc.list(run)
chains<-chainsGR<-window(chains, start=50000)

   

#chains<-chainsGR<-chains_new
#plot(chains[,"MW"])
summary(chains[,"MW"])
#sims<-500
nchains2<-2
nsims2<-ifelse(nchains2==1,
                 length(chains[,"MW"]),
                 length(chains[,"MW"][[1]])*2)
fix2<-0
  

  GR<-F#ifelse(nchains2==1,F,T)
  
  
# Is comparison for run from the same year? T if yes, F if no
# Note that older version should be set as Model 1
sameYear<-T
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2021)
  Years2<-c(1992:2021)
}


