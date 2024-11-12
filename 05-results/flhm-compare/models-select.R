library(tidyverse)
# Note! Comparison codes assume now each chains-variable to have only 1 chain
# If there's 2, combine them together to make things run smooth

# For gelman diagnostics to work, have 2-chain version of the model2



source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location


nstocks<-17

# Choose data
Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
Rivername_long<-read.table(str_c(PathData_FLHM, "rivernames.txt"))[,1]

# Model 1
# =================

#Cleaned version 2023
load(file=paste0(pathMain,"WGBAST_shared/flhm/2023/output/FLHM_2023_rivHR_data2023_thin350.RData"))
trolling1<-T;Mname1<-"2023 Model run"
#load("C:/Users/03195892/OneDrive - Valtion/WGBAST-antti/FLHM_2024/res_ORIG/FLHM_JAGS_2024_orig_data2024_run8.RData")
#trolling1=T;Mname1<-"2024 ORIG run"
chains1<-as.mcmc(run)
chains1_GR <- as.mcmc.list(run)
summary(chains1[,"MW"])

nchains1<-1
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-1


 # 2023 assessment data
   YearsB<-c(1987:2022)
   Years2B<-c(1992:2023)

  
  
    
# Model 2:
# =================
   #load(file=paste0(pathMain,"output/wgbast/flhm/2023/FLHM_2023_orig_data2023_thin100.RData")); trolling2<-T;Mname2<-"2023 orig"
   

# load(file=paste0(pathMain,"output/wgbast/flhm/2023/FLHM_2023_rivHR_data2023_thin100.RData")); trolling2<-T;Mname2<-"2023 rivHR"
# chains<-chainsGR<-window(as.mcmc.list(run), start=150000)
# chains<-chainsGR<-window(chains, thin=300)
# summary(chains[,"MW"])

#load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_orig_data2024.RData")); trolling2<-T
   
#load("C:/Users/03195892/OneDrive - Valtion/WGBAST-antti/FLHM_2024/res_ORIG/FLHM_JAGS_2024_orig_data2024_run8.RData")
#Mname2<-"2024 Model run (Ljungan in Southern river model)";trolling2<-T;trollingCT=F
load("C:/Users/03195892/OneDrive - Valtion/WGBAST-antti/FLHM_2024/res_CR/CR_2024_selected_chain.Rdata")
#load("C:/Users/03195892/OneDrive - Valtion/WGBAST-antti/FLHM_2024/res_CR/FLHM_JAGS_2024_CR_data2024_run9.RData")
Mname2<-"2024 CR trolling run";trolling2<-F;trollingCT=T

selCH=T

if(selCH==F){
  chainsGR<-as.mcmc.list(run)
  chains<-as.mcmc(run)
}
 #chains<-chainsGR<-window(chains, thin=700) #700=350*2

   

summary(chains[,"MW"])
nchains2<-1
nsims2<-ifelse(nchains2==1,
                 length(chains[,"MW"]),
                 length(chains[,"MW"][[1]])*2)
fix2<-0
  

  GR<-F#ifelse(nchains2==1,F,T)
  
  
# Is comparison for run from the same year? T if yes, F if no
# Note that older version should be set as Model 1
sameYear<-F
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2023)
  Years2<-c(1992:2024)
}


