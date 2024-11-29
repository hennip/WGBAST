
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

#Cleaned version 2024 (no stucked chains)
load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/CR_2024_selected_chain.RData")); trolling1<-T;Mname1<-"2024 base model, cleaned"
load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_spcount_data2024.RData")); trolling1<-T;Mname1<-"2024 Simo didson count removed"


chains1<-chains
summary(chains1[,"MW"])

nchains1<-1
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-1


 # 2022 assessment data
   YearsB<-c(1987:2023)
   Years2B<-c(1992:2023)

  
  
    
# Model 2:
# =================

   load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_coefDSHIER_data2024.RData")); trolling2<-T;Mname2<-"2024, Hierarchical priors over years for CoefDS"
   #load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_ar_data2024.RData")); trolling2<-T;Mname2<-"2024, AR model for Mps"
#load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_sp_ar_data2024.RData")); trolling2<-T;Mname2<-"2024 Simo didson count removed, AR model for Mps"
#load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_spcount_data2024.RData")); trolling2<-T;Mname2<-"2024 Simo didson count removed"
chains<-chainsGR<-as.mcmc.list(run)
#chains<-chainsGR<-window(chains, start=100000) #700=350*2

   

summary(chains[,"MW"])
nchains2<-2
nsims2<-ifelse(nchains2==1,
                 length(chains[,"MW"]),
                 length(chains[,"MW"][[1]])*2)
fix2<-0
  

  GR<-T#ifelse(nchains2==1,F,T)
  
  
# Is comparison for run from the same year? T if yes, F if no
# Note that older version should be set as Model 1
sameYear<-T
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2022)
  Years2<-c(1992:2022)
}


