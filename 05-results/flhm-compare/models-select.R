# Note! Comparison codes assume now each chains-variable to have only 1 chain
# If there's 2, combine them together to make things run smooth

# For gelman diagnostics to work, have 2-chain version of the model2


source("../run-this-first-wgbast.R")

nstocks<-17

# Choose data
Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
Rivername_long<-read.table(str_c(PathData_FLHM, "rivernames.txt"))[,1]

# Model 1
# =================

#Cleaned version 2024 (no stucked chains)
load(file=paste0(PathOut_FLHM,"CR_2024_selected_chain.RData")); trolling1<-T;Mname1<-"2024 base model, cleaned"
#load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2024_CR_spcount_data2024.RData")); trolling1<-T;Mname1<-"2024 Simo didson count removed";chains<-as.mcmc.list(run)
#load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2024_CR_coefDSEE_data2024.RData")); trolling1<-T;Mname1<-"2024 Simo didson new expert priors, no hierarchy";chains<-as.mcmc.list(run)


chains1<-chains

summary(chains1[,"MW"])

nchains1<-1
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-1


YearsB<-c(1987:2023)
Years2B<-c(1992:2023)

  
  
    
# Model 2:
# =================

#load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_coefDSHIER_data2024.RData")); trolling2<-T;Mname2<-"2024, Hierarchical priors over years for CoefDS"
#load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/FLHM_JAGS_2024_CR_ar_data2024.RData")); trolling2<-T;Mname2<-"2024, AR model for Mps"
#load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2024_CR_coefDS_data2024.RData")); trolling2<-T;Mname2<-"2024, prior given to CoefDS"

#load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2024_CR_coefDShierEE_data2024.RData")); trolling2<-T;Mname2<-"2024 Simo didson new expert priors with hierarchy"
load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2025_CR_river_catch1_coefDSEE_data2024.RData")); trolling2<-T;Mname2<-"2024 Simo CoefDS prior + river specific HR"

selCH=T

if(selCH==F){
  chainsGR<-as.mcmc.list(run)
  chains<-as.mcmc(run)
}
 #chains<-chainsGR<-window(chains, thin=700) #700=350*2
chains<-chainsGR<-as.mcmc.list(run)
chains<-chainsGR<-window(chains, start=100000) #700=350*2

   

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
  Years<-c(1987:2023)
  Years2<-c(1992:2024)
}


