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

#Cleaned version 2025 (no stucked chains)
load(file=paste0("../../WGBAST_shared/flhm/2025/output/chain_cleaned_2025_base4.RData")); trolling1<-T;Mname1<-"2025 assessment model (base4), cleaned"

#chains1<-chains
chains1<-chains_new

summary(chains1[,"MW"])

nchains1<-1
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-1


YearsB<-c(1987:2024)
Years2B<-c(1992:2024)

  
  
    
# Model 2:
# =================

load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2025_base4_data2025.RData")); trolling2<-T;Mname2<-"2025 base4 model, long run"

Rane_sp<-T

# selCH=F
# 
# if(selCH==F){
#   chainsGR<-as.mcmc.list(run)
#   chains<-as.mcmc(run)
# }
 #chains<-chainsGR<-window(chains, thin=700) #700=350*2
chains<-chainsGR<-as.mcmc.list(run)
chains<-chainsGR<-window(chains, start=600000)

   

summary(chains[,"MW"])
nchains2<-2
nsims2<-ifelse(nchains2==1,
                 length(chains[,"MW"]),
                 length(chains[,"MW"][[1]])*2)
fix2<-0
  
GR<-ifelse(nchains2==1,F,T)

# Is comparison for run from the same year? T if yes, F if no
# Note that older version should be set as Model 1
sameYear<-T
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2024)
  Years2<-c(1992:2025)
}


