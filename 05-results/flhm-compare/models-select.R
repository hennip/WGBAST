
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

#Cleaned version 2022 (no stucked chains)
load(file=paste0(pathMain,"WGBAST_shared/flhm/2024/output/CR_2024_selected_chain.RData")); trolling1<-T;Mname1<-"2022 base model, cleaned"


chains1<-chains_new
summary(chains1[,"MW"])

nchains1<-1
nsims1<-ifelse(nchains1==1,
               length(chains1[,"MW"]),
               length(chains1[,"MW"][[1]])*2)
fix1<-1


 # 2022 assessment data
   YearsB<-c(1987:2021)
   Years2B<-c(1992:2021)

  
  
    
# Model 2:
# =================
   #load(file=paste0(pathMain,"output/wgbast/flhm/2023/FLHM_2023_orig_data2023_thin100.RData")); trolling2<-T;Mname2<-"2023 orig"
   

# load(file=paste0(pathMain,"output/wgbast/flhm/2023/FLHM_2023_rivHR_data2023_thin100.RData")); trolling2<-T;Mname2<-"2023 rivHR"
# chains<-chainsGR<-window(as.mcmc.list(run), start=150000)
# chains<-chainsGR<-window(chains, thin=300)
# summary(chains[,"MW"])

load(file=paste0(pathMain,"output/wgbast/flhm/2023/FLHM_2023_rivHR_data2023_thin350.RData")); trolling2<-T;Mname2<-"2023 rivHR"
chains<-chainsGR<-as.mcmc.list(run)
 #chains<-chainsGR<-window(chains, thin=700) #700=350*2

   

summary(chains[,"MW"])
nchains2<-2
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
  Years<-c(1987:2022)
  Years2<-c(1992:2022)
}


