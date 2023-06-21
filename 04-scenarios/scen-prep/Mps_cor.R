# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Medians and cv's of (instantaneous) post smolt mortality 
# for autokorrelation scenarios. 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
source("00-basics/run-this-first.R")
source("00-basics/packages.R")

Years<-c(1987:2022)

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# POST-SMOLT-MORTALITY, input (Wild smolts)
#load(file="C:/output/wgbast/flhm/chain_cleaned_2021.RData"); chains<-chains_new
load(file=paste0(PathOutput_FLHM,"FLHM_2023_rivHR_data2023_thin350.RData"))
chains<-as.mcmc(run) #pool 2 chains together
nchains<-1


if(nchains==1){
  MpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"]),length(Years)))
  for(y in 1:length(Years)){   
    MpsW[,y]<-chains[,str_c("MpsW[",y,"]")]
  }
}

# Even when 2 chains, better to pool than to use just 1 (unless perfect convergence/mixing)
# if(nchains==2){
#   MpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"][[1]]),length(Years)))
#   for(y in 1:length(Years)){   
#     MpsW[,y]<-chains[,str_c("MpsW[",y,"]")][[1]]
#   }
# }

MpsW[1:10,]
dim(MpsW)
lMpsW<-log(MpsW)

lMpsW_cor<-cor(lMpsW)
round(lMpsW_cor,3)


MpsMed<-vector()
MpsHigh<-vector()
MpsLow<-vector()
CV<-vector()
for(i in 1:(length(Years))){
  SummaryMps<-summary(as.mcmc(MpsW[,i]))
  MpsMed[i]<-SummaryMps$quantiles[3]
  CV[i]<-SummaryMps$statistics[2]/SummaryMps$statistics[1]
  MpsHigh[i]<-SummaryMps$quantiles[5]
  MpsLow[i]<-SummaryMps$quantiles[1]

}
# => MpsMed and CV will be used in model-Mps.R




# # Reared
# MpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years)))
# for(y in 1:length(Years)){   
#   MpsR[,y]<-chains[,str_c("MpsR[",y,"]")][[1]]
# }
# 
# lMpsR<-log(MpsR)
# 
# lMpsR_cor<-cor(lMpsR)
# round(lMpsR_cor,3)
# 
# MpsMedR<-vector()
# CVR<-vector()
# for(i in 1:(length(Years))){
#   i<-1
#   SummaryMpsR<-summary(as.mcmc(MpsR[,i]))
#   MpsMedR[i]<-SummaryMpsR$quantiles[3]
#   CVR[i]<-SummaryMpsR$statistics[2]/SummaryMpsR$statistics[1]
# }
# cbind(MpsMedR,CVR)

