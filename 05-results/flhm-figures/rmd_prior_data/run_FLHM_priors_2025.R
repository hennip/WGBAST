
#river migration survival by stock/AU - currently wild stocks only (check) 
#Only Ume/Vindel currently has reduced spawner survival in some years
#Note that if extra mortality is added for wild stocks 1-4 in years with tagging data, 
#TxWs need to move into a loop where s is stock index, not AU - do something about this - use average surv_migr over stocks in each AU?
#In this version river is survrR or survrW and surv are re-numbered

#Now HrW instead of HrR for reared spawners (Torne and Simo) in population dynamics (tagged and untagged)
#Parr added to NrRsp not NrW (wild)

#flexible indexing not complete!!

#library(R2jags)
#library(runjags)
#library(rjags) 

load.module("mix")
load.module("glm")



#assessment_year<-2023

years<-length(seq(1987:assessment_year))
proj_years<-0
maxage<-6
rstocks<-2 #Lule, Dal
stock_indices<-c(1:17)
#NB if run with Torne and Simo they should have positions 1 and 2 because of exceptions for these rivers
stocks<-length(stock_indices)
allstocks<-17 #number of stocks in data files
AUS<-4
trolling<-1

##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixalven"
#4 "Ranealven"
#5 "Pitealven"
#6 "Abyalven"
#7 "Byskealven"
#8 "Ricklean"
#9 "Savaran"
#10 "Vindelalven"
#11 "Orealven"
#12 "Logdealven"
#13 "Ljungan"
#14 "Morrumsan"
#15 "Eman"
#16 "Kagealven"
#17 "Testeboan"

#source("/home/henni/WGBAST/03-histmodel/paths_FLHM.R") #AMD
#source("/home/ubuntu/WGBAST/03-histmodel/paths_FLHM.R") #cpouta
#source("03-histmodel/paths_FLHM.R") #windows

#source(paste0(PathFunctions,"plotfunctions.r"))
#source("03-histmodel/figures/modded for rmd/make_JAGS_data_2024.R")
#source("05-results/flhm-figures/rmd_prior_data/make_JAGS_data_2024.R")
source("03-flhm/make_JAGS_data_2025.R")


datalist<-list(stocks=stocks,M_R0=M_K[stock_indices],tau_R0=tau_K[stock_indices])  

inits.fn<-function() {
  list(    K=rlnorm(length(stock_indices),M_K[stock_indices]*1.1,0.50)) 
}

priors<-"model{
  for (s in 1:stocks){
    K[s]~dlnorm(M_R0[s],tau_R0[s])     #M_R0 and tau_R0 come from the data file K_prior
  }
}"



parnames<-c("K")



initsall<-list(inits.fn(),inits.fn())


#print(paste0(modelName,"_data", assessment_year))

jm=jags.model(textConnection(priors),data=datalist,n.chains=1)
chainsP=coda.samples(jm,c("K"),n.iter=100000)

#summary(chainsP)

