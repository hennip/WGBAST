
###############################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Project: 		 Baltic salmon stock assessment (WGBAST)
#
# DESCRIPTION: Simulates stock projections for the future.
#
#
#
# R-file:		   ProjEffort.r

# input:
# output:
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

###############################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

rm(list=ls(all=T))
# library(coda)
# 
# # Paths are specified in a separate file 
# 
# 
# # # Becky:
#    PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
#    PathData<-"C:/WGBAST15/WGBAST_2025/data_2025/" # extra input files 
#    PathScen<-"C:/WGBAST15/2025_scenarios/" # scenario results 
#    PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Scenarios/"

#source("04-scenarios/paths_scens.r") #Henni
source("../run-this-first-wgbast.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location
   
# ===============

#Give a model name
assess_year<-2025   #Note! change back year for releases to last year of assessment
Model<-paste0(assess_year,"_JAGS_base4")

#stocknames<-read.table(paste0(PathData,"rivernames.txt")) # proper names
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=12),3,3,3,4,3)    #Ljungan from 4 to 3 2024
nsim<-1000

# Time
# =============================================================

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-assess_year-1
ymax<-350
LastPredYear<-LastHistYear+ymax

#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear

years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1)
#Years indicate the historic part of the model.
Years<-c(1992:LastHistYear)
#Define a year that separates historic part from future part
yBreak<-length(Years)
Nyears<-yBreak+NumFutYears

# =============================================================

MaxCoef<-10000 # Optimisation terminates, if a value higher than this is proposed for Coef2. 
              # if this happens, in practice it means that the target is higher than
              # the number of ish vulnerable to fishing
 
Optim<-T # Turns on secant method optimisation. Initial values are not too critical, could be = 1 for all,
         # but guessing improves the speed a bit. Does not work for trolling only scenario (6) at the moment!

set.seed(6789)

#! Removal scenarios for the future

RCzero<-T # See line 449 in ProjEffort_loops
zero_st<-c(4,9,15:17)  #stocks with no river F, note this will be 10% of HR for other stocks in 2023
#2025 R?ne, S?var?n, Em?n, K?ge, Test


#for(EffScen in c(1:2)){
#SD31only<-FALSE
#EffScen<-21
#for(EffScen in c(3:19)){
SD31only<-F
EffScen<-2

# workflow for effort scenarios:                                                      
# 1. Run scenarios 1 (zero fishing sea & river) and 2 (zero fishing at sea) with Optim=F
# 2. Set Optim=T. Run scenario 22 to find coef for reared trolling HR -> plug this value into ProjEffort_loops
# 3. Run scenario 21 to find coef for wild trolling HR -> plug this value into ProjEffort_loops

# For all scenarios, remember to update CoefF OR Coef2 after the desired level of effort
# has been found with the while-loop!

# Fishing scenarios
# ==============================
# Target is the total sea removal, including commercial and recreational,
# discards, unrep and misrep

if(EffScen==1){Coef2<-0; target<-0} # Zero fishing 
if(EffScen==2){Coef2<-0; target<-0} # River only/zero sea fishing

# 2021 fishing pattern
if(EffScen==3){Coef2<-0.7996726; target<-60} 
if(EffScen==4){Coef2<-1.85087; target<-100} 
if(EffScen==5){Coef2<-1.85087; target<-150} 

if(EffScen==6){Coef2<-0.360217; target<-20}    #River + coastal trapnetting only/no offshore fishing
if(EffScen==7){Coef2<-0.7396615; target<-40} 
if(EffScen==8){Coef2<-1.141809; target<-60}   #20 with SD31=T
if(EffScen==9){Coef2<-1.57125; target<-80} 
if(EffScen==10){Coef2<-2.027943; target<-100}
if(EffScen==11){Coef2<-1.57125; target<-120} 
if(EffScen==12){Coef2<-2.027943; target<-150}  

# Target means comm fisheries, trolling is set based on scens 21 & 22
if(EffScen==13){Coef2<-0.596537; target<-20}  #River + coastal trapnetting + recr trolling/no commerical offshore fisheries
if(EffScen==14){Coef2<-1.407341; target<-40} 
if(EffScen==15){Coef2<-0.596537; target<-60}  #River + coastal trapnetting + recr trolling/no commerical offshore fisheries
if(EffScen==16){Coef2<-1.407341; target<-80}
if(EffScen==17){Coef2<-0.596537; target<-100}  #River + coastal trapnetting + recr trolling/no commerical offshore fisheries
if(EffScen==18){Coef2<-1.407341; target<-120}
if(EffScen==19){Coef2<-1.407341; target<-150}  
  
# Extra scenarios to find out suitable level of trolling harvesting (W/R) 
if(EffScen==21){Coef2<-0.6127289; target<-2.03} #find wild trolling coef, plug in to ProjEffort_loops 2024 this is dead wild salmon only, value from run_Inputs
if(EffScen==22){Coef2<-2.951814; target<-4.91} #check, this is landed median from Tapani's BUGS model 

# =============================================================
# Set years in which the target should be met:
# calendar year 2026 is year 35 for trapnetting and 
# year 34 for offshore fisheries (updated 28/03/2024)
yCTN<-35 
yOLL<-34


# =============================================================
# Update level of effort based on most recent efforts in Effort_ICES.txt files (data-folder in dropbox)
# First value is for interim year (assessment year) and next for future years
if(EffScen %in% c(3:5)){   #2021 fishing pattern with longlining (2020 VALUES FROM EFFORT FILE)
E_OLL_DEN<-c(1.25,1.25) # hundred thousand hookdays, 2023     
E_OLL_PL<-c(7.42,7.42) # 2023  
}else{
E_OLL_DEN<-c(0,0) # hundred thousand hookdays, 2023    
E_OLL_PL<-c(0,0) # 2023     
}



if(SD31only==F){
E_CTN_FIN_30<-c(rep(1.29,2)) # thousand trapdays updated Mar 24
E_CTN_SWE_30<-c(rep(0.106,2))  
}else{
E_CTN_FIN_30<-c(1.29,0) # thousand trapdays   #scens 12-15 run as 5-7,9 above
E_CTN_SWE_30<-c(0.106,0)             
}

E_CTN_FIN_31<-c(rep(2.93,2))     #updated Mar 24 
E_CTN_SWE_31<-c(rep(6.53,2)) 


# Load SR errors
load(paste0(PathOut_Scen, "SR_devs_2025.RData"))

# =============================================================

# Initialise arrays
source(paste0(PathFiles,"InitArrays_2025.r")) # time varying Htr, ql, qd

# =============================================================

# Run projections

Sys.setlocale("LC_ALL","English")
source(paste0(PathFiles,"ProjEffort_loops_2025.r")) # time varying Htr, ql, qd


# =============================================================
#Combine relevant information
Perform_Stats <- c(
  "MW", "MR", "F_seal", "AU",
  "BHalpha", "BHbeta",
  "May1stW","May1stR",
  "ImmW_1", "ImmR_1", # Immature at may 1st
  "MatW_1", "MatR_1", # "MigrW","MigrR",
  "MatW_2", "MatR_2", # number ascending to rivers, history currently NA (if added, needs to be saved from Inputs)
  "MatW_3", "MatR_3", # Same as spW_age but indexes in a different order
  "spW_age", "spR_age",
  "R0","Etot",
  "MatRateW", "MatRateR",#"MaturationW","MaturationR",
  "postsmolts","postsmoltsR","postsmoltsW", "Mps_All", "Mps_AllR", "M74_All",
  "SmoltW", "SmoltR", "SpawnerW","SpawnerR", "PSW", "PSR",
  "EffortAU", #"EffortAssesUnit",
  "CatchRiver",
  "WOLL_HR","ROLL_HR",
  "WODN_HR","RODN_HR",
  "WCTN_HR","RCTN_HR",
  "WCGN_HR","RCGN_HR",
  "WCDN_HR","RCDN_HR",
  "WTR_HR", "RTR_HR",
  "CoastW_HR","CoastR_HR","OffsW_HR","OffsR_HR",
  "Migr_Tornio","Migr_Simo","Migr_AU1W","Migr_AU13W",
  "Migr_AU1R","Migr_AU13R","Migr_AU13tot",
  "MorrumSeaCatch","MorrumRiverCatch",
  "RiverCatchW","RiverCatchR",
  "PFAW", "PFAR", "PropCW", "PropCR",#"PFAW2",
  "WOLL_C", "ROLL_C", #  "WOLLCtot", "ROLLCtot",
  "WCTN_C", "RCTN_C", #  "WCTNCtot", "RCTNCtot"
  "WTR_C", "RTR_C"
)

Coef<-ifelse(iter==1,Coef2-0.1,Coef2)

if(SD31only==F){
# Save to RData-file
if(RCzero==T){File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")}
if(RCzero==F){File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")}
save(list = Perform_Stats, file = File)
save(Coef, file=paste0(PathScen,"Coef2_",Model,"_EScen",EffScen,".RData"))
# =============================================================
}else if(SD31only==T){
# Save to RData-file
if(RCzero==T){File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen+12,"_RCzero23-35.RData")}
if(RCzero==F){File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen+12,".RData")}
save(list = Perform_Stats, file = File)
save(Coef, file=paste0(PathScen,"Coef2_",Model,"_EScen",EffScen+12,".RData"))
}

#} #Scens loop


#
#
#crsum<-(apply(RiverCatchW[,33,,],c(2,3),sum))
#as.matrix(apply(crsum,c(1),median))
