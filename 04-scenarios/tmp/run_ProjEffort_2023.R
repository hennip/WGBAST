
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
library(coda)

source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location

# ===============

Model<-"2023" # Assessment model version

#stocknames<-read.table(paste0(PathData,"rivernames.txt")) # proper names
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=13),3,3,4,3)
nsim<-1000

# Time
# =============================================================

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-2022
ymax<-40
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

#for(EffScen in 1:6){
set.seed(6789)

#! Removal scenarios for the future
#EffScen<-3
RCzero<-T # See line 449 in ProjEffort_loops
zero_st<-c(9,10,13,15:17)  #stocks with no river F, note this will be 10% of HR for other stocks in 2023
#S?v, Ume, Ljung, Em?n, K?ge, Test
#for(EffScen in 1:9){
EffScen<-21
# workflow for effort scenarios:
# 1. Run scenarios 1 (zero fishing sea & river) and 2 (zero fishing at sea)
# 2. Run scenario 22 to find coef for reared trolling HR -> plug this value
# 3. Run scenario 21 to find coef for wild trolling HR

# OLD WORKFLOW
# 1. Run scenario 5 and ScenarioTable.R for that scenario -> input total PFA to cell R4 in T4321_workflow.xlsx
# 2. Run scenario 6: Set up targetTr that corresponds to recr removal (V21 in T4321_workflow.xlsx). 
#    Then find such CoefTrollingF that produces targetTr catch.
#     Note that optimisation method is not currently used for this scenario.
# 3. Run scenario 1: Set up target as in W21 (Total removal at sea for scen 1) in T4321_workflow.xlsx.
#   Then find such Coef2 that produces target catch, update that on line 97 below as "Coef2"
# 4. Run scens 2/3/4 similarly as scen 1 and adjust Coef2 in corresponding line 


# For all scenarios, remember to update CoefF OR Coef2 after the desired level of effort
# has been found with the while-loop!

# Fishing scenarios
# ==============================
# Target is the total sea removal, including commercial and recreational,
# discards, unrep and misrep

if(EffScen==1){Coef2<-0; target<-0} # Zero fishing (sea & river)
if(EffScen==2){Coef2<-0; target<-0} # Zero sea fishing

# 2021 fishing pattern
if(EffScen==3){Coef2<-0.5700333; target<-50} 
if(EffScen==4){Coef2<-1.205542; target<-100} 

# Without any offshore fisheries
if(EffScen==5){Coef2<-1.934167; target<-20} 
if(EffScen==6){Coef2<-2.796902; target<-40} 
if(EffScen==7){Coef2<-1.934167; target<-60} 
if(EffScen==8){Coef2<-2.796902; target<-80} 
if(EffScen==9){Coef2<-2.796902; target<-100} 

# No commercial offshore fisheries but with recr trolling
# Target means comm fisheries, trolling is set based on scens 21 & 22
if(EffScen==10){Coef2<-2.464334; target<-40} 
if(EffScen==11){Coef2<-2.464334; target<-80} 

# Extra scenarios to find out suitable level of trolling harvesting (W/R) 
if(EffScen==21){Coef2<-0.6127289; target<-6.51} #find wild trolling coef
if(EffScen==22){Coef2<-2.951814; target<-10.79} #find reared trolling coef  
#2.967145

#Coef1<-1

# =============================================================
# Set years in which the target should be met:
# calendar year 2024 is year 33 for trapnetting and 
# year 32 for offshore fisheries (updated 24/03/2023)
yCTN<-33 
yOLL<-32


# =============================================================
# Update level of effort based on most recent efforts in Effort_ICES.txt files (data-folder in dropbox)
# First value is for interim year (assessment year) and next for future years
E_OLL_DEN<-c(1.27,1.27) # hundred thousand hookdays, 2021
#E_OLL_DEN<-c(0,0) # hundred thousand hookdays, 2022

E_OLL_PL<-c(0.493,0.493) #2021
#E_OLL_PL<-c(0,0)  #2022
#E_OLL_TROLLING<-c(5.34,5.34)

E_CTN_FIN_30<-c(rep(3.358,2)) # thousand trapdays
E_CTN_SWE_30<-c(rep(2.094,2))                 
E_CTN_FIN_31<-c(rep(5.930,2))       
E_CTN_SWE_31<-c(rep(5.258,2)) 


# Load SR errors
load(paste0(PathFiles, "SR_devs_2023.RData"))

# =============================================================

# Initialise arrays
source(paste0(PathFiles,"InitArrays_2023.R")) # time varying Htr, ql, qd

# =============================================================

# Run projections
source(paste0(PathFiles,"ProjEffort_loops_2023.R")) # time varying Htr, ql, qd


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

# Save to RData-file
if(RCzero==T){File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")}
if(RCzero==F){File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,".RData")}
save(list = Perform_Stats, file = File)

Coef<-Coef2-0.1
save(Coef, "Coef", file=paste0(PathOut_Scen,"Coef2_",Model,"_EScen",EffScen,".RData"))
# =============================================================
#}
