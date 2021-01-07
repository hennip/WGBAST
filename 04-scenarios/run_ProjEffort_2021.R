
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


# Paths are specified in a separate file 

source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r") #Henni
#source("C:/models/WGBAST_optim/paths_scens.r")# Samu

# ===============

#Model<-"2020" # Assessment model version, hist model from 2019 assessment
Model<-"2020_updated" # Assessment model version, updated with 2019 data

#stocknames<-read.table(paste0(PathData,"rivernames.txt")) # proper names
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=13),3,3,4,3)
nsim<-1000

# Time
# =============================================================

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-2019
#LastPredYear<-2032
ymax<-15
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
EffScen<-1

# workflow for effort scenarios:
# 1. Run scenario 5 and ScenarioTable.R for that scenario -> input total PFA to cell R4 in T4321_workflow.xlsx
# 2. Run scenario 6: Set up targetTr that corresponds to recr removal (V21 in T4321_workflow.xlsx). 
#    Then find such CoefTrollingF that produces targetTr catch.
#     Note that optimisation method is not currently used for this scenario.
# 3. Run scenario 1: Set up target as in W21 (Total removal at sea for scen 1) in T4321_workflow.xlsx.
#   Then find such Coef2 that produces target catch, update that on line 74 below as "Coef1"
# 4. Run scens 2/3/4 similarly as scen 1 but adjust only Coef2 in corresponding line 
#   (Coef1 remains the same).

# For all scenarios, remember to update CoefF OR Coef1 OR Coef2 after the desired level of effort
# has been found with the while-loop!


Coef1<-1 # This can be 1!
CoefTrollingF<-1.3 # # this coef produces the targetTr of recr catch when commercial fisheries are removed 

targetTr<-26.7 # Target trolling catch
# Target is the total removal, including commercial and recreational,
# discards, unrep and misrep
if(EffScen==1){Coef2<-2.686; target<-142.7} # previous advice approach
if(EffScen==2){Coef2<-2.78; target<-165.9} #+20%
if(EffScen==3){Coef2<-1.761; target<-119.5} #-20%
if(EffScen==8){Coef2<-5.237; target<-258.7} #+100% 

#F0.1a) approach, 0.1*pfa corresponds to commercial removal
if(EffScen==4){Coef2<-2.753; target<-164.7} # Updated target 

# No fishing scenario
if(EffScen==5){Coef2<-0; target<-0}

# No commercial fishing scenario
if(EffScen==6){Coef2<-0; target<-targetTr}

# No recreational fishing; CoefTrollingF = 0; no trolling
if(EffScen==7){
  Coef2<-2.21
  target<-116 # same as previous advice
  
}


# =============================================================
# Set years in which the target should be met:
# calendar year 2021 is year 30 for trapnetting and 
# year 29 for offshore fisheries (updated 3/4/2020)
yCTN<-30 
yOLL<-29


# =============================================================
# Update level of effort based on most recent efforts in Effort_ICES.txt files (data-folder in dropbox)
# First value is for interim year (assessment year) and next for future years
E_OLL_DEN<-c(0.77,0.77) # hundred thousand hookdays
E_OLL_PL<-c(2.83,2.83)
E_OLL_TROLLING<-c(5.34,5.34)

E_CTN_FIN_30<-c(rep(3.9,2)) # thousand trapdays
E_CTN_SWE_30<-c(rep(4.8,2))                 
E_CTN_FIN_31<-c(rep(5.6,2))       
E_CTN_SWE_31<-c(rep(7.7,2)) 




# Initialise arrays
source(paste0(PathFiles,"InitArrays_new.r"))

# =============================================================

# Run projections
#source(paste0(PathFiles,"ProjEffort_loops_new.r"))
source(paste0(PathFiles,"ProjEffort_loops_new_Fsealcorrected.r"))


# =============================================================
#Combine relevant information
Perform_Stats <- c(
  "MW", "MR", "F_seal", "AU",
  "BHalpha", "BHbeta",
  "May1stW","May1stR",
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
  "CoastW_HR","CoastR_HR","OffsW_HR","OffsR_HR",
  "Migr_Tornio","Migr_Simo","Migr_AU1W","Migr_AU13W",
  "Migr_AU1R","Migr_AU13R","Migr_AU13tot",
  "MorrumSeaCatch","MorrumRiverCatch",
  "RiverCatchW","RiverCatchR",
  "PFAW", "PFAR",
  "WOLL_C", "ROLL_C", #  "WOLLCtot", "ROLLCtot",
  "WCTN_C", "RCTN_C" #  "WCTNCtot", "RCTNCtot"
)

# Save to RData-file
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_new.RData")
save(list = Perform_Stats, file = File)

# =============================================================

