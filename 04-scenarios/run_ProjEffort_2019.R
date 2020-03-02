
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


# Paths are specified in a separate file 

# Henni:
source("C:/Users/412hpulkkin/Documents/Projects/WGBAST/04-scenarios/paths_scens_HP.r")

# ===============

Model<-"2019_LR_EPR"

#stocknames<-read.table(paste0(PathData,"rivernames.txt")) # proper names
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=13),3,3,4,3)

#! Set the 'choice' for Mps scenario according to the preferred mean in the
#! autocorrelation analysis
choice<-"MED"

# Time
# =============================================================

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-2018
LastPredYear<-2032

#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear

years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1)
#Years indicate the historic part of the model.
Years<-c(1992:LastHistYear)
#Define a year that separates historic part from future part
yBreak<-length(Years)

# =============================================================
#! Removal scenarios for the future
EffScen<-5

#for(EffScen in 1:6){
set.seed(6789)

# workflow for effort scenarios:
# 1. Run scenario 5 and ScenarioTable.R for that scenario -> input total PFA to cell R4 in T4321_workflow.xlsx
# 2. Run scenario 6: Set up targetTr that corresponds to recr removal (V21 in T4321_workflow.xlsx). 
#    Then find such CoefTrollingF that produces targetTr catch.
# 3. Run scenario 1: Set up target as in W21 (Total removal at sea for scen 1) in T4321_workflow.xlsx.
#   Then find such Coef2 that produces target catch, update that on line 74 below as "Coef1"
# 4. Run scens 2/3/4 similarly as scen 1 but adjust only Coef2 in corresponding line 
#   (Coef1 remains the same).

# For all scenarios, remember to update CoefF OR Coef1 OR Coef2 after the desired level of effort
# has been found with the while-loop!


Coef1<-0.699 #0.729 
CoefTrollingF<-0.518 # 0.66 # this coef produces the targetTr of recr catch when commercial fisheries are removed 
CoefTrollingF/Coef1 # should be close to 1

targetTr<-26.9 # Target trolling catch
# Target is the total removal, including commercial and recreational,
# discards, unrep and misrep
if(EffScen==1){Coef2<-1; target<-143} # previous advice approach
if(EffScen==2){Coef2<-1.217; target<-166} #+20%
if(EffScen==3){Coef2<-0.789; target<-120} #-20%
if(EffScen==8){Coef2<-2.186; target<-259} #+100%

#F0.1a) approach, 0.1*pfa corresponds to commercial removal
if(EffScen==4){Coef2<-1.285; target<-173} # Updated target 

# No fishing scenario
if(EffScen==5){Coef2<-0; target<-0}

# No commercial fishing scenario
if(EffScen==6){Coef2<-0; target<-targetTr}

# No recreational fishing; CoefTrollingF = 0; no trolling
if(EffScen==7){
  Coef2<-0.971
  target<-116 # same as previous advice
  
}


# =============================================================
# Set years in which the target should be met:
# calendar year 2020 is year 29 for trapnetting and 
# year 28 for offshore fisheries (update by HP 5/2/2019)
yCTN<-29 
yOLL<-28


# =============================================================
# Update level of effort based on most recent efforts in Effort_ICES.txt files (data-folder in dropbox)
# First value is for interim year (assessment year) and next for future years
E_OLL_DEN<-c(rep(0.42,2)) # hundred thousand hookdays      
E_OLL_PL<-c(rep(13.5,2))
E_OLL_TROLLING<-c(rep(6.56,2)) 

E_CTN_FIN_30<-c(rep(3.9,2)) # thousand trapdays
E_CTN_SWE_30<-c(rep(4.8,2))                 
E_CTN_FIN_31<-c(rep(5.6,2))       
E_CTN_SWE_31<-c(rep(7.7,2)) 

# Initialise arrays
source(paste0(PathFiles,"InitArrays.r"))

# =============================================================

# Run projections
source("ProjEffort_loops.r")


# =============================================================
#Combine relevant information
Perform_Stats <- c(
  "R0",
  "MaturationW","MaturationR",
  "postsmolts","postsmoltsR","postsmoltsW", "Mps_All", "Mps_AllR", "M74_All",
  "SmoltW", "SmoltR", "SpawnerW","SpawnerR", "PSW", "PSR",
  "spW_age", "spR_age",
  "EffortAU", "EffortAssesUnit",
  "CatchRiver",
  "WOLL_HR","ROLL_HR",
  "WODN_HR","RODN_HR",
  "WCTN_HR","RCTN_HR",
  "WCGN_HR","RCGN_HR",
  "WCDN_HR","RCDN_HR",
  "CoastW_HR","CoastR_HR","OffsW_HR","OffsR_HR",
  "May1stW","May1stR",
  "MigrW","MigrR",
  "Migr_Tornio","Migr_Simo","Migr_AU1W","Migr_AU13W",
  "Migr_AU1R","Migr_AU13R","Migr_AU13tot",
  "MorrumSeaCatch",
  "PFAW", "PFAR",
  "WOLLCtot", "ROLLCtot",
  "WCTNCtot", "RCTNCtot"
)

# Save to RData-file
File<-paste0(PathScen,"ScenProj_",Model,"_Mps",choice,"_EScen",EffScen,".RData")
save(list = Perform_Stats, file = File)


# =============================================================

