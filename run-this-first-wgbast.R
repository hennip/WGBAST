# run-this-first-wgbast.R - THE PATH STRUCTURE FOR WGBAST REPO

# This file contains a personal path structure that enables easy 
# usage of the files and models for multiple users

# Change 4/2026: 
# - run-this-first.R is now a common file located at the root of the repo 
# - This file contains personal settings of paths for calling the data and saving output
# (and other related stuff)

# Check the computer name 
# In case missing, create a file with one line: 
# computer_name<-"mycomputername"
# (and save it to the same folder in which your repo folder is, NOT inside the repo)
# =============================================================================
source("../computer_name.R")

# =============================================================================
# IN LINUX:
# If you run the scripts in Linux command prompt (eg Putty), 
# go first to project folder with 
# cd Rprojects/WGBAST/ 
# and then open R
# Then the relative paths should function the same as when using Rstudio
# =============================================================================

# Packages and plotting files
# =============================================================================
source("00-basics/packages.R")
source("00-basics/plotfunctions.r")
source("00-basics/tidy-functions_2chains.R")

# Personal settings
# =============================================================================

# # # Becky:
#    PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
#    PathData<-"C:/WGBAST15/WGBAST_2025/data_2025/" # extra input files 
#    PathOut_Scen<-"C:/WGBAST15/2025_scenarios/" # scenario results 
#    PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Scenarios/"

if(computer_name=="mikko_work"){# Submodel related paths, these could also be directed into shared folder
  # =============================================================================
  pathDataSST<-"../../01-Projects/WGBAST/SST/2025/" #.. -->siirtyy kansiossa "takaisin"
  pathM74_2021<-"../../01-Projects/WGBAST/SubE_M74/2021/" # stuff from 21 is needed to cover the older data
  pathM74_current<-"../../01-Projects/WGBAST/SubE_M74/2023/"
  
  # FLHM related paths
  # =============================================================================
  
  PathData_FLHM<-"02-data/flhm-input-files/data_2025/"#"../../WGBAST_shared/data/data_2025/"
  #PathData_FLHM<-paste0(pathMain,"WGBAST_shared/data/data_2024/") # Assessment model data files
  PathModel_FLHM<-"03-flhm/" #relative path, could be removed and used directly 
  PathOut_FLHM<-"../../WGBAST_shared/flhm/2025/output/"
  save(cars, file=paste0(PathOut_FLHM,"autot.RData")) # testing that folder exists by saving "cars" 
  
  PathModel_Nimble<-"03-flhm/Nimble" # relative path, use later directly and remove
  
  # Scenario related paths
  # =============================================================================
  PathSim<-PathOut_FLHM # simulations for scens read from where those were stored from FLHM
  PathData_Scen<-PathData_FLHM# currently the same as for FLHM data
  PathFiles<-"04-scenarios/" # Relative path, could be removed and used directly 
  #PathOut_Scen<-"../../WGBAST_shared/scen/2025/"
  PathOut_Scen_tmp<-"../../output/wgbast/scen/2025/"
  PathOut_Scen<-"../../output/wgbast/scen/2025/" #TESTAUSTA VARTEN
  #PathOut_Scen_tmp<-PathOut_Scen
  save(iris, file=paste0(PathOut_Scen,"kukat.RData")) # testing that folder exists by saving "iris"
  
  
  # Misc
  # =============================================================================
  #pathDataCatchEffort<-"../../WGBAST_shared/flhm/2025/dat/orig/" # location for catch&effort data
  pathDataCatchEffort<-"../../WGBAST - Tiedostot/General/2025 Pärnu/Data/" # location for catch&effort data

  
  }


if(computer_name=="hp_laptop"){
  
  # Submodel related paths, these could also be directed into shared folder
  # =============================================================================
  pathDataSST<-"../../01-Projects/WGBAST/SST/2025/"
  pathM74_2021<-"../../01-Projects/WGBAST/SubE_M74/2021/" # stuff from 21 is needed to cover the older data
  pathM74_current<-"../../01-Projects/WGBAST/SubE_M74/2023/"
  
  # FLHM related paths
  # =============================================================================
  
  PathData_FLHM<-"02-data/flhm-input-files/data_2025/"#"../../WGBAST_shared/data/data_2025/"
  #PathData_FLHM<-paste0(pathMain,"WGBAST_shared/data/data_2024/") # Assessment model data files
  PathModel_FLHM<-"03-flhm/" #relative path, could be removed and used directly 
  PathOut_FLHM<-"../../WGBAST_shared/flhm/2025/output/"
  save(cars, file=paste0(PathOut_FLHM,"autot.RData")) # testing that folder exists by saving "cars" 
  
  PathModel_Nimble<-"03-flhm/Nimble" # relative path, use later directly and remove
  
  # Scenario related paths
  # =============================================================================
  PathSim<-PathOut_FLHM # simulations for scens read from where those were stored from FLHM
  PathData_Scen<-PathData_FLHM# currently the same as for FLHM data
  PathFiles<-"04-scenarios/" # Relative path, could be removed and used directly 
  #PathOut_Scen<-"../../WGBAST_shared/scen/2025/"
  PathOut_Scen_tmp<-"../../output/wgbast/scen/2025/"
  PathOut_Scen<-"../../output/wgbast/scen/2025/" #TESTAUSTA VARTEN
  #PathOut_Scen_tmp<-PathOut_Scen
  save(iris, file=paste0(PathOut_Scen,"kukat.RData")) # testing that folder exists by saving "iris"
  
  # Misc
  # =============================================================================
  #pathDataCatchEffort<-"../../WGBAST_shared/flhm/2025/dat/orig/" # location for catch&effort data
  pathDataCatchEffort<-"../../WGBAST - Tiedostot/General/2025 Pärnu/Data/" # location for catch&effort data
  pathReleasesFI<-"../../WGBAST_shared/submodels/releases/"
} 



