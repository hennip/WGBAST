# run-this-first-wgbast.R - THE PATH STRUCTURE FOR WGBAST REPO

# This file contains a personal path structure that enables easy 
# usage of the files and models for multiple users

# This file is added in the .gitignore so that the personal files wouldn't be
# pushed to the Github or replaced with someone else's personal file
# However it seems best that this file would be located outside the WGBAST repo 
# because the file or recent changes can easily disappear when branches are
# merged (this happens because of the .gitignore)

# Recommended location for this files is alongside the WGBAST repo folder
# in this case this file can be called in R by
# source("../run-this-first-wgbast.R")

# IN LINUX:
# If you run the scripts in Linux command prompt (eg Putty), 
# go first to project folder with 
# cd Rprojects/WGBAST/ 
# and then open R
# Then the relative paths should function the same as when using Rstudio

# Packages and plotting files
# =============================================================================
source("00-basics/packages.R")
source("00-basics/plotfunctions.r")
source("00-basics/tidy-functions_2chains.R")

# Submodel related paths, these could also be directed into shared folder
# =============================================================================
#pathDataSST<-"../../01-Projects/WGBAST/SST/2025/"
#pathM74_2021<-"../../01-Projects/WGBAST/SubE_M74/2021/" # stuff from 21 is needed to cover the older data
#pathM74_current<-"../../01-Projects/WGBAST/SubE_M74/2023/"

# FLHM related paths
# =============================================================================
PathData_FLHM<-"02-data/flhm-input-files/"#"../../WGBAST_shared/data/data_2025/"
#PathData_FLHM<-"C:/WGBAST15/WGBAST_2025/data_2025/" # extra input files 
#PathData_FLHM<-paste0(pathMain,"WGBAST_shared/data/data_2024/") # Assessment model data files
PathModel_FLHM<-"03-flhm/" #relative path, could be removed and used directly 
PathOut_FLHM<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios


# Scenario related paths
# =============================================================================
PathSim<-PathOut_FLHM # simulations for scens read from where those were stored from FLHM
PathData_Scen<-PathData_FLHM# currently the same as for FLHM data
PathFiles<-"04-scenarios/" # Relative path, could be removed and used directly 
PathScen<-"C:/WGBAST15/2025_scenarios/"# scenario results

# Misc
# =============================================================================
#pathDataCatchEffort<-"../../WGBAST_shared/flhm/2025/dat/orig/" # location for catch&effort data

# # # Becky:
#    PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
#    PathData<-"C:/WGBAST15/WGBAST_2025/data_2025/" # extra input files 
#    PathOut_Scen<-"C:/WGBAST15/2025_scenarios/" # scenario results 
#    PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Scenarios/"



