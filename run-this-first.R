# run-this-first.R - THE PATH STRUCTURE FOR WGBAST REPO

# This file contains personal path structure and calls for basic source files 
# file calls in other files should be made relative to these
# this file is added to gitignore so personal paths are not taken to the remote 
# repository nor are they overwritten when pulling from the repo 
# pls take care of the backup of this file (since not stored in Github)

# Pls note that many of the paths could be relative if work took place always in
# Rstudio, however to function also in Linux, paths are defined explicitly

# Storing pathMain to C-disc can be handy if it contains user name etc and
# is used also in personal repositories (user names should not be stored in Github)
# Otherwise, one can just define pathMain directly here or below in specific paths
#source("C:/path_for_r/pathMain.R")
#   path main
pathMain <- "C:/Users/03195892/OneDrive - Valtion/"
# General paths and source files
# =============================================================================
#PathBasics<-paste0(pathMain,"Rprojects/WGBAST/00-basics/")
PathBasics<-paste0(pathMain,"WGBAST_git/WGBAST_2nd/00-basics/")

source(paste0(PathBasics,"packages.R"))
source(paste0(PathBasics,"plotfunctions.r"))
source(paste0(PathBasics,"tidy-functions_2chains.R"))


# Submodel related paths, these could also be directed into shared folder
# =============================================================================
#pathDataSST<-paste0(pathMain,"01-Projects/WGBAST/SST/2023/")
#pathDataSST<-paste0(pathMain,"WGBAST_shared/")

#pathM74_2021<-paste0(pathMain,"01-Projects/WGBAST/SubE_M74/2021/") # stuff from 21 is needed to cover the older data
#pathM74_current<-paste0(pathMain,"01-Projects/WGBAST/SubE_M74/2023/")

# FLHM related paths
# =============================================================================
#"C:/Users/03195892/OneDrive - Valtion/WGBAST_shared/flhm/2024/"
PathData_FLHM<-paste0(pathMain,"WGBAST_shared/data/data_2024/") # Assessment model data files
PathModel_FLHM<-paste0(pathMain,"WGBAST_shared/flhm/2024/") 
PathOut_FLHM<-paste0(pathMain,"WGBAST_shared/flhm/2024/output/")


# Scenario related paths
# =============================================================================
# Becky:
# PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
# PathData<-"C:/WGBAST15/WGBAST_2023/data_2023/" # extra input files NOW PathData_FLHM/PathData_Scen
# PathScen<-"C:/WGBAST15/2023 scenarios/" # scenario results NOW PathOut_Scen
# PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2023/"
# PathOut<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2023/R0_1000/" # Defined now in specific files relative to PathOut_Scen


PathSim<-PathOut_FLHM # simulations for scens read from where those were stored from FLHM
PathData_Scen<-PathData_FLHM# currently the same as for FLHM data
PathFiles<-paste0(pathMain,"WGBAST_git/WGBAST_2nd/04-scenarios/") # Relative path, this should not be needed
#PathOut_Scen<-paste0(pathMain,"output/wgbast/scen/2023/") # Previously PathScen
PathOut_Scen<-paste0(pathMain,"WGBAST_shared/scen/") # Previously PathScen
# scenario results written here. NOTE! Better not to write directly to shared folder, only copy-paste there final files


# Misc
# =============================================================================
pathDataCatchEffort<-paste0(pathMain,"01-Projects/WGBAST/FLHM/2023/") # location for catch&effort data




