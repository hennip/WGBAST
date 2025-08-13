


# Extra output that is run after assessment, usually during the summer or early 
# autumn. Output is stored to shared OneDrive folder.
# Use no fishing scenario (for most of output only history is of interest)

# Add here later output that is delivered eg. for HELCOM 

# Email from Atso to Henni 13/6/25
# - Number of salmon arriving to Gulf of Bothnia 
# o total wild
# o total reared
# o total wild + reared
# o total Simojoki
# o total Tornionjoki
# - Number of spawners
# o total wild per AU
# o total reared per AU
# o total Simojoki
# o total Tornionjoki
# - Age structure of the spawners, history plus no fishing scenarios until 2050
# o total wild
# o total reared
# o Tornionjoki
# - Slim, SMSY and SR0 (as number of eggs, or if possible, as number of salmon), 50%, 80% and 95% level of probability
# o Simojoki
# o Tornionjoki
# o all wild swedish stocks for SLU?
# - Coastal harvest rate per AU, preferably both grilse and MSW (but at least MSW)
# o wild
# o reared

# Define  
skip<-1 # When skip exists, definitions below are used and the ones in the source files skipped

Model<-"2025_JAGS_base4" # Assessment model version
nsim<-1000
LastHistYear<-2024
ymax<-350
LastPredYear<-LastHistYear+ymax
Years<-c(1992:LastPredYear)
length(Years)

Nyears<-yBreak<-length(Years)
sims<-c(1:nsim)
Nstocks<-17; Nunits<-4 

EffScen<-1 # No fishing at sea and rivers

#Load the file containing stats
File<-paste0(PathOut_Scen_tmp,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

File
load(File)



################################################################################
# Number of salmon arriving to Gulf of Bothnia
################################################################################
# o total wild
# o total reared
# o total wild + reared
# o total Simojoki
# o total Tornionjoki

source("06-misc/MigratingAU13tot.R")


# Saves in "../../WGBAST_shared/scen/2025/" files
# migratingAU1-3_wild.xlsx
# migratingAU1-3_reared.xlsx
# migratingAU1-3.xlsx
# migratingAU1-3_torne.xlsx
# migratingAU1-3_simo.xlsx
# AND
# migratingAU1-3R_GrilseProp.xlsx


################################################################################
# Number of spawners
################################################################################
# o total wild per AU
# o total reared per AU
# o total Simojoki
# o total Tornionjoki

source("06-misc/SpawnersByRiversAndUnits.R")

# Saves in "../../WGBAST_shared/scen/2025/" files
# SpawnersByUnit.xlsx, wild, reared and tot per AU 
# and
# SpawnersByRiver.xlsx per river

################################################################################
# - Age structure of the spawners, history plus no fishing scenarios until 2050
################################################################################
# o total wild
# o total reared
# o Tornionjoki

source("06-misc/Spawners_AgeDist.R")

# Saves in "../../WGBAST_shared/scen/2025/" files
# AgePropsW.xlsx per river and AgePropR.xlsx per AU


################################################################################
# - Slim, SMSY and SR0 (as number of eggs, or if possible, as number of salmon), 50%, 80% and 95% level of probability
################################################################################
# o for each stock


################################################################################
# - Coastal harvest rate per AU, preferably both grilse and MSW (but at least MSW)
################################################################################
# o wild
# o reared

source("06-misc/combinedHRs.R")















