library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

# setwd("C:/R/WGBAST/")
source("00-functions/tidy-functions_2chains.r")

nstocks<-17

# Choose data
#pathData<-"H:/Projects/WGBAST/FLHM/dat/data_2019/"
pathData<-"H:/Projects/WGBAST/FLHM/dat/data_2020/"

Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
Rivername_long<-read.table(str_c(pathData, "rivernames.txt"))[,1]

  # Model 1
  # =================
  
  # Long version of the assessment model 2019
 load(file="C:/output/wgbast/flhm/FLHM_results_2019_extended2019-04-11.RData");chains1<-chains; trolling1<-F; Mname1<-"2019 assessment model, extended run" # is trolling included as a separate fishery?
#load(file="C:/output/wgbast/flhm/FLHM_2019_trolling_LL_DN.RData"); trolling1<-T ;Mname1<-"2019 assessment data, AR model for trolling, LL, DN, reared q == wild q" # is trolling included as a separate fishery?
#chains1<-as.mcmc.list(run)

# 2019 assessment data
  YearsB<-c(1987:2018)
  Years2B<-c(1992:2018)
# 2020 data
  #YearsB<-c(1987:2019)
  #Years2B<-c(1992:2019)
  
# Model 2:
# =================
#load(file="C:/output/wgbast/flhm/FLHM_2019_trolling_LL_DN.RData"); trolling2<-T ;Mname2<-"2019 assessment data, AR model for trolling, LL, DN, reared q == wild q" # is trolling included as a separate fishery?
#load(file="C:/output/wgbast/flhm/FLHM_2020.RData"); trolling2<-F;Mname2<-"2020 data, 2018/2019 assessment model structure" # Same model structure as in 2019 wg version
#load(file="C:/output/wgbast/flhm/FLHM_2020Xdata_2020.RData"); trolling2<-T;Mname2<-"2020 data, AR models for offshore fisheries, qeff's, HrW corrected"
#load(file="C:/output/wgbast/flhm/FLHM_2020X_2_data2020.RData"); trolling2<-T;Mname2<-"2020 data, AR models for offshore and coastal fisheries, qeff's, HrW corrected"
load(file="C:/output/wgbast/flhm/FLHM_2020XY_data2020.RData"); trolling2<-T;Mname2<-"2020 data, AR models for offshore fisheries, fixed sdq & sdtr"
  #load(file="C:/output/wgbast/flhm/FLHM_trolling_LL_DN_wr2020.RData"); trolling2<-T ;Mname2<-"2020 assessment data, AR model for trolling, LL, DN, reared q != wild q (but not final version)" # is trolling included as a separate fishery?
  
chains<-as.mcmc.list(run)

# Is comparison for run from the same year? T if yes, F if no
# Note that older version should be set as Model 1
sameYear<-F
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2019)
  Years2<-c(1992:2019)
}


