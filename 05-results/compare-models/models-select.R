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

pathData<-"H:/Projects/WGBAST/FLHM/2019/dat/data_2019/"

Rivername<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage", "Testeboan")
Rivername_long<-read.table(str_c(pathData, "rivernames.txt"))[,1]

  # Model 1
  # =================
  
  # Long version of the assessment model 2019
  #load(file="C:/output/wgbast/flhm/FLHM_results_2019_extended2019-04-11.RData");chains1<-chains
  load(file="C:/output/wgbast/flhm/WGBAST19_trolling.RData");chains1<-as.mcmc.list(run)


  YearsB<-c(1987:2018)
  Years2B<-c(1992:2018)

# Model 2:
# =================
#load(file="C:/output/wgbast/flhm/WGBAST19_trolling.RData")
load(file="C:/output/wgbast/flhm/WGBAST19_trolling_LL.RData")
chains<-as.mcmc.list(run)

# Is comparison for run from the same year? T if yes, F if no
sameYear<-T
if(sameYear==T){
  Years<-YearsB
  Years2<-Years2B
}else{
  Years<-c(1987:2017)
  Years2<-c(1992:2017)
}


