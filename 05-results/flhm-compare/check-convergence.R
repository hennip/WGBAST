#load(file="H:/FLR/WGBAST18/WGBAST_JAGS_SRorig.RData")
#load(file="H:/FLR/WGBAST18/WGBAST_JAGS_new_SR.RData")
library(coda)
library(runjags)

buin_off <- function(res, nBI=NA, pBI=NA){
  #   getting orig BI and thin
  BI <- res$burnin
  thin <- res$thin
  n <- res$sample
  #   converting into mcmc.list
  #   to make subsetting possible
  mcmc <- as.mcmc.list(res)
  #   calculating starttpoint for new posterior
  if(is.na(nBI))nBI <- round(pBI*n)
  start = BI+thin*nBI
  #   subsetting posterior
  mcmc_m <- window(mcmc, start = start)
  #   adding new chains to res
  res$mcmc <- mcmc_m
  return(res)
  
}

library(runjags);library(coda)
library(tidyverse)
load("C:/Users/03195892/OneDrive - Valtion/WGBAST-antti/FLHM_2024/res_ORIG/FLHM_JAGS_2024_orig_data2024_run8.RData")

run1<-run
run2 <- buin_off(run, pBI = 0.2)

cbind(Years, 1:length(Years))

plot(run1, var="R0[1]")
summary(run1, var="R0[1]")

plot(run1, var="K[1]")
summary(run1, var="K[1]") # psrf 4

summary(run1, var="MW") #psrf 3.11 /3.8
summary(run1, var="MR")
plot(run1, var="MW")
plot(run1, var="MR") 
plot(run1, var=c("MW","MR"), plot.type = "trace")

summary(run1, var="MpsW")
plot(run1, var="MpsW[22]") #2008
plot(run1, var="MpsW[23]")#2009
plot(run1, var="MpsW[24]")#2010
plot(run1, var="MpsW[25]")#2011

plot(run1, var=c("MpsW[20]","MpsW[30]"), plot.type = "trace")

summary(run1, var="Wprop")

plot(run1, var="Wprop[18,1]")# 2004
plot(run1, var="Wprop[23,1]") # 2009
plot(run1, var="Wprop[25,1]", plot.type = "trace")# 2011

plot(run1, var="Wprop[18,2]")
plot(run1, var="Wprop[23,2]")
plot(run1, var="Wprop[25,2]")

summary(run1, var="LW")

plot(run1, var="LW[24,1]")
plot(run1, var="LW[25,1]")

plot(run1, var="LW[20,2]")
plot(run1, var="LW[26,2]")
plot(run1, var="LW[28,2]")

plot(run, var="tauL")

summary(run1, var ="Mpsw")
