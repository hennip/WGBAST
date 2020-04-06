# These to make tables and figures

library(coda)
library(writexl)
library(tidyverse)
library(stringr)


################################################################################
#! #############################################################################
# Version of the estimation model
Model<-"2020"

# Time
LastHistYear<-2018
LastPredYear<-2032
year<-c(1992:LastPredYear)
length(year)
Nyears<-length(year)
Nstocks<-17

source("00-functions/tidy-functions.r")
source("00-functions/tidy-functions_2chains.r")

