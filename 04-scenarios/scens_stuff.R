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
Years<-c(1992:LastHistYear)
yBreak<-length(Years)

source("00-functions/tidy-functions.r")
source("00-functions/tidy-functions_2chains.r")


#Introduce the different names for the different salmon stocks
RiverNames<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
              ,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävåran"
              ,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
              ,"Emån", "Kågeälven", "Testeboån")

