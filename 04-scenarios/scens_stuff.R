# These to make tables and figures

library(coda)
library(writexl)
library(tidyverse)
library(stringr)


################################################################################
#! #############################################################################
# Version of the estimation model
#Model<-"2020"
Model<-"2020_updated"

# Time
LastHistYear<-2019
#LastPredYear<-2032
ymax<-15
LastPredYear<-LastHistYear+ymax
year<-c(1992:LastPredYear)
Nyears<-length(year)
Nstocks<-17
Years<-c(1992:LastHistYear)
yBreak<-length(Years)
nsim<-1000

source("00-functions/tidy-functions.r")
source("00-functions/tidy-functions_2chains.r")


#Introduce the different names for the different salmon stocks
RiverNames<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
              ,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävåran"
              ,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
              ,"Emån", "Kågeälven", "Testeboån")

