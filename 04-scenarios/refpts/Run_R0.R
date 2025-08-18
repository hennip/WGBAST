##Make sure to use long input files with long F_seal !!!


library(coda)

# # Becky:

# # Becky:
   # PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
   # PathData<-"C:/WGBAST15/WGBAST_2025/data_2025/" # extra input files 
   # PathScen<-"C:/WGBAST15/2025_scenarios/" # scenario results 
   # PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Reference points/"

# Henni:
source("../run-this-first-wgbast.R")
  

#Give a model name
assess_year<-2025   #Note! change back year for releases to last year of assessment
Model<-paste0(assess_year,"_JAGS_base4")
#Model<-paste0(assess_year,"_Nimble")


#   PathOut<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2025/Reference points/"

#setwd(PathFiles)

#Give a model name
assess_year<-2025   #Note! change back year for releases to last year of assessment
Model<-paste0(assess_year,"_JAGS_base4")
#Model<-paste0(assess_year,"_Nimble")

# Fetch model
load(file=paste0(PathSim,"chain_cleaned_2025_base4.RData"))
load(file=paste0(PathFiles,"SR_devs_2025.RData"))
Nimble<-grepl("Nimble",Model)

if(Nimble){
v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)
chains<-mcmc.list(list(v1,v2)) 
}else{
#chains<-run$mcmc #JAGS
chains<-chains_new
}

d<-as.matrix(chains)
#keep.sample<-seq(7,7000,by=7)  #keep 1000 samples for scens
#keep.sample<-seq(3,6000,by=3)  #keep 2000 samples for scens
d<-d[1001:2000,]
nsim<-dim(d)[1]

stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
e_delay<-c(rep(4,times=12),3,3,3,4,3)    #Ljungan from 4 to 3 2024

# Time
#! Set the last year for historic part and the last year for predictions:
ymax<-350
LastHistYear<-assess_year-1
LastPredYear<-1992+ymax
#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear
HistYears<-c(1992:LastHistYear)

#Define a year that separates historic part from future part
yBreak<-length(HistYears)
    
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1)   #yBreak+NumFutYears
yCTN<-years[3]
 
#nsim<-1000    #number of posterior samples to run
#load(paste0(PathFiles,"SR_devs_2023.Rdata"))
#keep.sample<-seq(4,4000,by=4)
#errorSR<-errorSR[keep.sample,,] #thin to keep every 4th sample
#
##errorSR     1500 x 157 x 17

source(paste0(PathFiles,"refpts/get_R0.R"))
stocks<-c(1:17)                  #stocks to run

nstocks<-length(stocks)
ptm<-proc.time()

smolt0<-array(0,dim=c(nstocks,nsim))
spawner0<-array(0,dim=c(nstocks,nsim))
eggs0<-array(0,dim=c(nstocks,nsim))
z0<-array(0,dim=c(nstocks,nsim))

#simulate R0s
#for(ij in 1:nstocks){   #nstocks
for(ij in 13:17){   #nstocks
  print(ij)
  R0file<-paste0(PathOut_Scen,"eqm_stats_",stocknames[stocks[ij]],"_",Model,".csv")
  eqstats<-get_R0(Model,stocks[ij],histyr=LastHistYear,ymax=ymax,evec=errorSR[,,stocks[ij]],nsim=nsim)
  smolt0[ij,]<-eqstats[[1]]
  spawner0[ij,]<-eqstats[[2]]
  eggs0[ij,]<-eqstats[[3]]
  print.R0<-cbind(smolt0[ij,],spawner0[ij,],eggs0[ij,],z0[ij,])

  write.table(print.R0,row.names=F,col.names=F,file=R0file,sep=",",append=T)

}
proc.time()-ptm
