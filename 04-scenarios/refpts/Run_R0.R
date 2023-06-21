##Make sure to use long input files with long F_seal !!!


# PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios
# PathData<-"C:/WGBAST15/WGBAST_2023/data_2023/" # extra input files 
# PathScen<-"C:/WGBAST15/2023 scenarios/" # scenario results 
# PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2023/"
# PathOut<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2023/R0_1000/"
#setwd(PathFiles)

source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location
source(paste0(PathFiles,"refpts/get_R0.R"))

Model<-"2023"  #MSY  #"MSY_new_M74"   

stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=13),3,3,4,3)

# Time
#! Set the last year for historic part and the last year for predictions:
ymax<-300
LastHistYear<-2022
LastPredYear<-1992+ymax
#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear
HistYears<-c(1992:LastHistYear)

#Define a year that separates historic part from future part
yBreak<-length(HistYears)
    
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1)   #yBreak+NumFutYears
yCTN<-years[3]
 

nsim<-1000    #number of posterior samples to run
load(paste0(PathFiles,"SR_devs_2023.RData"))
keep.sample<-seq(4,4000,by=4)
errorSR<-errorSR[keep.sample,,] #thin to keep every 4th sample

#errorSR     1500 x 157 x 17


stocks<-c(1:17)                  #stocks to run

nstocks<-length(stocks)


ptm<-proc.time()

load(file=paste0(PathSim,"FLHM_2023_rivHR_data2023_thin350.RData"))
#chains<-as.mcmc.list(run)
chains<-as.mcmc(run)

#d<-as.matrix(chains$mcmc) # Gave Error in chains$mcmc : $ operator is invalid for atomic vectors
d<-as.matrix(chains)
keep.sample<-seq(4,4000,by=4)
d<-d[keep.sample,] #thin to keep every 4th sample
nsim<-dim(d)[1]

smolt0<-array(0,dim=c(nstocks,nsim))
spawner0<-array(0,dim=c(nstocks,nsim))


#simulate R0s
for(ij in 1:nstocks){   #nstocks
  print(ij)
  R0file<-paste0(PathOut_Scen,"R0_1000/eqm_stats_error1_",stocknames[stocks[ij]],".csv")
  eqstats<-get_R0(stocks[ij],histyr=LastHistYear,ymax=ymax,evec=errorSR[,,stocks[ij]],nsim=nsim)
  smolt0[ij,]<-eqstats[[1]]
  spawner0[ij,]<-eqstats[[2]]
  print.R0<-cbind(smolt0[ij,],spawner0[ij,])

  write.table(print.R0,row.names=F,col.names=F,file=R0file,sep=",",append=T)

}
proc.time()-ptm
