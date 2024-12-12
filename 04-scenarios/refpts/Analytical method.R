##Make sure to use long input files with long F_seal !!!


library(coda)

# # Becky:

# # ===============
   PathSim<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2024/Scenarios/" # results from the simulation model and output from scenarios
   PathData<-"C:/WGBAST15/WGBAST_2024/data_2024/" # extra input files 
   PathScen<-"C:/WGBAST15/2024_scenarios/" # scenario results 
   PathFiles<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2024/Reference points/"
   PathOut<-"//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2024/Reference points/"

setwd(PathFiles)

# Henni:
#source(paste0("/04-scenarios/paths_scens.r")) 

assess_year<-2024
Model<-paste0(assess_year,"_JAGS_Mps") 

# Fetch model
load(file=paste0(PathSim,"CR_2024_selected_chain.RData"))
load(file=paste0(PathSim,"SR_devs_2024.RData"))
Nimble<-grepl("Nimble",Model)

if(Nimble){
v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)
chains<-mcmc.list(list(v1,v2)) 
}else{
#chains<-run$mcmc #JAGS
chains<-chains
}

d<-as.matrix(chains)
keep.sample<-seq(7,7000,by=7)  #keep 1000 samples for scens
#keep.sample<-seq(3,6000,by=3)  #keep 2000 samples for scens
d<-d[keep.sample,]
nsim<-dim(d)[1]

stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
e_delay<-c(rep(4,times=12),3,3,3,4,3)    #Ljungan from 4 to 3 2024
stocks<-c(1:17)                  #stocks to run

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

Smolt_MSY<-array(0,dim=c(Nstocks,nsim))
Smolt_lim<-array(0,dim=c(Nstocks,nsim))
Eggs0<-array(0,dim=c(Nstocks,nsim))
Eggs_MSY<-array(0,dim=c(Nstocks,nsim))
Eggs_lim<-array(0,dim=c(Nstocks,nsim))
MSY<-array(0,dim=c(Nstocks,nsim))

comp.inds<-array(0,dim=c(Nstocks,nsim))  #which samples to use in comparisons (1 means ref pt exists)
comp.inds1<-array(0,dim=c(Nstocks,nsim))  #which samples to use in comparisons (1 means ref pt exists)

a<-d[1:nsim,grep("alphaSR",colnames(d))]
b<-d[1:nsim,grep("betaSR",colnames(d))]
tau<-d[1:nsim,grep("tau_SR",colnames(d))]  #precision of the recruitment process error deviates

#parameters are from this parameterisation of Bev-Holt
#log(R)<-log(E/(alpha+beta*E))
    

K<-array(0,dim=c(nsim,Nstocks))
for(s in 1:Nstocks){
   #K[,stocks[s]]<-exp(log(1/b[,stocks[s]])+0.5/tau) #correction applied in 2021 only
K[,stocks[s]]<-1/b[,stocks[s]]  #K is the maximum recruitment
}

rm(d,chains)

for(s in 1:length(stocks)){
#s<-1
a_s<-a[,stocks[s]]   #select parameters for a subset of stocks
b_s<-b[,stocks[s]]
K_s<-K[,stocks[s]]

a_s<-1/a_s
E<-1:10000


#e0<-E0_1[E0_1 <0 & E0_1 > -2000000]
#inds2<-which(E0_1 <0 & E0_1 > -2000000)
#R0[inds2]<-0.999*K_s[inds2] 
#E0<-R0[inds2]/(a_s[inds2]*(1-R0[inds2]/K_s[inds2]))  # Solve E0
#E0/(1/a_s+1/K_s*E0)

#Read in values for R0 (should be same length as for SR parameters above)
R0<-read.table(paste0(PathFiles,"eqm_stats_",stocknames[stocks[s]],"_",Model,".csv"),sep=",")[,1]
E0_1<-R0/(a_s*(1-R0/K_s))  # Solve E0

#inds<-which(E0_1>0)  #indices where R0 < K
#comp.inds[stocks[s],inds]<-1

inds1<-which(E0_1>0)  #indices where R0 < K
comp.inds1[stocks[s],inds1]<-1

R0[which(R0>K_s)]<-0.999*K_s[which(R0>K_s)] 
E0<-R0/(a_s*(1-R0/K_s))  # Solve E0
#E0/(1/a_s+1/K_s*E0)

par(mfrow=c(1,2))
hist(E0_1,breaks=50)
hist(E0,breaks=50)

#E75<-R0*0.75/(a_s*(1-R0*0.75/K_s))  
#E80<-R0*0.80/(a_s*(1-R0*0.80/K_s))

inds<-which(R0<K_s)
comp.inds[stocks[s],inds]<-1

K_s<-K_s[inds]
R0<-R0[inds]            
E0<-E0[inds]
a_s<-a_s[inds]
b_s<-K_s/a_s

#b_s=2*K_s/a_s
#gamma_s=(K_s/a_s)^2-(K_s/a_s)*K_s*E0/R0
# solution for the polynomial
#E_MSY=(-b_s+sqrt(b_s^2-4*a_s*gamma_s))/2*a_s

# E_MSY is where the derivative of the surplus production = 0
E_MSY<-(-2*(K_s/a_s)+sqrt((2*(K_s/a_s))^2-4*(b_s^2-E0*(K_s/a_s)*K_s/(R0))))/2   # That is the key result!
R_MSY<-E_MSY*K_s/(K_s/a_s+E_MSY) # R_MSY at E_MSY

#get Rlim

R_lim<-E_MSY*(R0/E0)
E_lim<-(R_lim/a_s)/(1-R_lim/K_s)

Eggs_MSY[stocks[s],which(comp.inds[stocks[s],]==1)]<-E_MSY
Eggs_lim[stocks[s],which(comp.inds[stocks[s],]==1)]<-E_lim
Eggs0[stocks[s],which(comp.inds[stocks[s],]==1)]<-E0
Smolt_MSY[stocks[s],which(comp.inds[stocks[s],]==1)]<-R_MSY
Smolt_lim[stocks[s],which(comp.inds[stocks[s],]==1)]<-R_lim
MSY[stocks[s],which(comp.inds[stocks[s],]==1)]<-R_MSY-R0*E_MSY/E0 # Surplus production

if(s==1){
    dev.new()
    par(mfrow=c(3,1),mar=c(2,4,1.5,0.1),oma=c(3,1,3,1),font=2,font.lab=2,font.axis=2)

    isim<-71

    E1<-seq(0,2000000,by=1000)
    R<-E1*K_s[isim]/(K_s[isim]/a_s[isim]+E1)
    #log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
    Replacement<-R0[isim]*E1/E0[isim]
    S<-R-R0[isim]*E1/E0[isim] # Surplus production
    plot(E1,R,ylab="Recruits")
    points(E_MSY[isim],R_MSY[isim],col="green",cex=3,pch=19,main="")
    abline(v=E_MSY[isim])
    abline(h=R_MSY[isim])
    points(E1,Replacement)
    points(E0[isim],R0[isim],col="blue",cex=3,pch=19)
    legend("topleft", "a)", bty="n")
    
    plot(E1,S,main="",ylab="Surplus recruits")
    abline(v=E_MSY[isim])
    abline(h=0)
    legend("topleft", "b)", bty="n")

 # Derivative of the surplus production. This is a second degree polynomial
     D<-K_s[isim]/(K_s[isim]/a_s[isim]+E1)-K_s[isim]*E1*(K_s[isim]/a_s[isim]+E1)^(-2)-R0[isim]/E0[isim]
    
     plot(E1,D,main="",ylab="Derivative") #,ylim=c(-1,1)
     abline(h=0)
     abline(v=Eggs_MSY[stocks[s],isim])
     legend("topleft", "c)", bty="n")
     mtext("Eggs", side = 1, line = 0.5, outer = TRUE, cex=0.7)
     
     dev.new()
     par(mfrow=c(1,1),mar=c(2,4,1.5,0.1),oma=c(3,1,3,1),font=2,font.lab=2,font.axis=2)
     
     E1<-seq(0,2000000,by=1000)
     R<-E1*K_s[isim]/(K_s[isim]/a_s[isim]+E1)
     #log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
     Replacement<-R0[isim]*E1/E0[isim]
     S<-R-R0[isim]*E1/E0[isim] # Surplus production
     plot(E1,R,ylab="Recruits")
     points(E_MSY[isim],R_MSY[isim],col="green",cex=3,pch=19,main="")
     abline(v=E_MSY[isim])
     abline(h=R_MSY[isim])
     points(E1,Replacement)
     points(E0[isim],R0[isim],col="blue",cex=3,pch=19)
     points(E_lim[isim],R_lim[isim],col="red",cex=3,pch=19)
     abline(h=E_MSY[isim]*R0[isim]/E0[isim])

    # isim<-73
    # E1<-seq(0,3000000,by=1000)
    # R<-E1*K_s[isim]/(K_s[isim]/a_s[isim]+E1)
    # #log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
    # Replacement<-R0[isim]*E1/E0[isim]
    # S<-R-R0[isim]*E1/E0[isim] # Surplus production
    # plot(E1,R)
    # points(E_MSY[isim],R_MSY[isim],col="red",cex=3,pch=19,main="S-R")
    # abline(v=E_MSY[isim])
    # abline(h=R_MSY[isim])
    # points(E1,Replacement)
    # points(E0[isim],R0[isim],col="green",cex=3,pch=19)
    # 
    # plot(E1,S,main="Sustainable yield")
    # abline(v=E_MSY[isim])
    # abline(h=0)
    # #
    # isim<-86
    # E1<-seq(0,1000000,by=1000)
    # R<-E1*K_s[isim]/(K_s[isim]/a_s[isim]+E1)
    # #log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
    # Replacement<-R0[isim]*E1/E0[isim]
    # S<-R-R0[isim]*E1/E0[isim] # Surplus production
    # plot(E1,R)
    # points(E_MSY[isim],R_MSY[isim],col="red",cex=3,pch=19,main="S-R")
    # abline(v=E_MSY[isim])
    # abline(h=R_MSY[isim])
    # points(E1,Replacement)
    # points(E0[isim],R0[isim],col="green",cex=3,pch=19)
    # 
    # plot(E1,S,main="Sustainable yield")
    # abline(v=E_MSY[isim])
    # abline(h=0)
    # 
    # isim<-88
    # E1<-seq(0,2000000,by=1000)
    # R<-E1*K_s[isim]/(K_s[isim]/a_s[isim]+E1)
    # #log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
    # Replacement<-R0[isim]*E1/E0[isim]
    # S<-R-R0[isim]*E1/E0[isim] # Surplus production
    # plot(E1,R)
    # points(E_MSY[isim],R_MSY[isim],col="red",cex=3,pch=19,main="S-R")
    # abline(v=E_MSY[isim])
    # abline(h=R_MSY[isim])
    # points(E1,Replacement)
    # points(E0[isim],R0[isim],col="green",cex=3,pch=19)
    # 
    # plot(E1,S,main="Sustainable yield")
    # abline(v=E_MSY[isim])
    # abline(h=0)
}

}#s


save(comp.inds,Smolt_MSY,Smolt_lim,Eggs_MSY,Eggs_lim,Eggs0,MSY,file=paste0("ref_pts_2024","_",Model,"_eggs.RData"))






