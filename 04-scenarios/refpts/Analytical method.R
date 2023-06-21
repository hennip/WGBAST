#Get R_MSY and R_lim reference points

rm(list=ls(all=T))   

source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location


Model<-"2023"  #MSY  #"MSY_new_M74"   

stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

stock_indices<-c(1:17)
Nstocks<-length(stock_indices) # number of stocks
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=13),3,3,4,3)

ymax<-70
LastHistYear<-2020
LastPredYear<-1992+ymax
#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear

years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1)

yCTN<-years[3]
HistYears<-c(1992:LastHistYear)

#Define a year that separates historic part from future part
yBreak<-length(HistYears) 
stocks<-c(1:17)                  #stocks to rune

load(file=paste0(PathSim,"FLHM_2023_rivHR_data2023_thin350.RData"))
#chains<-as.mcmc.list(run)
chains<-run$mcmc
d<-as.matrix(chains)
keep.sample<-seq(4,4000,by=4)
d<-d[keep.sample,]
nsamp<-dim(d)[1]

Smolt_MSY<-array(0,dim=c(Nstocks,nsamp))
Smolt_lim<-array(0,dim=c(Nstocks,nsamp))
Eggs_MSY<-array(0,dim=c(Nstocks,nsamp))
MSY<-array(0,dim=c(Nstocks,nsamp))

comp.inds<-array(0,dim=c(Nstocks,nsamp))  #which samples to use in comparisons (1 means ref pt exists)
comp.inds1<-array(0,dim=c(Nstocks,nsamp))  #which samples to use in comparisons (1 means ref pt exists)



a<-d[1:nsamp,grep("alphaSR",colnames(d))]
b<-d[1:nsamp,grep("betaSR",colnames(d))]
tau<-d[1:nsamp,grep("tau_SR",colnames(d))]  #precision of the recruitment process error deviates

#parameters are from this parameterisation of Bev-Holt
#log(R)<-log(E/(alpha+beta*E))
    

K<-array(0,dim=c(nsamp,17))
for(s in 1:length(stocks)){
   #K[,stocks[s]]<-exp(log(1/b[,stocks[s]])+0.5/tau) #correction applied in 2021 only
K[,stocks[s]]<-1/b[,stocks[s]]  #K is the maximum recruitment
}

rm(d,chains)

for(s in 1:length(stocks)){

a_s<-a[,stocks[s]]   #select parameters for a subset of stocks
b_s<-b[,stocks[s]]
K_s<-K[,stocks[s]]

a_s<-1/a_s
E<-1:10000

#Read in values for R0 (should be same length as for SR parameters above)
# Note! Make R0_1000-folder
R0<-read.table(paste0(PathOut_Scen,"R0_1000/eqm_stats_error1_",stocknames[stocks[s]],".csv"),sep=",")[,1]
E0_1<-R0/(a_s*(1-R0/K_s))  # Solve E0
#inds<-which(E0_1>0)  #indices where R0 < K
#comp.inds[stocks[s],inds]<-1

inds1<-which(E0_1>0)  #indices where R0 < K
comp.inds1[stocks[s],inds1]<-1

R0[which(R0>K_s)]<-0.999*K_s[which(R0>K_s)] 
E0<-R0/(a_s*(1-R0/K_s))  # Solve E0
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


save(comp.inds,Smolt_MSY,Smolt_lim,Eggs_MSY,MSY,file="ref_pts_long_2023.RData")






