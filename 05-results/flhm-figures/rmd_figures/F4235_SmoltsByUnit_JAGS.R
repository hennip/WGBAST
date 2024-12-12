 

##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalix?lven"
#4 "R?ne?lven"
#5 "Pite?lven"
#6 "?by?lven"
#7 "Byske?lven"
#8 "Rickle?n"
#9 "S?var?n"
#10 "Vindel?lven"
#11 "?re?lven"
#12 "Logde?lven"
#13 "Ljungan"
#14 "M?rrums?n"
#15 "Em?n"
#16 "K?ge?lven"

#setwd("C:/WGBAST15/")
#source("plotfunctions.r")
#source("//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2018/box_functions.r")

library(R2jags)
library(runjags)
library(rjags)

source("00-functions/box_functions.R")

load(file="C:/Users/03080932/OneDrive - Valtori/WGBAST_shared/flhm/chains_cleaned_2503.RData")  ; chains<-chains_new; nsim<-1500
d<-as.matrix(chains_new)
load("C:/output/wgbast/scen/2021/ref_pt_distns.RData")

first_year<-1987
p_year<-1997         #first year in plot
assess_year<-2021
years<-length(first_year:assess_year)
final_year<-years-1
maxage<-6
stock_indices<-1:17
stocks<-length(stock_indices)
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)

tend<-years+2
yend<-1987+years+1

#R0_mult<-0.75

#get SmoltW variables into array
sm<-d[ ,grep("SmoltW\\[",colnames(d))]  #34 x 16 for 2018 assess
smolts<-array(0,dim=c(years+2,stocks,dim(d)[1]))           
smolts_au<-array(0,dim=c(years+2,5,dim(d)[1]))     #AU 1-4, TOTAL

for(y in 1:(years+2)){  #year       
            for(s in 1:stocks){
                  smolts[y,s,]<-sm[ ,(s-1)*(years+2)+y]         
      }
}

for(y in 1:(years+2)){  #year
smolts_au[y,1,]<-apply(smolts[y,which(AU==1),],2,sum)
smolts_au[y,2,]<-apply(smolts[y,which(AU==2),],2,sum)
smolts_au[y,3,]<-apply(smolts[y,which(AU==3),],2,sum)
smolts_au[y,4,]<-apply(smolts[y,which(AU==4),],2,sum)
smolts_au[y,5,]<-apply(smolts[y,1:stocks,],2,sum)
}  

dim(R0_all)
dim(Smolt_MSY)
dim(Smolt_lim)

Rlim_AU<-Rmsy_AU<-R0_AU<-array(NA, dim=c(5,nsim))

for(s in 1:nsim){
  R0_AU[1,s]<-sum(R0_all[1:4,s])
  R0_AU[2,s]<-sum(R0_all[5:12,s])+R0_all[16,s]
  R0_AU[3,s]<-R0_all[13,s]+R0_all[17,s]
  R0_AU[4,s]<-sum(R0_all[14:15,s])
  R0_AU[5,s]<-sum(R0_all[1:17,s])
  
  Rlim_AU[1,s]<-sum(Smolt_lim[1:4,s])
  Rlim_AU[2,s]<-sum(Smolt_lim[5:12,s])+Smolt_lim[16,s]
  Rlim_AU[3,s]<-Smolt_lim[13,s]+Smolt_lim[17,s]
  Rlim_AU[4,s]<-sum(Smolt_lim[14:15,s])
  Rlim_AU[5,s]<-sum(Smolt_lim[1:17,s])
  
  Rmsy_AU[1,s]<-sum(Smolt_MSY[1:4,s])
  Rmsy_AU[2,s]<-sum(Smolt_MSY[5:12,s])+Smolt_MSY[16,s]
  Rmsy_AU[3,s]<-Smolt_MSY[13,s]+Smolt_MSY[17,s]
  Rmsy_AU[4,s]<-sum(Smolt_MSY[14:15,s])
  Rmsy_AU[5,s]<-sum(Smolt_MSY[1:17,s])
  
  }

# medians

medR0<-medRlim<-medRmsy<-c()
for(u in 1:5){
  medR0[u]<-median(R0_AU[u,])
  medRlim[u]<-median(Rlim_AU[u,])
  medRmsy[u]<-median(Rmsy_AU[u,])
}


#windows(12,12)
par(mfrow=c(3,2),mar=c(3,4,2,0.1),oma=c(1,1,1,1),font=2,font.lab=2,font.axis=2)

bx90(smolts_au[,1,],1997,yend,1,0,4000,beg=11,end=tend,border="black",main="AU 1", cex.main=1.2);
abline(h=medR0[1],col="black")
abline(h=medRlim[1],col="red")
abline(h=medRmsy[1],col="blue")

bx90(smolts_au[,2,],1997,yend,1,0,800,beg=11,end=tend,border="black",main="AU 2", cex.main=1.2);
abline(h=medR0[2],col="black")
abline(h=medRlim[2],col="red")
abline(h=medRmsy[2],col="blue")

bx90(smolts_au[,3,],1997,yend,1,0,10,beg=11,end=tend,border="black",main="AU 3", cex.main=1.2);
abline(h=medR0[3],col="black")
abline(h=medRlim[3],col="red")
abline(h=medRmsy[3],col="blue")

bx90(smolts_au[,4,],1997,yend,1,0,80,beg=11,end=tend,border="black",main="AU 4", cex.main=1.2);
abline(h=medR0[4],col="black")
abline(h=medRlim[4],col="red")
abline(h=medRmsy[4],col="blue")

bx90(smolts_au[,5,],1997,yend,1,0,4000,beg=11,end=tend,border="black",main="AU 1-4 total", cex.main=1.2);
abline(h=medR0[5],col="black")
abline(h=medRlim[5],col="red")
abline(h=medRmsy[5],col="blue")

mtext("Smolt production (thousands)", side = 2, line = -1, outer = TRUE,cex=1)
#mtext("Year", side = 1, line = 2, outer = TRUE, cex=0.8)



