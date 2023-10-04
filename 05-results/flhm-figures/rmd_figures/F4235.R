# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.5

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*



p_year<-1997         #first year in plot

final_year<-years-1
maxage<-6
stock_indices<-1:17
stocks<-length(stock_indices)
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)

tend<-years+2
yend<-1987+years+1


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

p <- recordPlot()
p



