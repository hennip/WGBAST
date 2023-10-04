# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Smolt_SpawnerGraphs.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               



AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)


#! Mps
choice<-"MED"

#! Set the last year for historic part and the last year for predictions:
stats<-function(dat){
  Mean1<-NULL; Median1<-NULL; Low1<-NULL; High1<-NULL;
  
  for(y in 1:(dim(dat)[1])){
    temp1<-as.mcmc(dat[y,])
    sum_temp1<-summary(temp1, quantiles=c(0.05,0.1,0.5,0.8,0.95))
    Mean1[y]<-mean(temp1)
    Low1[y]<-sum_temp1$quantiles[1]
    Median1[y]<-sum_temp1$quantiles[3]
    High1[y]<-sum_temp1$quantiles[5]
  }
  result<- cbind(Median1,Low1,High1)
  return(result)
}

#! #############################################################################
################################################################################

river<-c("Torne","Simo","Kalix","Råne","Pite","Åby","Byske","Rickleån",
         "Sävarån","Ume/Vindel","Öre","Lögde","Ljungan","Mörrumsån","Emån","Kåge", "Testeboån")

maxSmolt<-c(
  3000,120,1300,200,
  50,25,250,20,
  30,400,100,100,
  5,150,25,80,10)
maxSpawner<-c(
  600,20,250,25,
  6,3,40,3,
  4,30,10,10,
  0.6,20,5,10,0.5)


File<-c()
File[1]<-paste0(PathScen,"ScenProj_",Model,"_EScen1_RCzero23-35.RData")
File[2]<-paste0(PathScen,"ScenProj_",Model,"_EScen12_RCzero23-35.RData")
File[3]<-paste0(PathScen,"ScenProj_",Model,"_EScen15_RCzero23-35.RData")

Smolts<-Spawners<-array(NA, dim=c(Nyears,3,Nstocks,3))
for(f in 1:3){
  load(File[f]); print(File[f])
  
  for(r in 1:17){
    Smolts[,1,r,f]<-stats(SmoltW[r,,])[,1]
    Smolts[,2,r,f]<-stats(SmoltW[r,,])[,2]
    Smolts[,3,r,f]<-stats(SmoltW[r,,])[,3]
    
    Spawners[,1,r,f]<-stats(SpawnerW[r,,])[,1]
    Spawners[,2,r,f]<-stats(SpawnerW[r,,])[,2]
    Spawners[,3,r,f]<-stats(SpawnerW[r,,])[,3]
    
  }
}



plotyears<-which(year==assessment_year+15)
yStart<-c(rep(1,15),17,9)
colX<-c("dodgerblue3","black", "red")
## uncomment 1 tiff and 1 r-loop at a time!
par(mfrow=c(4,2))
par(mar=c(2.5,4,4,1))
for(r in 1:4){     
  
  for(f in 1:3){
    #load(File[f])
    med<-Smolts[,1,r,f]
    low<-Smolts[,2,r,f]
    high<-Smolts[,3,r,f]
    
    if(f==1){
      plot(Years[yStart[r]:plotyears],med[yStart[r]:plotyears], pch=19, ylab="1000's of salmon", 
           ylim=c(0,maxSmolt[r]), xlab="",
           main=paste(sep="",river[r]," smolts"),col=colX[f])
      segments(Years[yStart[r]:plotyears], low[yStart[r]:plotyears], 
               Years[yStart[r]:plotyears], high[yStart[r]:plotyears],col=colX[f])
      
    }else{
      points(Years[yStart[r]:plotyears]+f*0.2,med[yStart[r]:plotyears], pch=19, ylim=c(0,maxSmolt[r]),
             col=colX[f])
      segments(Years[yStart[r]:plotyears]+f*0.2, low[yStart[r]:plotyears], 
               Years[yStart[r]:plotyears]+f*0.2, high[yStart[r]:plotyears],col=colX[f])
    }
  }
  
  for(f in 1:3){
    #load(File[f])
    med<-Spawners[,1,r,f]
    low<-Spawners[,2,r,f]
    high<-Spawners[,3,r,f]
    
    if(f==1){
      plot(Years[yStart[r]:plotyears],med[yStart[r]:plotyears], pch=19, ylab="1000's of salmon", 
           ylim=c(0,maxSpawner[r]), xlab="",
           main=paste(sep="",river[r]," spawners"),col=colX[f])
      segments(Years[yStart[r]:plotyears], low[yStart[r]:plotyears], 
               Years[yStart[r]:plotyears], high[yStart[r]:plotyears],col=colX[f])
      
    }else{
      points(Years[yStart[r]:plotyears]+f*0.2,med[yStart[r]:plotyears], pch=19,
             ylim=c(0,maxSpawner[r]),col=colX[f])
      segments(Years[yStart[r]:plotyears]+f*0.2, low[yStart[r]:plotyears], 
               Years[yStart[r]:plotyears]+f*0.2, high[yStart[r]:plotyears],col=colX[f])
    }
  }
  
}

