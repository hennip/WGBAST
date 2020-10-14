# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Smolt_SpawnerGraphs.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
rm(list=ls(all=TRUE))

source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")


# ===============================================================================

cbind(year,c(1:length(year)))

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

#maxSmolt<-c(
#4000,90,1500,160,80,
#40,300,20,30,600,
#100,90,7,200,25,120)
#maxSpawner<-c(
#400,10,200,17,12,
#5,35,2,2.5,25,
#12,10,0.7,50,4,10)

#if(EffScen==5){
maxSmolt<-c(
  3000,120,1300,200,
  60,50,300,27,
  60,400,150,130,
  7,200,30,100, 30)
maxSpawner<-c(
  700,25,300,30,
  10,8,70,4,
  9,20,25,20,
  1,40,8,20,3)
#}
#yStart<-c(rep(1,15),22,22)
yStart<-c(rep(1,17))

File<-c()
File[3]<-paste0(PathScen,"ScenProj_",Model,"_EScen1.RData")
File[2]<-paste0(PathScen,"ScenProj_",Model,"_EScen4.RData")
File[1]<-paste0(PathScen,"ScenProj_",Model,"_EScen5.RData")

colX<-c("blue","red", "black")


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

save(Smolts, file=paste0(PathScen, "Smolts.RData"))
save(Spawners, file=paste0(PathScen, "Spawners.RData"))

File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")
save(list = Perform_Stats, file = File)

#tiff(paste0(PathScen,"F4328_fig2_of_4.tiff"),  width=1600, height=2000, res=200) 
#windows(record=T)

# uncomment 1 tiff and 1 r-loop at a time!
# tiff(paste0(PathScen,"F4328_fig1_of_4.tiff"),  width=1600, height=2000, res=200) 
# par(mfrow=c(4,2))
# par(mar=c(2.5,4,4,1))
# for(r in 1:4){     

# tiff(paste0(PathScen,"F4328_fig2_of_4.tiff"),  width=1600, height=2000, res=200)
# par(mfrow=c(4,2))
# par(mar=c(2.5,4,4,1))
# for(r in 5:8){

# tiff(paste0(PathScen,"F4328_fig3_of_4.tiff"),  width=1600, height=2000, res=200)
# par(mfrow=c(4,2))
# par(mar=c(2.5,4,4,1))
# for(r in 9:12){

 tiff(paste0(PathScen,"F4328_fig4_of_4.tiff"),  width=1600, height=2500, res=200)
 par(mfrow=c(5,2))
 par(mar=c(2.5,4,4,1))
 for(r in 13:17){
   

    for(f in 1:3){
      load(File[f])
      med<-stats(SmoltW[r,,])[,1]
      low<-stats(SmoltW[r,,])[,2]
      high<-stats(SmoltW[r,,])[,3]
      
      if(f==1){
        plot(year[yStart[r]:Nyears],med[yStart[r]:Nyears], pch=19, ylab="1000's of salmon", 
             ylim=c(0,maxSmolt[r]), xlab="",
             main=paste(sep="",river[r]," smolts"),col=colX[f])
        segments(year[yStart[r]:Nyears], low[yStart[r]:Nyears], 
                 year[yStart[r]:Nyears], high[yStart[r]:Nyears],col=colX[f])
        
      }else{
        points(year[yStart[r]:Nyears],med[yStart[r]:Nyears], pch=19, ylim=c(0,maxSmolt[r]),
               col=colX[f])
        segments(year[yStart[r]:Nyears], low[yStart[r]:Nyears], 
                 year[yStart[r]:Nyears], high[yStart[r]:Nyears],col=colX[f])
      }
    }
    
    for(f in 1:3){
      load(File[f])
      med<-stats(SpawnerW[r,,])[,1]
      low<-stats(SpawnerW[r,,])[,2]
      high<-stats(SpawnerW[r,,])[,3]
      
      if(f==1){
        plot(year[yStart[r]:Nyears],med[yStart[r]:Nyears], pch=19, ylab="1000's of salmon", 
             ylim=c(0,maxSpawner[r]), xlab="",
             main=paste(sep="",river[r]," spawners"),col=colX[f])
        segments(year[yStart[r]:Nyears], low[yStart[r]:Nyears], 
                 year[yStart[r]:Nyears], high[yStart[r]:Nyears],col=colX[f])
        
      }else{
        points(year[yStart[r]:Nyears],med[yStart[r]:Nyears], pch=19,
               ylim=c(0,maxSpawner[r]),col=colX[f])
        segments(year[yStart[r]:Nyears], low[yStart[r]:Nyears], 
                 year[yStart[r]:Nyears], high[yStart[r]:Nyears],col=colX[f])
      }
    }
    
  }
  

dev.off()

tmp<-0
if(tmp==1){
  # Only smolts (needed in advice)
  par(mfrow=c(4,2))
  par(mar=c(2.5,4,4,1))
  
  for(r in 1:15){    
    med<-stats(SmoltW[r,,])[,1]
    low<-stats(SmoltW[r,,])[,2]
    high<-stats(SmoltW[r,,])[,3]
    plot(year,med, pch=19, ylab="1000's of salmon", ylim=c(0,maxSmolt[r]),
         main=paste(sep="",river[r]," smolts"), xlim=c(1992,2020.5))
    segments(year, low, year, high)
  }
}

