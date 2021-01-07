# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Prob_Spawner_increase.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
rm(list=ls(all=TRUE))

# Henni:
source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")

library(writexl)


################################################################################
#! #############################################################################
# Version of the estimation model
Model<-"2020_updated"
LastPredYear<-2032  #2032

nrScen<-5

#! Set the last year for historic part and the last year for predictions:
LastHistYear<-2019    
yBreak<-length(c(1992:LastHistYear))

Nstocks<-17

#! #############################################################################
################################################################################


#Number of simulations in PerformStats file
sim<- 1000

#Number of years in future projections
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1) 
Years<-c(years[1]:years[2])

File<-paste0(PathScen,"ScenProj_",Model,"_EScen5_long.RData") # scenario 5, no fishing
load(File)
maxY=length(SmoltW[1,,1])

target_R0_3<-array(NA, dim=c(Nstocks))  #"True R0 with predicted vital rates"
Eggs<-array(NA,dim=c(years[3],Nstocks,1000)) #Etot?


for(r in 1:Nstocks){
  # for(i in 1:1000){
  target_R0_3[r]<-mean(SmoltW[r,(maxY-60):(maxY-12),]) #smolt production average in the distant future, "True R0"
  #    }
}


solveMSY<-function(a,K,R0,inc){
  if(R0>K) 
  { 
    print(paste("R0 > K!  :",R0/K))
    R0=0.99*K
  }
  Eggs=c()
  Eggs[1]=0
  R=c()
  R[1]=0
  i=1
  Rcurrent=0
  while(Rcurrent<R0){
    i=i+1
    Eggs[i]=Eggs[i-1]+inc
    R[i]=Eggs[i]*K/(K/a+Eggs[i])
    Rcurrent=R[i]
    #print(R[i])
  }
  
  
  E0=Eggs[i]
  print(paste("E0:",E0))
  Surplus=c()
  Surplus[1]=0
  solution=0
  k=2
  while(solution==0 ||k==i){
    Surplus[k]=R[k]-R0*Eggs[k]/E0
    #print(Surplus)
    if(Surplus[k]<Surplus[k-1]){
      solution=1
      MSY=k-1
      #print(R[k])
    }
    k=k+1
    
  }
  return(list(EMSY=Eggs[MSY],RMSY=R[MSY],RGEN=Eggs[MSY]*R0/E0))
}

target_R0<-array(NA, dim=c(1000,Nstocks))
target_R0_2<-array(NA, dim=c(1000,Nstocks)) 
  target_spawners80<-array(NA, dim=c(1000,Nstocks)) 
  smoltw_mean<-array(NA,dim=c(Nstocks,years[3],1000))
  spawner_mean<-array(NA,dim=c(Nstocks,years[3],1000))
  
  for(r in 1:Nstocks){
    for(i in 1:1000){
      target_R0[i,r]<-(mean(R0[(yBreak-4):yBreak,r,i])/mean(R0[(yBreak-4):yBreak,r,]))*target_R0_3[r] # adjusting the R0 so that the mean is the true R0, but variation retains the correlation with other parameters
      target_R0_2[i,r]<-mean(R0[(yBreak-3):yBreak,r,i]) # over last 4 years
      target_spawners80[i,r]<-(target_R0[i,r]*0.8/((1/BH_alpha[i,r])*(1-0.8*target_R0[i,r]*BH_beta[i,r]))) #SPawner target to keep smolt production at 80% of R0

      for(y in 1:years[3]){
        d=max(1,(y-3))  # smolt average over 4 years 
        smoltw_mean[r,y,i]<-mean(SmoltW[r,d:y,i])
        spawner_mean[r,y,i]<-mean(Eggs[d:y,r,i])
      }
      
    }
  }
  
  #Calculate the chance of some statistic (vector) being above some reference point
  risk<-function(x,ref) {sum(ifelse(x>ref,1,0))/length(x)}
  
  #Proportion of simulations in which the recruitment is above 75% carrying capacity
  #by year
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      RecRisk[r,scen,y]<-risk(SmoltW[r,y,],0.75*target_R0[,r])                 # probability that smoltprod > R0
      RecRisk3[r,scen,y]<-mean(SmoltW[r,y,]/0.75*target_R0[,r]) #    mean(SmoltW[r,y,]/target_MSY[,r])             # posterior mean ratio between smolt prod & msy target
      RecRisk4[r,scen,y]<-risk(smoltw_mean[r,y,],0.75*target_R0[,r])      # probability to exceed 75% R0
      RecRisk5[r,scen,y]<-risk(smoltw_mean[r,y,],target_MSY[,r])          # probability to exceed MSY recruitment
      #Sp80Risk[r,scen,y]<-risk(spawner_mean[r,y,],target_spawners80[,r])
      #SpMSYRisk[r,scen,y]<-risk(spawner_mean[r,y,],target_spawnersMSY[,r])
      Sp80Risk[r,scen,y]<-mean(spawner_mean[r,y,]/target_spawners80[,r])
      SpMSYRisk[r,scen,y]<-mean(spawner_mean[r,y,]/target_spawnersMSY[,r])
      Sp80RiskP[r,scen,y]<-risk(spawner_mean[r,y,],target_spawners80[,r])
      SpMSYRiskP[r,scen,y]<-risk(spawner_mean[r,y,],target_spawnersMSY[,r])
    }
  } 
  
  for(r in 1:Nstocks){
    # .	Smolt production (SP) relative to the maximal theoretical smolt production (R0) - SP/R0,
    RecRisk_1_28[r,scen]<-mean(smoltw_mean28[r,]/target_R0[,r])
    RecRisk_1_1525[r,scen]<-mean(smoltw_mean1525[r,]/target_R0[,r])
    
    # .	SP relative to SP at MSY stock level (SPMSY) - SP/SPMSY,
    RecRisk_2_28[r,scen]<-mean(smoltw_mean28[r,]/target_MSY[,r])
    RecRisk_2_1525[r,scen]<-mean(smoltw_mean1525[r,]/target_MSY[,r])
    
    # .	The probability that SP is above 0.75xR0 - P(SP>0.75xR0),
    RecRisk_3_28[r,scen]<-risk(smoltw_mean28[r,],0.75*target_R0[,r])
    RecRisk_3_1525[r,scen]<-risk(smoltw_mean1525[r,],0.75*target_R0[,r])   
    
    #.	The probability that SP is above 0.50xR0 - P(SP>0.50xR0),
    RecRisk_4_28[r,scen]<-risk(smoltw_mean28[r,],0.5*target_R0[,r])
    RecRisk_4_1525[r,scen]<-risk(smoltw_mean1525[r,],0.5*target_R0[,r])   
    
    #.	The probability that SP is above SPMSY - P(SP>SPMSY),
    RecRisk_5_28[r,scen]<-risk(smoltw_mean28[r,],target_MSY[,r])
    RecRisk_5_1525[r,scen]<-risk(smoltw_mean1525[r,],target_MSY[,r])  
    
    #.	The probability that SP is above 0.75xSPMSY - P(SP>0.75xSPMSY),
    RecRisk_6_28[r,scen]<-risk(smoltw_mean28[r,],0.75*target_MSY[,r])
    RecRisk_6_1525[r,scen]<-risk(smoltw_mean1525[r,],0.75*target_MSY[,r])  
    
    #.	The probabity that SP is above 0.50xSPMSY - P(SP>0.50xSPMSY),
    RecRisk_7_28[r,scen]<-risk(smoltw_mean28[r,],0.5*target_MSY[,r])
    RecRisk_7_1525[r,scen]<-risk(smoltw_mean1525[r,],0.5*target_MSY[,r])  
    
    # Probability that SP is above RGEN
    RecRisk_8_28[r,scen]<-risk(smoltw_mean28[r,],target_RGEN[,r])
    RecRisk_8_1525[r,scen]<-risk(smoltw_mean1525[r,],target_RGEN[,r])  
    
    #.	Estimated catch in the river (river catch),
    River_Catch_mean_28[r,scen]<-mean(River_Catch_28[r,])
    River_Catch_mean_1525[r,scen]<-mean(River_Catch_1525[r,])
    
    #.	Estimated catch in the sea (sea catch),
    Sea_Catch_mean_28[r,scen]<-mean(Sea_Catch_28[r,])
    Sea_Catch_mean_1525[r,scen]<-mean(Sea_Catch_1525[r,])
    
    Comm_Catch_mean_28[r,scen]<-mean(Comm_Catch_28[r,])
    Comm_Catch_mean_1525[r,scen]<-mean(Comm_Catch_1525[r,])
    
    # Spawners per river
    Spawners_mean_28[r,scen]<-mean(Spawners_28[r,])
    Spawners_mean_1525[r,scen]<-mean(Spawners_1525[r,])
    
  }
  ## Tornio & Simo: 4 year averages
  
  for(r in 1:Nstocks){
    
    RecRisk2[r,scen]<-risk(smoltw_mean[r,26,],0.8*target_R0[,r])
  }
  
  
}

dim(RecRisk)
cbind(t(RecRisk[1,,]), c(1992:LastPredYear))

FirstYear50<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk[r,s,y]>0.499){FirstYear50[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear50)<-RiverNames
colnames(FirstYear50)<-c(1:6);FirstYear50
write.xlsx(FirstYear50, file=paste0(PathOut,"FirstYear50.xlsx"))

FirstYear70<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk[r,s,y]>0.699){FirstYear70[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear70)<-RiverNames
colnames(FirstYear70)<-c(1:6);FirstYear70
write.xlsx(FirstYear70, file=paste0(PathOut,"FirstYear70.xlsx"))


#######################

#ScenNames<-c("1","2","3","4","5","6")
#ScenLty=c(1:6)
#ScenPch = c(19,22,3,25,8,5)
#ScenNames<-c("1","2","3","4","5","6","7","8","9","10","11")
ScenNames=c(0.079,
            0.095,
            0.063,
            0.1,
            "No fishing",
            "No commercial fishing",
            "Only commercial fishing @ 0.79",
            0.164,
            0.367,
            0.575,
            0.023)

ScenLty=c(1:11)
ScenPch = c(19,22,3,25,8,9,10,11,12,13,14)

v1<-2023.5 # What are these?
v2<-2025.5

#
windows(record=T)
#pdf(file="evaluationplots.pdf")
par(mfrow=c(2,1),cex=0.5)
#par(mfrow=c(3,1))
par(mar=c(2.5,4,3,3))

for(r in 1:Nstocks){
  Risk<-cbind(RecRisk4[r,1,],RecRisk4[r,2,],RecRisk4[r,3,],
              RecRisk4[r,4,],RecRisk4[r,5,] ,RecRisk4[r,6,],RecRisk4[r,7,],RecRisk4[r,8,],RecRisk4[r,9,],RecRisk4[r,10,],RecRisk4[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab = "P(1y smolt avg > 0.75R0)", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.7,col="red",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.7")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}

for(r in 1:Nstocks){
  Risk<-cbind(RecRisk5[r,1,],RecRisk5[r,2,],RecRisk5[r,3,],
              RecRisk5[r,4,],RecRisk5[r,5,] ,RecRisk5[r,6,],RecRisk5[r,7,],RecRisk5[r,8,],RecRisk5[r,9,],RecRisk5[r,10,],RecRisk5[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab = "P(1y smolt avg > R_MSY)", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.7,col="red",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.7")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}



for(r in 1:Nstocks){
  Risk<-cbind(RecRisk3[r,1,],RecRisk3[r,2,],RecRisk3[r,3,],
              RecRisk3[r,4,],RecRisk3[r,5,] ,RecRisk3[r,6,],RecRisk3[r,7,],RecRisk3[r,8,],RecRisk3[r,9,],RecRisk3[r,10,],RecRisk3[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,max(Risk[])), xlab = "Year", 
       ylab = "Smolts/R0)", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.75,col="red",lwd=2)
  abline(h=RMSY[r],col="blue",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.75")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}

for(r in 1:Nstocks){
  Risk<-cbind(Sp80Risk[r,1,],Sp80Risk[r,2,],Sp80Risk[r,3,],
              Sp80Risk[r,4,],Sp80Risk[r,5,] ,Sp80Risk[r,6,],Sp80Risk[r,7,],Sp80Risk[r,8,],Sp80Risk[r,9,],Sp80Risk[r,10,],Sp80Risk[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,max(Risk[])), xlab = "Year", 
       ylab = "Eggs/ Eggs@ .80 R0", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.75,col="red",lwd=2)
  #abline(h=RMSY[r],col="blue",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.75")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}

for(r in 1:Nstocks){
  Risk<-cbind(SpMSYRisk[r,1,],SpMSYRisk[r,2,],SpMSYRisk[r,3,],
              SpMSYRisk[r,4,],SpMSYRisk[r,5,] ,SpMSYRisk[r,6,],SpMSYRisk[r,7,],SpMSYRisk[r,8,],SpMSYRisk[r,9,],SpMSYRisk[r,10,],SpMSYRisk[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,max(Risk[])), xlab = "Year", 
       ylab = "Eggs/Eggs @ .MSY R0", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.75,col="red",lwd=2)
  #abline(h=RMSY[r],col="blue",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.75")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}


for(r in 1:Nstocks){
  Risk<-cbind(Sp80RiskP[r,1,],Sp80RiskP[r,2,],Sp80RiskP[r,3,],
              Sp80RiskP[r,4,],Sp80RiskP[r,5,] ,Sp80RiskP[r,6,],Sp80RiskP[r,7,],Sp80RiskP[r,8,],Sp80RiskP[r,9,],Sp80RiskP[r,10,],Sp80RiskP[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,max(Risk[])), xlab = "Year", 
       ylab = "P(Eggs > Eggs@.80 R0)", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.75,col="red",lwd=2)
  #abline(h=RMSY[r],col="blue",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.75")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}

for(r in 1:Nstocks){
  Risk<-cbind(SpMSYRiskP[r,1,],SpMSYRiskP[r,2,],SpMSYRiskP[r,3,],
              SpMSYRiskP[r,4,],SpMSYRiskP[r,5,] ,SpMSYRiskP[r,6,],SpMSYRiskP[r,7,],SpMSYRiskP[r,8,],SpMSYRiskP[r,9,],SpMSYRiskP[r,10,],SpMSYRiskP[r,11,])              ####!
  plot(Years, Risk[,1], type = "n", ylim=c(0,max(Risk[])), xlab = "Year", 
       ylab = "P(Eggs > Sp@ .MSY R0", main = RiverNames[r])
  for(i in 1:nrScen){
    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  abline(h=1)
  abline(h=0.75,col="red",lwd=2)
  #abline(h=RMSY[r],col="blue",lwd=2)
  abline(h=0.5)
  text(2033,0.8,"0.75")
  text(2033,0.6,"0.5")
  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")# bty ="n"
  abline(v=v1); abline(v=v2)
  
}
#windows(record=TRUE)
#### Number of stocks with expected smolt production over 075R0
par(mfrow=c(2,2))
over75<-array(0, dim=c(Nstocks,nrScen,years[3]))
over75_total=array(NA,dim=c(nrScen,years[3]))
for(s in 1:nrScen){
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      if(RecRisk3[r,s,y]>0.75) over75[r,s,y]=1
    }
    
    over75_total[s,y]=sum(over75[,s,y])
  }
}

plot(Years,over75_total[5,],type="l",ylim=c(0,Nstocks),xlab="Year",ylab="Number of rivers over 0.75R0")
for(i in 1:nrScen){
  if(i==1) lwd=3
  else lwd=1
  points(Years,over75_total[i,],type="l",col=i,lwd=lwd)
}

#### Number of rivers with P(smolts>0.75R0)>0.7

over75<-array(0, dim=c(Nstocks,nrScen,years[3]))
over75_total=array(NA,dim=c(nrScen,years[3]))
for(s in 1:nrScen){
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      if(RecRisk4[r,s,y]>0.7) over75[r,s,y]=1
    }
    
    over75_total[s,y]=sum(over75[,s,y])
  }
}

plot(Years,over75_total[5,],type="l",ylim=c(0,Nstocks),xlab="Year",ylab="Number of rivers with P(smolts>0.75R0)>0.7")
for(i in 1:nrScen){
  if(i==1) lwd=3
  else lwd=1
  points(Years,over75_total[i,],type="l",col=i,lwd=lwd)
}

#### Number of stocks with expected smolt production over R_MSY

over75<-array(0, dim=c(Nstocks,nrScen,years[3]))
over75_total=array(NA,dim=c(nrScen,years[3]))
for(s in 1:nrScen){
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      if(RecRisk3[r,s,y]>RMSY[r]) over75[r,s,y]=1
    }
    
    over75_total[s,y]=sum(over75[,s,y])
  }
}

plot(Years,over75_total[5,],type="l",ylim=c(0,Nstocks),xlab="Year",ylab="Number of rivers over R_MSY")

for(i in 1:nrScen){
  if(i==1) lwd=3
  else lwd=1
  points(Years,over75_total[i,],type="l",col=i,lwd=lwd)
}

#### Number of rivers with P(smolts>RMSY)>0.7

over75<-array(0, dim=c(Nstocks,nrScen,years[3]))
over75_total=array(NA,dim=c(nrScen,years[3]))
for(s in 1:nrScen){
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      if(RecRisk5[r,s,y]>0.7) over75[r,s,y]=1
    }
    
    over75_total[s,y]=sum(over75[,s,y])
  }
}

plot(Years,over75_total[5,],type="l",ylim=c(0,Nstocks),xlab="Year",ylab="Number of rivers with P(smolts>RMSY)>0.7")
for(i in 1:nrScen){
  if(i==1) lwd=3
  else lwd=1
  points(Years,over75_total[i,],type="l",col=i,lwd=lwd)
}

#dev.off()

classification=array(0,dim=c(Nstocks,nrScen))
short=32 #2024
long=years[3] #2088
for(s in 1:nrScen){
  for(r in 1:Nstocks){
    if(RecRisk3[r,s,short]> 0.75 & RecRisk3[r,s,long]>0.75) classification[r,s]="Good"
    if(RecRisk3[r,s,short]< 0.75 & RecRisk3[r,s,long]>0.75){
      year=0
      for(j in short:long){
        if(RecRisk3[r,s,j]<0.75) year=year+1
      }
      classification[r,s]=paste("Promising:",year)
    }
    if(RecRisk3[r,s,short]> 0.75 & RecRisk3[r,s,long]<0.75){
      year=0
      for(j in short:long){
        if(RecRisk3[r,s,j]>0.75) year=year+1
      }
      classification[r,s]=paste("Alarming:",year)
    } 
    if(RecRisk3[r,s,short]< 0.75 & RecRisk3[r,s,long]<0.75) classification[r,s]="Poor"
    
  }
}

classification=rbind(ScenNames,classification)
classification=cbind(c("River",RiverNames),classification)
classification

write.csv(classification,file="classification_msy.csv")

#windows()
#par(mfrow=c(4,1))
#par(mar=c(2.5,4,3,3))

#for(r in 13:15){
#  Risk<-cbind(RecRisk3[r,1,],RecRisk3[r,2,],RecRisk3[r,3,],
#  RecRisk3[r,4,],RecRisk3[r,5,]) #,RecRisk[r,6,],RecRisk[r,7,],RecRisk[r,8,])
#  plot(Years, Risk[,1], type = "n", ylim=c(0,2), xlab = "Year", 
#  ylab = "Smolts/R0", main = RiverNames[r])
#  for(i in 1:nrScen){
#    points(Years, Risk[,i], pch = ScenPch[i], type="b", lty=ScenLty[i])
#  }
#  abline(h=1)
#  abline(h=0.75,col="red",lwd=2)
#  abline(h=0.5)
#  text(2033,0.8,"0.75")
#  text(2033,0.6,"0.5")
#  legend(x="topleft", ScenNames, lty=ScenLty, pch=ScenPch,bg="white")
#  abline(v=v1); abline(v=v2)
#  
#}

## Values for PSPC table 

Compyear1=which(Years==v2)
Compyear2=which(Years==(v2-1))
Compyear=c(rep(Compyear1,times=13),Compyear2,Compyear2,Compyear1,Compyear1)

PSPC<-matrix(NA,nrow=Nstocks,ncol=nrScen)
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    PSPC[r,s]=RecRisk[r,s,Compyear[r]]
  }
}

PSPC=cbind(RiverNames,Years[Compyear],PSPC)
PSPC # Copy-paste to excel

barplot(rbind(RMSY,RGEN),names.arg=RiverNames,beside=T,ylim=c(0,1),ylab="Smolt production/PSPC",las=2,legend.text = c("R_MSY","R_lim"))
abline(h=0.75)

