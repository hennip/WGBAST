# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# Prob_Spawner_increase.R  from Polina (9/2008)
# 
# Makes the graphs of the smolt and spawner amounts for different rivers. 
#
# Changes by Henni.
# - graphs without FLCore 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~               
#rm(list=ls(all=TRUE))


#Number of simulations in PerformStats file
LastPredYear
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1) 
Years<-c(years[1]:years[2])
#Years = c(1992:(1992+381))
sim<- nsim   
nrScen<-length(scen_risk)
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
################################################################################
#! #############################################################################
# Version of the estimation model

assessment_year<-2024


#! Mps
choice<-"MED"


#! Set the last year for historic part and the last year for predictions:


#Number of years in future projections


#Set AU-specific generation times for years of comparison
gen_time.old<-c(7,7,7,6)  #AU-specific generation time in years
#6th July
gen_time<-c(6,6,5,5) 

Compyear1<-rep(which(Years==(assessment_year-1)),times=Nstocks)   #current stock status 2022
Compyear2<-match((assessment_year-1)+gen_time[AU],Years)      #fishing in 2024 -> 2027/2028
Compyear3<-match((assessment_year-1)+5*gen_time[AU],Years)
Compyear4<-match((assessment_year-1)+2*gen_time[AU],Years)

#Calculate the chance of some statistic (vector) being above some reference point
#risk<-function(x,ref) {sum(ifelse(x>ref,1,0))/length(x)}
riskz<-function(x,ref,zprop,thr) {sum(ifelse(x[which(zprop>thr)]>ref[which(zprop>thr)],1,0))/length(x)}


#load the reference points 
load(paste0(PathRefP,"ref_pts_2024_JAGS_Mps.RData"))
load(paste0(PathRefP,"eqm_distns_Mps.RData"))


#final year R0 from assessment
yBreak_assess<-yBreak+5

d<-chains


R_zero<-array(NA, dim=c(sim,yBreak_assess,Nstocks))
R0_final<-array(NA, dim=c(sim,Nstocks))
smolt_sub<-d[,grep("SmoltW[34",colnames(d),fixed=TRUE)]
for(r in 1:Nstocks){
  for(y in 1:yBreak_assess){ 
    tempr<-paste0("R0[",y,",",r,"]")
    
    R_zero[1:sim,y,r]<-d[,grep(tempr,colnames(d),fixed=TRUE)]
    
  }
  R0_final[,r]<-apply(R_zero[,(yBreak_assess-4):yBreak_assess,r],1,mean)
}


refs_all<-array(0,dim=c(Nstocks,6))
#make a table of ref pt medians
for(r in 1:Nstocks){
  refs_all[r,1]<-median(Smolt_MSY[r,])
  refs_all[r,2]<-median(Smolt_lim[r, ])
  refs_all[r,3]<-median(0.75*R0_all[r,])
  refs_all[r,4]<-median(Smolt_MSY[r,]/R0_all[r,])
  refs_all[r,5]<-median(MSY[r,])
  refs_all[r,6]<-median(Smolt_lim[r,]/R0_all[r,])
  
}                      
rownames(refs_all)<-RiverNames
colnames(refs_all)<-c("RMSY analytical","Rlim","RMSY proxy","Analytical R0 prop","Analytical MSY","R_lim prop")


################################################################################
################################################################################


#Store risk values
RecRisk<-array(NA, dim=c(Nstocks,nrScen,years[3]))
RecRisk2<-array(NA, dim=c(Nstocks,nrScen,years[3]))
#RecRisk3<-array(NA, dim=c(Nstocks,nrScen,years[3]))
#RecRisk4<-array(NA, dim=c(Nstocks,nrScen,years[3])) #average of last 5 years' R0 for 2021 Annex
#RecRisk5<-array(NA, dim=c(Nstocks,nrScen,years[3])) #RMSY simulation for 2021 Annex

for(scen_id in 1:length(scen_risk)){ # number of scenarios
  scen = scen_risk[scen_id]
  print(scen)
  #scen<-7
  EffScen<-scen
  File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")
  load(File)

  #Proportion of simulations in which the recruitment is above R_MSY and Rlim by year
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      RecRisk[r,scen_id,y]<-riskz(SmoltW[r,y,which(comp.inds[r,]==1)],Smolt_MSY[r,which(comp.inds[r,]==1)],R0_all[r,],0.1)
      RecRisk2[r,scen_id,y]<-riskz(SmoltW[r,y,which(comp.inds[r,]==1)],Smolt_lim[r,which(comp.inds[r,]==1)],R0_all[r,],0.1)    
      #RecRisk3[r,scen,y]<-riskz(SmoltW[r,y,],0.75*R0_all[r,])    
      #RecRisk4[r,scen,y]<-riskz(SmoltW[r,y,],0.75*R0_final[,r])   
      #RecRisk5[r,scen,y]<-risk(SmoltW[r,y,1:1000],RMSY_sim[r,])   
    }
  } 
  
}  #loop over scenarios  

###############################################################################################



FirstYear_Rmsy50<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  #r<-12
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+6 #33 go through future years, starting at assessment year + 1  2024
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      #print(y+1991)
      #print(RecRisk[r,s,y])
      if(RecRisk[r,s,y]>=0.50){FirstYear_Rmsy50[r,s]<-y+1991; tmp<-1}
      else{y<-y+1}
    }
  }
}   
rownames(FirstYear_Rmsy50)<-RiverNames
colnames(FirstYear_Rmsy50)<-c(scen_risk)#;FirstYear_Rmsy50
#write.xlsx(FirstYear_Rmsy50, file=paste0(PathOut,"FirstYear_Rmsy50.xlsx"))

FirstYear_Rmsy70<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk[r,s,y]>=0.70){FirstYear_Rmsy70[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear_Rmsy70)<-RiverNames
colnames(FirstYear_Rmsy70)<-c(scen_risk)#;FirstYear_Rmsy70
#write.xlsx(FirstYear_Rmsy70, file=paste0(PathOut,"FirstYear_Rmsy70.xlsx"))


FirstYear_Rlim50<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years, starting at assessment year + 1
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk2[r,s,y]>=0.50){FirstYear_Rlim50[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear_Rlim50)<-RiverNames
colnames(FirstYear_Rlim50)<-c(scen_risk)#;FirstYear_Rlim50
#write.xlsx(FirstYear_Rlim50, file=paste0(PathOut,"FirstYear_Rlim50.xlsx"))

FirstYear_Rlim70<-array(NA, dim=c(Nstocks, nrScen))
for(r in 1:Nstocks){
  for(s in 1:nrScen){
    tmp<-0
    y<-yBreak+2 # go through future years
    while(tmp==0){
      if(y==years[3]){tmp<-1}
      if(RecRisk2[r,s,y]>=0.70){FirstYear_Rlim70[r,s]<-y+1991; tmp<-1}else{y<-y+1}
    }
  }
}   
rownames(FirstYear_Rlim70)<-RiverNames
colnames(FirstYear_Rlim70)<-c(scen_risk)#;FirstYear_Rlim70
#write.xlsx(FirstYear_Rlim70, file=paste0(PathOut,"FirstYear_Rlim70.xlsx"))

# #############################################################################################################
# ############################# Make Figure 4.3.2.6 ###########################################################
# 
ScenNames<-as.character(1:nrScen)
ScenLty=c(1:nrScen)
ScenPch = c(19,22,3,25,8,9,10,11,12,13,14,15,16,17, 23,24,26, 27, 28, 29)[scen_risk]
scenPch = 1:nrScen
# 

#plot scenarios 1:6
#windows(record=T)
#stock_sel<-c(1:4)  #a ,e
#stock_sel<-c(5:8)  #b, f
#stock_sel<-c(9:12)   #c, g
#stock_sel<-c(13:16)   #d, h
stock_sel <- c(1:Nstocks)
#scen_sel<-c(1,5,7,12,14)
scen_sel<-c(1:nrScen)
nsel<-length(scen_sel)

#2021 figs a-e scens 1-6
#figs f-j scen 1 + 7:10

#png(paste0("05-results/scen-figures/figures/riskriver/","F4326_MSY.png"),  width=1600, height=2000, res=300) 
par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)

for(r in stock_sel[1]:stock_sel[length(stock_sel)]){
  
  plot(Years, RecRisk[r,scen_sel[1],], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab ="", main = RiverNames[r])
  for(i in 1:nsel){
    points(Years, RecRisk[r,scen_sel[i],], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  
  abline(v=Years[1]+Compyear2[r]-1,lty=2)
  abline(v=Years[1]+Compyear3[r]-1)
  abline(h=0.50,lty=2)
  legend(x="topleft", paste(scen_risk), lty=ScenLty[1:nsel], pch=ScenPch[1:nsel],bg="white",cex=0.8,bty="n")
  
  mtext("Probability of reaching RMSY", side = 2, line = 2, outer = TRUE, cex=0.7)
  mtext("Year", side = 1, line = 0, outer = TRUE, cex=0.7)
}

#dev.off()

#png(paste0("05-results/scen-figures/figures/riskriver/","F4326_LIM.png"),  width=1600, height=2000, res=300) 
par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)

for(r in stock_sel[1]:stock_sel[length(stock_sel)]){
  
  plot(Years, RecRisk2[r,scen_sel[1],], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab ="", main = RiverNames[r])
  for(i in 1:nsel){
    points(Years, RecRisk2[r,scen_sel[i],], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  
  abline(v=Years[1]+Compyear2[r]-1,lty=2)
  abline(v=Years[1]+Compyear3[r]-1)
  abline(h=0.50,lty=2)
  legend(x="topleft", paste(scen_risk), lty=ScenLty[1:nsel], pch=ScenPch[1:nsel],bg="white",cex=0.8,bty="n")
  
  mtext("Probability of reaching RLIM", side = 2, line = 2, outer = TRUE, cex=0.7)
  mtext("Year", side = 1, line = 0, outer = TRUE, cex=0.7)
}
#dev.off()


