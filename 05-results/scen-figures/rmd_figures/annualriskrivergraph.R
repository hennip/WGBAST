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
LastPredYear<-2060
years<-c(1992,LastPredYear)
years<-c(years[],years[2]-years[1]+1) 
Years<-c(years[1]:years[2])
sim<- nsim   
nrScen<-16
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
################################################################################
#! #############################################################################
# Version of the estimation model

assessment_year<-2023


#! Mps
choice<-"MED"


#! Set the last year for historic part and the last year for predictions:


#Number of years in future projections


#Set AU-specific generation times for years of comparison
gen_time.old<-c(7,7,7,6)  #AU-specific generation time in years
#6th July
gen_time<-c(6,6,6,5) 

Compyear1<-rep(which(Years==(assessment_year-1)),times=Nstocks)   #current stock status 2022
Compyear2<-match((assessment_year-1)+gen_time[AU],Years)      #fishing in 2024 -> 2027/2028
Compyear3<-match((assessment_year-1)+5*gen_time[AU],Years)
Compyear4<-match((assessment_year-1)+2*gen_time[AU],Years)

#Calculate the chance of some statistic (vector) being above some reference point
risk<-function(x,ref) {sum(ifelse(x>ref,1,0))/length(x)}

#load the reference points 
load(paste0(PathRefP,"ref_pts_long_2023.RData"))
load(paste0(PathRefP,"eqm_distns.RData"))


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
RecRisk3<-array(NA, dim=c(Nstocks,nrScen,years[3]))
RecRisk4<-array(NA, dim=c(Nstocks,nrScen,years[3])) #average of last 5 years' R0 for 2021 Annex
#RecRisk5<-array(NA, dim=c(Nstocks,nrScen,years[3])) #RMSY simulation for 2021 Annex

for(scen in 1:nrScen){ # number of scenarios
  
  #scen<-7
  EffScen<-scen
  
  #Load the file containing stats
  if(scen==1){File<-paste0(PathScen,"ScenProj_",Model,"_EScen1_RCzero23-35.RData")}  #F=0 
  if(scen==2){File<-paste0(PathScen,"ScenProj_",Model,"_EScen2_RCzero23-35.RData")}  #river F only
  if(scen==3){File<-paste0(PathScen,"ScenProj_",Model,"_EScen3_RCzero23-35.RData")}
  if(scen==4){File<-paste0(PathScen,"ScenProj_",Model,"_EScen4_RCzero23-35.RData")}
  if(scen==5){File<-paste0(PathScen,"ScenProj_",Model,"_EScen5_RCzero23-35.RData")} 
  if(scen==6){File<-paste0(PathScen,"ScenProj_",Model,"_EScen6_RCzero23-35.RData")} 
  if(scen==7){File<-paste0(PathScen,"ScenProj_",Model,"_EScen7_RCzero23-35.RData")} 
  if(scen==8){File<-paste0(PathScen,"ScenProj_",Model,"_EScen8_RCzero23-35.RData")} 
  if(scen==9){File<-paste0(PathScen,"ScenProj_",Model,"_EScen9_RCzero23-35.RData")} 
  if(scen==10){File<-paste0(PathScen,"ScenProj_",Model,"_EScen10_RCzero23-35.RData")} 
  if(scen==11){File<-paste0(PathScen,"ScenProj_",Model,"_EScen11_RCzero23-35.RData")} 
  if(scen==12){File<-paste0(PathScen,"ScenProj_",Model,"_EScen12_RCzero23-35.RData")} 
  if(scen==13){File<-paste0(PathScen,"ScenProj_",Model,"_EScen13_RCzero23-35.RData")} 
  if(scen==14){File<-paste0(PathScen,"ScenProj_",Model,"_EScen14_RCzero23-35.RData")} 
  if(scen==15){File<-paste0(PathScen,"ScenProj_",Model,"_EScen15_RCzero23-35.RData")} 
  
  #File
  load(File)
  
  
  
  #Proportion of simulations in which the recruitment is above R_MSY and Rlim by year
  for(y in 1:years[3]){
    for(r in 1:Nstocks){
      RecRisk[r,scen,y]<-risk(SmoltW[r,y,which(comp.inds[r,]==1)],Smolt_MSY[r,which(comp.inds[r,]==1)])
      RecRisk2[r,scen,y]<-risk(SmoltW[r,y,which(comp.inds[r,]==1)],Smolt_lim[r,which(comp.inds[r,]==1)])    
      RecRisk3[r,scen,y]<-risk(SmoltW[r,y,],0.75*R0_all[r,])    
      RecRisk4[r,scen,y]<-risk(SmoltW[r,y,],0.75*R0_final[,r])   
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
colnames(FirstYear_Rmsy50)<-c(1:nrScen)#;FirstYear_Rmsy50
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
colnames(FirstYear_Rmsy70)<-c(1:nrScen)#;FirstYear_Rmsy70
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
colnames(FirstYear_Rlim50)<-c(1:nrScen)#;FirstYear_Rlim50
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
colnames(FirstYear_Rlim70)<-c(1:nrScen)#;FirstYear_Rlim70
#write.xlsx(FirstYear_Rlim70, file=paste0(PathOut,"FirstYear_Rlim70.xlsx"))

# #############################################################################################################
# ############################# Make Figure 4.3.2.6 ###########################################################
# 
ScenNames<-as.character(1:14)
ScenLty=c(1:14)
ScenPch = c(19,22,3,25,8,9,10,11,12,13,14,15,16,17)
# 
# 
option<-"R_MSY"  #"Rlim"  #R0"   #choose the option for plotting
# 
if(option=="R_MSY"){
  risk_array<-RecRisk
  ylabtxt<-"Probability of reaching RMSY"
} else if(option=="Rlim"){
  risk_array<-RecRisk2
  ylabtxt<-"Probability of reaching Rlim"
} else if(option=="R0"){
  risk_array<-RecRisk3
  ylabtxt<-"Probability of reaching R0"
}

#plot scenarios 1:6
#windows(record=T)
#stock_sel<-c(1:4)  #a ,e
#stock_sel<-c(5:8)  #b, f
#stock_sel<-c(9:12)   #c, g
stock_sel<-c(13:16)   #d, h

scen_sel<-c(1,5,7,12,14)
nsel<-length(scen_sel)

#2021 figs a-e scens 1-6
#figs f-j scen 1 + 7:10

#tiff(paste0(PathScen,"F4326d.tiff"),  width=1600, height=2000, res=300) 
par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)

for(r in stock_sel[1]:stock_sel[length(stock_sel)]){              
  plot(Years, risk_array[r,scen_sel[1],], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab ="", main = RiverNames[r])
  for(i in 1:nsel){
    points(Years, risk_array[r,scen_sel[i],], pch = ScenPch[i], type="b", lty=ScenLty[i])
  }
  
  abline(v=Years[1]+Compyear2[r]-1,lty=2)
  abline(v=Years[1]+Compyear3[r]-1)
  abline(h=0.50,lty=2)
  legend(x="topleft", ScenNames[scen_sel], lty=ScenLty[1:nsel], pch=ScenPch[1:nsel],bg="white",cex=0.8,bty="n")
  
  mtext(ylabtxt, side = 2, line = 2, outer = TRUE, cex=0.7)
  mtext("Year", side = 1, line = 0, outer = TRUE, cex=0.7)
}

#dev.off()

r<-17 #Testebo?n
par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)          
plot(Years, risk_array[r,scen_sel[1],], type = "n", ylim=c(0,1), xlab = "Year", 
     ylab =ylabtxt, main = RiverNames[r])
for(i in 1:nsel){
  points(Years, risk_array[r,scen_sel[i],], pch = ScenPch[i], type="b", lty=ScenLty[i])
}

abline(v=Years[1]+Compyear2[r]-1,lty=2)
abline(v=Years[1]+Compyear3[r]-1)
abline(h=0.50,lty=2)
legend(x="topleft", ScenNames[scen_sel], lty=ScenLty[1:nsel], pch=ScenPch[1:nsel],bg="white",cex=1,bty="n")


# ####################################################################################################
# #plot scenarios 7-10 with 1 for reference
# 
#windows(record=T)
#stock_sel<-c(1:4)
#stock_sel<-c(5:8)
#stock_sel<-c(9:12)
stock_sel<-c(13:16)


par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)
scen_sel<-c(1,7,8,9,10)
for(r in stock_sel[1]:stock_sel[length(stock_sel)]){
  Risk<-cbind(risk_array[r,1,],risk_array[r,7,],
              risk_array[r,8,],risk_array[r,9,],risk_array[r,10,])            
  plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
       ylab = ylabtxt, main = RiverNames[r])
  for(i in 1:length(scen_sel)){
    points(Years, Risk[,i], pch = ScenPch[scen_sel[i]], type="b", lty=ScenLty[scen_sel[i]])
  }
  
  abline(v=Years[1]+Compyear2[r]-1,lty=2)
  abline(v=Years[1]+Compyear3[r]-1)
  abline(h=0.50,lty=2)
  legend(x="topleft", ScenNames[scen_sel], lty=ScenLty[scen_sel], pch=ScenPch[scen_sel],bg="white",cex=0.8,bty="n")
  
  mtext("Probability of reaching RMSY", side = 2, line = 2, outer = TRUE, cex=0.7)
  mtext("Year", side = 1, line = 0, outer = TRUE, cex=0.7)
}


r<-17 #Testebo?n
par(mfrow=c(4,1),cex=0.6,mar=c(2,1,2,1),oma=c(2.5,3.5,1,1),font=2,font.lab=2,font.axis=2)
Risk<-cbind(risk_array[r,1,],risk_array[r,7,],risk_array[r,8,],
            risk_array[r,9,],risk_array[r,10,])              
plot(Years, Risk[,1], type = "n", ylim=c(0,1), xlab = "Year", 
     ylab =ylabtxt, main = RiverNames[r])
for(i in 1:length(scen_sel)){
  points(Years, Risk[,i], pch = ScenPch[scen_sel[i]], type="b", lty=ScenLty[scen_sel[i]])
}

abline(v=Years[1]+Compyear2[r]-1,lty=2)
abline(v=Years[1]+Compyear3[r]-1)
abline(h=0.50)
legend(x="topleft", ScenNames[scen_sel], lty=ScenLty[scen_sel], pch=ScenPch[scen_sel],bg="white",cex=0.8,bty="n")


##############################################################################################################
##############################################################################################################
## Values for PSPC table 


pr.target1<-array(NA,dim=c(Nstocks,nrScen,4)) #MSY
pr.target2<-array(NA,dim=c(Nstocks,nrScen,4)) #Rlim
pr.target3<-array(NA,dim=c(Nstocks,nrScen,4)) #0.75*R0 MSY proxy

for(r in 1:Nstocks){
  for(s in 1:nrScen){
    pr.target1[r,s,1]<-RecRisk[r,s,Compyear1[r]]      #MSY
    pr.target2[r,s,1]<-RecRisk2[r,s,Compyear1[r]]     #Rlim
    pr.target3[r,s,1]<-RecRisk3[r,s,Compyear1[r]]     #R0
    
    pr.target1[r,s,2]<-RecRisk[r,s,Compyear2[r]]
    pr.target2[r,s,2]<-RecRisk2[r,s,Compyear2[r]]
    pr.target3[r,s,2]<-RecRisk3[r,s,Compyear2[r]]
    
    pr.target1[r,s,3]<-RecRisk[r,s,Compyear3[r]]
    pr.target2[r,s,3]<-RecRisk2[r,s,Compyear3[r]]
    pr.target3[r,s,3]<-RecRisk3[r,s,Compyear3[r]]
    
    pr.target1[r,s,4]<-RecRisk[r,s,Compyear4[r]]
    pr.target2[r,s,4]<-RecRisk2[r,s,Compyear4[r]]
    pr.target3[r,s,4]<-RecRisk3[r,s,Compyear4[r]]
  }
}

# write.xlsx(pr.target1[,,1],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_curr_new.xlsx"))
# write.xlsx(pr.target1[,,2],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_1gen_new.xlsx")) 
# write.xlsx(pr.target1[,,3],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_5gen_new.xlsx")) 
# write.xlsx(pr.target1[,,4],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_2gen_new.xlsx")) 
# 
# write.xlsx(pr.target2[,,1], row.names=F,col.names=F,file=paste0(PathOut,"plim_curr_new.xlsx"))
# write.xlsx(pr.target2[,,2], row.names=F,col.names=F,file=paste0(PathOut,"plim_1gen_new.xlsx")) 
# write.xlsx(pr.target2[,,3], row.names=F,col.names=F,file=paste0(PathOut,"plim_5gen_new.xlsx")) 
# write.xlsx(pr.target2[,,4], row.names=F,col.names=F,file=paste0(PathOut,"plim_2gen_new.xlsx")) 
# 
# write.xlsx(pr.target3[,,1], row.names=F,col.names=F,file=paste0(PathOut,"pR0_curr_new.xlsx"))
# write.xlsx(pr.target3[,,2], row.names=F,col.names=F,file=paste0(PathOut,"pR0_1gen_new.xlsx"))
# write.xlsx(pr.target3[,,3], row.names=F,col.names=F,file=paste0(PathOut,"pR0_5gen_new.xlsx")) 
# write.xlsx(pr.target3[,,4], row.names=F,col.names=F,file=paste0(PathOut,"pR0_2gen_new.xlsx")) 

###################################################################################################
########################################################################################################


##New! AU-specific probabilities to reach targets
AUsums<-array(0,dim=c(5,sim,3))

for(u in 1:max(AU)){
  AUsums[u,,1]<-apply(Smolt_MSY[which(AU==u),],2,sum)
  AUsums[u,,2]<-apply(Smolt_lim[which(AU==u),],2,sum)
  AUsums[u,,3]<-apply(R0_all[which(AU==u),],2,sum)
}

AUsums[5,,1]<-apply(Smolt_MSY,2,sum)
AUsums[5,,2]<-apply(Smolt_lim,2,sum)
AUsums[5,,3]<-apply(R0_all,2,sum)

AU13<-apply(R0_all[which(AU %in% c(1:3)),],2,sum)

#Store risk values
AURisk<-array(NA, dim=c(5,nrScen,years[3]))
AURisk2<-array(NA, dim=c(5,nrScen,years[3]))
AURisk3<-array(NA, dim=c(5,nrScen,years[3]))

Smoltsum<-array(NA,dim=c(5,years[3],sim))
for(scen in 1:15){ # number of scenarios
  
  #  
  #Load the file containing stats
  #Load the file containing stats
  if(scen==1){File<-paste0(PathScen,"ScenProj_",Model,"_EScen1_RCzero23-35.RData")}  #F=0 
  if(scen==2){File<-paste0(PathScen,"ScenProj_",Model,"_EScen2_RCzero23-35.RData")}  #river F only
  if(scen==3){File<-paste0(PathScen,"ScenProj_",Model,"_EScen3_RCzero23-35.RData")}
  if(scen==4){File<-paste0(PathScen,"ScenProj_",Model,"_EScen4_RCzero23-35.RData")}
  if(scen==5){File<-paste0(PathScen,"ScenProj_",Model,"_EScen5_RCzero23-35.RData")} 
  if(scen==6){File<-paste0(PathScen,"ScenProj_",Model,"_EScen6_RCzero23-35.RData")} 
  if(scen==7){File<-paste0(PathScen,"ScenProj_",Model,"_EScen7_RCzero23-35.RData")} 
  if(scen==8){File<-paste0(PathScen,"ScenProj_",Model,"_EScen8_RCzero23-35.RData")} 
  if(scen==9){File<-paste0(PathScen,"ScenProj_",Model,"_EScen9_RCzero23-35.RData")} 
  if(scen==10){File<-paste0(PathScen,"ScenProj_",Model,"_EScen10_RCzero23-35.RData")} 
  if(scen==11){File<-paste0(PathScen,"ScenProj_",Model,"_EScen11_RCzero23-35.RData")} 
  if(scen==12){File<-paste0(PathScen,"ScenProj_",Model,"_EScen12_RCzero23-35.RData")} 
  if(scen==13){File<-paste0(PathScen,"ScenProj_",Model,"_EScen13_RCzero23-35.RData")} 
  if(scen==14){File<-paste0(PathScen,"ScenProj_",Model,"_EScen14_RCzero23-35.RData")} 
  if(scen==15){File<-paste0(PathScen,"ScenProj_",Model,"_EScen15_RCzero23-35.RData")} 
  
  
 # File
  load(File)
  
  #NB code below needs changing in case any samples are omitted from reference point calculations
  
  for(y in 1:years[3]){
    for(u in 1:max(AU)){
      Smoltsum[u,y,]<-apply(SmoltW[which(AU==u),y,],2,sum)
      AURisk[u,scen,y]<-risk(Smoltsum[u,y,],AUsums[u,,1])  #MSY
      AURisk2[u,scen,y]<-risk(Smoltsum[u,y,],AUsums[u,,2])  #Rlim
      AURisk3[u,scen,y]<-risk(Smoltsum[u,y,],AUsums[u,,3])  #R0
      
    }
    Smoltsum[5,y,]<-apply(SmoltW[,y,],2,sum)
    AURisk[5,scen,y]<-risk(Smoltsum[5,y,],AUsums[5,,1])  #MSY
    AURisk2[5,scen,y]<-risk(Smoltsum[5,y,],AUsums[5,,2])  #Rlim
    AURisk3[5,scen,y]<-risk(Smoltsum[5,y,],AUsums[5,,3])  #R0
  } 
}  #loop over scenarios  

pr.target1<-array(NA,dim=c(5,nrScen,3)) #MSY
pr.target2<-array(NA,dim=c(5,nrScen,3)) #Rlim
pr.target3<-array(NA,dim=c(5,nrScen,3)) #0.75*R0 MSY proxy

Compyear1<-rep(which(Years==(assessment_year-1)),times=(max(AU)+1))
Compyear2<-c(match((assessment_year-1)+gen_time,Years),match((assessment_year-1)+min(gen_time),Years))
Compyear3<-c(match((assessment_year-1)+5*gen_time,Years),match((assessment_year-1)+5*min(gen_time),Years))

for(u in 1:5){
  for(s in 1:nrScen){
    pr.target1[u,s,1]<-AURisk[u,s,Compyear1[u]]
    pr.target2[u,s,1]<-AURisk2[u,s,Compyear1[u]]
    pr.target3[u,s,1]<-AURisk3[u,s,Compyear1[u]]
    
    pr.target1[u,s,2]<-AURisk[u,s,Compyear2[u]]
    pr.target2[u,s,2]<-AURisk2[u,s,Compyear2[u]]
    pr.target3[u,s,2]<-AURisk3[u,s,Compyear2[u]]
    
    pr.target1[u,s,3]<-AURisk[u,s,Compyear3[u]]
    pr.target2[u,s,3]<-AURisk2[u,s,Compyear3[u]]
    pr.target3[u,s,3]<-AURisk3[u,s,Compyear3[u]]
  }
}

# write.xlsx(pr.target1[,,1],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_curr_AU.xlsx"))
# write.xlsx(pr.target1[,,2],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_1gen_AU.xlsx")) 
# write.xlsx(pr.target1[,,3],row.names=F,col.names=F, file=paste0(PathOut,"pMSY_5gen_AU.xlsx")) 
# 
# write.xlsx(pr.target2[,,1], row.names=F,col.names=F,file=paste0(PathOut,"plim_curr_AU.xlsx"))
# write.xlsx(pr.target2[,,2], row.names=F,col.names=F,file=paste0(PathOut,"plim_1gen_AU.xlsx")) 
# write.xlsx(pr.target2[,,3], row.names=F,col.names=F,file=paste0(PathOut,"plim_5gen_AU.xlsx")) 
# 
# write.xlsx(pr.target3[,,1], row.names=F,col.names=F,file=paste0(PathOut,"pR0_curr_AU.xlsx"))
# write.xlsx(pr.target3[,,2], row.names=F,col.names=F,file=paste0(PathOut,"pR0_1gen_AU.xlsx"))
# write.xlsx(pr.target3[,,3], row.names=F,col.names=F,file=paste0(PathOut,"pR0_5gen_AU.xlsx")) 

