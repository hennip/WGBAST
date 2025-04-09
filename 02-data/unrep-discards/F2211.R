# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure 2.2.1, catches of salmon in % of TAC

# R-file:		   f2_2_1_ReportDiscard.r

# input: 		   
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/unrep-discards/unrep-and-discards.R")


# Total Reported Commercial Catches for Main Basin & Gulf of Bothnia (T2.2.7) 
# 1993-present
#! Update
TRC_MBGB<-c(
676115,584404,553113,455999,
395618,333726,286209,311989, #until 2000
B_TotRepCom_sea[,1])
length(TRC_MBGB)

# Total Reported Commercial Catches for Gulf of Finland (T2.2.7)1993->
#! Update
TRC_GF<-c(
99565,54287,33273,76900,
74889,28869,27456,32056, #Until 2000
B_TotRepCom_sea[,2]
)
length(TRC_GF)

# Non-Commercial sea (S+C) Catches for Main Basin & Gulf of Bothnia 
# T2.2.6) 1993->
#! Update
# This includes Swedish 'almost commercial' catches
TRecrSea
dim(TRecrSea)

tmp<-array(NA, dim=c(NumYears,2))
for(i in 1: NumYears){
  for(k in 1:2){
      tmp[i,k]<-sum(TRecrSea[i,1:9,k]) # Sum over countries
  }
}
tmp

NCC_MBGB<-c(
NA,NA,NA,NA,
NA,11040,20390,15450, #Until 2000
tmp[,1]
) # 2023
length(NCC_MBGB)

# Non-Commercial sea (S+C) Catches for Gulf of Finland (T2.2.6) 
# 1993-present
#! Update
NCC_GF<-c(
NA,NA,NA,NA,NA,
5150,5150,14180,
tmp[,2]) # 2023
length(NCC_GF)

# River Catches for Main Basin & Gulf of Bothnia (T2.2.6) 1997-present
# Both recr & comm & broodstock
#! Update
TRiver
tmp<-array(NA, dim=c(NumYears,2))
for(i in 1: NumYears){
  for(k in 1:2){
    tmp[i,k]<-sum(TRiver[i,1:9,k]) # Sum over countries
  }
}
tmp

RC_MBGB<-c(
NA, NA, NA, NA, 
17000,5100,532,4150,
tmp[,1]
)
length(RC_MBGB)

# River Catches for Gulf of Finland (T2.2.6) 1997-present
# Both recr & comm & broodstock
#! Update
RC_GF<-c(
NA,NA,NA,NA,
NA,NA,1232,2943,
# 3608,
# 3200,
# 1700,
# 1500,
# 2903,
# 1812,
# 1557,
# 1368,
# 2320,
# 585,
# 785,
# 802,
# 971,
# 568,
# 196,
# 641,
# 508,
# 244,
# 311,
# 438,
# 529,
# 350,
# 468
tmp[,2]
) 
length(RC_GF)


RecrProp_MBGB<-NCC_MBGB/TRC_MBGB
RecrProp_GF<-NCC_GF/TRC_GF

# Commercial Reported Catches
CRC_MBGB<-(1-RecrProp_MBGB)*TRC_MBGB
CRC_GF<-(1-RecrProp_GF)*TRC_GF

# Recreational (=Non-Commercial) Reported Catches and River Catches

RRC_MBGB<- RecrProp_MBGB*TRC_MBGB+RC_MBGB
RRC_GF<- RecrProp_GF*TRC_GF+RC_GF

#! Update
Years<-c(1993:2024)
cbind(Years,CRC_MBGB,RRC_MBGB,CRC_GF,RRC_GF)

#! Update annual TAC Main Basin & GoB
TAC_MBGB<-c(
650000,600000,500000,450000,410000,
410000,410000,450000,450000,450000,
460000,460000,460000,460000,437437,
371315,309733,294246,250109,122553,
108762,106366,95928,95928,95928,
91132,91132,86575,94496,63811,
63811, 53967) # 2024

#! Update TAC GoF
TAC_GF<-c(
120000,120000,120000,120000,110000,
110000,100000,90000,70000,60000,
50000,35000,17000,17000,15419,
15419,15419,15419,15419,15419,
15419,13106,13106,13106,10486,
10003,9703,9703,8883,9455,
9455, 10144) # 2024

PCRC_MBGB<-CRC_MBGB/TAC_MBGB
PRRC_MBGB<-RRC_MBGB/TAC_MBGB
PCRC_GF<-CRC_GF/TAC_GF
PRRC_GF<-RRC_GF/TAC_GF


round(cbind(Years,PCRC_MBGB,PRRC_MBGB,PCRC_GF,PRRC_GF),2)

#####################
                        
# Graphs

# Main Basin & Gulf of Bothnia
####################################

lastYear<-32 #! 32=2024 Add one each year!

# Estimates for total misrep+unrep+discards from Tapsa
# for years 2001-present

# from unrep-and_discards.R:
dim(Tunrep_F2) # : Unrep + Misrep
dim(B_TotDis_dead)

tmp<-array(NA, dim=c(NumYears,2,Nsim))
#med<-array(NA, dim=c(NumYears,2))
res<-array(NA, dim=c(NumYears,3,2))
for(i in 1: NumYears){
  for(k in 1:2){
    for(s in 1:Nsim){
#i<-1;k<1;s<-1
        tmp[i,k,s]<-sum(Tunrep_F2[i,1:9,k,s]) + B_TotDis_dead[i,k,s] +B_TotSeal[i,k,s]
    }
    #med[i,k]<-median(tmp[i,k,])
    res[i,,k]<-summary(as.mcmc(tmp[i,k,]), quantiles=c(0.5, 0.05, 0.95))$quantiles
    
    
  }
}
round(res,0)

medUD_MBGB<-res[,1,1]
lowUD_MBGB<-res[,2,1]
highUD_MBGB<-res[,3,1]

# For years 1993-2000 (first 8 years) use stuff from old analyses 

# Nominal additional Polish catch 1993-2000
addPolC<-c(
4100,16572,64046,62679,85861,
60378,122836,159251) #2000

# years 1-8, add pol catch
udOld<-c(0.35,0.31,0.37,0.52,0.48,0.40,0.36,0.39)*TAC_MBGB[1:8]+addPolC[1:8]

UD<-c(udOld, medUD_MBGB)/TAC_MBGB*100

# DO NOT UPDATE! Use only to create values for years 1-5 i.e. 93-97.
#####################################################################
dat<-read.table("../../WGBAST_shared/submodels/reporting rates/MBGoBData09.txt", header=T)
summary(dat)
Lands<-c(dat$Landings[1:5], rep(0, (lastYear-5)))

length(PCRC_MBGB)
for (i in 1 :lastYear){
  if(is.na(PCRC_MBGB[i])==T){
    PCRC_MBGB[i]<-0    
  }
  if(is.na(PRRC_MBGB[i])==T){
    PRRC_MBGB[i]<-0    
  }
}

LowOld<-dat$Low[1:8]
HighOld<-dat$High[1:8]
#####################################################################

test<-rbind(Lands, PCRC_MBGB*100, PRRC_MBGB*100, UD)

rownames(test)<-c("Total reported catch","Reported commercial catch", 
"Reported recreational catch", "Estimated discarding, unreporting and misreporting")

CRDtot<-apply(test, 2, sum)

LowNew<-c(rep(NA,8),lowUD_MBGB/TAC_MBGB[9:lastYear]*100+test[2,9:lastYear]+test[3,9:lastYear])
HighNew<-c(rep(NA,8),highUD_MBGB/TAC_MBGB[9:lastYear]*100+test[2,9:lastYear]+test[3,9:lastYear])

# par(mfrow=c(2,1)) #in large screen MB+GoB and GoF can be drawn in one graph

  png("../../WGBAST_shared/flhm/2025/dat/der/F2211_SD2231.png", units = "px", width = 1920*1.2, height=1080*1.2, res = 300)
  par(mfrow=c(1,1)) #in small screen run MB+GoB and GoF separetely 
  par(mar=c(4,5,3,1)+0.1)
  barplot(test, space=1,names.arg=c(1993:(1993+lastYear-1)), ylim=c(0,400), ylab="% of TAC",
  main="Main Basin and Gulf of Bothnia, subdivisions 22-31",
  legend=T)
  abline(h=100, lwd=2)
  
  for(i in 1:8){
  segments(i+(i-1)+0.5, CRDtot[i]-LowOld[i],i+(i-1)+0.5,CRDtot[i]+HighOld[i], lwd=2)
  segments(i+(i-1)+0.1, CRDtot[i]-LowOld[i],i+(i-1)+0.75,CRDtot[i]-LowOld[i], lwd=2)
  segments(i+(i-1)+0.1, CRDtot[i]+HighOld[i],i+(i-1)+0.75,CRDtot[i]+HighOld[i], lwd=2)
  }
  for(i in 9:lastYear){
  segments(i+(i-1)+0.5, LowNew[i],i+(i-1)+0.5,HighNew[i], lwd=2)
  segments(i+(i-1)+0.1, LowNew[i],i+(i-1)+0.75,LowNew[i], lwd=2)
  segments(i+(i-1)+0.1, HighNew[i],i+(i-1)+0.75,HighNew[i], lwd=2)
  }
  dev.off()
  

# Gulf of Finland
####################################
# Estimates of total unreporting & discarding

udOld<-c(61,52,46,22,18,36,39,18) 

medUD_GF<-res[,1,2]
lowUD_GF<-res[,2,2]
highUD_GF<-res[,3,2]

UD<-c(udOld, medUD_GF/TAC_GF[9:lastYear]*100)

# DO NOT UPDATE! Use only to create values for years 1-5 i.e. 93-97.
#####################################################################
dat<-read.table("../../WGBAST_shared/submodels/reporting rates/GoFData09.txt", header=T)
Lands<-c(dat$Landings[1:6], rep(0, lastYear-6))

length(PCRC_GF)
for (i in 1 :lastYear){
  if(is.na(PCRC_GF[i])==T){
    PCRC_GF[i]<-0    
  }
  if(is.na(PRRC_GF[i])==T){
    PRRC_GF[i]<-0    
  }
}

LowOld<-dat$Low[1:8]
HighOld<-dat$High[1:8]
#####################################################################

test<-rbind(Lands, PCRC_GF*100, PRRC_GF*100, UD)

rownames(test)<-c("Total reported catch","Reported commercial catch", "Reported recreational catch", 
"Estimated discarding & unreporting")

LowNew<-c(rep(NA,8),lowUD_GF/TAC_GF[9:lastYear]*100+test[2,9:lastYear]+test[3,9:lastYear])
HighNew<-c(rep(NA,8),highUD_GF/TAC_GF[9:lastYear]*100+test[2,9:lastYear]+test[3,9:lastYear])

CRDtot<-apply(test, 2, sum)

png("../../WGBAST_shared/flhm/2025/dat/der/F2211_SD32.png", units = "px", width = 1920*1.2, height=1080*1.2, res = 300)
par(mfrow=c(1,1)) #in small screen run MB+GoB and GoF separetely 
par(mar=c(4,5,3,1)+0.1)
barplot(test, space=1,names.arg=c(1993:(1993+lastYear-1)), ylim=c(0,275), ylab="% of TAC",
main="Gulf of Finland, subdivision 32",
)
abline(h=100, lwd=2)

for(i in 1:8){
segments(i+(i-1)+0.5, CRDtot[i]-LowOld[i],i+(i-1)+0.5,CRDtot[i]+HighOld[i], lwd=2)
segments(i+(i-1)+0.1, CRDtot[i]-LowOld[i],i+(i-1)+0.75,CRDtot[i]-LowOld[i], lwd=2)
segments(i+(i-1)+0.1, CRDtot[i]+HighOld[i],i+(i-1)+0.75,CRDtot[i]+HighOld[i], lwd=2)
}
for(i in 9:(1993+lastYear-1)){
segments(i+(i-1)+0.5, LowNew[i],i+(i-1)+0.5,HighNew[i], lwd=2)
segments(i+(i-1)+0.1, LowNew[i],i+(i-1)+0.75,LowNew[i], lwd=2)
segments(i+(i-1)+0.1, HighNew[i],i+(i-1)+0.75,HighNew[i], lwd=2)
}
dev.off()



