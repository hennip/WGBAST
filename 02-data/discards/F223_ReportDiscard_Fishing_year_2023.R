# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure 2.2.1, catches of salmon in % of TAC

# R-file:		   f2_2_1_ReportDiscard.r

# input: 		   
# output:  	

# R ver:	  	  2.8.0

# programmed:		2009 hpulkkin
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
      
# Total Reported Commercial Catches for Main Basin & Gulf of Bothnia (T2.2.7) 
# 1993-present
#! Update
TRC_MBGB<-c(
676115,584404,553113,455999,
395618,333726,286209,311989,
358517,337643,329180,367870,
256480,173876,160818,110192,
145600,
127300,
125400,
110300,
87830,
85610,
81740,
71700,
58620,
69040,
65560,
52980,
63630,
36250,
30390
) # 2023
length(TRC_MBGB)

# Total Reported Commercial Catches for Gulf of Finland (T2.2.7)1993->
#! Update
TRC_GF<-c(
99565,54287,33273,76900,
74889,28869,27456,32056,
14109,11447,8223,7750,
10588,11623,11131,14643,
12650,
5609,
7430,
10890,
9722,
9318,
7395,
7324,
7384,
6437,
9301,
9399,
5458,
4462,
4640
) #2023
length(TRC_GF)

# Non-Commercial sea (S+C) Catches for Main Basin & Gulf of Bothnia 
# T2.2.6) 1993->
#! Update
# This includes Swedish 'almost commercial' catches
NCC_MBGB<-c(
NA,NA,NA,NA,NA,
11040,20390,15450,
33700,
28580,
22660,
43000,
38190,
23570,
21530,
26760,
31460,
20620,
17850,
16430,
13930,
13860,
11020,
16760,
22050,
24400,
25660,
22310,
22580,
10780,
6355
) # 2023
length(NCC_MBGB)

# Non-Commercial sea (S+C) Catches for Gulf of Finland (T2.2.6) 
# 1993-present
#! Update
NCC_GF<-c(
NA,NA,NA,NA,NA,
5150,5150,14180,
14180,
2550,
2550,
3090,
3296,
318,
180,
1024,
730,
360,
360,
810,
1175,
1127,
1327,
1291,
1128,
480,
514,
1201,
1139,
690,
614
) # 2023
length(NCC_GF)

# River Catches for Main Basin & Gulf of Bothnia (T2.2.6) 1997-present
# Both recr & comm & broodstock
#! Update
RC_MBGB<-c(
NA, NA, NA, NA, 
17000,5100,532,4150,
30485,
28064,
22467,
25111,
30612,
18898,
23364,
44982,
32818,
18508,
19978,
49722,
39542,
43033,
49431,
53201,
38942,
42396,
43528,
52726,
48969,
26398,
18738
) # 2023
length(RC_MBGB)

# River Catches for Gulf of Finland (T2.2.6) 1997-present
# Both recr & comm & broodstock
#! Update
RC_GF<-c(
NA,NA,NA,NA,
NA,NA,1232,
2943,
3608,
3200,
1700,
1500,
2903,
1812,
1557,
1368,
2320,
585,
785,
802,
971,
568,
196,
641,
508,
244,
311,
438,
529,
350,
468
) # 2023
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
Years<-c(1993:2023)
cbind(Years,CRC_MBGB,RRC_MBGB,CRC_GF,RRC_GF)

#! Update annual TAC Main Basin & GoB
TAC_MBGB<-c(
650000,600000,500000,450000,410000,
410000,410000,450000,450000,450000,
460000,460000,460000,460000,437437,
371315,309733,294246,250109,122553,
108762,106366,95928,95928,95928,91132,91132,86575,94496,63811,63811) # 2023

#! Update TAC GoF
TAC_GF<-c(
120000,120000,120000,120000,110000,
110000,100000,90000,70000,60000,
50000,35000,17000,17000,15419,
15419,15419,15419,15419,15419,
15419,13106,13106,13106,10486,10003,9703,9703,8883,9455,9455) # 2023

PCRC_MBGB<-CRC_MBGB/TAC_MBGB
PRRC_MBGB<-RRC_MBGB/TAC_MBGB
PCRC_GF<-CRC_GF/TAC_GF
PRRC_GF<-RRC_GF/TAC_GF


round(cbind(Years,PCRC_MBGB,PRRC_MBGB,PCRC_GF,PRRC_GF),2)

#####################
                        
# Graphs

# Main Basin & Gulf of Bothnia
####################################

lastYear<-31 #! 31=2023 Add one each year!

# Estimates for total misrep+unrep+discards from Tapsa
# for years 2001-present

#! Update
medUD_MBGB<-c(
  213200,
  198600,
  227100,
  352600,
  182200,
  96160,
  98110,
  33340,
  109000,
  109600,
  71670,
  44290,
  36320,
  32130,
  32840,
  42710,
  46980,
  54300,
  9899,
  10190,
  12330,
  5172,
  4144
) # 2023

lowUD_MBGB<-c(
  198700,
  184200,
  213200,
  333900,
  169200,
  87220,
  90060,
  25930,
  98600,
  102100,
  64130,
  38760,
  32220,
  28380,
  30060,
  40020,
  44540,
  51980,
  8643,
  9066,
  11160,
  4262,
  3356
) # 2023

highUD_MBGB<-c(
  237000,
  222500,
  250300,
  384200,
  204800,
  111300,
  111800,
  48320,
  131100,
  123200,
  85980,
  54220,
  42910,
  38470,
  36880,
  46760,
  50260,
  58030,
  12190,
  12300,
  14440,
  6863,
  5628
) # 2023


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
dat<-read.table("C:/Users/03081178/Work Folders/WGBAST/2024 Gävle/F2211_R_model/data/MBGoBData09.txt", header=T)
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
par(mfrow=c(1,1)) #in small screen run MB+GoB and GoF separetely 
par(mar=c(4,5,3,1)+0.1)

barplot(test, space=1,names.arg=c(1993:(1993+lastYear-1)), ylim=c(0,400), ylab="% of TAC",
main="Main Basin and Gulf of Bothnia, subdivisions 22-31",
legend=T)
abline(h=100, lwd=2)

for(i in 1:8){
segments(i+(i-1)+0.5, CRDtot[i]-LowOld[i],i+(i-1)+0.5,CRDtot[i]+HighOld[i], lwd=2)
segments(i+(i-1)+0.25, CRDtot[i]-LowOld[i],i+(i-1)+0.75,CRDtot[i]-LowOld[i], lwd=2)
segments(i+(i-1)+0.25, CRDtot[i]+HighOld[i],i+(i-1)+0.75,CRDtot[i]+HighOld[i], lwd=2)
}
for(i in 9:lastYear){
segments(i+(i-1)+0.5, LowNew[i],i+(i-1)+0.5,HighNew[i], lwd=2)
segments(i+(i-1)+0.25, LowNew[i],i+(i-1)+0.75,LowNew[i], lwd=2)
segments(i+(i-1)+0.25, HighNew[i],i+(i-1)+0.75,HighNew[i], lwd=2)
}


# Gulf of Finland
####################################
# Estimates of total unreporting & discarding

udOld<-c(61,52,46,22,18,36,39,18) 


#! Update
medUD_GF<-c(
  4959,
  4428,
  4520,
  4741,
  2844,
  3848,
  2804,
  3624,
  3009,
  1474,
  1619,
  2107,
  1709,
  1545,
  1642,
  1050,
  1233,
  803,
  1231,
  1153,
  502,
  528,
  560
) # 2023

lowUD_GF<-c(
  4420,
  4025,
  4121,
  4336,
  2417,
  3340,
  2343,
  2999,
  2491,
  1243,
  1320,
  1668,
  1340,
  1180,
  1477,
  896,
  1073,
  667,
  1037,
  960,
  388,
  423,
  460
) # 2023

highUD_GF<-c(
  5993,
  5156,
  5313,
  5556,
  3707,
  4865,
  3732,
  4875,
  4042,
  1927,
  2222,
  2992,
  2456,
  2270,
  2054,
  1411,
  1588,
  1115,
  1705,
  1612,
  752,
  733,
  778
) # 2023

UD<-c(udOld, medUD_GF/TAC_GF[9:lastYear]*100)

# DO NOT UPDATE! Use only to create values for years 1-5 i.e. 93-97.
#####################################################################
dat<-read.table("C:/Users/03081178/Work Folders/WGBAST/2024 Gävle/F2211_R_model/data/GoFData09.txt", header=T)
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

barplot(test, space=1,names.arg=c(1993:(1993+lastYear-1)), ylim=c(0,275), ylab="% of TAC",
main="Gulf of Finland, subdivision 32",
)
abline(h=100, lwd=2)

for(i in 1:8){
segments(i+(i-1)+0.5, CRDtot[i]-LowOld[i],i+(i-1)+0.5,CRDtot[i]+HighOld[i], lwd=2)
segments(i+(i-1)+0.25, CRDtot[i]-LowOld[i],i+(i-1)+0.75,CRDtot[i]-LowOld[i], lwd=2)
segments(i+(i-1)+0.25, CRDtot[i]+HighOld[i],i+(i-1)+0.75,CRDtot[i]+HighOld[i], lwd=2)
}
for(i in 9:(1993+lastYear-1)){
segments(i+(i-1)+0.5, LowNew[i],i+(i-1)+0.5,HighNew[i], lwd=2)
segments(i+(i-1)+0.25, LowNew[i],i+(i-1)+0.75,LowNew[i], lwd=2)
segments(i+(i-1)+0.25, HighNew[i],i+(i-1)+0.75,HighNew[i], lwd=2)
}




