#2024 ljungan egg to smolt delay from 4 to 3
#Added priors for recreational trolling release mortality (and proportion of fish released by year)

#NB modified ccd_ObsW file starts from 0SW not 1SW as before

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixalven"
#4 "Ranealven"
#5 "Pitealven"
#6 "Abyalven"
#7 "Byskealven"
#8 "Ricklean"
#9 "Savaran"
#10 "Vindelalven"
#11 "Orealven"
#12 "Logdealven"
#13 "Ljungan"
#14 "Morrumsan"
#15 "Eman"
#16 "Kagealven"
#17 "Testeboan"

#folder<-paste0(PathData,"data_",assessment_year,"/")  
folder<-PathData_FLHM



# Packages are called in run-this-first.r (calls packages.r) 
#library(reshape)
#library(abind)

avail_r<-c(1:17)  #note Emån Mörrum now added, 2023!
avail_dc<-c(1:13,16:17)  #should Mörrum be added here next year?

AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
SR_unit<-c(rep(1,times=13),2,2,1,1)
au1_stocks<-which(AU==1)
au2_stocks<-which(AU==2)
au3_stocks<-which(AU==3)
au4_stocks<-which(AU==4)
avail_seal<-c(1:3)    #AUS with seal M
smolt_year<-c(rep(10,times=12),9,9,9,31,22)  #year first model-predicted smolts produced AU4 stocks and Testebo?n takes 9 years not 10. Testebo?n start year is 2000 (yr 14) 
e_delay<-c(rep(4,times=12),3,3,3,4,3)
iinds<-seq(1,(allstocks*2-1),by=2)


#K prior
carrying_capacity<-as.matrix(read.table(paste0(folder,"K_prior.txt"),header=T))
M_K<-carrying_capacity[,1]
tau_K<-carrying_capacity[,2]

#prior modes
pmodes<-numeric(17)
pi90<-array(0,dim=c(17,3))

pmodes<-numeric(17)
xsum<-numeric(1000000)
for(i in au4_stocks){
  x<-rlnorm(1000000,M_K[i],sqrt(1/tau_K[i]))
  xsum<-xsum+x
  cv<-sd(x)/mean(x)
  pmodes[i]<-quantile(x,0.50)/(cv*cv+1)
  pi90[i,1]<-quantile(x,0.05)
  pi90[i,2]<-quantile(x,0.50)
  pi90[i,3]<-quantile(x,0.95)
}

cv<-sd(xsum)/mean(xsum)
modesum<-quantile(xsum,0.50)/(cv*cv+1)
p90<-c(quantile(xsum,0.05),quantile(xsum,0.50),quantile(xsum,0.95))



#M74
M74<-as.matrix(read.table(paste0(folder,"M74.txt"),header=T))        
M74_alpha<-M74[ ,iinds]
M74_beta<-M74[ ,(iinds+1)]

if(proj_years>=1){
  M74_alpha<-rbind(M74_alpha, t(replicate(proj_years,M74_alpha[years,])))
  M74_beta<-rbind(M74_beta, t(replicate(proj_years,M74_beta[years,])))
}

#mean_alpha<-apply(M74_alpha[1:30, ], 2, mean)
#mean_beta<-apply(M74_beta[1:30, ], 2, mean)
#mean_m74<-mean_alpha/(mean_alpha+mean_beta)
#proj_m74<-M74_alpha[60,]/(M74_alpha[60,]+M74_beta[60,])    #mean
#proj_m74_sd<-sqrt((M74_alpha[60,]*M74_beta[60,])/((M74_alpha[60,]+M74_beta[60,])^2*(M74_alpha[60,]+M74_beta[60,]+1)))          
#Parr and smolts
Parr<-as.matrix(read.table(paste0(folder,"Parr.txt"),header=T))
#mu_ParrT[]	tau_ParrT[]	mu_SmoltT[]	mu_ParrS[]	tau_ParrS[]	mu_SmoltS[]
mu_Parr<-array(-10,dim=c(years,allstocks))
tau_Parr<-array(1000,dim=c(years,allstocks))
Smolt_Rsp<-array(0,dim=c(years,allstocks))

mu_Parr[,1]<-Parr[,1]
mu_Parr[,2]<-Parr[,4]

tau_Parr[,1]<-Parr[,2]
tau_Parr[,2]<-Parr[,5]

#fill in years 20 -> Torne and 22-> Simo with 0
mu_Parr[20:years,1]<- -10
mu_Parr[22:years,2]<- -10

tau_Parr[20:years,1]<- 1000
tau_Parr[22:years,2]<- 1000

Smolt_Rsp[,1]<-Parr[,3]
Smolt_Rsp[,2]<-Parr[,6]

Smolt_Rsp[Smolt_Rsp[,1]==0,1]<-0.001     #! Jan 2018
Smolt_Rsp[Smolt_Rsp[,2]==0,2]<-0.001

Rsp<-which(apply(Smolt_Rsp,2,sum)>0)

SmoltW<-as.matrix(read.table(paste0(folder,"SmoltW.txt"),header=T))
SmoltRdata<-as.matrix(read.table(paste0(folder,"SmoltR.txt"),header=T))  #goes straight to model


mu_SmoltW<-SmoltW[ ,iinds]
tau_SmoltW<-SmoltW[ ,(iinds+1)]

#Tagging data
releases<-dget(file=paste0(folder,"tag_releases.txt"))

rel_W<-releases$t_ObsW
rel_R<-releases$t_ObsR
rel_Rsp<-releases$t_ObsRsp

temp<-melt(releases$t_ObsW)
temp1<-melt(releases$t_ObsR)
temp2<-melt(releases$t_ObsRsp)

rel_W1<-array(0,dim=c(dim(rel_W)[1],dim(rel_W)[2]))
rel_R1<-array(0,dim=c(dim(rel_R)[1],dim(rel_R)[2]))
rel_Rsp1<-array(0,dim=c(dim(rel_Rsp)[1],dim(rel_Rsp)[2]))

for(k in 1:dim(rel_W)[1]){
  rel_W1[k,(1:dim(rel_W)[2])]<-temp[(k-1)*dim(rel_W)[2]+1:dim(rel_W)[2],3]
  rel_R1[k,(1:dim(rel_R)[2])]<-temp1[(k-1)*dim(rel_R)[2]+1:dim(rel_R)[2],3]  
  rel_Rsp1[k,(1:dim(rel_Rsp)[2])]<-temp2[(k-1)*dim(rel_Rsp)[2]+1:dim(rel_Rsp)[2],3]    
}

rel_W1<-cbind(rel_W1,rep(0,times=years))
rel_R1<-cbind(rel_R1,rep(0,times=years))
rel_Rsp1<-cbind(rel_Rsp1,rep(0,times=years))

cc_ObsW<-array(0,dim=c(years,maxage,1))
cc_ObsR<-array(0,dim=c(years,maxage,3))

cr_ObsW<-as.matrix(read.table(paste0(folder,"cr_ObsW.txt"),header=T))
cr_ObsRsp<-as.matrix(read.table(paste0(folder,"cr_ObsRsp.txt"),header=T))
cr_ObsR<-as.matrix(read.table(paste0(folder,"cr_ObsR.txt"),header=T))
cl_ObsW<-as.matrix(read.table(paste0(folder,"cl_ObsW.txt"),header=T))
cl_ObsR<-as.matrix(read.table(paste0(folder,"cl_ObsR.txt"),header=T))
cdo_ObsW<-as.matrix(read.table(paste0(folder,"cd_ObsW.txt"),header=T))
cdo_ObsR<-as.matrix(read.table(paste0(folder,"cd_ObsR.txt"),header=T))
cdc_ObsW<-as.matrix(read.table(paste0(folder,"ccd_ObsW.txt"),header=T))
cdc_ObsR<-as.matrix(read.table(paste0(folder,"ccd_ObsR.txt"),header=T))
cc_ObsW[,,1]<-as.matrix(read.table(paste0(folder,"cc_ObsW.txt"),header=T))
cc_OR<-as.matrix(read.table(paste0(folder,"cc_ObsR.txt"),header=T))
cc_ObsR[1:years,1:maxage,1]<-cc_OR[1:years,1:maxage]
cc_ObsR[1:years,1:maxage,2]<-cc_OR[1:years,(maxage+1):(2*maxage)]
cc_ObsR[1:years,1:maxage,3]<-cc_OR[1:years,(2*maxage+1):(3*maxage)]

#Effort
Effort1<-as.matrix(read.table(paste0(folder,"Effort1_withoutTrolling.txt"),header=T))
#       Effort1<-as.matrix(read.table(paste0(folder,"Effort1.txt"),header=T)))

Effort2<-as.matrix(read.table(paste0(folder,"Effort2.txt"),header=T))

Ectn<-array(0,dim=c(years,6,4)) 
Ecgn<-array(0,dim=c(years,6,4))
Er<-as.matrix(Effort1[ ,19:24])
El<-as.matrix(Effort1[ ,7:12])
Edo<-as.matrix(Effort1[ ,1:6])
Edc<-as.matrix(Effort1[ ,13:18])

Ectn[ ,1,1:4]<-Effort2[1:years,1:4]
Ectn[ ,2,1:4]<-Effort2[1:years,5:8]
Ectn[ ,3,1:4]<-Effort2[1:years,9:12]
Ectn[ ,4,1:4]<-Effort2[1:years,13:16]
Ectn[ ,5,1:4]<-Effort2[1:years,17:20]
Ectn[ ,6,1:4]<-Effort2[1:years,21:24]
Ecgn[ ,1,1:4]<-Effort2[1:years,25:28]
Ecgn[ ,2,1:4]<-Effort2[1:years,29:32]
Ecgn[ ,3,1:4]<-Effort2[1:years,33:36]
Ecgn[ ,4,1:4]<-Effort2[1:years,37:40]
Ecgn[ ,5,1:4]<-Effort2[1:years,41:44]
Ecgn[ ,6,1:4]<-Effort2[1:years,45:48]

Er0<-array(0,dim=c(proj_years,6)) 
El0<-array(0,dim=c(proj_years,6)) 
Edo0<-array(0,dim=c(proj_years,6)) 
Edc0<-array(0,dim=c(proj_years,6)) 

Ectn0<-array(0,dim=c(proj_years,6,4)) 
Ecgn0<-array(0,dim=c(proj_years,6,4))

Er<-rbind(Er,Er0)
El<-rbind(El,El0)
Edo<-rbind(Edo,Edo0)
Edc<-rbind(Edc,Edc0)

Ectn<-abind(Ectn,Ectn0, along=1)
Ecgn<-abind(Ecgn,Ecgn0, along=1)
############################################################
#Total catch data (summed over stocks and ages)

#river catch up to current year-1, NA thereafter
#coastal catch up to current year-1, NA thereafter
#offshore catch up to current year-2, NA thereafter
if(RiverCatch1==T){
  cat<-as.matrix(read.table(paste0(folder,"Catch_TrollingSeparated_riv_reared.txt"),header=T))
}else{
  cat<-as.matrix(read.table(paste0(folder,"Catch_TrollingSeparated_CR.txt"),header=T))
}

cat_r<-cat[,1]
cat_c<-cat[,2]
cat_o<-cat[,3]
cat_t<-cat[,4]

cat_r<-c(cat_r,NA)
cat_c<-c(cat_c,NA)
cat_o<-c(cat_o,NA)
cat_t<-c(cat_t,NA)

cat_ratio<-as.matrix(read.table(paste0(folder,"PropAU16.txt"),header=T))

#Wild vs. reared proportions

Scale<-as.matrix(read.table(paste0(folder,"Scale.txt"),header=T))
WpropObs<-array(NA,dim=c(years,2))
sd_wr<-array(NA,dim=c(years,2))
WpropObs[,1]<-Scale[1:years,1]
sd_wr[,1]<-Scale[1:years,2]
WpropObs[,2]<-Scale[1:years,3]
sd_wr[,2]<-Scale[1:years,4]

#Note Testebo?n data to be added to spawner counts in 2020.  It is an index river
#if(SimoMSW){
  spawner_counts<-as.matrix(read.table(paste0(folder,"spawner_counts_SimoMSW.txt"),header=T, encoding="UTF-8"))
#}else{
#  spawner_counts<-as.matrix(read.table(paste0(folder,"spawner_counts.txt"),header=T, encoding="UTF-8"))
#}

MSWladder<-as.matrix(read.table(paste0(folder,"Fishladder.txt"),header=T))
sp_count<-array(NA,dim=c(years,allstocks))
ladder_count<-array(NA,dim=c(years,allstocks))

sp_count[,1]<-spawner_counts[,1]  #Torne 
sp_count[,2]<-spawner_counts[,2]  #Simo
sp_count[,3]<-spawner_counts[,3]  #Kalix

ladder_count[,5]<-spawner_counts[,5]  #Pite
ladder_count[,10]<-spawner_counts[,10]  #Vindel
ladder_count[,17]<-spawner_counts[,17]  #Testeboån												  
#set counts before year 6 to NA (not used)


#set counts before year 6 to NA (not used)
sp_count[1:5, ]<-NA
ladder_count[1:5, ]<-NA
ladder_count[1:10,5]<-NA  #use spawner counts in Pite instead of Smolt likelihood approx from year 10


mean_sp_count<-apply(sp_count, 2, mean,na.rm=T)
mean_ladder_count<-apply(ladder_count, 2, mean,na.rm=T)

#survcalc<-(sp_count/1000)/exp(mu_SmoltW[1:32,])
#apply(survcalc,2,mean,na.rm=T)
#Torne         Simo         Kalix         Vindel
#0.035         0.075        0.016         


###############################################################################
Ume_probs<-as.matrix(read.table(paste0(folder,"pFLUme.txt"),header=T))
alpha_ladder<-array(10000,dim=c(years+5,allstocks))
beta_ladder<-array(10,dim=c(years+5,allstocks))

####################################################################################3

alpha_ladder[,5]<-8   #Pite see SP email 6 Feb 2018
beta_ladder[,5]<-2

alpha_ladder[,10]<-Ume_probs[,1]
beta_ladder[,10]<-Ume_probs[,2]

alpha_ladder[,17]<-1
beta_ladder[,17]<-1		
if(proj_years>=1){
  alpha_ladder<-rbind(alpha_ladder, t(replicate(proj_years,alpha_ladder[years,])))
  beta_ladder<-rbind(beta_ladder, t(replicate(proj_years,beta_ladder[years,])))
}

#survcalc<-(sp_count/1000)/exp(mu_SmoltW[1:years,])
#apply(survcalc,2,mean,na.rm=T)
#Torne         Simo         Kalix         Vindel
#0.034         0.066        0.012         0.037

#new data arrays - parameters for detection efficiency of spawner counting
#Lule and Dal are reared need separate data matrix

mu_sp_alpha<-rep(1, allstocks) 
mu_sp_beta<-rep(1, allstocks)

mu_sp_alpha[1]<-324    #Torne
mu_sp_beta[1]<-48

mu_sp_alpha[2]<-950   #Simo        #CHECK/CHANGE
mu_sp_beta[2]<-50

mu_sp_alpha[3]<-16   #Kalix
mu_sp_beta[3]<-13

mu_sp_alpha[4]<-8.03  #Råne #updated 25.02
mu_sp_beta[4]<-2.83

mu_sp_alpha[14]<-5.64    #Mörrum    updated 25.02
mu_sp_beta[14]<-26.22

CV_sp_alpha<-rep(1, allstocks)
CV_sp_beta<-rep(1, allstocks)

CV_sp_alpha[1]<-3.75    #Torne
CV_sp_beta[1]<-71.25

CV_sp_alpha[2]<-10   #Simo   
CV_sp_beta[2]<-990

CV_sp_alpha[3]<-10   #Kalix
CV_sp_beta[3]<-30

CV_sp_alpha[4]<-10   #Råne 
CV_sp_beta[4]<-30

CV_sp_alpha[14]<-10   #Mörrum
CV_sp_beta[14]<-30

N_sp_count<-array(10,dim=c(years,allstocks))   #N for MSW proportion observation model
MSWprop<-array(NA,dim=c(years,allstocks))   #N for MSW proportion observation model

N_sp_count[,1]<-MSWladder[1:years,2]  #Torne
N_sp_count[,3]<-MSWladder[1:years,4]  #Kalix
N_sp_count[,7]<-MSWladder[1:years,6]  #Byske
N_sp_count[,10]<-MSWladder[1:years,8]  #Vindel
N_sp_count[,11]<-MSWladder[1:years,10]  #Ore
N_sp_count[,5]<-MSWladder[1:years,12]  #Pite
#N_sp_count[,17]<-MSWladder[1:years,14]  #Test

MSWprop[,1]<-MSWladder[1:years,1]/N_sp_count[,1]  #Torne
MSWprop[,3]<-MSWladder[1:years,3]/N_sp_count[,3]  #Kalix
MSWprop[,7]<-MSWladder[1:years,5]/N_sp_count[,7]  #Byske
MSWprop[,10]<-MSWladder[1:years,7]/N_sp_count[,10]  #Vindel
MSWprop[,11]<-MSWladder[1:years,9]/N_sp_count[,11]  #Ore
MSWprop[,5]<-MSWladder[1:years,11]/N_sp_count[,5]  #Pite
#MSWprop[,17]<-MSWladder[1:years,13]/N_sp_count[,17]  #Test														   

#set counts before year 6 to NA (not used)

river_w_prop<-as.matrix(read.table(paste0(folder,"Wprop.txt"),header=T))
WGrilse<-array(NA,dim=c((years-1),allstocks))
WMSW<-array(NA,dim=c((years-1),allstocks))
Grilse_all<-array(1,dim=c((years-1),allstocks))
MSW_all<-array(1,dim=c((years-1),allstocks))

WMSW[,1]<-river_w_prop[,1]
MSW_all[,1]<-river_w_prop[,2]
WGrilse[,1]<-river_w_prop[,3]
Grilse_all[,1]<-river_w_prop[,4]

#########Fish ladders reared stocks

Rprop<-as.matrix(read.table(paste0(folder,"Rprop.txt"),header=T,sep="\t"))   
#rstocks: 1 Lule?lven (AU2)
#         2 Dal?lven (AU3)
RProp<-array(NA,dim=c(years,2))
TrapTot<-array(NA,dim=c(years,2))
CatchR<-array(NA,dim=c(years,2))

RProp<-Rprop[ ,c(1,4)]
TrapTot<-Rprop[ ,c(2,5)]
CatchR<-Rprop[ ,c(3,6)]

Rmark<-as.matrix(read.table(paste0(folder,"Lule_MarkRecap.txt"),header=T,sep="\t"))   
NLuleRel<-Rmark[,1]
NLuleRec<-Rmark[,2]
yLule<-Rmark[,3]

#Temperature data for maturation prior

temp<-as.matrix(read.table(paste0(folder,"Temperature.txt"),header=T))
muTemp<-temp[,1]
tauTemp<-temp[,2]

muTemp<-c(muTemp,rep(muTemp[length(muTemp)],times=proj_years))
tauTemp<-c(tauTemp,rep(tauTemp[length(tauTemp)],times=proj_years))

##Seal predation mortality

seals<-dget(file=paste0(folder,"seal_mortality_coeffs.txt"))
sMort<-seals$sealMort
temps<-melt(seals$sealMort)

sealM<-array(0,dim=c(dim(sMort)[1],dim(sMort)[2]))

for(k in 1:dim(sMort)[1]){
  sealM[k,(1:dim(sMort)[2])]<-temps[(k-1)*dim(sMort)[2]+1:dim(sMort)[2],3]         
}
if(proj_years>=1){
  sealM<-rbind(sealM, t(replicate(proj_years,sealM[years,])))
}

sealMort<-array(1,dim=c(dim(sealM)[1],dim(sealM)[2],AUS))
for(k in avail_seal){
  
  sealMort[,,k]<-sealM
  
}




##Reporting coeffs

repo<-dget(file=paste0(folder,"reportcAdj.txt"))$reportcAdj
tmp<-melt(repo)

reportcAdj<-array(0,dim=c(dim(repo)[1],dim(repo)[2]))

for(k in 1:dim(repo)[1]){
  reportcAdj[k,(1:dim(repo)[2])]<-tmp[(k-1)*dim(repo)[2]+1:dim(repo)[2],3]         
}
if(proj_years>=1){
  reportcAdj<-rbind(reportcAdj, t(replicate(proj_years,reportcAdj[years,])))
}

unrep<-read.table(paste0(folder,"unrep_coefs.txt"),header=T)

Ume_prop_fem<-as.matrix(read.table(paste0(folder,"MSW_prop_fem_Ume_Vindel.txt"),row.names=1))
prop_fem<-array(0,dim<-c(years,5,allstocks))
prop_fem[,,1]<-rep(c(0.06,0.73,0.73,0.89,0.89),each=years)
for(s in 2:9){
  prop_fem[,,s]<-prop_fem[,,1]
}
prop_fem[ ,1,10]<-0.06
for(i in 1:years){
  prop_fem[i,2:5,10]<-Ume_prop_fem[i]
}
for(s in 11:allstocks){
  prop_fem[,,s]<-prop_fem[,,1]
}

#2019 read in prior parameters for reduced survival after counting for Ume/Vindel, now defined for all years
#model development use fitted Beta distributions in the FLHM see Ume priors.R
Ume_migr<-as.matrix(read.table(paste0(folder,"Vindel_survival_after_counting.txt"),header=T))

alpha_migr<-array(10000,dim=c(years+5,allstocks))
beta_migr<-array(10,dim=c(years+5,allstocks))

alpha_migr[1:(years-1),10]<-Ume_migr[,1]
beta_migr[1:(years-1),10]<-Ume_migr[,2]

alpha_migr[30:34,13]<-1.75  #Ljungan 2016-2020 prop that dies~Beta(2.75,1.75)
beta_migr[30:34,13]<-2.75

rivHR<-as.matrix(read.table(paste0(folder,"rivHR.txt"),header=T,row.names=1))
colnames(rivHR)<-NULL      
rownames(rivHR)<-NULL 

tmort<-read.table(paste0(folder,"Trolling_p.release.txt"),sep="\t",header=T)
#calendar year 2022 is model year 2021!
addna<-matrix(rep(tmort[dim(tmort)[1],],times=7),ncol=2, byrow=TRUE)
colnames(addna)<-c("alpha_rel","beta_rel")  

tmort<-rbind(tmort[2:dim(tmort)[1],],addna)
alpha_rel<-as.numeric(tmort$alpha_rel)
beta_rel<-as.numeric(tmort$beta_rel)

rivcat<-as.matrix(read.table(paste0(folder,"river_catches.txt"),header=F))
ncrW_ObsTot<-rivcat








