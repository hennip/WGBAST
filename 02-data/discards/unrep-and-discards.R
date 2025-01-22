# This model computes estimates (median, 90% prob. intervals) for
#	1) total catch, discards and unreported catch for the whole Baltic Sea (Tables 2.2.1 and 2.2.2)
#        Notice that misreported catch is not included in unreported catch in these tables since WGBAST 2019
#	2) dicards by management unit and gear (undersized, seal damaged Table 2.3.2) 
#	3) discards (seal damaged+other), unreported catch (sea+river) and misreported catch by management
#	    unit  and country (Table 2.3.4)
#	4) Total catch at sea (commercial and recreational) as well as discarding, unreporting and misreporting
# 	   for the Figure 2.2.3
#	5) the same elements as above plus river catch (including unreporting and some commercial catch in
#	    rivers) for the Figure 4.3.2.9
#	6) Shares of different catch elements (under wanted and wanted catches) for the outlook tables in the
#		Baltic salmon catch advice
#
# Catch data (year, country, fisheries come from the WGBAST catch data base.
# Country, area and fishery specific conversion factors are from Table 2.3.1;
# these are read in at the end of the model

# Monitor and input Tunrep, Tdis and Tcatch to Misreport&Discard-file.

#
# indexing [i,j,k]
#	i=year {1,...,n}; 1=2001,...
#	j=country {1,...,9}; 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU
#	k=management unit {1,2}; 1=SD22-31(Main Basin and Gulf of Bothnia), 2=SD32 (Gulf of Finland)


source("../run-this-first-wgbast.R")
source("02-data/discards/functions-unrep-discards.R")
source("02-data/discards/dat-unrep-discards.R")
source("02-data/discards/model-unrep-discards.R") # Original structure

###############
# KORJATTAVAA:
# - Jokisaaliin jakaminen reported ja unreported -osiin
# - Dead discards, nimeäminen selvemmin. Nyt Dis_LLD tarkoittaa kuolleita, Dis_LLD_alive eläviä
#  eli Dis_LLD voisi muuttaa Dis_LLD_dead





# Decide if you wish to run the model for numb or weight

#numb<-T # catch in number
#numb<-F # catch in weight


l1<-list(
  cry=cry,
  MLLtau=as.matrix(unname(MLLtau)), MLLM=as.matrix(unname(MLLM)), 
  MDNtau=as.matrix(unname(MDNtau)), MDNM=as.matrix(unname(MDNM)), 
  MTNtau=as.matrix(unname(MTNtau)), MTNM=as.matrix(unname(MTNM)), 
  OM=as.matrix(unname(OM)), Otau=as.matrix(unname(Otau)),
  CM=as.matrix(unname(CM)), Ctau=as.matrix(unname(Ctau)),
  RM=as.matrix(unname(RM)), Rtau=as.matrix(unname(Rtau)),
  
  LLM=as.matrix(unname(LLM)), LLtau=as.matrix(unname(LLtau)),
  DNM=as.matrix(unname(DNM)), DNtau=as.matrix(unname(DNtau)),
  TNM=as.matrix(unname(TNM)), TNtau=as.matrix(unname(TNtau)),
  SLLDM=as.matrix(unname(SLLDM)), SLLDtau=as.matrix(unname(SLLDtau)),
  SGNDM=as.matrix(unname(SGNDM)), SGNDtau=as.matrix(unname(SGNDtau)),
  STNM=as.matrix(unname(STNM)), STNtau=as.matrix(unname(STNtau)),

PLfactor=as.vector(unname(PL_sealfac))[[1]],

PLMisr=as.vector(unname(PL_misrep_N))[[1]],
LLD=LLD_N, FYK=FYK_N, GND=GND_N, MIS=MIS_N,
Recr=Recr_N, River=River_N,
SealGND=SealGND_N, SealLLD=SealLLD_N, SealFYK=SealFYK_N, SealMIS=SealMIS_N,
Dis=Dis_N

# PLMisr=as.vector(unname(PL_misrep_W))[[1]],
# LLD=LLD_W, FYK=FYK_W, GND=GND_W, MIS=MIS_W,
# Recr=Recr_W, River=River_W,
# SealGND=SealGND_W, SealLLD=SealLLD_W, SealFYK=SealFYK_W, SealMIS=SealMIS_W,
# Dis=Dis_W
)

datalist<-l1


parnames<-c(
  
  "epsilon", "MDisLL", "MDisDN", "MDisC", 
  "Oconv", "Cconv", "Rconv", 
  "DisLL", "DisDN", "DisC", 
  "SealLL", "SealDN", "SealC")

  
#   
parnames<-c(

  "A_TotUnrep_BS", # for T2.2.1 and T2.2.2
            "A_TotDis_BS","A_TotCatch_BS","A_TotSeal_BS",

            # Just in case
            "B_TotUnrep_MU","B_TotCatch_MU",

            # for F4.3.2.9
            "B_TotDisSeal_MU","B_TotRiver",
            "B_TotRecr_sea","B_TotMisr_sea",

            "B_TotUnrep_sea", # for F2.2.3
            "B_TotUnrep_river", # for T2.3.4
            "B_TotRepCom_sea", # reported comm catch for F2.2.3
            "B_TotCatchCom_sea", # sum of all components of COMM dead catch
            "B_TotCatch_sea", # for F2.2.3
            "B_TotUnrepDis_sea", # Estimate of the total unrep, misrep and disdards for F2.2.3 so far only this is used in the Henni's R-model apart from catch data from T2.6.1 and T2.7.1

            # dead discard components by MU for T2.3.2
            "B_TotDis_GND","B_TotDis_LLD",
            "B_TotDis_FYK","B_TotDis_MIS","B_TotDis_dead",

            "B_TotSeal_GND","B_TotSeal_LLD",
            "B_TotSeal_FYK","B_TotSeal_MIS","B_TotSeal",

            # alive discards bu MU
            "B_TotDis_GND_alive","B_TotDis_LLD_alive",
            "B_TotDis_FYK_alive","B_TotDis_alive",

            # by country and MU
            "TcatchCom",		# COMM at sea
            "TRecrSea",					# RECR at sea
            "TRiver",					# River catch
            "Tcatch",				# Total catch
            "Tdis_alive",		# Alive discards
            "Tdis",					# Dead discards
            "Tseal","TMisr","Runrep","Sunrep",
            "Tunrep_T2" #=misrep+runrep+sunrep
)

# **************************************************************
#   # the following is only for the checking purposes - ignore in regular runs
#   **************************************************************
#   # 
# "DNdis","LLdis",
# "TNdis","OTdis",
# "DNseal", "LLseal",
# "OTseal","TNseal",
# "Tseal", "Tdis",
# 
# "Oconv", "Cconv",
# "Rconv", "DisLL",
# "DisDN", "DisC",
# 
# "SealLL", "SealDN", "SealC",


run0 <- run.jags(M1, monitor= parnames,
                 data=datalist,#inits = initsall,
                 n.chains = 2, method = 'parallel', thin=100,
                 burnin =10000, modules = "mix",
                 sample =1000, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=100)

summary(run0)
chains<-as.mcmc(run0)










###############################################################################
# try running only stochastic variables in JAGS and calculating the rest in R

source("02-data/discards/model-unrep-discards-cleaned.R") # Only stochastic variables 


l1<-list(
  MLLtau=as.matrix(unname(MLLtau)), MLLM=as.matrix(unname(MLLM)), 
  MDNtau=as.matrix(unname(MDNtau)), MDNM=as.matrix(unname(MDNM)), 
  MTNtau=as.matrix(unname(MTNtau)), MTNM=as.matrix(unname(MTNM)), 
  OM=as.matrix(unname(OM)), Otau=as.matrix(unname(Otau)),
  CM=as.matrix(unname(CM)), Ctau=as.matrix(unname(Ctau)),
  RM=as.matrix(unname(RM)), Rtau=as.matrix(unname(Rtau)),
  
  LLM=as.matrix(unname(LLM)), LLtau=as.matrix(unname(LLtau)),
  DNM=as.matrix(unname(DNM)), DNtau=as.matrix(unname(DNtau)),
  TNM=as.matrix(unname(TNM)), TNtau=as.matrix(unname(TNtau)),
  SLLDM=as.matrix(unname(SLLDM)), SLLDtau=as.matrix(unname(SLLDtau)),
  SGNDM=as.matrix(unname(SGNDM)), SGNDtau=as.matrix(unname(SGNDtau)),
  STNM=as.matrix(unname(STNM)), STNtau=as.matrix(unname(STNtau)))

datalist<-l1


parnames<-c(
  
  "epsilon", "MDisLL", "MDisDN", "MDisC", 
  "Oconv", "Cconv", "Rconv", 
  "DisLL", "DisDN", "DisC", 
  "SealLL", "SealDN", "SealC")


run00 <- run.jags(M1, monitor= parnames,
                 data=datalist,#inits = initsall,
                 n.chains = 2, method = 'parallel', thin=100,
                 burnin =10000, modules = "mix",
                 sample =1000, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=100)

summary(run00)
chains<-as.mcmc.list(run00)


# Tässä kaikki stokastiset muuttujat poimittuna ajotiedostosta (poislukien epsilon, joka on vain tekninen)
# Kaikki loput pitäisi pystyä laskemaan näiden ja datan pohjalta deterministisesti.

Ncry<-length(cry)
Ni<-23
Nsim<-1000
DisLL<-DisDN<-DisC<-array(NA, dim=c(Ni, Ncry, Nsim))
SealLL<-SealDN<-SealC<-array(NA, dim=c(Ni, Ncry, Nsim))
Oconv<-Cconv<-Rconv<-array(NA, dim=c(Ni, Ncry, Nsim))
for(i in 1:Ni){
  for(j in 1:Ncry){
    Oconv[i,j,]<-chains[,paste0("Oconv[",i,",",j,"]")][[1]]
    Cconv[i,j,]<-chains[,paste0("Cconv[",i,",",j,"]")][[1]]
    Rconv[i,j,]<-chains[,paste0("Rconv[",i,",",j,"]")][[1]]
   
    DisLL[i,j,]<-chains[,paste0("DisC[",i,",",j,"]")][[1]]
    DisDN[i,j,]<-chains[,paste0("DisC[",i,",",j,"]")][[1]]
    DisC[i,j,]<-chains[,paste0("DisC[",i,",",j,"]")][[1]]
  
    SealLL[i,j,]<-chains[,paste0("SealC[",i,",",j,"]")][[1]]
    SealDN[i,j,]<-chains[,paste0("SealC[",i,",",j,"]")][[1]]
    SealC[i,j,]<-chains[,paste0("SealC[",i,",",j,"]")][[1]]
  }}

MDisLL<-chains[,"MDisLL"][[1]]
MDisDN<-chains[,"MDisDN"][[1]]
MDisC<-chains[,"MDisC"][[1]]

SealGND<-SealGND_N
SealLLD<-SealLLD_N
SealFYK<-SealFYK_N
SealMIS<-SealMIS_N

LLD<-LLD_N
GND<-GND_N
FYK<-FYK_N
Dis<-Dis_N
MIS<-MIS_N

River<-River_N
Recr<-Recr_N


dim(Oconv)

Tseal<-Seal_MIS<-Seal_FYK<-Seal_LLD<-Seal_GND<-array(NA, dim=c(Ni, Ncry, 2, Nsim))
Dis_MIS<-Dis_FYK<-Dis_LLD<-Dis_GND<-array(NA, dim=c(Ni, Ncry, 2, Nsim))
Tdis<-TMisr<-array(NA, dim=c(Ni, Ncry, 2, Nsim))

# Muunnokset raportoimattomuuskertoimille. Mikä näiden tulkinta on?
Oconv_trans<-Cconv_trans<-Rconv_trans<-array(NA, dim=c(Ni, Ncry, Nsim))
for(i in 1:Ni){
  for(j in 1:Ncry){
    Oconv_trans[i,j,]<-(1+Oconv[i,j,s]/(1-Oconv[i,j,s]))
    Cconv_trans[i,j,]<-(1+Cconv[i,j,s]/(1-Cconv[i,j,s]))
    Rconv_trans[i,j,]<-(1+Rconv[i,j,s]/(1-Rconv[i,j,s]))
  }
}


# PL
for(i in 1:Ni){ 
  for(k in 1:2){
    for(s in 1:Nsim){
      TMisr[i,4,1,s]<-as.vector(unname(PL_misrep_N))[[1]][i]#*epsilon
}}}
#PLfactor=as.vector(unname(PL_sealfac))[[1]],



# Kaikki maat
# Dead discards
for(i in 1:Ni){ 
  for(j in 1:Ncry){ 
    for(k in 1:2){
      for(s in 1:Nsim){
    # dead discards of LLD+Misreporting
    Dis_LLD[i,j,k,s]<- (LLD[i,j,k] + TMisr[i,j,k,s])*Oconv_trans[i,j,s]*
                        DisLL[i,j,s]/(1-DisLL[i,j,s])*MDisLL[s]	
    # dead discards of DNS fishery; stopped in 2007
    Dis_GND[i,j,k,s]<- GND[i,j,k]*Oconv_trans[i,j,s] * DisDN[i,j,s]/(1-DisDN[i,j,s])*MDisDN[s]	
}}}}


# FI
for(i in 1:Ni){ 
  for(k in 1:2){
    # Mitä tämä tekee? Lisää raportoimattomuuden?
    # Mistä nimi tulee, miksi ei ole esim. unrep?
    Seal_GND[i,1,k,]<- SealGND[i,1,k]*Oconv_trans[i,1,]
    Seal_LLD[i,1,k,]<- SealLLD[i,1,k]*Oconv_trans[i,1,]
    Seal_FYK[i,1,k,]<- SealFYK[i,1,k]*Cconv_trans[i,1,]
    Seal_MIS[i,1,k,]<- SealMIS[i,1,k]*Cconv_trans[i,1,] 
    
    # Total seal damages by year
    Tseal[i,1,k,]<- Seal_GND[i,1,k,] + Seal_LLD[i,1,k,] + Seal_FYK[i,1,k,] + Seal_MIS[i,1,k,]       
    
    Tdis[i,1,k,]<- Dis_LLD[i,1,k,] + Dis_GND[i,1,k,] + Dis_FYK[i,1,k,] + Dis_MIS[i,1,k,]	#Total discards by year
    Dis_FYK[i,1,k,]<- Dis[i,1,k]*Oconv_trans[i,1,]	# all reported disgards allocated to FYK
    Dis_MIS[i,1,k,]<- MIS[i,1,k]*Cconv_trans[i,1,] * DisC[i,1,]/(1-DisC[i,1,]) 
    # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
    
}}

#   countries EE and RU has no estimates for seal damages and other discards
for(i in 1:Ni){ 
  for (j in 8:9){         
    for(k in 1:2){
      
      Seal_LLD[i,j,k,]<- LLD[i,j,k]*Oconv_trans[i,j,] * SealLL[i,j,]/(1-SealLL[i,j,])		# Seal damages LLD+Misreporting
      Seal_GND[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,] * SealDN[i,j,]/(1-SealDN[i,j,])	# Seal damage DNS fishery; stopped in 2007
      Seal_FYK[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * SealC[i,j,]/(1-SealC[i,j,]) 	# catches are corrected with relevant unreporting
      Seal_MIS[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * SealC[i,j,]/(1-SealC[i,j,])
      # Seal damage coastal fishery; mainly TN but all coastal caches included
      
      Tseal[i,j,k,]<- Seal_LLD[i,j,k,] + Seal_GND[i,j,k,] + Seal_FYK[i,j,k,] + Seal_MIS[i,j,k,] 	#Total seal damages by year
  
      Tdis[i,j,k,]<-  Dis_LLD[i,j,k,] + Dis_GND[i,j,k,] + Dis_FYK[i,j,k,] + Dis_MIS[i,j,k,]   	#Total discards by year 		
      Dis_FYK[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
      Dis_MIS[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,]) 
      # all fish die; no reported Dis_MIS
      
    }}}


Ounrep<-Cunrep<-Runrep<-Sunrep<-array(NA, dim=c(Ni, Ncry,2, Nsim))
Tunrep_F2<-Tunrep_F4<-Tunrep_T2<-array(NA, dim=c(Ni, Ncry,2, Nsim))
TRiver<-TRecrSea<-array(NA, dim=c(Ni, Ncry,2))

#  countries FI, EE and RU
# NOTE! Use cry-parameter here to pick up countries 1, 8 and 9
for(i in 1:Ni){ 
  for (j in 1:3){         #   countries EE and RU has no estimates for seal damages and other discards
    for(k in 1:2){
      
      # Huomaa että tässä Oconv:n muunnoksessa ei ole mukana 1-
      Ounrep[i,cry[j],k,]<- (GND[i,cry[j],k]+LLD[i,cry[j],k])* Oconv[i,cry[j],]/(1-Oconv[i,cry[j],])	
      # unreported catch in off-shore fisheries
      Cunrep[i,cry[j],k,]<- (FYK[i,cry[j],k]+MIS[i,cry[j],k]) * Cconv[i,cry[j],]/(1-Cconv[i,cry[j],])	 # coast
      Runrep[i,cry[j],k,]<- River[i,cry[j],k] * Rconv[i,cry[j],] /(1-Rconv[i,cry[j],])	 # river
      Sunrep[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] # total unreporting in sea
  
      Tunrep_F2[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] + TMisr[i,cry[j],k,] # unreporting in river excluded from unreporting in F2.2.3
      Tunrep_F4[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
      Tunrep_T2[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] + Runrep[i,cry[j],k,]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
      TRiver[i,cry[j],k]<- River[i,cry[j],k]
      TRecrSea[i,cry[j],k]<- Recr[i,cry[j],k]
 
    }}}  
       #    TMisr[i,cry[j],k,]<- 0.0001*epsilon
      
      # Total unreported by year, country and management unit
      # input values for catches ( from file xxxx)
      # GND[,,] driftnet	LLD[,,] longline	FYK[,,] trapnet	MIS[,,] other gears
      # Recr[,,] recreational  River[,,] river cathes
      # Misr[,,] misreporting Poland only
      # Seal_GND[,,] Seal_LLD[,,] Seal_FYK[,,] Seal_MIS[,,] gear specific reported seal damages
      # Dis[,,] reported discards
      
      # Dead discards
      Dis_LLD[i,cry[j],k]<- LLD[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]]))* DisLL[i,cry[j]]/(1-DisLL[i,cry[j]])*MDisLL	# dead discards of LLD+Misreporting
      Dis_GND[i,cry[j],k]<- GND[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * DisDN[i,cry[j]]/(1-DisDN[i,cry[j]])*MDisDN	# dead discards of DNS fishery; stopped in 2007
      
      # Alive discards; not added to the total catch
      Dis_LLD_alive[i,cry[j],k]<- LLD[i,cry[j],k] *(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]]))* DisLL[i,cry[j]]/(1-DisLL[i,cry[j]])*(1-MDisLL)	# Alive discards of LLD+Misreporting
      Dis_GND_alive[i,cry[j],k]<- GND[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * DisDN[i,cry[j]]/(1-DisDN[i,cry[j]])*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
      Dis_FYK_alive[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]])*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
      Tdis_alive[i,cry[j],k]<-  Dis_LLD_alive[i,cry[j],k] + Dis_GND_alive[i,cry[j],k] + Dis_FYK_alive[i,cry[j],k]   	#Total alive discards by year, MU and country	
      
      Tcatch[i,cry[j],k]<- GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k] + 
        Recr[i,cry[j],k] + River[i,cry[j],k]  + Tunrep_T2[i,cry[j],k] + Tdis[i,cry[j],k]
      TcatchCom[i,cry[j],k]<- (GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k])*epsilon						
      # Total catch by year, country and management unit
      
    }}}


for(j in 1:3){        
  # order of courtries in loop list(cry=c(1,8,9,2,3,4,5,6,7)); given in a data file
  
}


