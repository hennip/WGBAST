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

min_year<-2001
max_year<-2024
years<-min_year:max_year
NumYears<-length(years)


source("02-data/unrep-discards/functions-unrep-discards.R")
source("02-data/unrep-discards/dat-unrep-discards.R")


###############
# KORJATTAVAA:
# - Jokisaaliin jakaminen reported ja unreported -osiin


###############################################################################
# try running only stochastic variables in JAGS and calculating the rest in R

source("02-data/unrep-discards/model-unrep-discards.R") # Only stochastic variables 


l1<-list(
  Nyears=NumYears,
  
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
  #"epsilon", 
  "MDisLL", "MDisDN", "MDisC", 
  "Oconv", "Cconv", "Rconv", 
  "DisLL", "DisDN", "DisC", 
  "SealLL", "SealDN", "SealC")

# # 
# run00 <- run.jags(M1, monitor= parnames,
#                  data=datalist,#inits = initsall,
#                  n.chains = 2, method = 'parallel', thin=100,
#                  burnin =10000, modules = "mix",
#                  sample =1000, adapt = 10000,
#                  keep.jags.files=F,
#                  progress.bar=TRUE, jags.refresh=100)
# 
# summary(run00, var="Oconv")
# summary(run00, var="DisC")
#
# summary(run00, var="DisC[20,2]")
#
# summary(chains[,"DisC[20,2]"])
# chains<-as.mcmc.list(run00)
#saveRDS(chains, file="02-data/discards/chains_unrep_discards_2025.rds")

# The same estimates are used for both catch in weight and catch in number
chains<-readRDS("02-data/unrep-discards/chains_unrep_discards_2025.rds")



################################################################################
# Decide if you wish to get calculations for catch in number or in weight

if(exists("skip")==F){
number_or_weight<-"N" # catch in number
#number_or_weight<-"W" # catch in weight
}

# Tässä kaikki stokastiset muuttujat poimittuna ajotiedostosta (poislukien epsilon, joka on vain tekninen)
# Kaikki loput pitäisi pystyä laskemaan näiden ja datan pohjalta deterministisesti.

Ncry<-length(cry)
Ni<-NumYears
Nsim<-1000
DisLL<-DisDN<-DisC<-array(NA, dim=c(Ni, Ncry, Nsim))
SealLL<-SealDN<-SealC<-array(NA, dim=c(Ni, Ncry, Nsim))
Oconv<-Cconv<-Rconv<-array(NA, dim=c(Ni, Ncry, Nsim))
for(i in 1:Ni){
    for(j in 1:Ncry){
    Oconv[i,j,]<-chains[,paste0("Oconv[",i,",",j,"]")][[1]]
    Cconv[i,j,]<-chains[,paste0("Cconv[",i,",",j,"]")][[1]]
    Rconv[i,j,]<-chains[,paste0("Rconv[",i,",",j,"]")][[1]]
   
    DisLL[i,j,]<-chains[,paste0("DisLL[",i,",",j,"]")][[1]]
    DisDN[i,j,]<-chains[,paste0("DisDN[",i,",",j,"]")][[1]]
    DisC[i,j,]<-chains[,paste0("DisC[",i,",",j,"]")][[1]]
  
    SealLL[i,j,]<-chains[,paste0("SealLL[",i,",",j,"]")][[1]]
    SealDN[i,j,]<-chains[,paste0("SealDN[",i,",",j,"]")][[1]]
    SealC[i,j,]<-chains[,paste0("SealC[",i,",",j,"]")][[1]]
  }
  }

MDisLL<-chains[,"MDisLL"][[1]]
MDisDN<-chains[,"MDisDN"][[1]]
MDisC<-chains[,"MDisC"][[1]]

if(number_or_weight=="N"){
  
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
}


if(number_or_weight=="W"){
  
  SealGND<-SealGND_W
  SealLLD<-SealLLD_W
  SealFYK<-SealFYK_W
  SealMIS<-SealMIS_W
  
  LLD<-LLD_W
  GND<-GND_W
  FYK<-FYK_W
  Dis<-Dis_W
  MIS<-MIS_W
  
  River<-River_W
  Recr<-Recr_W
}

#dim(Oconv)

Tdis<-Tseal<-Seal_MIS<-Seal_FYK<-Seal_LLD<-Seal_GND<-array(0, dim=c(Ni, Ncry, 2, Nsim))
TMisr<-array(0, dim=c(Ni, Ncry, 2))

Ounrep<-Cunrep<-Runrep<-Sunrep<-array(0, dim=c(Ni, Ncry,2, Nsim))
Tcatch<-Tunrep_misrep_OC<-Tunrep_OC<-Tunrep_OCR<-array(0, dim=c(Ni, Ncry,2, Nsim))
TcatchCom<-TRiver<-TRecrSea<-array(0, dim=c(Ni, Ncry,2))

Dis_MIS_dead<-Dis_FYK_dead<-Dis_LLD_dead<-Dis_GND_dead<-array(0, dim=c(Ni, Ncry, 2, Nsim))
Dis_FYK_alive<-Dis_LLD_alive<-Dis_GND_alive<-Tdis_alive<-array(0, dim=c(Ni, Ncry,2, Nsim))


# Muunnokset raportoimattomuuskertoimille.
# Tulkinta (Tapsa 1/25):
# (Reported + Unreported)*Conv = Unreported
# <=> Unreported - Unreported*Conv = Reported*Conv
# <=> Unreported(1-Conv) = Reported*Conv
# <=> Unreported = Reported*Conv/(1-Conv)
# Tässä siis ajatellaan että elisitoimalla saatu kerroin on osuus kokonaissaaliista
# Huomaa, että allaolevassa muunnoksessa on mukana +1, mikä johtuu siitä 
# että tätä muunnosta käytetään arvioitaessa esim. kokonaismäärää hylkeen pilaamille
# (tai muuten raportoidun kokonaissaaliin perusteella) jolloin huomioidaan sekä
# raportoitu että raportoimaton osa.
Oconv_trans<-Cconv_trans<-Rconv_trans<-array(0, dim=c(Ni, Ncry, Nsim))
for(i in 1:Ni){
  for(j in 1:Ncry){
    Oconv_trans[i,j,]<-(1+Oconv[i,j,]/(1-Oconv[i,j,]))
    Cconv_trans[i,j,]<-(1+Cconv[i,j,]/(1-Cconv[i,j,]))
    Rconv_trans[i,j,]<-(1+Rconv[i,j,]/(1-Rconv[i,j,]))
  }
}


# PL
PL_SD26<-c()
for(i in 1:Ni){ 
  for(k in 1:2){
    if(number_or_weight=="N"){
      TMisr[i,4,1]<-as.vector(unname(PL_misrep_N))[[1]][i]#*epsilon
    }else{
      TMisr[i,4,1]<-as.vector(unname(PL_misrep_W))[[1]][i]#*epsilon
    }
  }
  PL_SD26[i]<-as.vector(unname(PL_sealfac))[[1]][i]#*epsilon
}
#PLfactor=as.vector(unname(PL_sealfac))[[1]],


################################################################################
# Calculations

# All countries!
for(k in 1:2){
  for(i in 1:Ni){ 
    for(j in 1:9){
# dead discards of LLD+Misreporting
Dis_LLD_dead[i,j,k,]<- (LLD[i,j,k] + TMisr[i,j,k])*Oconv_trans[i,j,]*
  (DisLL[i,j,]/(1-DisLL[i,j,]))*MDisLL #MDisLL:mortality

# dead discards of DNS fishery; stopped in 2007
Dis_GND_dead[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,] * (DisDN[i,j,]/(1-DisDN[i,j,]))*MDisDN	
}}}

# FI  # country FI seal damages and other discards are given
for(k in 1:2){
  for(i in 1:Ni){ 
    Seal_GND[i,1,k,]<- SealGND[i,1,k]*Oconv_trans[i,1,]
    Seal_LLD[i,1,k,]<- SealLLD[i,1,k]*Oconv_trans[i,1,]
    Seal_FYK[i,1,k,]<- SealFYK[i,1,k]*Cconv_trans[i,1,]
    Seal_MIS[i,1,k,]<- SealMIS[i,1,k]*Cconv_trans[i,1,] 
    
    Dis_FYK_dead[i,1,k,]<- Dis[i,1,k]*Oconv_trans[i,1,]	# all reported disgards allocated to FYK
    Dis_MIS_dead[i,1,k,]<- MIS[i,1,k]*Cconv_trans[i,1,] * (DisC[i,1,]/(1-DisC[i,1,])) 
    # discards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
  }}

# SE# country 2=SE, seal damages in LLD and FYK are given in data
for(k in 1:2){
    for(i in 1:17){ 
      Seal_MIS[i,2,k,]<- MIS[i,2,k]*Cconv_trans[i,2,] * (SealC[i,2,]/(1-SealC[i,2,]))
      # no data from MIS until 2018
    }
    for(i in 18:Ni){ 
      Seal_MIS[i,2,k,]<- SealMIS[i,2,k]*Cconv_trans[i,2,] 
    }  
    
  for(i in 1:Ni){ 
    
  # Reported gear specific seal damages are corrected with fishery specific unreporting
    Seal_GND[i,2,k,]<- GND[i,2,k]*Oconv_trans[i,2,] * (SealDN[i,2,]/(1-SealDN[i,2,]))	# Seal damage in GND; stopped in 2007
    Seal_LLD[i,2,k,]<- SealLLD[i,2,k]*Oconv_trans[i,2,]
    Seal_FYK[i,2,k,]<- SealFYK[i,2,k]*Cconv_trans[i,2,]

    Dis_FYK_dead[i,2,k,]<- FYK[i,2,k]*Cconv_trans[i,2,] * (DisC[i,2,]/(1-DisC[i,2,]))*MDisC	# no reported Dis until 2018
    #Dis_FYK_dead[i,2,k]<- Dis[i,2,k]*Cconv_trans[i,2,] 	# all reported disgards allocated to FYK, reported Dis from year 2018 onwards
    Dis_MIS_dead[i,2,k,]<- MIS[i,2,k]*Cconv_trans[i,2,] * (DisC[i,2,]/(1-DisC[i,2,])) 
    # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
  
  }
}
# DK country 3=DK reported Sea_LLD,  Seal_MIS and Dis_MIS_dead from 2018 onwards
for(k in 1:2){
  for(i in 1:17){  # no reported Seal_MIS until 2018 
    Seal_LLD[i,3,k,]<- (LLD[i,3,k] + TMisr[i,3,k])*Oconv_trans[i,3,] * (SealLL[i,3,]/(1-SealLL[i,3,]))		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
    Seal_MIS[i,3,k,]<- MIS[i,3,k]*Cconv_trans[i,3,] * (SealC[i,3,]/(1-SealC[i,3,]))
    Dis_MIS_dead[i,3,k,]<- MIS[i,3,k]*Cconv_trans[i,3,] * (DisC[i,3,]/(1-DisC[i,3,])) 
    # all fish die; no reported Dis_MIS_dead until 2018
  }
  for(i in 18:Ni){ 
    Seal_LLD[i,3,k,]<- SealLLD[i,3,k]*Oconv_trans[i,3,]
    Seal_MIS[i,3,k,]<- SealMIS[i,3,k]*Cconv_trans[i,3,]
    Dis_MIS_dead[i,3,k,]<- Dis[i,3,k]*Oconv_trans[i,3,] 	# all reported disgards allocated to MIS, from year 2018 onwards
  }
  for(i in 1:Ni){
    Dis_FYK_dead[i,3,k,]<- FYK[i,3,k]*Cconv_trans[i,3,] * (DisC[i,3,]/(1-DisC[i,3,]))*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
    Seal_GND[i,3,k,]<- GND[i,3,k]*Oconv_trans[i,3,] * (SealDN[i,3,]/(1-SealDN[i,3,]))	# Seal damage DNS fishery; stopped in 2007
    Seal_FYK[i,3,k,]<- FYK[i,3,k]*Cconv_trans[i,3,] * (SealC[i,3,]/(1-SealC[i,3,])) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
  }

}


# country 4=PL no reported Sea_LLD and Seal_MIS until 2018
for(k in 1:2){

  for(i in 1:15){
    Seal_LLD[i,4,k,]<- (LLD[i,4,k] + TMisr[i,4,k])*Oconv_trans[i,4,] *PL_SD26[i]* (SealLL[i,4,]/(1-SealLL[i,4,]))		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
    Seal_MIS[i,4,k,]<- MIS[i,4,k]*Cconv_trans[i,4,] * (SealC[i,4,]/(1-SealC[i,4,]))
    # no reported Seal_MIS until 2018 
  }
  
  # 2016 alkaen PL ilmoittaa hylkeenpilaamien määrän, tähän ei enää oteta väärinraportoituja mukaan
  for(i in 16:Ni){# country 4=PL reported Sea_LLD and Seal_MIS from 2018 onwards
    Seal_LLD[i,4,k,]<- SealLLD[i,4,k]*Oconv_trans[i,4,]
    Seal_MIS[i,4,k,]<- SealMIS[i,4,k]*Cconv_trans[i,4,]
    
  }
    for(i in 1:Ni){
      Seal_FYK[i,4,k,]<- FYK[i,4,k]*Cconv_trans[i,4,] * (SealC[i,4,]/(1-SealC[i,4,])) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
      Seal_GND[i,4,k,]<- GND[i,4,k]*Oconv_trans[i,4,] * (SealDN[i,4,]/(1-SealDN[i,4,]))	# Seal damage DNS fishery; stopped in 2007
      Dis_FYK_dead[i,4,k,]<- FYK[i,4,k]*Cconv_trans[i,4,] * (DisC[i,4,]/(1-DisC[i,4,]))*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting

      # all fish die; no reported Dis_MIS_dead
      Dis_MIS_dead[i,4,k,]<- MIS[i,4,k]*Cconv_trans[i,4,] * (DisC[i,4,]/(1-DisC[i,4,])) 

  }
}

# 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported seal damages or discards
for(j in 5:9){         
  for(k in 1:2){
   for(i in 1:Ni){
    # Seal_LLD[i,j,k,]<- (LLD[i,j,k] + TMisr[i,j,k])*Oconv_trans[i,j,] * (SealLL[i,j,]/(1-SealLL[i,j,]))		# Seal damages LLD+Misreporting
      Seal_LLD[i,j,k,]<- LLD[i,j,k]*Oconv_trans[i,j,] * (SealLL[i,j,]/(1-SealLL[i,j,]))		# Seal damages LLD+Misreporting
      Seal_GND[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,] * (SealDN[i,j,]/(1-SealDN[i,j,]))	# Seal damage DNS fishery; stopped in 2007
      Seal_FYK[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * (SealC[i,j,]/(1-SealC[i,j,])) 	# catches are corrected with relevant unreporting
      Seal_MIS[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * (SealC[i,j,]/(1-SealC[i,j,]))
      # Seal damage coastal fishery; mainly TN but all coastal caches included
     
      Dis_FYK_dead[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * (DisC[i,j,]/(1-DisC[i,j,]))*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
      Dis_MIS_dead[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * (DisC[i,j,]/(1-DisC[i,j,])) 
      # all fish die; no reported Dis_MIS_dead
}}}


# All countries! 
for(i in 1:Ni){ 
  for (j in 1:9){       
    for(k in 1:2){
      Tseal[i,j,k,]<- Seal_LLD[i,j,k,] + Seal_GND[i,j,k,] + Seal_FYK[i,j,k,] + Seal_MIS[i,j,k,] 	#Total seal damages by year
      Tdis[i,j,k,]<-  Dis_LLD_dead[i,j,k,] + Dis_GND_dead[i,j,k,] + Dis_FYK_dead[i,j,k,] + Dis_MIS_dead[i,j,k,]   	#Total discards by year 		
}}}

# All countries! 
for(i in 1:Ni){ 
  for(j in 1:9){       
    for(k in 1:2){
      
      # Huomaa että tässä Oconv:n muunnoksessa ei ole mukana 1-
      # tämä siksi että kyse on pelkästä raportoimatta jääneestä
      Ounrep[i,j,k,]<- (GND[i,j,k]+LLD[i,j,k])* (Oconv[i,j,]/(1-Oconv[i,j,]))	
      # unreported catch in off-shore fisheries
      Cunrep[i,j,k,]<- (FYK[i,j,k]+MIS[i,j,k]) * (Cconv[i,j,]/(1-Cconv[i,j,]))	 # coast
      Runrep[i,j,k,]<- River[i,j,k] * (Rconv[i,j,] /(1-Rconv[i,j,]))	 # river
      Sunrep[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,] # total unreporting in sea
      
      #Tunrep_F2[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,] + TMisr[i,j,k] # unreporting in river excluded from unreporting in F2.2.3
      #Tunrep_F4[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
      #Tunrep_T2[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,] + Runrep[i,j,k,]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
      Tunrep_misrep_OC[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,] + TMisr[i,j,k] # unreporting in river excluded from unreporting in F2.2.3
      Tunrep_OC[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
      Tunrep_OCR[i,j,k,]<- Ounrep[i,j,k,] + Cunrep[i,j,k,] + Runrep[i,j,k,]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
      TRiver[i,j,k]<- River[i,j,k]
      TRecrSea[i,j,k]<- Recr[i,j,k] 

      # Alive discards; not added to the total catch
      Dis_LLD_alive[i,j,k,]<- (LLD[i,j,k]+TMisr[i,j,k])*Oconv_trans[i,j,]*(DisLL[i,j,]/(1-DisLL[i,j,]))*(1-MDisLL)	# Alive discards of LLD+Misreporting
      Dis_GND_alive[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,]*(DisDN[i,j,]/(1-DisDN[i,j,]))*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
      Dis_FYK_alive[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,]*(DisC[i,j,]/(1-DisC[i,j,]))*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
      
      Tdis_alive[i,j,k,]<-  Dis_LLD_alive[i,j,k,] + Dis_GND_alive[i,j,k,] + Dis_FYK_alive[i,j,k,]   	#Total alive discards by year, MU and country	
      
      Tcatch[i,j,k,]<- GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k] + 
                          Recr[i,j,k] + River[i,j,k]  + Tunrep_OCR[i,j,k,] + Tdis[i,j,k,]+TMisr[i,j,k]
      TcatchCom[i,j,k]<- (GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k])					
      # Total catch by year, country and management unit
      
    }}}


# Summation
################################################################################

# A: Total Baltic sea
# B: SD22-31 and SD32 separated
# d: data, no uncertainty
# s: contains at least one stochastic component, Nsim number of simulations

Bd_TotMisr_sea<-Bd_TotRepCom_sea<-Bd_TotRecr_sea<-array(0, dim=c(Ni, 2))

Bs_TotUnrep_OC<-Bs_TotUnrep_R<-Bs_TotDisSeal<-Bs_TotUnrep<-Bs_TotCatch<-
Bs_TotDis_dead<-Bs_TotSeal<-Bs_TotDis_dead_GND<-Bs_TotDis_dead_LLD<-
Bs_TotDis_dead_FYK<-Bs_TotDis_dead_MIS<-
Bs_TotSeal_GND<-Bs_TotSeal_LLD<-
Bs_TotSeal_FYK<-Bs_TotSeal_MIS<-
Bs_TotDis_alive<-Bs_TotDis_GND_alive<-
Bs_TotDis_LLD_alive<-Bs_TotDis_FYK_alive<-
Bs_TotRiver<-Bs_TotCatchCom_sea<-Bs_TotCatch_sea<-Bs_TotUnrepDis_sea<-
  array(0, dim=c(Ni, 2, Nsim))

for(i in 1:Ni){
  for(k in 1:2){		# management unit 2, SD32 only FI, EE and RU operate here
    Bd_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
    Bd_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
    Bd_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
  
  for(s in 1:Nsim){
  # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Bs_TotUnrepDis_sea[i,k,s]<-sum(Tunrep_misrep_OC[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s]) + sum(Tdis[i,1:9,k,s]) # Estimate of the total unrep, misrep and discards for F2.2.3
  Bs_TotCatch_sea[i,k,s]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+Bs_TotUnrepDis_sea[i,k,s]  # for F2.2.3
  Bs_TotCatchCom_sea[i,k,s]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s]) + sum(Tunrep_misrep_OC[i,1:9,k,s]) # comm reported, discarded dead + seal damaged,  unreported, misreported
  Bs_TotRiver[i,k,s]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k,s])  # for  F4.3.2.9 the river catch include also unreporting in river
  
  Bs_TotUnrep[i,k,s]<-sum(Tunrep_OCR[i,1:9,k,s])
  Bs_TotUnrep_OC[i,k,s]<-sum(Tunrep_OC[i,1:9,k,s])	# F4.3.2.9 misreporting excluded here
  Bs_TotUnrep_R[i,k,s]<-sum(Runrep[i,1:9,k,s])	# T2.3.4
  Bs_TotDisSeal[i,k,s]<-sum(Tdis[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s])	# F4.3.2.9
  Bs_TotCatch[i,k,s]<-sum(Tcatch[i,1:9,k,s]) # All catches including unreporting and misreporting by MU
  
  Bs_TotDis_dead[i,k,s]<-sum(Tdis[i,1:9,k,s])	# Total dead discards by MU 
  Bs_TotSeal[i,k,s]<-sum(Tseal[i,1:9,k,s]) # Total seal damages by MU
  Bs_TotDis_dead_GND[i,k,s]<-sum(Dis_GND_dead[i,1:9,k,s]) # dead discards by component and MU
  Bs_TotDis_dead_LLD[i,k,s]<-sum(Dis_LLD_dead[i,1:9,k,s])
  Bs_TotDis_dead_FYK[i,k,s]<-sum(Dis_FYK_dead[i,1:9,k,s])
  Bs_TotDis_dead_MIS[i,k,s]<-sum(Dis_MIS_dead[i,1:9,k,s])
  Bs_TotSeal_GND[i,k,s]<-sum(Seal_GND[i,1:9,k,s]) # dead seal damages by component and MU
  Bs_TotSeal_LLD[i,k,s]<-sum(Seal_LLD[i,1:9,k,s]) 
  Bs_TotSeal_FYK[i,k,s]<-sum(Seal_FYK[i,1:9,k,s])
  Bs_TotSeal_MIS[i,k,s]<-sum(Seal_MIS[i,1:9,k,s])
  
  Bs_TotDis_alive[i,k,s]<-sum(Tdis_alive[i,1:9,k,s])	# Total alive discards by MU
  Bs_TotDis_GND_alive[i,k,s]<-sum(Dis_GND_alive[i,1:9,k,s]) # alive discards by component and MU
  Bs_TotDis_LLD_alive[i,k,s]<-sum(Dis_LLD_alive[i,1:9,k,s])
  Bs_TotDis_FYK_alive[i,k,s]<-sum(Dis_FYK_alive[i,1:9,k,s])
  
}
}}

As_TotDis<-As_TotUnrep<-As_TotCatch<-As_TotSeal<-array(0, dim=c(Ni, Nsim))
for(i in 1:Ni){  
  for(s in 1:Nsim){  
    #  Whole Baltic sea
  As_TotDis[i,s]<-sum(Tdis[i,1:9,1:2,s])+sum(Tseal[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2
  As_TotUnrep[i,s]<-sum(Tunrep_OCR[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2 
  As_TotCatch[i,s]<-sum(Tcatch[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2
  As_TotSeal[i,s]<-sum(Tseal[i,1:9,1:2,s]) 
}}

dim(Bs_TotSeal)

# Save medians per area (SD22-31 and SD32) for further usage
#dis_seal<-unrep_river<-tot_catch<-unrep_sea<-unrep_tot<-dis<-array(NA, dim=c(NumYears, 2, Nsim))
med_TotCatchSea<-med_dead_dis_seal<-med_Runrep<-rep_river<-med_Tcatch<-med_unrep<-med_dis<-
med_alive_dis<-med_unrep_sea<-med_seal<-misr<-med_river<-med_recr<-
med_dead_dis<-array(NA, dim=c(NumYears, 2))

for(i in 1:NumYears){
  for(k in 1:2){
    #for(s in 1:Nsim){
     # dis[i,k,s]<-sum(Tdis[i, 1:9,k,s], na.rm=T) # This is Bs_TotDis_dead
      #unrep_tot[i,k,s]<-sum(Tunrep_OCR[i, 1:9,k,s], na.rm=T) # This is Bs_TotUnrep
      #unrep_sea[i,k,s]<-sum(Sunrep[i, 1:9,k,s], na.rm=T) #Unrep O+C # This is Bs_TotUnrep_OC
      #tot_catch[i,k,s]<-sum(Tcatch[i,1:9,k,s], na.rm=T) # This is Bs_TotCatch
      #unrep_river[i,k,s]<-sum(Runrep[i, 1:9,k,s], na.rm=T) # This is Bs_TotUnrep_R
      
      # dis_seal[i,k,s]<-sum(Tdis[i, 1:9,k,s], na.rm=T)+sum(Tseal[i, 1:9,k,s], na.rm=T) # This is Bs_TotDisSeal
      
    #  }
    med_dis[i,k]<-median(Bs_TotDis_dead[i,k,]) # Total discared
    med_unrep[i,k]<-median(Bs_TotUnrep[i,k,]) #Total unreported
    med_unrep_sea[i,k]<-median(Bs_TotUnrep_OC[i,k,]) #Total unreported
    med_Tcatch[i,k]<-median(Bs_TotCatch[i,k,])
    med_Runrep[i,k]<-median(Bs_TotUnrep_R[i,k,])
    med_dead_dis_seal[i,k]<-median(Bs_TotDisSeal[i,k,])
    med_TotCatchSea[i,k]<-median(Bs_TotCatch_sea[i,k,])
    med_river[i,k]<-median(Bs_TotRiver[i,k,]) 
    med_recr[i,k]<-median(Bs_TotRecr_sea[i,k,]) 
    med_dead_dis[i,k]<-median(Bs_TotDis_dead[i,k,])
    med_alive_dis[i,k]<-median(Bs_TotDis_alive[i,k,])
    med_seal[i,k]<-median(Bs_TotSeal[i,k,])
    
    misr[i,k]<-sum(TMisr[i,,k]) # Data
    rep_river[i,k]<-sum(River[i,,k]) # Data
}}




