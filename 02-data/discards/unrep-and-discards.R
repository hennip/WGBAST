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
#source("02-data/discards/model-unrep-discards.R") # Original structure

###############
# KORJATTAVAA:
# - Jokisaaliin jakaminen reported ja unreported -osiin
# - Dead discards, nimeäminen selvemmin. Nyt Dis_LLD tarkoittaa kuolleita, Dis_LLD_alive eläviä
#  eli Dis_LLD voisi muuttaa Dis_LLD_dead





# Decide if you wish to run the model for numb or weight

#numb<-T # catch in number
#numb<-F # catch in weight



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
  
  #"epsilon", 
  "MDisLL", "MDisDN", "MDisC", 
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
saveRDS(chains, file="02-data/discards/chains_unrep_discards_cleaned.rds")
#chains<-readRDS("02-data/discards/chains_unrep_discards_cleaned.rds")

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


#dim(Oconv)

Tdis<-Tseal<-Seal_MIS<-Seal_FYK<-Seal_LLD<-Seal_GND<-array(0, dim=c(Ni, Ncry, 2, Nsim))
TMisr<-array(0, dim=c(Ni, Ncry, 2))

Ounrep<-Cunrep<-Runrep<-Sunrep<-array(0, dim=c(Ni, Ncry,2, Nsim))
Tcatch<-Tunrep_F2<-Tunrep_F4<-Tunrep_T2<-array(0, dim=c(Ni, Ncry,2, Nsim))
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
PL_factor<-c()
for(i in 1:Ni){ 
  for(k in 1:2){
      TMisr[i,4,1]<-as.vector(unname(PL_misrep_N))[[1]][i]#*epsilon
  }
  PL_factor[i]<-as.vector(unname(PL_sealfac))[[1]][i]#*epsilon
}
#PLfactor=as.vector(unname(PL_sealfac))[[1]],



# Kaikki maat
# Dead discards
for(i in 1:Ni){ 
  for(j in 1:Ncry){ 
    for(k in 1:2){
    # dead discards of LLD+Misreporting
    Dis_LLD_dead[i,j,k,]<- (LLD[i,j,k] + TMisr[i,j,k])*Oconv_trans[i,j,]*
                        DisLL[i,j,]/(1-DisLL[i,j,])*MDisLL
    # dead discards of DNS fishery; stopped in 2007
    Dis_GND_dead[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,] * DisDN[i,j,]/(1-DisDN[i,j,])*MDisDN	
}}}


# FI  # country FI seal damages and other discards are given
for(k in 1:2){
  for(i in 1:Ni){ 
    # Mitä tämä tekee? Lisää raportoimattomuuden?
    # Mistä nimi tulee, miksi ei ole esim. unrep?
    Seal_GND[i,1,k,]<- SealGND[i,1,k]*Oconv_trans[i,1,]
    Seal_LLD[i,1,k,]<- SealLLD[i,1,k]*Oconv_trans[i,1,]
    Seal_FYK[i,1,k,]<- SealFYK[i,1,k]*Cconv_trans[i,1,]
    Seal_MIS[i,1,k,]<- SealMIS[i,1,k]*Cconv_trans[i,1,] 
    
    # Total seal damages by year
    Tseal[i,1,k,]<- Seal_GND[i,1,k,] + Seal_LLD[i,1,k,] + Seal_FYK[i,1,k,] + Seal_MIS[i,1,k,]       
    
    Tdis[i,1,k,]<- Dis_LLD_dead[i,1,k,] + Dis_GND_dead[i,1,k,] + Dis_FYK_dead[i,1,k,] + Dis_MIS_dead[i,1,k,]	#Total discards by year
    Dis_FYK_dead[i,1,k,]<- Dis[i,1,k]*Oconv_trans[i,1,]	# all reported disgards allocated to FYK
    Dis_MIS_dead[i,1,k,]<- MIS[i,1,k]*Cconv_trans[i,1,] * DisC[i,1,]/(1-DisC[i,1,]) 
    # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
  }}

# SE# country 2=SE, seal damages in LLD and FYK are given in data
for(k in 1:2){
    for(i in 1:17){ 
      Seal_MIS[i,2,k,]<- MIS[i,2,k]*Cconv_trans[i,2,] * SealC[i,2,]/(1-SealC[i,2,])
      # no data from MIS until 2018
    }
    for(i in 18:Ni){ 
      Seal_MIS[i,2,k,]<- SealMIS[i,2,k]*Cconv_trans[i,2,] 
    }  
    # Reported gearspecific seal damages are corrected with fishery specific unreporting
    Seal_GND[i,2,k,]<- GND[i,2,k]*Oconv_trans[i,2,] * SealDN[i,2,]/(1-SealDN[i,2,])	# Seal damage in GND; stopped in 2007
    Seal_LLD[i,2,k,]<- SealLLD[i,2,k]*Oconv_trans[i,2,]
    Seal_FYK[i,2,k,]<- SealFYK[i,2,k]*Cconv_trans[i,2,]
    Seal_MIS[i,2,k,]<- SealMIS[i,2,k]*Cconv_trans[i,2,]     # seal damages by year, Seal MIS reported from 2018 onwards  
      
    Tseal[i,2,k,]<- Seal_GND[i,2,k,] + Seal_LLD[i,2,k,] + Seal_FYK[i,2,k,] + Seal_MIS[i,2,k,]       # Total seal damages by year
    
    Tdis[i,2,k,]<-  Dis_LLD_dead[i,2,k,] + Dis_GND_dead[i,2,k,] + Dis_FYK_dead[i,2,k,] + Dis_MIS_dead[i,2,k,]	#Total discards by year
    Dis_FYK_dead[i,2,k,]<- FYK[i,2,k]*Cconv_trans[i,2,] * DisC[i,2,]/(1-DisC[i,2,])*MDisC	# no reported Dis until 2018
    #Dis_FYK_dead[i,2,k]<- Dis[i,2,k]*Cconv_trans[i,2,] 	# all reported disgards allocated to FYK, reported Dis from year 2018 onwards
    Dis_MIS_dead[i,2,k,]<- MIS[i,2,k]*Cconv_trans[i,2,] * DisC[i,2,]/(1-DisC[i,2,]) 
    # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
    # TMisr[i,j,k]<-0.0001*epsilon
  }

# DK country 3=DK reported Sea_LLD,  Seal_MIS and Dis_MIS_dead from 2018 onwards
for(k in 1:2){
  for(i in 1:17){  # no reported Seal_MIS until 2018 
    Seal_LLD[i,3,k,]<- (LLD[i,3,k] + TMisr[i,3,k])*Oconv_trans[i,3,] * SealLL[i,3,]/(1-SealLL[i,3,])		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
    Seal_MIS[i,3,k,]<- MIS[i,3,k]*Cconv_trans[i,3,] * SealC[i,3,]/(1-SealC[i,3,])
    Dis_MIS_dead[i,3,k,]<- MIS[i,3,k]*Cconv_trans[i,3,] * DisC[i,3,]/(1-DisC[i,3,]) 
    # all fish die; no reported Dis_MIS_dead until 2018
    #Dis_MIS_dead[i,3,k]<- Dis[i,3,k]*Oconv_trans[i,3,] 	# all reported disgards allocated to MIS, from year 2018 onwards
  }
  for(i in 18:Ni){ 
    Seal_LLD[i,3,k,]<- SealLLD[i,3,k]*Oconv_trans[i,3,]
    Seal_MIS[i,3,k,]<- SealMIS[i,3,k]*Cconv_trans[i,3,]
    Dis_MIS_dead[i,3,k,]<- Dis[i,3,k]*Oconv_trans[i,3,] 	# all reported disgards allocated to MIS, from year 2018 onwards
  }
  for(i in 1:Ni){
    Dis_FYK_dead[i,3,k,]<- FYK[i,3,k]*Cconv_trans[i,3,] * DisC[i,3,]/(1-DisC[i,3,])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
    Seal_GND[i,3,k,]<- GND[i,3,k]*Oconv_trans[i,3,] * SealDN[i,3,]/(1-SealDN[i,3,])	# Seal damage DNS fishery; stopped in 2007
    Seal_FYK[i,3,k,]<- FYK[i,3,k]*Cconv_trans[i,3,] * SealC[i,3,]/(1-SealC[i,3,]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
    Tseal[i,3,k,]<- Seal_LLD[i,3,k,] + Seal_GND[i,3,k,] + Seal_FYK[i,3,k,] + Seal_MIS[i,3,k,] 	#Total seal damages by year
    Tdis[i,3,k,]<-  Dis_LLD_dead[i,3,k,] + Dis_GND_dead[i,3,k,] + Dis_FYK_dead[i,3,k,] + Dis_MIS_dead[i,3,k,]   	#Total discards by year 
  }

}


# country 4=PL no reported Sea_LLD and Seal_MIS until 2018
for(k in 1:2){

  for(i in 1:17){
    #Seal_LLD[i,4,k,]<- (LLD[i,4,k] + PLMisr[i])*Oconv_trans[i,4,] *PLfactor[i]* SealLL[i,4,]/(1-SealLL[i,4,])		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
    Seal_LLD[i,4,k,]<- (LLD[i,4,k] + TMisr[i,4,k])*Oconv_trans[i,4,] *PL_factor[i]* SealLL[i,4,]/(1-SealLL[i,4,])		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
    Seal_MIS[i,4,k,]<- MIS[i,4,k]*Cconv_trans[i,4,] * SealC[i,4,]/(1-SealC[i,4,])
    # no reported Seal_MIS until 2018 
  }
  for(i in 18:Ni){# country 4=PL reported Sea_LLD and Seal_MIS from 2018 onwards
    Seal_LLD[i,4,k,]<- SealLLD[i,4,k]*Oconv_trans[i,4,]
    Seal_MIS[i,4,k,]<- SealMIS[i,4,k]*Cconv_trans[i,4,]
    
  }
    for(i in 1:Ni){
      Seal_FYK[i,4,k,]<- FYK[i,4,k]*Cconv_trans[i,4,] * SealC[i,4,]/(1-SealC[i,4,]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
      Seal_GND[i,4,k,]<- GND[i,4,k]*Oconv_trans[i,4,] * SealDN[i,4,]/(1-SealDN[i,4,])	# Seal damage DNS fishery; stopped in 2007
      Dis_FYK_dead[i,4,k,]<- FYK[i,4,k]*Cconv_trans[i,4,] * DisC[i,4,]/(1-DisC[i,4,])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
      Tseal[i,4,k,]<- Seal_LLD[i,4,k,] + Seal_GND[i,4,k,] + Seal_FYK[i,4,k,] + Seal_MIS[i,4,k,] 	#Total seal damages by year
      
      # all fish die; no reported Dis_MIS_dead
      Dis_MIS_dead[i,4,k,]<- MIS[i,4,k]*Cconv_trans[i,4,] * DisC[i,4,]/(1-DisC[i,4,]) 
      Tdis[i,4,k,]<-  Dis_LLD_dead[i,4,k,] + Dis_GND_dead[i,4,k,] + Dis_FYK_dead[i,4,k,] + Dis_MIS_dead[i,4,k,]   	#Total discards by year 
      
  }
}

# 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported seal damages or discards
for(k in 1:2){
  for(i in 1:Ni){
    for(j in 5:9){         
      Seal_LLD[i,j,k,]<- (LLD[i,j,k] + TMisr[i,j,k])*Oconv_trans[i,j,] * SealLL[i,j,]/(1-SealLL[i,j,])		# Seal damages LLD+Misreporting
      Seal_GND[i,j,k,]<- GND[i,j,k]*Oconv_trans[i,j,] * SealDN[i,j,]/(1-SealDN[i,j,])	# Seal damage DNS fishery; stopped in 2007
      Seal_FYK[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * SealC[i,j,]/(1-SealC[i,j,]) 	# catches are corrected with relevant unreporting
      Seal_MIS[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * SealC[i,j,]/(1-SealC[i,j,])
      # Seal damage coastal fishery; mainly TN but all coastal caches included
     
      Tseal[i,j,k,]<- Seal_LLD[i,j,k,] + Seal_GND[i,j,k,] + Seal_FYK[i,j,k,] + Seal_MIS[i,j,k,] 	#Total seal damages by year
      
      Tdis[i,j,k,]<-  Dis_LLD_dead[i,j,k,] + Dis_GND_dead[i,j,k,] + Dis_FYK_dead[i,j,k,] + Dis_MIS_dead[i,j,k,]   	#Total discards by year 		
      Dis_FYK_dead[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
      Dis_MIS_dead[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,]) 
      # all fish die; no reported Dis_MIS_dead
}}}


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
  
      Tdis[i,j,k,]<-  Dis_LLD_dead[i,j,k,] + Dis_GND_dead[i,j,k,] + Dis_FYK_dead[i,j,k,] + Dis_MIS_dead[i,j,k,]   	#Total discards by year 		
      Dis_FYK_dead[i,j,k,]<- FYK[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
      Dis_MIS_dead[i,j,k,]<- MIS[i,j,k]*Cconv_trans[i,j,] * DisC[i,j,]/(1-DisC[i,j,]) 
      # all fish die; no reported Dis_MIS_dead
      
    }}}


#  countries FI, EE and RU
# NOTE! Use cry-parameter here to pick up countries 1, 8 and 9
for(i in 1:Ni){ 
  for (j in 1:3){         #   countries EE and RU has no estimates for seal damages and other discards
    for(k in 1:2){
      
      # Huomaa että tässä Oconv:n muunnoksessa ei ole mukana 1-
      # tämä siksi että kyse on pelkästä raportoimatta jääneestä
      Ounrep[i,cry[j],k,]<- (GND[i,cry[j],k]+LLD[i,cry[j],k])* Oconv[i,cry[j],]/(1-Oconv[i,cry[j],])	
      # unreported catch in off-shore fisheries
      Cunrep[i,cry[j],k,]<- (FYK[i,cry[j],k]+MIS[i,cry[j],k]) * Cconv[i,cry[j],]/(1-Cconv[i,cry[j],])	 # coast
      Runrep[i,cry[j],k,]<- River[i,cry[j],k] * Rconv[i,cry[j],] /(1-Rconv[i,cry[j],])	 # river
      Sunrep[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] # total unreporting in sea
  
      Tunrep_F2[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] + TMisr[i,cry[j],k] # unreporting in river excluded from unreporting in F2.2.3
      Tunrep_F4[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
      Tunrep_T2[i,cry[j],k,]<- Ounrep[i,cry[j],k,] + Cunrep[i,cry[j],k,] + Runrep[i,cry[j],k,]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
      TRiver[i,cry[j],k]<- River[i,cry[j],k]
      TRecrSea[i,cry[j],k]<- Recr[i,cry[j],k]
 
      #    TMisr[i,cry[j],k]<- 0.0001*epsilon
      # Total unreported by year, country and management unit
      # input values for catches ( from file xxxx)
      # GND[,,] driftnet	LLD[,,] longline	FYK[,,] trapnet	MIS[,,] other gears
      # Recr[,,] recreational  River[,,] river cathes
      # Misr[,,] misreporting Poland only
      # Seal_GND[,,] Seal_LLD[,,] Seal_FYK[,,] Seal_MIS[,,] gear specific reported seal damages
      # Dis[,,] reported discards
      
      # Dead discards
      Dis_LLD_dead[i,cry[j],k,]<- LLD[i,cry[j],k]*Oconv_trans[i,j,]* DisLL[i,cry[j],]/(1-DisLL[i,cry[j],])*MDisLL	# dead discards of LLD+Misreporting
      Dis_GND_dead[i,cry[j],k,]<- GND[i,cry[j],k]*Oconv_trans[i,j,] * DisDN[i,cry[j],]/(1-DisDN[i,cry[j],])*MDisDN	# dead discards of DNS fishery; stopped in 2007
      
      # Alive discards; not added to the total catch
      Dis_LLD_alive[i,cry[j],k,]<- LLD[i,cry[j],k]*Oconv_trans[i,j,]*DisLL[i,cry[j],]/(1-DisLL[i,cry[j],])*(1-MDisLL)	# Alive discards of LLD+Misreporting
      Dis_GND_alive[i,cry[j],k,]<- GND[i,cry[j],k]*Oconv_trans[i,j,]*DisDN[i,cry[j],]/(1-DisDN[i,cry[j],])*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
      Dis_FYK_alive[i,cry[j],k,]<- FYK[i,cry[j],k]*Cconv_trans[i,j,]*DisC[i,cry[j],]/(1-DisC[i,cry[j],])*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
      Tdis_alive[i,cry[j],k,]<-  Dis_LLD_alive[i,cry[j],k,] + Dis_GND_alive[i,cry[j],k,] + Dis_FYK_alive[i,cry[j],k,]   	#Total alive discards by year, MU and country	
      
      Tcatch[i,cry[j],k,]<- GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k] + 
                          Recr[i,cry[j],k] + River[i,cry[j],k]  + Tunrep_T2[i,cry[j],k,] + Tdis[i,cry[j],k,]
      TcatchCom[i,cry[j],k]<- (GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k])					
      # Total catch by year, country and management unit
      
    }}}

B_TotMisr_sea<-B_TotRepCom_sea<-B_TotRecr_sea<-  array(0, dim=c(Ni, 2))
B_TotUnrep_sea<-B_TotUnrep_river<-B_TotDisSeal_MU<-B_TotUnrep_MU<-B_TotCatch_MU<-
B_TotDis_dead<-B_TotSeal<-B_TotDis_dead_GND<-B_TotDis_dead_LLD<-
B_TotDis_dead_FYK<-B_TotDis_dead_MIS<-B_TotSeal_GND<-B_TotSeal_LLD<-
B_TotSeal_FYK<-B_TotSeal_MIS<-B_TotDis_alive<-B_TotDis_GND_alive<-
B_TotDis_LLD_alive<-B_TotDis_FYK_alive<-
B_TotRiver<-B_TotCatchCom_sea<-B_TotCatch_sea<-B_TotUnrepDis_sea<-
  array(0, dim=c(Ni, 2, Nsim))

for(i in 1:Ni){
  for(k in 2:2){		# management unit 2, SD32 only FI, EE and RU operate here
    B_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
    B_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
    B_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
    for(s in 1:Nsim){
  # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  B_TotUnrepDis_sea[i,k,s]<-sum(Tunrep_F2[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s]) + sum(Tdis[i,1:9,k,s]) # Estimate of the total unrep, misrep and disdards for F2.2.3
  B_TotCatch_sea[i,k,s]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+B_TotUnrepDis_sea[i,k,s]  # for F2.2.3
  B_TotCatchCom_sea[i,k,s]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s]) + sum(Tunrep_F2[i,1:9,k,s]) # comm reported, discarded dead + seal damaged,  unreported, misreported
  B_TotRiver[i,k,s]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k,s])  # for  F4.3.2.9 the river catch include also unreporting in river
  
  B_TotUnrep_sea[i,k,s]<-sum(Tunrep_F4[i,1:9,k,s])	# F4.3.2.9 misreporting excluded here
  B_TotUnrep_river[i,k,s]<-sum(Runrep[i,1:9,k,s])	# T2.3.4
  B_TotDisSeal_MU[i,k,s]<-sum(Tdis[i,1:9,k,s]) + sum(Tseal[i,1:9,k,s])	# F4.3.2.9
  B_TotUnrep_MU[i,k,s]<-sum(Tunrep_T2[i,1:9,k,s])
  B_TotCatch_MU[i,k,s]<-sum(Tcatch[i,1:9,k,s]) # All catches including unreporting and misreporting by MU
  
  B_TotDis_dead[i,k,s]<-sum(Tdis[i,1:9,k,s])	# Total dead discards by MU 
  B_TotSeal[i,k,s]<-sum(Tseal[i,1:9,k,s]) # Total seal damages by MU
  B_TotDis_dead_GND[i,k,s]<-sum(Dis_GND_dead[i,1:9,k,s]) # dead discards by component and MU
  B_TotDis_dead_LLD[i,k,s]<-sum(Dis_LLD_dead[i,1:9,k,s])
  B_TotDis_dead_FYK[i,k,s]<-sum(Dis_FYK_dead[i,1:9,k,s])
  B_TotDis_dead_MIS[i,k,s]<-sum(Dis_MIS_dead[i,1:9,k,s])
  B_TotSeal_GND[i,k,s]<-sum(Seal_GND[i,1:9,k,s]) # dead seal damages by component and MU
  B_TotSeal_LLD[i,k,s]<-sum(Seal_LLD[i,1:9,k,s]) 
  B_TotSeal_FYK[i,k,s]<-sum(Seal_FYK[i,1:9,k,s])
  B_TotSeal_MIS[i,k,s]<-sum(Seal_MIS[i,1:9,k,s])
  
  B_TotDis_alive[i,k,s]<-sum(Tdis_alive[i,1:9,k,s])	# Total alive discards by MU
  B_TotDis_GND_alive[i,k,s]<-sum(Dis_GND_alive[i,1:9,k,s]) # alive discards by component and MU
  B_TotDis_LLD_alive[i,k,s]<-sum(Dis_LLD_alive[i,1:9,k,s])
  B_TotDis_FYK_alive[i,k,s]<-sum(Dis_FYK_alive[i,1:9,k,s])
  
}
}}

A_TotDis_BS<-A_TotUnrep_BS<-A_TotCatch_BS<-A_TotSeal_BS<-array(0, dim=c(Ni, Nsim))
for(i in 1:Ni){  
  for(s in 1:Nsim){  
    #  Whole Baltic sea
  A_TotDis_BS[i,s]<-sum(Tdis[i,1:9,1:2,s])+sum(Tseal[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2
  A_TotUnrep_BS[i,s]<-sum(Tunrep_T2[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2 
  A_TotCatch_BS[i,s]<-sum(Tcatch[i,1:9,1:2,s]) # for T2.2.1 and T2.2.2
  A_TotSeal_BS[i,s]<-sum(Tseal[i,1:9,1:2,s]) 
}}
  
  
  
  
  
  