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

# Countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU	







datalist<-list(
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
  
)



parnames<-c("A_TotUnrep_BS", # for T2.2.1 and T2.2.2
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
            "Tunrep_T2", #=misrep+runrep+sunrep
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
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=10000)


