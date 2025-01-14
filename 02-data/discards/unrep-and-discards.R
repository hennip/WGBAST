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


# Countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU	



# Catch&Effort DB

min_year<-2000
max_year<-2023
years<-min_year:max_year
NumYears<-length(years)

pathIn<-pathDataCatchEffort

df_all<-read_xlsx(str_c(pathIn, 
                        "dat/orig/WGBAST_2024_Catch_final.xlsx"), # Update!
                  range="A1:Q17736", # Update!
                  sheet="Catch data", col_names = T, guess_max = 8000, na=c("",".", "NaN", "NA")) |> 
  # filter(YEAR>2005)%>% # Include results only 2009 onwards, catch DB has only updates from those years 
  # mutate(NUMB=parse_double(NUMB))%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, 
         EFFORT, everything()) |> 
  mutate(TP_TYPE=ifelse(TP_TYPE=="QRT", "QTR", TP_TYPE)) |> 
  filter(SPECIES=="SAL", YEAR>2000)|> 
  filter(SPECIES!="ALV")
# NOTE: Alive discards are not taken into account. A way to include them should
# be figured out at some point.


# Countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE and 9=RU																			  						


df<-df_all |> 
  mutate(country_nr=ifelse(COUNTRY=="FI",1, 
                           ifelse(COUNTRY=="SE",2,
                           ifelse(COUNTRY=="DK", 3,
                           ifelse(COUNTRY=="PL", 4,
                           ifelse(COUNTRY=="LV", 5,
                           ifelse(COUNTRY=="LT", 6,
                           ifelse(COUNTRY=="DE", 7,
                           ifelse(COUNTRY=="EE", 8,
                           ifelse(COUNTRY=="RU", 9,COUNTRY))))))))))



yrs<-tibble(YEAR=c(2001:2023))
yrs1<-tibble(YEAR=c(2001:2023), sub_div2="22-31")
yrs2<-tibble(YEAR=c(2001:2023), sub_div2="32")
yrs<-full_join(yrs1, yrs2)

################################################################################
# Landed catches in number and in weight

# Dataset for 
df2 <- df |> filter(F_TYPE!="SEAL", F_TYPE!="DISC")


# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
numb<-func_country_catches(df2,1)

River_N<-numb[[1]]
Recr_N<-numb[[2]]
GND_N<-numb[[3]]
LLD_N<-numb[[4]]
FYK_N<-numb[[5]]
MIS_N<-numb[[6]]

weight<-func_country_catches(df2,2)
River_W<-weight[[1]]
Recr_W<-weight[[2]]
GND_W<-weight[[3]]
LLD_W<-weight[[4]]
FYK_W<-weight[[5]]
MIS_W<-weight[[6]]

################################################################################
# Seal damaged in number and in weight per country

df3 <- df |> filter(F_TYPE=="SEAL")

# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
numb<-func_country_sealdam(df3,1)
SealGND_N<-numb[[1]]
SealLLD_N<-numb[[2]]
SealFYK_N<-numb[[3]]
SealMIS_N<-numb[[4]]

weight<-func_country_sealdam(df3,2)
SealGND_W<-weight[[1]]
SealLLD_W<-weight[[2]]
SealFYK_W<-weight[[3]]
SealMIS_W<-weight[[4]]


################################################################################
# Other discards in number and in weight per country

df4 <- df |> filter(F_TYPE=="DISC")

# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
Dis_N<-func_country_discards(df4,1)

Dis_W<-func_country_discards(df4,2)


################################################################################
################################################################################
# Polish seal damages

# Correction factor for Polish seal damages to make the given estimate to apply only in SD26 catch	
# in other words in years 2013-2015 given estimate applies only for 55% of the Total LLD catch	
# and in years 2016-2017 applies for 65% of the total LLD catch
# Eli näillä luvuilla kerrotaan Puolan ilmoittamaa saalista
# from 2018 onwards seal damage data are given in numbers of fish (see SealLLD variable)
# ONKO VIRHE ETTÄ PLfactor-muuttujassa on annettu arvo myös vuosille 2018->???
PL_sealfac<-read.table("../../WGBAST_shared/submodels/reporting rates/data/PL_Seal_corr_factor.txt", header=T)

# Use unname if col names are problem
# unname(River)

################################################################################
# Polish misreporting

PL_misrep_N<-read.table("../../WGBAST_shared/submodels/reporting rates/data/PL_Misrep_numb_2024.txt", header=T)
PL_misrep_W<-read.table("../../WGBAST_shared/submodels/reporting rates/data/PL_Misrep_weight_2024.txt", header=T)


################################################################################
# Country specific unreporting and discard rates based on expert elicitation


df<-read.table("../../WGBAST_shared/submodels/reporting rates/data/Unrep_discard_rates_2024.txt", header=T)
#df<-as_tibble(df)

# Kuinka suuri osuus saaliista on alamittaisia, perustuu kolmiojakaumaan ja expert elisitointiin
Omu<-df |> select(contains("Omu")) 
Osd<-df |> select(contains("Osd"))
Cmu<-df |> select(contains("Cmu"))
Csd<-df |> select(contains("Csd"))
Rmu<-df |> select(contains("Rmu"))
Rsd<-df |> select(contains("Rsd"))
LLmu<-df |> select(contains("LLmu"))
LLsd<-df |> select(contains("LLsd"))
DNmu<-df |> select(contains("DNmu"))
DNsd<-df |> select(contains("DNsd"))
TNmu<-df |> select(contains("TNmu"))
TNsd<-df |> select(contains("TNsd"))
SLLDmu<-df |> select(contains("SLLDmu"))
SLLDsd<-df |> select(contains("SLLDsd"))
SGNDmu<-df |> select(contains("SGNDmu"))
SGNDsd<-df |> select(contains("SGNDsd"))
STNmu<-df |> select(contains("STNmu"))
STNsd<-df |> select(contains("STNsd"))

Ocv<-Osd/Omu 		#Oconv, unreporting off-shore
TNcv<-TNsd/TNmu		#Dis_FYK, discarded undersized trapnet
DNcv<-DNsd/DNmu		#Dis_GND, discarded undersized driftnet
LLcv<-LLsd/LLmu		#Dis_LLD, discarded undersized longline
Ccv<-Csd/Cmu		#Cconv, unreporting coast
Rcv<-Rsd/Rmu		#Rconv, unreporting river
SLLDcv<-SLLDsd/SLLDmu		#Seal LLD, seal damages longline
SGNDcv<-SGNDsd/SGNDmu		#Seal GND, seal damages driftnet
STNcv<-STNsd/STNmu		#Seal TN, seal damages trapnet

Otau<-1/log(Ocv*Ocv+1)
OM<-log(Omu)-0.5/Otau
Ctau<-1/log(Ccv*Ccv+1)
CM<-log(Cmu)-0.5/Ctau
Rtau<-1/log(Rcv*Rcv+1)
RM<-log(Rmu)-0.5/Rtau
LLtau<-1/log(LLcv*LLcv+1)
LLM<-log(LLmu)-0.5/LLtau
DNtau<-1/log(DNcv*DNcv+1)
DNM<-log(DNmu)-0.5/DNtau
TNtau<-1/log(TNcv*TNcv+1)
TNM<-log(TNmu)-0.5/TNtau
SLLDtau<-1/log(SLLDcv*SLLDcv+1)
SLLDM<-log(SLLDmu)-0.5/SLLDtau
SGNDtau<-1/log(SGNDcv*SGNDcv+1)
SGNDM<-log(SGNDmu)-0.5/SGNDtau
STNtau<-1/log(STNcv*STNcv+1)
STNM<-log(STNmu)-0.5/STNtau

MLLcv<-0.02822/0.7698
MLLtau<-1/log(MLLcv*MLLcv+1)
MLLM<-log(0.7698)-0.5/MLLtau
MDNcv<-0.03227/0.6535
MDNtau<-1/log(MDNcv*MDNcv+1)
MDNM<-log(0.6535)-0.5/MDNtau
MTNcv<-0.059/0.3832
MTNtau<-1/log(MTNcv*MTNcv+1)
MTNM<-log(0.3832)-0.5/MTNtau


# Country list:
#read.table("../../WGBAST_shared/submodels/reporting rates/data/Country_list_MU2.txt")
cry=c(1,8,9,2,3,4,5,6,7)




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
  LLD=LLD_N, FYK=FYK_N, GND=GND_N, MIS=MIS_N,
  Recr=Recr_N, River=River_N,
  SealGND=SealGND_N, SealLLD=SealLLD_N, SealFYK=SealFYK_N, SealMIS=SealMIS_N,
  Dis=Dis_N
  
)



parnames<-c("A_TotDis_BS")



run0 <- run.jags(M1, monitor= parnames,
                 data=datalist,#inits = initsall,
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10, adapt = 10000,
                 keep.jags.files=T,
                 progress.bar=TRUE, jags.refresh=100)


M1<-"model{
  
  for (i in 1:17){       # years 2001-2017, see years 2018-2022 further down
    #  Whole Baltic sea
    A_TotDis_BS[i]<-sum(Tdis[i,1:9,1:2])+sum(Tseal[i,1:9,1:2]) # for T2.2.1 and T2.2.2
    A_TotUnrep_BS[i]<-sum(Tunrep_T2[i,1:9,1:2]) # for T2.2.1 and T2.2.2
    A_TotCatch_BS[i]<-sum(Tcatch[i,1:9,1:2]) # for T2.2.1 and T2.2.2
    A_TotSeal_BS[i]<-sum(Tseal[i,1:9,1:2])
    
    
    for(k in 1:1){		# management unit 1, SD22-31
      # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	 
      B_TotUnrepDis_sea[i,k]<-sum(Tunrep_F2[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tdis[i,1:9,k]) # Estimate of the total unrep, misrep and disdards for F2.2.3
      B_TotCatch_sea[i,k]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+B_TotUnrepDis_sea[i,k]  # for F2.2.3
      B_TotCatchCom_sea[i,k]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tunrep_F2[i,1:9,k]) # comm reported, discarded dead + seal damaged,  unreported, misreported
      B_TotRiver[i,k]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k])  # for  F4.3.2.9 the river catch include also unreporting in river
      B_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
      B_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
      B_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
      B_TotUnrep_sea[i,k]<-sum(Tunrep_F4[i,1:9,k])	# F4.3.2.9 misreporting excluded here
      B_TotUnrep_river[i,k]<-sum(Runrep[i,1:9,k])	# T2.3.4
      B_TotDisSeal_MU[i,k]<-sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k])	# F4.3.2.9
      B_TotUnrep_MU[i,k]<-sum(Tunrep_T2[i,1:9,k])
      B_TotCatch_MU[i,k]<-sum(Tcatch[i,1:9,k]) # All dead catches including unreporting and misreporting by MU
      
      
      B_TotDis_dead[i,k]<-sum(Tdis[i,1:9,k])	# Total dead discards by MU
      B_TotSeal[i,k]<-sum(Tseal[i,1:9,k]) # Total seal damages by MU
      B_TotDis_GND[i,k]<-sum(Dis_GND[i,1:9,k]) # dead discards by component and MU
      B_TotDis_LLD[i,k]<-sum(Dis_LLD[i,1:9,k])
      B_TotDis_FYK[i,k]<-sum(Dis_FYK[i,1:9,k])
      B_TotDis_MIS[i,k]<-sum(Dis_MIS[i,1:9,k])
      B_TotSeal_GND[i,k]<-sum(Seal_GND[i,1:9,k]) # dead seal damages by component and MU
      B_TotSeal_LLD[i,k]<-sum(Seal_LLD[i,1:9,k]) 
      B_TotSeal_FYK[i,k]<-sum(Seal_FYK[i,1:9,k])
      B_TotSeal_MIS[i,k]<-sum(Seal_MIS[i,1:9,k])
      
      B_TotDis_alive[i,k]<-sum(Tdis_alive[i,1:9,k])	# Total alive discards by MU
      B_TotDis_GND_alive[i,k]<-sum(Dis_GND_alive[i,1:9,k]) # alive discards by component and MU
      B_TotDis_LLD_alive[i,k]<-sum(Dis_LLD_alive[i,1:9,k])
      B_TotDis_FYK_alive[i,k]<-sum(Dis_FYK_alive[i,1:9,k])
      
      
      for(j in 1:9){         #  for countries 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported discards
        Ounrep[i,j,k]<- (GND[i,j,k]+LLD[i,j,k])* Oconv[i,j]/(1-Oconv[i,j])	
        # unreported catch in off-shore fisheries
        Cunrep[i,j,k]<- (FYK[i,j,k]+MIS[i,j,k]) * Cconv[i,j]/(1-Cconv[i,j])	 # coast
        Runrep[i,j,k]<- River[i,j,k] * Rconv[i,j] /(1-Rconv[i,j])	 # river
        Sunrep[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] # total unreporting in sea
        Tunrep_F2[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] + TMisr[i,j,k] # unreporting in river excluded from unreporting in F2.2.3
        Tunrep_F4[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k]  # misreporting at sea and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
        Tunrep_T2[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] + Runrep[i,j,k]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
        TRiver[i,j,k]<- River[i,j,k]*epsilon
        TRecrSea[i,j,k]<- Recr[i,j,k]*epsilon
        
        
        # Total unreported by year, country and management unit
        # input values for catches ( from file xxxx)
        # GND[,,] driftnet	LLD[,,] longline	FYK[,,] trapnet	MIS[,,] other gears
        # Recr[,,] recreational at sea  River[,,] river cathes
        # Misr[,,] misreporting Poland only
        # SealGND[,,] SealLLD[,,] SealFYK[,,] SealMIS[,,] gear specific reported seal damages
        # Dis[,,] reported discards
        
        # Dead discards
        Dis_LLD[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j]))* DisLL[i,j]/(1-DisLL[i,j])*MDisLL	# dead discards of LLD+Misreporting
        Dis_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * DisDN[i,j]/(1-DisDN[i,j])*MDisDN	# dead discards of DNS fishery; stopped in 2007
        
        
        # Alive discards; not added to the total catch
        Dis_LLD_alive[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j]))* DisLL[i,j]/(1-DisLL[i,j])*(1-MDisLL)	# Alive discards of LLD+Misreporting
        Dis_GND_alive[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * DisDN[i,j]/(1-DisDN[i,j])*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
        Dis_FYK_alive[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
        Tdis_alive[i,j,k]<-  Dis_LLD_alive[i,j,k] + Dis_GND_alive[i,j,k] + Dis_FYK_alive[i,j,k]   	#Total alive discards by year, MU and country; same procedure for all countries	
        
        
        Tcatch[i,j,k]<- GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k] + 
          Recr[i,j,k] + River[i,j,k]  + Tunrep_T2[i,j,k] + Tdis[i,j,k] + TMisr[i,j,k]
        TcatchCom[i,j,k]<- (GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k])*epsilon						
        # Total catch by year, country and management unit
      }
      
      for (j in 1:1){         #   country 1=FI, seal damages and other discards are given
        Tseal[i,j,k]<- Seal_GND[i,j,k] + Seal_LLD[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k]       # Total seal damages by year
        # Reported gearspecific seal damages are corrected with fishery specific unreporting
        Seal_GND[i,j,k]<- SealGND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,j,k]<- SealFYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,j,k]<- SealMIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Tdis[i,j,k]<- Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]	#Total dead discards by year
        Dis_FYK[i,j,k]<- Dis[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))	# all reported disgards allocated to FYK
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
        TMisr[i,j,k]<-0.0001*epsilon
      }
      for (j in 2:2){         # country 2=SE, seal damages in LLD and FYK are given in data
        Tseal[i,j,k]<- Seal_GND[i,j,k] + Seal_LLD[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k]     # Total seal damages by year
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage in GND; stopped in 2007
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,j,k]<- SealFYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j])
        # no data from MIS until 2018
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]	#Total discards by year
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# no reported Dis until 2018
        #Dis_FYK[i,j,k]<- Dis[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) 	# all reported disgards allocated to FYK, reported Dis from year 2018 onwards
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
        TMisr[i,j,k]<-0.0001*epsilon
      }
      
      for(j in 3:3){         # country 3=DK reported Sea_LLD,  Seal_MIS and Dis_MIS from 2018 onwards
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year
        Seal_LLD[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealLL[i,j]/(1-SealLL[i,j])		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
        Seal_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j])
        # no reported Seal_MIS until 2018 
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # all fish die; no reported Dis_MIS until 2018
        #Dis_MIS[i,j,k]<- Dis[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) 	# all reported disgards allocated to MIS, from year 2018 onwards
        TMisr[i,j,k]<-0.0001*epsilon
      }
      for(j in 4:4){         # country 4=PL no reported Sea_LLD and Seal_MIS until 2018
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year
        Seal_LLD[i,j,k]<- (LLD[i,j,k] + PLMisr[i])*(1+Oconv[i,j]/(1-Oconv[i,j])) *PLfactor[i]* SealLL[i,j]/(1-SealLL[i,j])		# Seal damages LLD+Misreporting  # no reported Seal_LLD until 2018
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
        Seal_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j])
        # no reported Seal_MIS until 2018 
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # all fish die; no reported Dis_MIS
        TMisr[i,j,k]<-PLMisr[i]*epsilon
      }
      for(j in 5:9){         # 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported seal damages or discards
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year
        Seal_LLD[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealLL[i,j]/(1-SealLL[i,j])		# Seal damages LLD+Misreporting
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting
        Seal_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j])
        # Seal damage coastal fishery; mainly TN but all coastal caches included
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 		
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # all fish die; no reported Dis_MIS
        TMisr[i,j,k]<-0.0001*epsilon
      }
    }
    
    for(k in 2:2){		# management unit 2, SD32 only FI, EE and RU operate here
      # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      B_TotUnrepDis_sea[i,k]<-sum(Tunrep_F2[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tdis[i,1:9,k]) # Estimate of the total unrep, misrep and disdards for F2.2.3
      B_TotCatch_sea[i,k]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+B_TotUnrepDis_sea[i,k]  # for F2.2.3
      B_TotCatchCom_sea[i,k]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tunrep_F2[i,1:9,k]) # comm reported, discarded dead + seal damaged,  unreported, misreported
      B_TotRiver[i,k]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k])  # for  F4.3.2.9 the river catch include also unreporting in river
      B_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
      B_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
      B_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
      B_TotUnrep_sea[i,k]<-sum(Tunrep_F4[i,1:9,k])	# F4.3.2.9 misreporting excluded here
      B_TotUnrep_river[i,k]<-sum(Runrep[i,1:9,k])	# T2.3.4
      B_TotDisSeal_MU[i,k]<-sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k])	# F4.3.2.9
      B_TotUnrep_MU[i,k]<-sum(Tunrep_T2[i,1:9,k])
      B_TotCatch_MU[i,k]<-sum(Tcatch[i,1:9,k]) # All catches including unreporting and misreporting by MU
      
      
      B_TotDis_dead[i,k]<-sum(Tdis[i,1:9,k])	# Total dead discards by MU
      B_TotSeal[i,k]<-sum(Tseal[i,1:9,k]) # Total seal damages by MU
      B_TotDis_GND[i,k]<-sum(Dis_GND[i,1:9,k]) # dead discards by component and MU
      B_TotDis_LLD[i,k]<-sum(Dis_LLD[i,1:9,k])
      B_TotDis_FYK[i,k]<-sum(Dis_FYK[i,1:9,k])
      B_TotDis_MIS[i,k]<-sum(Dis_MIS[i,1:9,k])
      B_TotSeal_GND[i,k]<-sum(Seal_GND[i,1:9,k]) # dead seal damages by component and MU
      B_TotSeal_LLD[i,k]<-sum(Seal_LLD[i,1:9,k]) 
      B_TotSeal_FYK[i,k]<-sum(Seal_FYK[i,1:9,k])
      B_TotSeal_MIS[i,k]<-sum(Seal_MIS[i,1:9,k])
      
      B_TotDis_alive[i,k]<-sum(Tdis_alive[i,1:9,k])	# Total alive discards by MU
      B_TotDis_GND_alive[i,k]<-sum(Dis_GND_alive[i,1:9,k]) # alive discards by component and MU
      B_TotDis_LLD_alive[i,k]<-sum(Dis_LLD_alive[i,1:9,k])
      B_TotDis_FYK_alive[i,k]<-sum(Dis_FYK_alive[i,1:9,k])
      
      for(j in 1:3){         #  countries FI, EE and RU
        # order of courtries in the loop list(cry=c(1,8,9,2,3,4,5,6,7)); given in a data file
        
        Ounrep[i,cry[j],k]<- (GND[i,cry[j],k]+LLD[i,cry[j],k])* Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])	
        # unreported catch in off-shore fisheries
        Cunrep[i,cry[j],k]<- (FYK[i,cry[j],k]+MIS[i,cry[j],k]) * Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])	 # coast
        Runrep[i,cry[j],k]<- River[i,cry[j],k] * Rconv[i,cry[j]] /(1-Rconv[i,cry[j]])	 # river
        Sunrep[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] # total unreporting in sea
        Tunrep_F2[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] # unreporting in river excluded from unreporting in F2.2.3
        Tunrep_F4[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
        Tunrep_T2[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] + Runrep[i,cry[j],k]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
        TRiver[i,cry[j],k]<- River[i,cry[j],k]*epsilon
        TRecrSea[i,cry[j],k]<- Recr[i,cry[j],k]*epsilon
        TMisr[i,cry[j],k]<- 0.0001*epsilon
        
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
        Dis_LLD_alive[i,cry[j],k]<- LLD[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]]))* DisLL[i,cry[j]]/(1-DisLL[i,cry[j]])*(1-MDisLL)	# Alive discards of LLD+Misreporting
        Dis_GND_alive[i,cry[j],k]<- GND[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * DisDN[i,cry[j]]/(1-DisDN[i,cry[j]])*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
        Dis_FYK_alive[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]])*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
        Tdis_alive[i,cry[j],k]<-  Dis_LLD_alive[i,cry[j],k] + Dis_GND_alive[i,cry[j],k] + Dis_FYK_alive[i,cry[j],k]   	#Total alive discards by year, MU and country	
        
        Tcatch[i,cry[j],k]<- GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k] + 
          Recr[i,cry[j],k] + River[i,cry[j],k]  + Tunrep_T2[i,cry[j],k] + Tdis[i,cry[j],k]
        TcatchCom[i,cry[j],k]<- (GND[i,cry[j],k] + LLD[i,cry[j],k] + FYK[i,cry[j],k] + MIS[i,cry[j],k])*epsilon						
        # Total catch by year, country and management unit
      }
      
      for(j in 4:9){         #  countries SE, DK, PL, LV, LT and DE don't operate in MU2
        Ounrep[i,cry[j],k]<- 0.00001*epsilon 	# unreported catch in off-shore fisheries
        Cunrep[i,cry[j],k]<- 0.00001*epsilon	 # unrep at coast
        Runrep[i,cry[j],k]<- 0.00001*epsilon	 # unrep river
        Sunrep[i,cry[j],k]<- 0.00001*epsilon     # total unreporting in sea
        Tunrep_F2[i,cry[j],k]<- 0.00001*epsilon  
        Tunrep_F4[i,cry[j],k]<- 0.00001*epsilon  
        Tunrep_T2[i,cry[j],k]<- 0.00001*epsilon 
        TRiver[i,cry[j],k]<- 0.00001*epsilon
        
        TRecrSea[i,cry[j],k]<- 0.00001*epsilon
        TMisr[i,cry[j],k]<-0.0001*epsilon
        
        # Dead discards
        Dis_LLD[i,cry[j],k]<- 0.00001*epsilon
        Dis_GND[i,cry[j],k]<- 0.00001*epsilon
        
        # Alive discards; not added to the total catch
        Dis_LLD_alive[i,cry[j],k]<- 0.00001*epsilon	# Alive discards of LLD
        Dis_GND_alive[i,cry[j],k]<- 0.00001*epsilon	# alive discards of DNS fishery; stopped in 2007
        Dis_FYK_alive[i,cry[j],k]<- 0.00001*epsilon	# alive discards of TN fishery; catches are corrected with relevant unreporting		
        Tdis_alive[i,cry[j],k]<-  0.00001*epsilon   	#Total alive discards by year, MU and country; same procedure for all countries	
        
        Tcatch[i,cry[j],k]<- 0.00001*epsilon 
        TcatchCom[i,cry[j],k]<- 0.00001*epsilon	# Total catch by year, country and management unit
      }
      
      for (j in 1:1){         #   country FI seal damages and other discards are given
        Tseal[i,cry[j],k]<- Seal_GND[i,cry[j],k] + Seal_LLD[i,cry[j],k] + Seal_FYK[i,cry[j],k] + Seal_MIS[i,cry[j],k]       # Total seal damages by year
        # Reported gearspecific seal damages are corrected with fishery specific unreporting
        Seal_GND[i,cry[j],k]<- SealGND[i,cry[j],k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_LLD[i,cry[j],k]<- SealLLD[i,cry[j],k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,cry[j],k]<- SealFYK[i,cry[j],k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,cry[j],k]<- SealMIS[i,cry[j],k]*(1+Cconv[i,j]/(1-Cconv[i,j])) 
        
        Tdis[i,cry[j],k]<- Dis_LLD[i,cry[j],k] + Dis_GND[i,cry[j],k] + Dis_FYK[i,cry[j],k] + Dis_MIS[i,cry[j],k]	#Total discards by year
        Dis_FYK[i,cry[j],k]<- Dis[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]]))	# all reported disgards allocated to FYK
        Dis_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
      }
      for (j in 2:3){         #   countries EE and RU has no estimates for seal damages and other discards
        Tseal[i,cry[j],k]<- Seal_LLD[i,cry[j],k] + Seal_GND[i,cry[j],k] + Seal_FYK[i,cry[j],k] + Seal_MIS[i,cry[j],k] 	#Total seal damages by year
        Seal_LLD[i,cry[j],k]<- LLD[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * SealLL[i,cry[j]]/(1-SealLL[i,cry[j]])		# Seal damages LLD+Misreporting
        Seal_GND[i,cry[j],k]<- GND[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * SealDN[i,cry[j]]/(1-SealDN[i,cry[j]])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * SealC[i,cry[j]]/(1-SealC[i,cry[j]]) 	# catches are corrected with relevant unreporting
        Seal_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * SealC[i,cry[j]]/(1-SealC[i,cry[j]])
        # Seal damage coastal fishery; mainly TN but all coastal caches included
        
        Tdis[i,cry[j],k]<-  Dis_LLD[i,cry[j],k] + Dis_GND[i,cry[j],k] + Dis_FYK[i,cry[j],k] + Dis_MIS[i,cry[j],k]   	#Total discards by year 		
        Dis_FYK[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]]) 
        # all fish die; no reported Dis_MIS
      }
      for (j in 4:9){         # countries SE, DK, PL, LV, LT, DE don't operate in MU2 and 
        Tseal[i,cry[j],k]<- 0.00001*epsilon     # Total seal damages by year
        Seal_GND[i,cry[j],k]<- 0.00001*epsilon	# Seal damage in GND
        Seal_LLD[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in LLD
        Seal_FYK[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in FYK
        Seal_MIS[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in MIS
        
        Tdis[i,cry[j],k]<-  0.00001*epsilon	#Total discards by year
        Dis_FYK[i,cry[j],k]<- 0.00001*epsilon
        Dis_MIS[i,cry[j],k]<- 0.00001*epsilon
      }
    }
  }
  
  for (i in 18:23){       # years 2018-2023
    #  Whole Baltic sea
    A_TotDis_BS[i]<-sum(Tdis[i,1:9,1:2])+sum(Tseal[i,1:9,1:2]) # for T2.2.1 and T2.2.2
    A_TotUnrep_BS[i]<-sum(Tunrep_T2[i,1:9,1:2]) # for T2.2.1 and T2.2.2 
    A_TotCatch_BS[i]<-sum(Tcatch[i,1:9,1:2]) # for T2.2.1 and T2.2.2
    A_TotSeal_BS[i]<-sum(Tseal[i,1:9,1:2]) 
    
    
    for(k in 1:1){		# management unit 1, SD22-31
      # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *	 
      B_TotUnrepDis_sea[i,k]<-sum(Tunrep_F2[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tdis[i,1:9,k]) # Estimate of the total unrep, misrep and disdards for F2.2.3
      B_TotCatch_sea[i,k]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+B_TotUnrepDis_sea[i,k]  # for F2.2.3
      B_TotCatchCom_sea[i,k]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tunrep_F2[i,1:9,k]) # comm reported, discarded dead + seal damaged,  unreported, misreported
      B_TotRiver[i,k]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k])  # for  F4.3.2.9 the river catch include also unreporting in river
      B_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
      B_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
      B_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
      B_TotUnrep_sea[i,k]<-sum(Tunrep_F4[i,1:9,k])	# F4.3.2.9 misreporting excluded here
      B_TotUnrep_river[i,k]<-sum(Runrep[i,1:9,k])	# T2.3.4
      B_TotDisSeal_MU[i,k]<-sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k])	# F4.3.2.9
      B_TotUnrep_MU[i,k]<-sum(Tunrep_T2[i,1:9,k])
      B_TotCatch_MU[i,k]<-sum(Tcatch[i,1:9,k]) # All catches including unreporting and misreporting by MU
      
      
      B_TotDis_dead[i,k]<-sum(Tdis[i,1:9,k])	# Total dead discards by MU
      B_TotSeal[i,k]<-sum(Tseal[i,1:9,k]) # Total seal damages by MU
      B_TotDis_GND[i,k]<-sum(Dis_GND[i,1:9,k]) # dead discards by component and MU
      B_TotDis_LLD[i,k]<-sum(Dis_LLD[i,1:9,k])
      B_TotDis_FYK[i,k]<-sum(Dis_FYK[i,1:9,k])
      B_TotDis_MIS[i,k]<-sum(Dis_MIS[i,1:9,k])
      B_TotSeal_GND[i,k]<-sum(Seal_GND[i,1:9,k]) # dead seal damages by component and MU
      B_TotSeal_LLD[i,k]<-sum(Seal_LLD[i,1:9,k]) 
      B_TotSeal_FYK[i,k]<-sum(Seal_FYK[i,1:9,k])
      B_TotSeal_MIS[i,k]<-sum(Seal_MIS[i,1:9,k])
      
      B_TotDis_alive[i,k]<-sum(Tdis_alive[i,1:9,k])	# Total alive discards by MU
      B_TotDis_GND_alive[i,k]<-sum(Dis_GND_alive[i,1:9,k]) # alive discards by component and MU
      B_TotDis_LLD_alive[i,k]<-sum(Dis_LLD_alive[i,1:9,k])
      B_TotDis_FYK_alive[i,k]<-sum(Dis_FYK_alive[i,1:9,k])
      
      
      for(j in 1:9){         #  for countries 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported discards
        Ounrep[i,j,k]<- (GND[i,j,k]+LLD[i,j,k])* Oconv[i,j]/(1-Oconv[i,j])	
        # unreported catch in off-shore fisheries
        Cunrep[i,j,k]<- (FYK[i,j,k]+MIS[i,j,k]) * Cconv[i,j]/(1-Cconv[i,j])	 # coast
        Runrep[i,j,k]<- River[i,j,k] * Rconv[i,j] /(1-Rconv[i,j])	 # river
        Sunrep[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] # total unreporting in sea
        Tunrep_F2[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] + TMisr[i,j,k] # unreporting in river excluded from unreporting in F2.2.3
        Tunrep_F4[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
        Tunrep_T2[i,j,k]<- Ounrep[i,j,k] + Cunrep[i,j,k] + Runrep[i,j,k]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
        TRiver[i,j,k]<- River[i,j,k]*epsilon
        TRecrSea[i,j,k]<- Recr[i,j,k]*epsilon
        
        # Total unreported by year, country and management unit
        # input values for catches ( from files xxxx)
        # GND[,,] driftnet	LLD[,,] longline	FYK[,,] trapnet	MIS[,,] other gears
        # Recr[,,] recreational  River[,,] river cathes
        # Misr[,,] misreporting Poland only
        # Seal_GND[,,] Seal_LLD[,,] Seal_FYK[,,] Seal_MIS[,,] gear specific reported seal damages
        # Dis[,,] reported discards
        
        # Dead discards
        Dis_LLD[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j]))* DisLL[i,j]/(1-DisLL[i,j])*MDisLL	# dead discards of LLD+Misreporting
        Dis_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * DisDN[i,j]/(1-DisDN[i,j])*MDisDN	# dead discards of DNS fishery; stopped in 2007
        
        
        # Alive discards; not added to the total catch
        Dis_LLD_alive[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j]))* DisLL[i,j]/(1-DisLL[i,j])*(1-MDisLL)	# Alive discards of LLD+Misreporting
        Dis_GND_alive[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * DisDN[i,j]/(1-DisDN[i,j])*(1-MDisDN)	# alive discards of DNS fishery; stopped in 2007
        Dis_FYK_alive[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*(1-MDisC)	# alive discards of TN fishery; catches are corrected with relevant unreporting		
        Tdis_alive[i,j,k]<-  Dis_LLD_alive[i,j,k] + Dis_GND_alive[i,j,k] + Dis_FYK_alive[i,j,k]   	#Total alive discards by year, MU and country; same procedure for all countries	
        
        
        Tcatch[i,j,k]<- GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k] + 
          Recr[i,j,k] + River[i,j,k]  + Tunrep_T2[i,j,k] + Tdis[i,j,k]+ TMisr[i,j,k]
        TcatchCom[i,j,k]<- (GND[i,j,k] + LLD[i,j,k] + FYK[i,j,k] + MIS[i,j,k])*epsilon						
        # Total catch by year, country and management unit
      }
      
      for (j in 1:1){         #   country 1=FI, seal damages and other discards are given
        Tseal[i,j,k]<- Seal_GND[i,j,k] + Seal_LLD[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k]       # Total seal damages by year
        # Reported gearspecific seal damages are corrected with fishery specific unreporting
        Seal_GND[i,j,k]<- SealGND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,j,k]<- SealFYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,j,k]<- SealMIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Tdis[i,j,k]<- Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]	#Total discards by year
        Dis_FYK[i,j,k]<- Dis[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))	# all reported disgards allocated to FYK
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
        TMisr[i,j,k]<-0.0001*epsilon	
      }
      for (j in 2:2){         # country 2=SE, seal damages in LLD and FYK are given in data
        Tseal[i,j,k]<- Seal_GND[i,j,k] + Seal_LLD[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k]       # Total seal damages by year
        # Reported gearspecific seal damages are corrected with fishery specific unreporting
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage in GND; stopped in 2007
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,j,k]<- SealFYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,j,k]<- SealMIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))     # seal damages by year, Seal MIS reported from 2018 onwards
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]	#Total discards by year
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# no reported Dis until 2018
        #Dis_FYK[i,j,k]<- Dis[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) 	# all reported disgards allocated to FYK, reported Dis from year 2018 onwards
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
        TMisr[i,j,k]<-0.0001*epsilon
      }
      for(j in 3:3){         # country 3=DK reported Sea_LLD,  Seal_MIS and Dis_MIS from 2018 onwards
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year, SealLLD and SealMIS reported from 2018 onwards
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_MIS[i,j,k]<- SealMIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- Dis[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) 	# all reported disgards allocated to MIS, from year 2018 onwards
        TMisr[i,j,k]<-0.0001*epsilon
      }
      for(j in 4:4){         # country 4=PL reported Sea_LLD and Seal_MIS from 2018 onwards
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year, SealLLD and SealMIS reported from 2018 onwards
        Seal_LLD[i,j,k]<- SealLLD[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_MIS[i,j,k]<- SealMIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting; no no reported Seal_FYK
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # all fish die; no reported Dis_MIS
        TMisr[i,j,k]<-PLMisr[i]*epsilon
      }
      for(j in 5:9){         # 5=LV, 6=LT, 7=DE, 8=EE, 9=RU no reported seal damages or discards
        Tseal[i,j,k]<- Seal_LLD[i,j,k] + Seal_GND[i,j,k] + Seal_FYK[i,j,k] + Seal_MIS[i,j,k] 	#Total seal damages by year
        Seal_LLD[i,j,k]<- (LLD[i,j,k] + TMisr[i,j,k])*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealLL[i,j]/(1-SealLL[i,j])		# Seal damages LLD+Misreporting
        Seal_GND[i,j,k]<- GND[i,j,k]*(1+Oconv[i,j]/(1-Oconv[i,j])) * SealDN[i,j]/(1-SealDN[i,j])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j]) 	# catches are corrected with relevant unreporting
        Seal_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * SealC[i,j]/(1-SealC[i,j])
        # Seal damage coastal fishery; mainly TN but all coastal caches included
        
        Tdis[i,j,k]<-  Dis_LLD[i,j,k] + Dis_GND[i,j,k] + Dis_FYK[i,j,k] + Dis_MIS[i,j,k]   	#Total discards by year 		
        Dis_FYK[i,j,k]<- FYK[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,j,k]<- MIS[i,j,k]*(1+Cconv[i,j]/(1-Cconv[i,j])) * DisC[i,j]/(1-DisC[i,j]) 
        # all fish die; no reported Dis_MIS
        TMisr[i,j,k]<-0.0001*epsilon
      }
    }
    
    for(k in 2:2){		# management unit 2, SD32 only FI, EE and RU operate here
      # * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      B_TotUnrepDis_sea[i,k]<-sum(Tunrep_F2[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tdis[i,1:9,k]) # Estimate of the total unrep, misrep and disdards for F2.2.3
      B_TotCatch_sea[i,k]<-sum(TcatchCom[i,1:9,k])+sum(TRecrSea[i,1:9,k])+B_TotUnrepDis_sea[i,k]  # for F2.2.3
      B_TotCatchCom_sea[i,k]<-sum(TcatchCom[i,1:9,k]) + sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k]) + sum(Tunrep_F2[i,1:9,k]) # comm reported, discarded dead + seal damaged,  unreported, misreported
      B_TotRiver[i,k]<- sum(TRiver[i,1:9,k])+sum(Runrep[i,1:9,k])  # for  F4.3.2.9 the river catch include also unreporting in river
      B_TotRepCom_sea[i,k]<-sum(TcatchCom[i,1:9,k])  # reported catch for F2.2.3 & F4.3.2.9
      B_TotRecr_sea[i,k]<-sum(TRecrSea[i,1:9,k]) 	# Recr catch in the sea for F4.3.2.9 and F2.2.3
      B_TotMisr_sea[i,k]<-sum(TMisr[i,1:9,k])			# F4.3.2.9
      B_TotUnrep_sea[i,k]<-sum(Tunrep_F4[i,1:9,k])	# F4.3.2.9 misreporting excluded here
      B_TotUnrep_river[i,k]<-sum(Runrep[i,1:9,k])	# T2.3.4
      B_TotDisSeal_MU[i,k]<-sum(Tdis[i,1:9,k]) + sum(Tseal[i,1:9,k])	# F4.3.2.9
      B_TotUnrep_MU[i,k]<-sum(Tunrep_T2[i,1:9,k])
      B_TotCatch_MU[i,k]<-sum(Tcatch[i,1:9,k]) # All catches including unreporting and misreporting by MU
      
      
      B_TotDis_dead[i,k]<-sum(Tdis[i,1:9,k])	# Total dead discards by MU 
      B_TotSeal[i,k]<-sum(Tseal[i,1:9,k]) # Total seal damages by MU
      B_TotDis_GND[i,k]<-sum(Dis_GND[i,1:9,k]) # dead discards by component and MU
      B_TotDis_LLD[i,k]<-sum(Dis_LLD[i,1:9,k])
      B_TotDis_FYK[i,k]<-sum(Dis_FYK[i,1:9,k])
      B_TotDis_MIS[i,k]<-sum(Dis_MIS[i,1:9,k])
      B_TotSeal_GND[i,k]<-sum(Seal_GND[i,1:9,k]) # dead seal damages by component and MU
      B_TotSeal_LLD[i,k]<-sum(Seal_LLD[i,1:9,k]) 
      B_TotSeal_FYK[i,k]<-sum(Seal_FYK[i,1:9,k])
      B_TotSeal_MIS[i,k]<-sum(Seal_MIS[i,1:9,k])
      
      B_TotDis_alive[i,k]<-sum(Tdis_alive[i,1:9,k])	# Total alive discards by MU
      B_TotDis_GND_alive[i,k]<-sum(Dis_GND_alive[i,1:9,k]) # alive discards by component and MU
      B_TotDis_LLD_alive[i,k]<-sum(Dis_LLD_alive[i,1:9,k])
      B_TotDis_FYK_alive[i,k]<-sum(Dis_FYK_alive[i,1:9,k])
      
      for(j in 1:3){         #  countries FI, EE and RU
        # order of courtries in loop list(cry=c(1,8,9,2,3,4,5,6,7)); given in a data file
        
        Ounrep[i,cry[j],k]<- (GND[i,cry[j],k]+LLD[i,cry[j],k])* Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])	
        # unreported catch in off-shore fisheries
        Cunrep[i,cry[j],k]<- (FYK[i,cry[j],k]+MIS[i,cry[j],k]) * Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])	 # coast
        Runrep[i,cry[j],k]<- River[i,cry[j],k] * Rconv[i,cry[j]] /(1-Rconv[i,cry[j]])	 # river
        Sunrep[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] # total unreporting in sea
        Tunrep_F2[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] + TMisr[i,cry[j],k] # unreporting in river excluded from unreporting in F2.2.3
        Tunrep_F4[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k]  # misreporting and unreporting in river ARE NOT included in the total  unreporting in F4.3.2.9
        Tunrep_T2[i,cry[j],k]<- Ounrep[i,cry[j],k] + Cunrep[i,cry[j],k] + Runrep[i,cry[j],k]  # misreporting IS NOT included to the total unreporting in T2.2.1 and T2.2.2
        TRiver[i,cry[j],k]<- River[i,cry[j],k]*epsilon
        TRecrSea[i,cry[j],k]<- Recr[i,cry[j],k]*epsilon
        TMisr[i,cry[j],k]<- 0.0001*epsilon
        
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
      }
      
      for(j in 4:9){         #  countries SE, DK, PL, LV, LT and DE don't operate in MU2
        Ounrep[i,cry[j],k]<- 0.00001*epsilon 	# unreported catch in off-shore fisheries
        Cunrep[i,cry[j],k]<- 0.00001*epsilon	 # unrep at coast
        Runrep[i,cry[j],k]<- 0.00001*epsilon	 # unrep river
        Sunrep[i,cry[j],k]<- 0.00001*epsilon     # total unreporting in sea
        Tunrep_F2[i,cry[j],k]<- 0.00001*epsilon  
        Tunrep_F4[i,cry[j],k]<- 0.00001*epsilon  
        Tunrep_T2[i,cry[j],k]<- 0.00001*epsilon 
        TRiver[i,cry[j],k]<- 0.00001*epsilon
        TRecrSea[i,cry[j],k]<- 0.00001*epsilon
        TMisr[i,cry[j],k]<-0.0001*epsilon
        
        # Dead discards
        Dis_LLD[i,cry[j],k]<- 0.00001*epsilon
        Dis_GND[i,cry[j],k]<- 0.00001*epsilon
        
        # Alive discards; not added to the total catch
        Dis_LLD_alive[i,cry[j],k]<- 0.00001*epsilon
        Dis_GND_alive[i,cry[j],k]<- 0.00001*epsilon
        Dis_FYK_alive[i,cry[j],k]<- 0.00001*epsilon
        Tdis_alive[i,cry[j],k]<-  0.00001*epsilon
        
        Tcatch[i,cry[j],k]<- 0.00001*epsilon 
        TcatchCom[i,cry[j],k]<- 0.00001*epsilon
      }
      
      for (j in 1:1){         #   country FI seal damages and other discards are given
        Tseal[i,cry[j],k]<- Seal_GND[i,cry[j],k] + Seal_LLD[i,cry[j],k] + Seal_FYK[i,cry[j],k] + Seal_MIS[i,cry[j],k]       # Total seal damages by year
        # Reported gearspecific seal damages are corrected with fishery specific unreporting
        Seal_GND[i,cry[j],k]<- SealGND[i,cry[j],k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_LLD[i,cry[j],k]<- SealLLD[i,cry[j],k]*(1+Oconv[i,j]/(1-Oconv[i,j]))
        Seal_FYK[i,cry[j],k]<- SealFYK[i,cry[j],k]*(1+Cconv[i,j]/(1-Cconv[i,j]))
        Seal_MIS[i,cry[j],k]<- SealMIS[i,cry[j],k]*(1+Cconv[i,j]/(1-Cconv[i,j])) 
        
        Tdis[i,cry[j],k]<- Dis_LLD[i,cry[j],k] + Dis_GND[i,cry[j],k] + Dis_FYK[i,cry[j],k] + Dis_MIS[i,cry[j],k]	#Total discards by year
        Dis_FYK[i,cry[j],k]<- Dis[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]]))	# all reported disgards allocated to FYK
        Dis_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]]) 
        # disgards coastal fishery; same proportion of undersized as in TN fishery; all fish assumed to die
      }
      for (j in 2:3){         #   countries EE and RU has no estimates for seal damages and other discards
        Tseal[i,cry[j],k]<- Seal_LLD[i,cry[j],k] + Seal_GND[i,cry[j],k] + Seal_FYK[i,cry[j],k] + Seal_MIS[i,cry[j],k] 	#Total seal damages by year
        Seal_LLD[i,cry[j],k]<- LLD[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * SealLL[i,cry[j]]/(1-SealLL[i,cry[j]])		# Seal damages LLD+Misreporting
        Seal_GND[i,cry[j],k]<- GND[i,cry[j],k]*(1+Oconv[i,cry[j]]/(1-Oconv[i,cry[j]])) * SealDN[i,cry[j]]/(1-SealDN[i,cry[j]])	# Seal damage DNS fishery; stopped in 2007
        Seal_FYK[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * SealC[i,cry[j]]/(1-SealC[i,cry[j]]) 	# catches are corrected with relevant unreporting
        Seal_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * SealC[i,cry[j]]/(1-SealC[i,cry[j]])
        # Seal damage coastal fishery; mainly TN but all coastal caches included
        
        Tdis[i,cry[j],k]<-  Dis_LLD[i,cry[j],k] + Dis_GND[i,cry[j],k] + Dis_FYK[i,cry[j],k] + Dis_MIS[i,cry[j],k]   	#Total discards by year 		
        Dis_FYK[i,cry[j],k]<- FYK[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]])*MDisC	# dead discards of TN fishery; catches are corrected with relevant unreporting
        Dis_MIS[i,cry[j],k]<- MIS[i,cry[j],k]*(1+Cconv[i,cry[j]]/(1-Cconv[i,cry[j]])) * DisC[i,cry[j]]/(1-DisC[i,cry[j]]) 
        # all fish die; no reported Dis_MIS
      }
      for (j in 4:9){         # countries SE, DK, PL, LV, LT, DE don't operate in MU2 and 
        Tseal[i,cry[j],k]<- 0.00001*epsilon     # Total seal damages by year
        Seal_GND[i,cry[j],k]<- 0.00001*epsilon	# Seal damage in GND
        Seal_LLD[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in LLD
        Seal_FYK[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in FYK
        Seal_MIS[i,cry[j],k]<- 0.00001*epsilon 	# Seal damage in MIS
        
        Tdis[i,cry[j],k]<-  0.00001*epsilon	#Total discards by year
        Dis_FYK[i,cry[j],k]<- 0.00001*epsilon
        Dis_MIS[i,cry[j],k]<- 0.00001*epsilon
        
      }
    }
  }
  
  epsilon~dnorm(1,10000)I(0,)	# a super narrow pd just to deal with discrete attributes
  
  # Mortalities of discarded undersized salmon
  # These are considered to be same for all coutries and years in the whole Baltic Sea
  # and therefore not in the input file where country and year specific parameter values are taken
  # notice:	1) driftnet fishery stopped at the end of 2007 i.e. zero catches in drifnets from 2008
  #			   2) discarding ban for longline fishery valid from 2015 but not implemented so far
  
  # longline
  MDisLL~dlnorm(MLLM,MLLtau)I(0.5,1.1)
  
  #  driftnet
  MDisDN~dlnorm(MDNM,MDNtau)I(0.4,1.1)
  
  # trapnet
  MDisC~dlnorm(MTNM,MTNtau)I(0.1,1.1)
  
  # Country and year specific Mean and SD values of lognormal disributions (from input file xxxx)
  # same values are used for both management units
  
  #	Omu[,,]	Osd[,,]	unreporting all fisheries off-shore
  #	Cmu[,,]	Csd[,,]	unreporting all fisheries coast
  #	Rmu[,,]	Rsd[,,]	unreporting all fisheries river
  #	LLmu[,,]	LLsd[,,]		share of discarded undersized longline
  #	DNmu[,,]	DNvar[,,]	share if discarded undersized driftnet
  #	TNmu[,,]	TNsd[,,]	share if discarded undersized trapnet and other coastal gears
  #	SLLDmu[,,]	SLLDsd[,,]	share if seal damages longline
  #	SGNDmu[,,]	SGNDsd[,,]	share of seal damages driftnet
  #	STNmu[,,]	STNsd[,,]	share of seal damages trapnet  and other coastal gears
  
  for (j in 1:9){          # countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU
    for (i in 1:23){       # years 2001-2023				
      
      Oconv[i,j]~dlnorm(OM[i,j],Otau[i,j])I(0,0.6)	# unreporting all fisheries off-shore
      Cconv[i,j]~dlnorm(CM[i,j],Ctau[i,j])I(0,0.7)	# unreporting all fisheries coast
      Rconv[i,j]~dlnorm(RM[i,j],Rtau[i,j])I(0,0.7)	# unreporting all fisheries river
      
      DisLL[i,j]~dlnorm(LLM[i,j],LLtau[i,j])I(0,0.3)	# share of discarded undersized longline
      DisDN[i,j]~dlnorm(DNM[i,j],DNtau[i,j])I(0,0.2)	# share if discarded undersized driftnet
      DisC[i,j]~dlnorm(TNM[i,j],TNtau[i,j])I(0,0.2)	# share if discarded undersized trapnet and other coastal gears
      
      SealLL[i,j]~dlnorm(SLLDM[i,j],SLLDtau[i,j])I(0,0.2)	# share if seal damages longline
      SealDN[i,j]~dlnorm(SGNDM[i,j],SGNDtau[i,j])I(0,0.2)	# share of seal damages driftne
      SealC[i,j]~dlnorm(STNM[i,j],STNtau[i,j])I(0,0.35)	# share of seal damages trapnet  and other coastal gears
      
      
    }
  }
}"

