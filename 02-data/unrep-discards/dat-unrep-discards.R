# Catch&Effort DB - Country specific landings per gear
# 
#
# NOTE!!! This file does not run currently independently, run first
# necessry bits from workflow-unrep-and-discards.r
################################################################################


pathIn<-pathDataCatchEffort

source("02-data/catch-effort/read-in-wgbast-catch&effort.r")

df_all<-wgbast_catch_data|> 
  filter(SPECIES=="SAL", YEAR>2000)|> 
  filter(F_TYPE!="ALV", F_TYPE!="BROOD")

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



yrs<-tibble(YEAR=c(2001:max_year))
yrs1<-tibble(YEAR=c(2001:max_year), sub_div2="22-31")
yrs2<-tibble(YEAR=c(2001:max_year), sub_div2="32")
yrs<-full_join(yrs1, yrs2)

################################################################################
# Landed catches in number and in weight

# Dataset for 
df2 <- df |> filter(F_TYPE!="SEAL", F_TYPE!="DISC")


# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
#if(number_or_weight=="N"){
numb<-func_country_catches(df2,1)

River_N<-numb[[1]]
Recr_N<-numb[[2]]
GND_N<-numb[[3]]
LLD_N<-numb[[4]]
FYK_N<-numb[[5]]
MIS_N<-numb[[6]]
#}

#if(number_or_weight=="W"){
  weight<-func_country_catches(df2,2)
River_W<-weight[[1]]
Recr_W<-weight[[2]]
GND_W<-weight[[3]]
LLD_W<-weight[[4]]
FYK_W<-weight[[5]]
MIS_W<-weight[[6]]
#}
################################################################################
# Seal damaged in number and in weight per country

df3 <- df |> filter(F_TYPE=="SEAL")

# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
#if(number_or_weight=="N"){
  numb<-func_country_sealdam(df3,1)
SealGND_N<-numb[[1]]
SealLLD_N<-numb[[2]]
SealFYK_N<-numb[[3]]
SealMIS_N<-numb[[4]]
#}
#if(number_or_weight=="W"){
  weight<-func_country_sealdam(df3,2)
SealGND_W<-weight[[1]]
SealLLD_W<-weight[[2]]
SealFYK_W<-weight[[3]]
SealMIS_W<-weight[[4]]
#}

################################################################################
# Other discards in number and in weight per country

df4 <- df |> filter(F_TYPE=="DISC")

# Choose numb_or_weight==1 for NUMB and numb_or_weight==2 for weight 
#if(number_or_weight=="N"){
  Dis_N<-func_country_discards(df4,1)
#}
#if(number_or_weight=="W"){
  Dis_W<-func_country_discards(df4,2)
#}
################################################################################
################################################################################
# Polish seal damages

# Correction factor for Polish seal damages to make the given estimate to apply only in SD26 catch	
# in other words in years 2013-2015 given estimate applies only for 55% of the Total LLD catch	
# and in years 2016-2017 applies for 65% of the total LLD catch
# Eli näillä luvuilla kerrotaan Puolan ilmoittamaa saalista
# from 2018 onwards seal damage data are given in numbers of fish (see SealLLD variable)
# ONKO VIRHE ETTÄ PLfactor-muuttujassa on annettu arvo myös vuosille 2018->???
PL_sealfac<-read.table("../../WGBAST_shared/submodels/reporting rates/data/2025/PL_Seal_corr_factor.txt", header=T)

# Use unname if col names are problem
# unname(River)

################################################################################
# Polish misreporting

PL_misrep_N<-read.table("../../WGBAST_shared/submodels/reporting rates/data/2025/PL_Misrep_numb.txt", header=T)
PL_misrep_W<-read.table("../../WGBAST_shared/submodels/reporting rates/data/2025/PL_Misrep_weight.txt", header=T)


################################################################################
# Country specific unreporting and discard rates based on expert elicitation


df<-read.table("../../WGBAST_shared/submodels/reporting rates/data/2025/Unrep_discard_rates.txt", header=T)
#df<-as_tibble(df)

# Kuinka suuri osuus saaliista on alamittaisia, perustuu kolmiojakaumaan ja expert elisitointiin
Omu<-df |> select(starts_with("Omu")) 
Osd<-df |> select(starts_with("Osd"))
Cmu<-df |> select(starts_with("Cmu"))
Csd<-df |> select(starts_with("Csd"))
Rmu<-df |> select(starts_with("Rmu"))
Rsd<-df |> select(starts_with("Rsd"))
LLmu<-df |> select(starts_with("LLmu"))
LLsd<-df |> select(starts_with("LLsd"))
DNmu<-df |> select(starts_with("DNmu"))
DNsd<-df |> select(starts_with("DNsd"))
TNmu<-df |> select(starts_with("TNmu"))
TNsd<-df |> select(starts_with("TNsd"))
SLLDmu<-df |> select(starts_with("SLLDmu"))
SLLDsd<-df |> select(starts_with("SLLDsd"))
SGNDmu<-df |> select(starts_with("SGNDmu"))
SGNDsd<-df |> select(starts_with("SGNDsd"))
STNmu<-df |> select(starts_with("STNmu"))
STNsd<-df |> select(starts_with("STNsd"))

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

