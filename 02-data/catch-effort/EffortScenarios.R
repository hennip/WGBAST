## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:   Combine effort data for scenarios

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#Run first CatchEffort.r
source("02-catch-effort/CatchEffort.r")

yearX<-2022 # Assessment year -1

# =======================================================================
# Longlining:
# =====================

# Copy last observed effort at HYR==1 for assessment year for scenarios

# Denmark
# ==================
DenE_OLL<-Den_OLL%>%select(-Catch)

DenE_OLL%>%filter(YEAR==yearX) # check if there's data on HYR2 of last year

# if HYR2 Effort is partly or completely missing, replace with HYR2 data from previous year
DenE_OLL<-DenE_OLL%>%
  mutate(Effort=ifelse(YEAR==yearX & HYR==2, 
                       DenE_OLL$Effort[length(DenE_OLL$Effort)-2],Effort))

# Replace HYR1 on assessment year with HYR1 on assessment year -1
tmp<-filter(DenE_OLL, YEAR==yearX, HYR==1)%>% 
  ungroup()%>%
  mutate(YEAR=YEAR+1) # Assessement year

full_join(tmp,DenE_OLL)%>% 
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=sum(Effort)/100000) # if there were NA's you'd see it here

# Poland
# ==================
PolE_OLL%>%filter(YEAR==yearX)# check if there's data on HYR2 of last year

# if HYR2 Effort is partly or completely missing, replace with HYR2 data from previous year

PolE_OLL%>%filter(YEAR==yearX-1, HYR==2)# HYR2 of last year

tmp1<-filter(PolE_OLL, YEAR==yearX-1, HYR==2)%>%
  ungroup()%>%
  mutate(YEAR=YEAR+1) # Assessement year -1 HYR 2 -> use prev years effort
#
# PolE_OLL<-PolE_OLL%>%
#   mutate(Effort=ifelse(YEAR==yearX & HYR==2,
#                        PolE_OLL$Effort[length(PolE_OLL$Effort)-2],Effort))


tmp<-filter(PolE_OLL, YEAR==yearX, HYR==1)%>% 
  ungroup()%>%
  mutate(YEAR=YEAR+1) # Assessement year HYR 1 -> use prev years effort on HYR 1

full_join(tmp, tmp1)%>% # if HYR 2 data missing
full_join(PolE_OLL)%>%
#  full_join(tmp,PolE_OLL)%>% # if HYR 2 data not missing
  mutate(Myear=ifelse(HYR==2, YEAR, YEAR-1))%>% # Model year
  ungroup()%>%
  group_by(Myear)%>%
  summarise(Effort=sum(Effort)/100000) # if there were NA's you'd see it here
# 
# # Trolling
# # ==================
#  (OLL_CPUE<-full_join(OLL_E,OLL_C)%>%
#    mutate(CPUE=Catch/Effort)%>%
#    filter(Myear<yearX))
# # 
#  OLL_CPUE%>%
#    filter(Myear<yearX& Myear>(yearX-5))%>% 
#    summarise(CPUE=sum(Catch)/sum(Effort))%>%# Average over 2013-2016
#    mutate(YEAR=yearX)
# 
# # Plug the above value into Catch&Effort.xlsx F75

  
# =======================================================================
# Trapnetting & Gillnetting:
# =====================

# This goes to EffortCTN_ICES.txt
############################
cbind(
FinE30_CTN%>%mutate(Fin30=Effort/1000)%>%select(-Effort),
SweE30_CTN%>%mutate(Swe30=Effort/1000)%>%select(-YEAR, -Effort),
FinE31_CTN%>%mutate(Fin31=Effort/1000)%>%select(-YEAR, -Effort),
SweE31_CTN%>%mutate(Swe31=Effort/1000)%>%select(-YEAR, -Effort))


# This goes to EffortCGN_ICES.txt
############################
cbind(
  FinE30_COT%>%mutate(Fin30=Effort/100000)%>%select(-Effort),
  SweE30_COT%>%mutate(Swe30=Effort/100000)%>%select(-YEAR, -Effort),
  FinE31_COT%>%mutate(Fin31=Effort/100000)%>%select(-YEAR, -Effort),
  SweE31_COT%>%mutate(Swe31=Effort/100000)%>%select(-YEAR, -Effort))


# =======================================================================
# Note! Kate (or Adam or someone else) will ask OLL efforts from all countries
# for a certain graph in the report. Print the following (half year
# specific numbers) for them. 2014 Finnish longline effort is to be ignored
# (Tapsa will give details on this if needed). 
(tmp1<-PolE_OLL%>%mutate(Effort_PL=Effort)%>%select(-Effort))
(tmp2<-Fin_OLL%>%mutate(Effort_FI=Effort)%>%select(-Effort,-Catch))
(tmp3<-DenE_OLL%>%mutate(Effort_DK=Effort)%>%select(-Effort))
(tmp4<-SweE_OLL%>%mutate(Effort_SE=Effort)%>%select(-Effort))
(tmp5<-LatE_OLL_est%>%mutate(Effort_LT=Effort)%>%select(-Effort,-CPUE_tot, -Catch))

tmp<-full_join(tmp1, tmp2)%>%
  full_join(tmp3)%>%
  full_join(tmp4)%>%
  full_join(tmp5)%>%
  filter(YEAR>2019)
  
View(tmp)

# =======================================================================


