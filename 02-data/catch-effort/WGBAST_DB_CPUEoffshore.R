## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Poland based on CPUE's of Finland,
#              Sweden, Denmark and Latvia

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


# Calculate combined CPUE from FIN, SWE, DEN and LAT data for ODN and OLL (only data
# with both C&E known included)
# -> use combined CPUE and PL effort to calculate PL catch for desired time period


# ODN
# ============

latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="O")
Lat_ODN_CPUE<-filter(latvia, GEAR=="GND",is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="LV")

Den_ODN_CPUE<-filter(denmark, GEAR=="GND", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="DK")

Fin_ODN_CPUE<-filter(finland, GEAR=="GND", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="FI")

Swe_ODN_CPUE<-filter(sweden, GEAR=="GND", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="SE")

ODN_CPUE<-full_join(Lat_ODN_CPUE, Den_ODN_CPUE)%>%
  full_join(Fin_ODN_CPUE)%>%
  full_join(Swe_ODN_CPUE)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_tot=sum(Catch),
            Effort_tot=sum(Effort))%>%
  mutate(CPUE_tot=Catch_tot/Effort_tot)


# OLL
# ============

Lat_OLL_CPUE<-filter(latvia, GEAR=="LLD",is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="LV")

Den_OLL_CPUE<-filter(denmark, GEAR=="LLD", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="DK")

Fin_OLL_CPUE<-filter(finland, GEAR=="LLD", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="FI")

Swe_OLL_CPUE<-filter(sweden, GEAR=="LLD", is.na(NUMB)==F, is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))%>%
  mutate(country="SE")

OLL_CPUE<-full_join(Lat_OLL_CPUE, Den_OLL_CPUE)%>%
  full_join(Fin_OLL_CPUE)%>%
  full_join(Swe_OLL_CPUE)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_tot=sum(Catch),
            Effort_tot=sum(Effort))%>%
  mutate(CPUE_tot=Catch_tot/Effort_tot)%>%
  ungroup()

tmp<-OLL_CPUE%>%
  group_by(YEAR)%>%
  summarise(CPUE_tot2=sum(Catch_tot)/sum(Effort_tot))

# If HYR catch is <2000 salmon, take CPUE over total calendar year
OLL_CPUE<-full_join(OLL_CPUE,tmp)%>%
  #mutate(CUPE_tot3=ifelse(Catch_tot<2000, CPUE_tot2, CPUE_tot))
  mutate(CPUE_tot=ifelse(Catch_tot<2000, CPUE_tot2, CPUE_tot))%>%
  select(-CPUE_tot2)

#View(OLL_CPUE)

# Proportions per HYR

# ODN
pHYR1_ODN<-ODN_CPUE%>%
  group_by(YEAR)%>%
  mutate(sumC=sum(Catch_tot), sumE=sum(Effort_tot))%>%
  mutate(pC=Catch_tot/sumC, pE=Effort_tot/sumE)%>%
  filter(HYR==1)%>%
  select(YEAR,pC,pE)

# OLL
pHYR1_OLL<-OLL_CPUE%>%
  group_by(YEAR)%>%
  mutate(sumC=sum(Catch_tot), sumE=sum(Effort_tot))%>%
  mutate(pC=Catch_tot/sumC, pE=Effort_tot/sumE)%>%
  filter(HYR==1)%>%
  select(YEAR,pC,pE)




