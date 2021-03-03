## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Finland

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


finland<-filter(salmon, COUNTRY=="FI")

finland%>%
  group_by(FISHERY)%>%
  count(TP_TYPE)
# YR catch is RECR MIS at coast & river

finland%>%
  group_by(FISHERY)%>%
  count(GEAR)

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Offshore fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

################################################################################
#  Offshore driftnetting:                                                                  
################################################################################

Fin_ODN<-finland%>%
  filter(FISHERY=="O", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))


# ################################################################################
#  Offshore longlining:
################################################################################


Fin_OLL<-finland%>%
  filter(FISHERY=="O", GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))
#View(Fin_OLL)

################################################################################
#  Other offshore gear (OT)
################################################################################
# These will be added to commercial coastal other gear

Fin_OOT<-finland%>%
  filter(FISHERY=="O", GEAR=="MIS")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_OOT=round(sum(NUMB, na.rm=T)),
            Effort_OOT=round(sum(EFFORT, na.rm=T)))

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
#  Coastal fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~

finland%>%
  filter(FISHERY=="C")%>%
  count(GEAR)

# Groups in coastal catch and effort for Finland are
# CDN (GND), CTN (FYK) and Coastal OT (MIS). 
# What to do with LLD=14?
# Because NA's RECR => put those with GEAR=MIS (Tapani)

finland%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR, F_TYPE)%>%
  count(TP_TYPE)

# YR data is at RECR MIS
# -> need to combine HYR (COMM) & YR (RECR) MIS

################################################################################
#  Coastal driftnetting:                                                                  
################################################################################


Fin_CDN<-finland%>%
  filter(FISHERY=="C", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))


################################################################################
#  Coastal trapnetting:
################################################################################

Fin_CTN<-finland%>%
  filter(FISHERY=="C", GEAR=="FYK")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))
#View(Fin_CTN)


################################################################################
#  Other coastal gear (OT)
################################################################################

finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)
# MIS : HYR, MON & YR


Fin_COT<-finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE!="YR")%>% # take HYR/MON here and include YR later 
  group_by(YEAR, HYR)%>%
  summarise(Catch_COT=round(sum(NUMB, na.rm=T)),
            Effort_COT=round(sum(EFFORT, na.rm=T)))
#View(Fin_COT)


###########################################
# Combine coastal and offshore other gear
###########################################
Fin_OTtot<-full_join(Fin_OOT, Fin_COT)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_COT_tot=sum(Catch_OOT,Catch_COT, na.rm=T),
         Effort_COT_tot=sum(Effort_OOT,Effort_COT, na.rm=T))%>%
  mutate(Catch_COT=Catch_COT_tot,
         Effort_COT=Effort_COT_tot)%>%
  select(YEAR, HYR, Catch_COT, Effort_COT)
#View(Fin_OTtot)


################################################################################
#  Coastal gear = MIS, TP_TYPE= YR
# THIS WAS PREVIOUSLY COASTAL GEAR==NA!!!
################################################################################

finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="YR")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)

Fin_COT_YR<-finland%>%
  filter(FISHERY=="C", GEAR=="MIS"| GEAR=="LLD", TP_TYPE=="YR")%>% 
  group_by(YEAR, HYR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T))) # only catch data



################################################################################
# Combine OT catches and NA catches

tmp1<-Fin_OTtot%>%
  select(-Effort_COT)%>%
  ungroup()%>%
  group_by(YEAR, .add=T)%>%
  mutate(sumC=sum(Catch_COT))%>%
  mutate(p=Catch_COT/sumC)

tmp2<-Fin_COT_YR%>%
  mutate(Catch_COT_YR=Catch_YR)%>%
  select(YEAR, Catch_COT_YR)
  
FinC_OTtot<-full_join(tmp1, tmp2, by="YEAR")%>%
  mutate(Catch_COT_YR2=Catch_COT_YR*p)%>%
  mutate(Catch_COT_HYR=Catch_COT)%>%
  mutate(Catch_COT=round(Catch_COT_HYR+Catch_COT_YR2))%>%
  select(YEAR, HYR, Catch_COT)
#View(FinC_OTtot)
  


################################################################################
# CPUE's
################################################################################
# Calculate Finnish gillnet and trapnet efforts for area 30 and area 31.
# Note that in this case area 30 needs to contain all the other subdivisions than 31,
# so that all catches and effort will be dealt (those are minor outside 30/31, but
# yet need to be included).

# include only those lines of data where both catch and effort data are known (Tapani)


finland%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR)%>%
  count(SUB_DIV)

Fin_coast_cpue<-finland%>%
  filter(FISHERY=="C", is.na(NUMB)==F, is.na(EFFORT)==F)
  
Fin_coast_cpue%>%
  group_by(GEAR)%>%
  count(SUB_DIV)


#TN
########################
# SD 30 & 31

Fin_CTN<-Fin_coast_cpue%>%
  filter(GEAR=="FYK")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_CTN30<-Fin_coast_cpue%>%
  filter(GEAR=="FYK", SUB_DIV!=31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_CTN31<-Fin_coast_cpue%>%
  filter(GEAR=="FYK", SUB_DIV==31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)



# GN
########################
# SD 30 & 31

Fin_COT<-Fin_coast_cpue%>%
  filter(GEAR=="MIS")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_COT30<-Fin_coast_cpue%>%
  filter(GEAR=="MIS", SUB_DIV!=31)%>%
  filter(is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

Fin_COT31<-Fin_coast_cpue%>%
  filter(GEAR=="MIS", SUB_DIV==31)%>%
  filter(is.na(EFFORT)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))%>%
  mutate(CPUE=Catch/Effort)

# # ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
################################################################################
# River fishery                                                                  
################################################################################
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~


Fin_R<-finland%>%
  filter(FISHERY=="R")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))%>%
  select(-HYR)
Fin_R        

