## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Denmark

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


denmark<-filter(salmon, COUNTRY=="DK")

denmark%>%count(TP_TYPE)

denmark%>%
  group_by(FISHERY, TP_TYPE)%>%
  count(GEAR)

filter(denmark, TP_TYPE=="YR", F_TYPE!="RECR", GEAR!="AN") # All YR data is RECR AN O

# Do not take into account RECR AN at Offshore, that will be calculated separately!

################################################################################
#  Driftnetting:                                                                  
################################################################################

# Gear GND, only HYR/MON data


Den_ODN<-denmark%>%
  filter(GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))

################################################################################
#  Longlining:
################################################################################

# Effort
DenE_OLL<-denmark%>%
  filter(GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
    summarise(Effort=round(sum(EFFORT, na.rm=T)))

# Catch
DenC_OLL<-denmark%>%
  filter(GEAR!="GND" & GEAR!="AN")%>% 
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

# denmark%>%filter(GEAR!="LLD" & GEAR!="GND")%>%count(GEAR)
# 
# tmp1<-DenC_OLL%>%filter(is.na(HYR)==F)%>% # Not AN
#   mutate(sumC=sum(Catch))%>%
#   group_by(YEAR, add=T)%>%
#   mutate(p=Catch/sumC)
# 
# tmp2<-DenC_OLL%>%filter(is.na(HYR)==T)%>% #AN
#   mutate(AN=Catch)%>%
#   select(YEAR, AN)
# 
# DenC_OLL<-full_join(tmp1, tmp2, by="YEAR")%>%
#   mutate(Catch_AN=AN*p)%>%
#   mutate(Catch2=Catch)%>%
#   mutate(Catch=round(Catch_AN+Catch2))%>%
#   select(YEAR, HYR, Catch)

Den_OLL<-full_join(DenC_OLL, DenE_OLL)

