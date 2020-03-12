## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Latvia

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


latvia<-filter(salmon, COUNTRY=="LV", FISHERY=="O")

latvia%>%count(TP_TYPE)
# Only MON/QTR

latvia%>%
  group_by(FISHERY)%>%
  count(GEAR)

################################################################################
#  Driftnetting:                                                                  
################################################################################
# Gear GND

Lat_ODN<-latvia%>%
  filter(GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))


################################################################################
#  Longlining:
################################################################################

# Effort
LatE_OLL<-latvia%>%
  filter(GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort=round(sum(EFFORT, na.rm=T)))

# Catch
LatC_OLL<-latvia%>%
  filter(GEAR!="GND" & GEAR!="AN")%>% 
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

Lat_OLL<-full_join(LatC_OLL, LatE_OLL)

