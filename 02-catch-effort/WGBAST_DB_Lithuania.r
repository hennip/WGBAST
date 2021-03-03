## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Lithuania

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


lithuania<-filter(salmon, COUNTRY=="LT", FISHERY=="O" | FISHERY=="C")

lithuania%>%count(TP_TYPE)
# MON/QTR/YR

lithuania%>%
  group_by(FISHERY)%>%
  count(GEAR)

################################################################################
#  Driftnetting:                                                                  
################################################################################
# Gear GND

Lit_ODN<-lithuania%>%
  filter(GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T),
            Effort=sum(EFFORT, na.rm=T))


################################################################################
#  Longlining:
################################################################################

#lithuania%>%
#  filter(GEAR!="GND" & GEAR!="AN")%>%
#  filter(is.na(HYR)==T)

# Effort
LitE_OLL<-lithuania%>%
  filter(GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort=round(sum(EFFORT, na.rm=T)))

# Catch
LitC_OLL<-lithuania%>%
  filter(GEAR!="GND" & GEAR!="AN")%>% 
  group_by(YEAR, HYR)%>%
  mutate(HYR=ifelse(is.na(HYR)==T, 1, HYR))%>% # 2008 catch data missing HYR information, set HYR as 1
  summarise(Catch=round(sum(NUMB, na.rm=T)))

Lit_OLL<-full_join(LitC_OLL, LitE_OLL)

#filter(Lit_OLL, YEAR>2016)%>%
#  mutate(cpue=Catch/Effort)

