## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches for Germany

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


germany<-filter(salmon, COUNTRY=="DE")

germany%>%count(TP_TYPE)
# QTR and YR data
#View(germany)
#filter(germany, TP_TYPE=="YR") # YR data only RECR AN


germany%>%
  group_by(FISHERY)%>%
  count(GEAR)

filter(germany, FISHERY=="O")
filter(germany, GEAR=="GND")

################################################################################
#  Offshore catches                                                                  
################################################################################
# All german fishery goes to LLD (no effort)

Ger_OLL<-germany%>%
  filter(GEAR!="AN")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=sum(NUMB, na.rm=T))

