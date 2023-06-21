## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Sweden

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


sweden<-filter(salmon, COUNTRY=="SE")

sweden%>%
  group_by(FISHERY)%>%
  count(TP_TYPE)
# YR data in coastal and river fisheries

################################################################################
#  Offshore driftnetting:                                                                  
################################################################################

sweden%>%
  filter(FISHERY=="O")%>%
  group_by(GEAR)%>%
  count(TP_TYPE)
# YR MIS & AN is minor -> goes to OLL

Swe_ODN<-sweden%>%
  filter(FISHERY=="O", GEAR=="GND")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)))


################################################################################
#  Offshore longlining:
################################################################################
# 2008-> Pool all sea catch with longline, divide it for half years based on LL proportions

sweden%>%
  filter(FISHERY=="O" , (GEAR=="LLD"| GEAR=="MIS" ),#| GEAR=="AN"), 
         TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)

sweden%>%
  filter(FISHERY=="C" , SUB_DIV<30,
         TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)

#Effort
SweE_OLL<-sweden%>%
  filter(FISHERY=="O", GEAR=="LLD")%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort=round(sum(EFFORT, na.rm=T)))


# Catch

Swe_sea<-sweden%>%
  filter((FISHERY=="O" & (GEAR=="LLD"| GEAR=="MIS"))|# | GEAR=="AN")) | 
        (FISHERY=="C" & SUB_DIV<30))

Swe_sea%>%count(TP_TYPE) # MON, HYR & YR!

filter(Swe_sea, YEAR>2008)%>%group_by(SUB_DIV)%>%count(GEAR)

tmp1<-Swe_sea%>%
  filter(TP_TYPE!="YR")%>% 
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_sea%>%
  filter(TP_TYPE=="YR")%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))
  
SweC_OLL<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)

Swe_OLL<-full_join(SweC_OLL, SweE_OLL)
#View(Swe_OLL)

################################################################################
################################################################################
################################################################################
# Coastal fisheries


sweden%>%
  filter(FISHERY=="C")%>%
  group_by(GEAR)%>%
  group_by(SUB_DIV)%>%
  count(TP_TYPE)

sweden%>%
  filter(FISHERY=="C" , SUB_DIV>29,
         TP_TYPE=="YR", is.na(EFFORT)==F, EFFORT!=0)
# 4 rows of RECR FYK with effort
# But coastal swe effort is largely missing anyway

#View(sweden%>%
#       filter(FISHERY=="C" , SUB_DIV>29))


################################################################################
#  Coastal TN & GN catches
################################################################################

Swe_coast<-sweden%>%filter(FISHERY=="C", SUB_DIV>29)

Swe_coast%>%
  group_by(F_TYPE)%>%
  count(GEAR)

Swe_coast%>%
  group_by(F_TYPE, GEAR)%>%
  count(TP_TYPE)
# All COMM is HYR/MON, RECR is all YR


#  TN
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE!="YR")%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR")%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)

# OT
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE!="YR")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

# No YR OT data!
#tmp2<-Swe_coast%>%
#  filter(GEAR!="FYK",TP_TYPE=="YR")%>%
#  group_by(YEAR)%>%
#  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

#SweC_COT<-
#  full_join(tmp1, tmp2)%>%
#  group_by(YEAR, HYR)%>%
#  mutate(Catch_YR2=Catch_YR*p)%>%
#  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
#  select(YEAR, HYR, Catch)

SweC_COT<-tmp1%>%
  mutate(Catch=Catch_HYR)%>%
  select(YEAR, HYR, Catch)


####################
# Effort
####################

# 2013-> account for real swedish data
SweE_CTN_rep<-filter(Swe_coast, YEAR>2012)

filter(SweE_CTN_rep, is.na(EFFORT)==T)%>%
  group_by(YEAR)%>%
  count(GEAR)

filter(SweE_CTN_rep, is.na(EFFORT)==F)%>%
  group_by(YEAR)%>%
  count(GEAR)

# Small amount (=small catch) of RECR FYK EFFORT missing in 2015 -> ignore
filter(SweE_CTN_rep, YEAR==2015, GEAR=="FYK")

SweE_CTN_rep<-SweE_CTN_rep%>%
  filter(GEAR=="FYK", is.na(EFFORT)==F)%>%
    group_by(YEAR, SUB_DIV)%>%
    summarise(Effort_rep=round(sum(EFFORT)))

SweE_CTN_rep30<-filter(SweE_CTN_rep, SUB_DIV==30)%>%select(-SUB_DIV)
SweE_CTN_rep31<-filter(SweE_CTN_rep, SUB_DIV==31)%>%select(-SUB_DIV)


################################################################################
# Effort for trapnet and other gear
################################################################################
# 3.3.2010: Lars: Commercial Swedish trapnet cpue is 20% smaller than
# the Finnish commercial trapnet cpue, and recreational swedish 
# trapnet cpue is 20% smaller than the swedish commercial trapnet cpue.

# TN
Fin_CTN_CPUE<-Fin_CTN%>%select(YEAR, HYR, CPUE)
SweC_CTN_COMM<-SweC_CTN%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN_RECR<-SweC_CTN%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN_COMM<-full_join(SweC_CTN_COMM, Fin_CTN_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN_RECR<-full_join(Swe_CTN_COMM, SweC_CTN_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN<-full_join(select(Swe_CTN_COMM, YEAR, HYR, Effort_COMM),
                    select(Swe_CTN_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%
  select(YEAR, HYR, Effort)

Swe_CTN<-full_join(SweC_CTN, SweE_CTN)%>%
  mutate(Catch=Catch_TOT)%>%
  select(YEAR, HYR, Catch, Effort)

# GN
Fin_COT_CPUE<-Fin_COT%>%select(YEAR, HYR, CPUE)

Swe_COT<-full_join(Fin_COT_CPUE,SweC_COT)%>%
  mutate(Effort=Catch/CPUE)


################################################################################
# Calculate Swedish gillnet and trapnet efforts for area 30 and area 31.

Swe_coast%>%group_by(F_TYPE, TP_TYPE, GEAR)%>%
  count(SUB_DIV)


#  TN 30
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE!="YR", SUB_DIV==30)%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==30)%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN30<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)

# TN 31
tmp1<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE!="YR", SUB_DIV==31)%>% # All COMM!
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR=="FYK",TP_TYPE=="YR", SUB_DIV==31)%>% # All RECR!
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_CTN31<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_RECR=round(Catch_YR*p))%>%
  mutate(Catch_COMM=Catch_HYR)%>%
  mutate(Catch_TOT=round(sum(Catch_COMM,Catch_RECR, na.rm=T)))%>%
  select(YEAR, HYR, Catch_TOT, Catch_RECR, Catch_COMM)


# GN 30
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE!="YR", SUB_DIV==30)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE=="YR", SUB_DIV==30)%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_COT30<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)

#View(SweC_COT31)

# GN 31
tmp1<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE!="YR", SUB_DIV==31)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_HYR=round(sum(NUMB, na.rm=T)))%>%
  mutate(sumC=sum(Catch_HYR))%>%
  group_by(YEAR, .add=T)%>%
  mutate(p=Catch_HYR/sumC)

tmp2<-Swe_coast%>%
  filter(GEAR!="FYK",TP_TYPE=="YR", SUB_DIV==31)%>%
  group_by(YEAR)%>%
  summarise(Catch_YR=round(sum(NUMB, na.rm=T)))

SweC_COT31<-
  full_join(tmp1, tmp2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_YR2=Catch_YR*p)%>%
  mutate(Catch=round(sum(Catch_HYR,Catch_YR2, na.rm=T)))%>%
  select(YEAR, HYR, Catch)


# Calculate Swe effort with Swe catch & Fin CPUE

# TN 30
Fin_CTN30_CPUE<-Fin_CTN30%>%select(YEAR, HYR, CPUE)
SweC_CTN30_COMM<-SweC_CTN30%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN30_RECR<-SweC_CTN30%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN30_COMM<-full_join(SweC_CTN30_COMM, Fin_CTN30_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN30_RECR<-full_join(Swe_CTN30_COMM, SweC_CTN30_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN30<-full_join(select(Swe_CTN30_COMM, YEAR, HYR, Effort_COMM),
                    select(Swe_CTN30_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%select(YEAR, HYR, Effort)

Swe_CTN30<-full_join(SweC_CTN30, SweE_CTN30)%>%
  mutate(Catch=Catch_TOT)%>%select(YEAR, HYR, Catch, Effort)

# TN 31
Fin_CTN31_CPUE<-Fin_CTN31%>%select(YEAR, HYR, CPUE)
SweC_CTN31_COMM<-SweC_CTN31%>%select(YEAR, HYR, Catch_COMM)
SweC_CTN31_RECR<-SweC_CTN31%>%select(YEAR, HYR, Catch_RECR)

Swe_CTN31_COMM<-full_join(SweC_CTN31_COMM, Fin_CTN31_CPUE, by=c("YEAR", "HYR"))%>%
  mutate(Effort_COMM=round(Catch_COMM/(CPUE*0.8)))

Swe_CTN31_RECR<-full_join(Swe_CTN31_COMM, SweC_CTN31_RECR)%>%
  mutate(Effort_RECR=round(Catch_RECR/(Catch_COMM/Effort_COMM*0.8)))

SweE_CTN31<-full_join(select(Swe_CTN31_COMM, YEAR, HYR, Effort_COMM),
                      select(Swe_CTN31_RECR, YEAR, HYR, Effort_RECR))%>%
  mutate(Effort=Effort_COMM+Effort_RECR)%>%select(YEAR, HYR, Effort)

Swe_CTN31<-full_join(SweC_CTN31, SweE_CTN31)%>%
  mutate(Catch=Catch_TOT)%>%select(YEAR, HYR, Catch, Effort)


# GN 30
Fin_COT30_CPUE<-Fin_COT30%>%select(YEAR, HYR, CPUE)
Swe_COT30<-full_join(Fin_COT30_CPUE,SweC_COT30)%>%
  mutate(Effort=ifelse(is.na(Catch)==T, 0, round(Catch/CPUE)))
#View(Swe_COT30)

# GN 31
Fin_COT31_CPUE<-Fin_COT31%>%select(YEAR, HYR, CPUE)
Swe_COT31<-full_join(Fin_COT31_CPUE,SweC_COT31)%>%
  mutate(Effort=ifelse(is.na(Catch)==T, 0, round(Catch/CPUE)))

################################################################################
# River


Swe_R<-sweden%>%
  filter(FISHERY=="R")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))%>%
  select(-HYR)
Swe_R      

