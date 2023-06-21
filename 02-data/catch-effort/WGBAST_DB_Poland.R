## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 Calculate catches and efforts for Poland

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#salmon<-df%>%filter(SPECIES=="SAL", SUB_DIV!=32, F_TYPE!="DISC", F_TYPE!="SEAL")

SalCoef<-0.97 # Assumed proportion of salmon in offshore sal+trs catch. Use 2009->

df2%>%count(F_TYPE)

# df2 contains both SAL and TRS
poland_offs<-df2%>%filter(FISHERY=="O" , COUNTRY=="PL")
poland_coast<-df2%>%filter(( (FISHERY=="C" & SPECIES=="SAL")), COUNTRY=="PL")

poland_coast%>%
  count(GEAR)

poland_offs%>%
  count(GEAR)

poland_C<-full_join(poland_coast, poland_offs)

mis<-poland_C%>%
  filter(GEAR=="MIS")%>%
  group_by(YEAR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)),
            CPUE=Catch/Effort)
#View(mis)

gns<-poland_C%>%
  filter(GEAR=="GNS")%>%
  group_by(YEAR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)),
            Effort=round(sum(EFFORT, na.rm=T)),
            CPUE=Catch/Effort)
#View(gns)


poland_E_sal<-df2%>%filter(SPECIES=="SAL", COUNTRY=="PL" & FISHERY=="O")
poland_E_trs<-df2%>%filter(SPECIES=="TRS", COUNTRY=="PL" & FISHERY=="O")
                           
poland_E_sal%>%
  filter(YEAR==2017)%>%
  count(GEAR)


poland_E_trs%>%
  filter(YEAR==2017)%>%
  count(GEAR)


################################################################################
#  Catches based on reported SAL+TRS:                                                                 
################################################################################

# Put coastal catch to LL, same with offshore GEAR!=GND

#ODN
tmp1<-poland_C%>%
  filter((FISHERY=="O" & GEAR=="GND"))%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

tmp2<-filter(tmp1, is.na(HYR)==T)%>%
  select(-HYR)

p_ODN<-select(pHYR1_ODN, YEAR, pC)

HYR1<-full_join(tmp2, p_ODN)%>%
  mutate(CatchNA=pC*Catch, HYR=1)%>%
  select(YEAR, HYR, CatchNA)
  
HYR2<-full_join(tmp2, p_ODN)%>%
    mutate(CatchNA=(1-pC)*Catch, HYR=2)%>%
  select(YEAR, HYR, CatchNA)

tmp1<-filter(tmp1, is.na(HYR)==F)

PolC_ODN_rep<-full_join(HYR1,HYR2)%>%
  full_join(tmp1)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_tot= sum(CatchNA, Catch, na.rm=T))%>%
  mutate(Catch=round(Catch_tot))%>%
  select(YEAR, HYR, Catch)


#OLL
poland_C%>%filter(FISHERY=="O")%>%
  count(GEAR)

tmp1<-poland_C%>%
  filter(FISHERY=="O" & GEAR!="GND" & GEAR!="AN")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

tmp2<-filter(tmp1, is.na(HYR)==T)%>%
  select(-HYR)

p_OLL<-select(pHYR1_OLL, YEAR, pC)

HYR1<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=pC*Catch, HYR=1)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

HYR2<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=(1-pC)*Catch, HYR=2)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

tmp1<-filter(tmp1, is.na(HYR)==F)

PolC_OLL_rep<-full_join(HYR1,HYR2)%>%
  full_join(tmp1)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_tot= sum(CatchNA, Catch, na.rm=T))%>%
  mutate(Catch=round(Catch_tot))%>%
  mutate(Catch=round(ifelse(YEAR>2008, Catch*SalCoef, Catch)))%>% # Use 0.97 coef!
  select(YEAR, HYR, Catch)


# Coastal catch
tmp1<-poland_C%>%
  filter(FISHERY=="C")%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

tmp2<-filter(tmp1, is.na(HYR)==T)%>%
  select(-HYR)

p_OLL<-select(pHYR1_OLL, YEAR, pC)

HYR1<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=pC*Catch, HYR=1)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

HYR2<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=(1-pC)*Catch, HYR=2)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

tmp1<-filter(tmp1, is.na(HYR)==F)

PolC_coast<-full_join(HYR1,HYR2)%>%
  full_join(tmp1)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_tot= sum(CatchNA, Catch, na.rm=T))%>%
  mutate(Catch=round(Catch_tot))%>%
  select(YEAR, HYR, Catch)



################################################################################
#  Efforts:                                                                 
################################################################################

#ODN

Etrs<-poland_E_trs%>%
  filter((FISHERY=="O" & GEAR=="GND"))%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_trs=round(sum(EFFORT, na.rm=T)))%>%
  filter(HYR==1 | is.na(HYR)==T) # remove HYR 2, this doesn't matter since trsE is small in 01-02 -> salE will be used insted

Esal<-poland_E_sal%>%
  filter((FISHERY=="O" & GEAR=="GND"))%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_sal=round(sum(EFFORT, na.rm=T)))

tmp<-full_join(Etrs, Esal)%>%
  mutate(Effort=max(Effort_trs,Effort_sal, na.rm=T))%>%
  select(YEAR, Effort)

p_ODN<-select(pHYR1_ODN, YEAR, pE)

HYR1<-full_join(tmp, p_ODN)%>%
  mutate(EffortNA=pE*Effort, HYR=1)%>%
  select(YEAR, HYR, EffortNA)%>%
  filter(is.na(EffortNA)==F)

HYR2<-full_join(tmp, p_ODN)%>%
  mutate(EffortNA=(1-pE)*Effort, HYR=2)%>%
  select(YEAR, HYR, EffortNA)%>%
  filter(is.na(EffortNA)==F)

PolE_ODN<-full_join(HYR1,HYR2)%>%
  group_by(YEAR, HYR)%>%
  mutate(Effort= round(sum(EffortNA, na.rm=T)))%>%
  select(YEAR, HYR, Effort)

#OLL

poland_E_sal%>%
  filter(GEAR=="LLD")%>%
  group_by(TP_TYPE)%>%
  count(YEAR)

poland_E_trs%>%
  filter(GEAR=="LLD")%>%
  group_by(TP_TYPE)%>%
  count(YEAR)

# Before 2009: YR data. Choose max(E_trs,E_sal) and divide between HYRs

Etrs<-poland_E_trs%>%
  filter(FISHERY=="O", GEAR=="LLD", YEAR<2009, YEAR>2003)%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_trs=round(sum(EFFORT, na.rm=T)))%>%
  select(-HYR)

Esal<-poland_E_sal%>%
  filter(FISHERY=="O", GEAR=="LLD", YEAR<2009)%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_sal=round(sum(EFFORT, na.rm=T)))%>%
  select(-HYR)

tmp<-full_join(Etrs, Esal)%>%
  mutate(Effort=max(Effort_trs,Effort_sal, na.rm=T))%>%
  select(YEAR, Effort)

p_OLL<-select(pHYR1_OLL, YEAR, pE)%>%
  filter(YEAR<2009)

HYR1<-full_join(tmp,p_OLL)%>%
  mutate(Effort=round(pE*Effort), HYR=1)%>%
  select(YEAR, HYR, Effort)
  
HYR2<-full_join(tmp,p_OLL)%>%
  mutate(Effort=round((1-pE)*Effort), HYR=2)%>%
  select(YEAR, HYR, Effort)
  
PolE_OLL_1<-full_join(HYR1,HYR2)

# 2009-> MON data. Choose max(E_trs,E_sal) and divide between HYRs


Etrs<-poland_E_trs%>%
  filter(FISHERY=="O", GEAR=="LLD", YEAR>2008)%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_trs=round(sum(EFFORT, na.rm=T)))

Esal<-poland_E_sal%>%
  filter(FISHERY=="O", GEAR=="LLD", YEAR>2008)%>%
  group_by(YEAR, HYR)%>%
  summarise(Effort_sal=round(sum(EFFORT, na.rm=T)))

PolE_OLL_2<-full_join(Etrs, Esal)%>%
  group_by(YEAR,HYR)%>%
  mutate(Effort=max(Effort_trs,Effort_sal, na.rm=T))%>%
  select(YEAR, HYR, Effort)

PolE_OLL<-full_join(PolE_OLL_1, PolE_OLL_2)


################################################################################
# To calculate Polish catches with Polish effort and CPUE from other countries:


PolC_ODN_est<-full_join(PolE_ODN, select(ODN_CPUE, YEAR, HYR, CPUE_tot))%>%
  mutate(Catch=round(CPUE_tot*Effort*Feff))

PolC_OLL_est<-full_join(PolE_OLL, select(OLL_CPUE, YEAR, HYR, CPUE_tot))%>%
  mutate(Catch=round(CPUE_tot*Effort*Feff))

 #View(PolC_OLL_est)


# We can also calculate GNS_est with GND CPUE, but it seems this will result with
# lower catch than reported -> can well be that Polish GNS fishery is more effective (fishing only
# on best days) than average GND fishery by other countries

# ODN_CPUE%>%
#   summarise(E=sum(Effort_tot), C=sum(Catch_tot))%>%
#   mutate(CPUE=C/E)%>%
#   filter(YEAR>2002)%>%
#   summarise(mu=mean(CPUE))
# # 0.145 : mean CPUE in 2003-2007 for GDN -> use for GNS
# 
# 
# Etrs<-poland_E_trs%>%
#   filter(FISHERY=="S", GEAR=="GNS", YEAR>2008)%>%
#   group_by(YEAR, HYR)%>%
#   summarise(Effort_trs=round(sum(EFFORT, na.rm=T)))
# 
# Esal<-poland_E_sal%>%
#   filter(FISHERY=="S", GEAR=="GNS", YEAR>2008)%>%
#   group_by(YEAR, HYR)%>%
#   summarise(Effort_sal=round(sum(EFFORT, na.rm=T)))
# 
# PolE_GNS<-full_join(Etrs, Esal)%>%
#   group_by(YEAR,HYR)%>%
#   mutate(Effort=max(Effort_trs,Effort_sal))%>%
#   select(YEAR, HYR, Effort)
# 
# PolC_GNS_est<-PolE_GNS%>%
#   mutate(Catch=round(0.145*Effort*Feff))
# 


# Reported catch in coast and offshore other than ODN/OLL

O_OT<-df2%>%filter(FISHERY=="O", GEAR!="LLD", GEAR!="GND", COUNTRY=="PL")

tmp1<-full_join(O_OT, poland_coast)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch=round(sum(NUMB, na.rm=T)))

tmp2<-filter(tmp1, is.na(HYR)==T)%>%
  select(-HYR)

p_OLL<-select(pHYR1_OLL, YEAR, pC)

HYR1<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=pC*Catch, HYR=1)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

HYR2<-full_join(tmp2, p_OLL)%>%
  mutate(CatchNA=(1-pC)*Catch, HYR=2)%>%
  select(YEAR, HYR, CatchNA)%>%
  filter(is.na(CatchNA)==F)

tmp1<-filter(tmp1, is.na(HYR)==F)

# Note that SalCoef takes off also 3% of reported salmon catch at coast. However this is
# such a small error that let's leave it as such.
PolC_OT_rep<-full_join(HYR1,HYR2)%>%
  full_join(tmp1)%>%
  group_by(YEAR, HYR)%>%
  mutate(Catch_tot= sum(CatchNA, Catch, na.rm=T))%>%
  mutate(Catch=round(Catch_tot))%>%
  mutate(Catch=round(ifelse(YEAR>2008, Catch*SalCoef, Catch)))%>% # Use 0.97 coef!
  select(YEAR, HYR, Catch)

#View(PolC_OT_rep)
  

# PL reported salmon catch (salmon only!)
######################################
poland_C_rep<-filter(poland_C, SPECIES=="SAL")  

PolC_repSAL_year<-poland_C_rep%>%
  group_by(YEAR)%>%
  summarise(Catch_rep=round(sum(NUMB, na.rm=T)))

# PL reported salmon + trout catch * SalCoef : WGBAST method
######################################

PolC_rep<-full_join(PolC_ODN_rep, PolC_OLL_rep)%>%
  full_join(PolC_coast)%>%
  summarise(Catch=sum(Catch))

PolC_rep_year<-PolC_rep%>%
  summarise(Catch_est2=round(sum(Catch)))

# PL estimated catch based on PL effort & Feff*CPUE[other countries]: FULL MISREP method
######################################
PolC_est_year<-full_join(PolC_OLL_est,PolC_OT_rep)%>%
  full_join(PolC_ODN_est)%>%
  filter(is.na(Catch)==F)%>%
  group_by(YEAR, HYR)%>%
  summarise(Catch_tot_est=sum(Catch))%>%
  summarise(Catch_est=sum(Catch_tot_est))


# Compare catches with different estimation methods
######################################

PolC<-full_join(PolC_est_year, PolC_repSAL_year)%>%
  full_join(PolC_rep_year)%>%
  mutate(misrep=Catch_est-Catch_rep,
         prop=round(misrep/Catch_rep,1),
         misrep2=Catch_est2-Catch_rep,
         prop2=round(misrep2/Catch_rep,1))%>%
  select(YEAR,Catch_rep, Catch_est2, Catch_est, misrep2,misrep, prop2, prop)
#View(PolC)

