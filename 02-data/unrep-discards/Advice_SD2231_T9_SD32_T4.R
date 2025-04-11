

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"W"
source("02-data/unrep-discards/unrep-and-discards.R")

med_dead_catch<-array(NA, dim=c(NumYears,2))
nom_landings<-array(NA, dim=c(NumYears,2))
unrep_misrep<-array(NA, dim=c(NumYears,2, Nsim))
med_unrep_misrep<-array(NA, dim=c(NumYears,2))
med_disseal<-array(NA, dim=c(NumYears,2))

for(i in 1: NumYears){
  for(k in 1:2){
  # Dead catch, including non-commercial and river catches) 
  # B_TotCatch_sea includes reported comm and recr sea catch, Ounrep, Cunrep, misrep, discards and seal damaged
  # B_TotRiver includes reported and unreported river catch
  med_dead_catch[i,k]<-median((Bs_TotCatch_sea+Bs_TotRiver)[i,k,])

  # Nominal landings (commercial and recreational at sea and in rivers)
  nom_landings[i,k]<-sum(TRiver[i,1:9,k])+Bd_TotRepCom_sea[i,k]+Bd_TotRecr_sea[i,k] # Data, no uncertainty!
  
  # Unreported and misreported
  for(s in 1:Nsim){
    unrep_misrep[i,k,s]<-Bs_TotUnrep[i,k,s]+Bd_TotMisr_sea[i,k]
  }
  med_unrep_misrep[i,k]<-median(unrep_misrep[i,k,])

  # Dead discards (including seal damaged)
  med_disseal[i,k]<-median(Bs_TotDisSeal[i,k,])
  
  }}

med_dead_catch

landings_tot<-nom_landings+med_unrep_misrep

p_landings<-nom_landings/landings_tot
p_unrep_misrep<-med_unrep_misrep/landings_tot

tbl1<-cbind(round(med_dead_catch[,1],0), round(landings_tot[,1],0), round(p_landings[,1],3)*100, round(p_unrep_misrep[,1],3)*100, round(med_disseal[,1],0))
tbl2<-cbind(round(med_dead_catch[,2],0), round(landings_tot[,2],0), round(p_landings[,2],3)*100, round(p_unrep_misrep[,2],3)*100, round(med_disseal[,2],0))

T9<-cbind(c(2001:(2000+NumYears)), tbl1)
colnames(T9)<-c("Year", "Total catch", "Landings",
              "Nom_landings (%)", "unrep&misrep (%)", "Dead_discards")
T9
write_xlsx(as.data.frame(T9), "../../WGBAST_shared/flhm/2025/dat/der/Advice_SD2231_T9.xlsx")

T4<-cbind(c(2001:(2000+NumYears)), tbl2)
colnames(T4)<-c("Year", "Total catch", "Landings",
                "Nom_landings (%)", "unrep&misrep (%)", "Dead_discards")
T4
write_xlsx(as.data.frame(T4), "../../WGBAST_shared/flhm/2025/dat/der/Advice_SD32_T4.xlsx")



