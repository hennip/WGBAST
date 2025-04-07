

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"W"
source("02-data/discards/unrep-and-discards.R")


# Dead catch, including recr and river catches

med_Tcatch<-round(med_Tcatch,0)
#med_Tcatch[NumYears]

# River
med_river[,1]

# Discards, Unrep , misrep
med_dis[,1]
med_unrep[,1]
B_TotMisr_sea[,1]

# Landings: commercial and recreational at sea and in rivers
Landings_total<-round(B_TotRepCom_sea[,1]+B_TotRecr_sea[,1]+med_river[,1]+
                med_unrep[,1]+B_TotMisr_sea[,1],0)

Landings_total

# share of nominal landings
nominal_landings_p<-round((B_TotRepCom_sea[,1]+B_TotRecr_sea[,1]+med_river[,1])/Landings_total,3)

# share of unreported and misreported
unrep_misrep_p<-round((med_unrep[,1]+B_TotMisr_sea[,1])/Landings_total,3)

T9<-cbind(c(2001:(2000+NumYears)), med_Tcatch[,1],Landings_total, nominal_landings_p*100,
      unrep_misrep_p*100, round(med_dead_dis[,1],1))

cbind(c(2001:(2000+NumYears)),med_dead_dis)

colnames(T9)<-c("Year", "Total catch", "Landings",
              "Nom landings (%)", "unrep&misrep (%)", "Dead discards")
T9
write_xlsx(as.data.frame(T9), "../../WGBAST_shared/flhm/2025/dat/der/Advice2231_T9.xlsx")
