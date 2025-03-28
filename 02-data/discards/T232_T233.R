source("02-data/discards/unrep-and-discards.R")

# B-taulut, mngt uniteittain
# Dim years x sd's x sims

# T2.3.3.

# Seal damaged
B_TotSeal_res<-stats_y_k(B_TotSeal)

# Dead discards
B_TotDis_dead_res<-stats_y_k(B_TotDis_dead)
#Tämä on hyvä

# Unreported
B_TotUnrep_sea_res<-stats_y_k(B_TotUnrep_sea)
# SD 32:lla klappia viimeisimpinä vuosina

#Misreported

# River unreported
B_TotUnrep_river_res<-stats_y_k(B_TotUnrep_river)
# Ok
# Huom, 2021 estimaatti sd32 eri kuin edellisvuonna, johtuu siitä että ed. vuoden taulukossa mukaan on lipsahtanut ALV-kalat

taul<-function(input){
  df<-as.data.frame(input)
  med<-round(df[,1:2],0)
  PI<-str_c(round(df[,3],0), "-", round(df[,4],0))
  cbind(med, PI)
}

SealDamage_SD2231<-taul(B_TotSeal_res[[1]])
SealDamage_SD32<-taul(B_TotSeal_res[[2]])

Discards_SD2231<-taul(B_TotDis_dead_res[[1]])
Discards_SD32<-taul(B_TotDis_dead_res[[2]])

Unrep_SD2231<-taul(B_TotUnrep_sea_res[[1]])
Unrep_SD32<-taul(B_TotUnrep_sea_res[[2]])

RiverUnrep_SD2231<-taul(B_TotUnrep_river_res[[1]])
RiverUnrep_SD32<-taul(B_TotUnrep_river_res[[2]])


T233_SD2231<-cbind(SealDamage_SD2231, Discards_SD2231[,2:3], Unrep_SD2231[,2:3], rep(0,NumYears),RiverUnrep_SD2231[,2:3] )
colnames(T233_SD2231)<-c("", "Seal damage","","Discards","","Unreported","","Misrep", "River Unrep", "")

T233_SD32<-cbind(SealDamage_SD32, Discards_SD32[,2:3], Unrep_SD32[,2:3], rep(0,NumYears),RiverUnrep_SD32[,2:3] )
colnames(T233_SD32)<-c("", "Seal damage","","Discards","","Unreported","","Misrep", "River Unrep", "")

write_xlsx(T233_SD2231, "../../WGBAST_shared/flhm/2025/dat/der/T233_SD2231.xlsx")
write_xlsx(T233_SD32, "../../WGBAST_shared/flhm/2025/dat/der/T233_SD32.xlsx")


# Table 2.3.2
B_TotDis_dead_GND_res<-stats_y_k(B_TotDis_dead_GND)
B_TotDis_dead_LLD_res<-stats_y_k(B_TotDis_dead_LLD)
B_TotDis_dead_FYK_res<-stats_y_k(B_TotDis_dead_FYK)
B_TotDis_dead_MIS_res<-stats_y_k(B_TotDis_dead_MIS)

B_TotSeal_GND_res<-stats_y_k(B_TotSeal_GND)
B_TotSeal_LLD_res<-stats_y_k(B_TotSeal_LLD)
B_TotSeal_FYK_res<-stats_y_k(B_TotSeal_FYK)
B_TotSeal_MIS_res<-stats_y_k(B_TotSeal_MIS)


# Gulf of Bothnia + Baltic Main Basin (SD22-31)
Dis_SD2231<-round(cbind(
  B_TotDis_dead_GND_res[[1]][,1:2],
  B_TotDis_dead_LLD_res[[1]][,2],
  B_TotDis_dead_FYK_res[[1]][,2],
  B_TotDis_dead_MIS_res[[1]][,2]),0)

Seal_SD2231<-round(cbind(
  B_TotSeal_GND_res[[1]][,1:2],
  B_TotSeal_LLD_res[[1]][,2],
  B_TotSeal_FYK_res[[1]][,2],
  B_TotSeal_MIS_res[[1]][,2]),0)


BStot2231<-totSeal2231<-totDis2231<-c()
for(i in 1:NumYears){
  totDis2231[i]<-sum(Dis_SD2231[i,2:5])
  totSeal2231[i]<-sum(Seal_SD2231[i,2:5])
  BStot2231[i]<-totSeal2231[i]+totDis2231[i]
}

T232_SD2231<-cbind(Dis_SD2231, totDis2231, Seal_SD2231[,2:5], totSeal2231, BStot2231)
colnames(T232_SD2231)<-c("year", "DisGND", "DisLLD", "DisTN", "DisOT", "Total",
                         "SealGND", "SealLLD", "SealTN", "SealOT", "Total", "Grand Total")


# Gulf of Finland (SD32)
Dis_SD32<-round(cbind(
  B_TotDis_dead_GND_res[[2]][,1:2],
  B_TotDis_dead_LLD_res[[2]][,2],
  B_TotDis_dead_FYK_res[[2]][,2],
  B_TotDis_dead_MIS_res[[2]][,2]),0)

Seal_SD32<-round(cbind(
  B_TotSeal_GND_res[[2]][,2],
  B_TotSeal_LLD_res[[2]][,2],
  B_TotSeal_FYK_res[[2]][,2],
  B_TotSeal_MIS_res[[2]][,2]),0)


BStot32<-totSeal32<-totDis32<-c()
for(i in 1:NumYears){
  totDis32[i]<-sum(Dis_SD32[i,1:4])
  totSeal32[i]<-sum(Seal_SD32[i,1:4])
  BStot32[i]<-totDis32[i]+totSeal32[i]
}

T232_SD32<-cbind(Dis_SD32, totDis32, Seal_SD32, totSeal32, BStot32)
colnames(T232_SD32)<-c("year", "DisGND", "DisLLD", "DisTN", "DisOT", "Total",
                       "SealGND", "SealLLD", "SealTN", "SealOT", "Total", "Grand Total")

write_xlsx(as.data.frame(T232_SD2231), "../../WGBAST_shared/flhm/2025/dat/der/T232_SD2231.xlsx")
write_xlsx(as.data.frame(T232_SD32), "../../WGBAST_shared/flhm/2025/dat/der/T232_SD32.xlsx")

