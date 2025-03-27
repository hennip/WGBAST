
dim(Rconv)
Rconv_res<-median_y_c(Rconv);Rconv_res


Cunrep_res<-median_y_c_k(Cunrep);Cunrep_res
Ounrep_res<-median_y_c_k(Ounrep);Ounrep_res
Runrep_res<-median_y_c_k(Runrep);Runrep_res

Tcatch_res<-median_y_c_k(Tcatch); Tcatch_res
Tseal_res<-median_y_c_k(Tseal); Tseal_res
Tdis_res<-median_y_c_k(Tdis);Tdis_res
Tdis_alive_res<-median_y_c_k(Tdis_alive);Tdis_alive_res

dim(Tunrep_T2)
Tunrep_T2_res<-median_y_c_k(Tunrep_T2); Tunrep_T2_res

dim(Dis_LLD_alive)
Dis_LLD_alive_res<-median_y_c_k(Dis_LLD_alive);Dis_LLD_alive_res
Dis_GND_alive_res<-median_y_c_k(Dis_GND_alive);Dis_GND_alive_res
Dis_FYK_alive_res<-median_y_c_k(Dis_FYK_alive);Dis_FYK_alive_res


Dis_LLD_dead_res<-median_y_c_k(Dis_LLD_dead);Dis_LLD_dead_res
Dis_GND_dead_res<-median_y_c_k(Dis_GND_dead);Dis_GND_dead_res
Dis_FYK_dead_res<-median_y_c_k(Dis_FYK_dead);Dis_FYK_dead_res

# Dis_LLD_dead ok
#Tdis[i,j,k,]<-  Dis_LLD_dead[i,j,k,] + Dis_GND_dead[i,j,k,] + Dis_FYK_dead[i,j,k,] + Dis_MIS_dead[i,j,k,]   	#Total discards by year 		

Seal_LLD_res<-median_y_c_k(Seal_LLD);Seal_LLD_res
Seal_FYK_res<-median_y_c_k(Seal_FYK)

DisLL_res<-median_y_c(DisLL)

# T 2.2.1.1: Painot
# T 2.2.1.2: Kappaleet
#Estimated unreported catch		Estimated discarded catch		Total catch	

# Nämä eivät tarvitse funktiota:
B_TotRepCom_sea
B_TotRecr_sea
B_TotMisr_sea


# A-taulut: Koko itämeri
# Dim years x sims
dim(A_TotCatch_BS)
dim(A_TotDis_BS)
dim(A_TotUnrep_BS)
dim(A_TotSeal_BS)
A_TotUnrep_BS_res<-stats_y(A_TotUnrep_BS);round(A_TotUnrep_BS_res,0)
A_TotDis_BS_res<-stats_y(A_TotDis_BS);round(A_TotDis_BS_res,0)
A_TotCatch_BS_res<-stats_y(A_TotCatch_BS);round(A_TotCatch_BS_res,0)

B_TotMisr_sea

A_TotSeal_BS_res<-stats_y(A_TotSeal_BS)
A_TotCatch_BS_res
A_TotDis_BS_res
A_TotUnrep_BS_res
A_TotSeal_BS_res

round(A_TotUnrep_BS_res,0)
round(A_TotCatch_BS_res,0)

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





dim(B_TotDis_alive)
dim(B_TotDis_LLD_alive)
B_TotDis_alive_res<-stats_y_k(B_TotDis_alive)
B_TotDis_LLD_alive_res<-stats_y_k(B_TotDis_LLD_alive)
B_TotDis_FYK_alive_res<-stats_y_k(B_TotDis_FYK_alive)
B_TotDis_GND_alive_res<-stats_y_k(B_TotDis_GND_alive)
B_TotDis_alive_res
B_TotDis_LLD_alive_res
B_TotDis_FYK_alive_res
B_TotDis_GND_alive_res



B_TotRiver_res<-stats_y_k(B_TotRiver)
B_TotUnrepDis_sea_res<-stats_y_k(B_TotUnrepDis_sea)
B_TotCatch_sea_res<-stats_y_k(B_TotCatch_sea)
B_TotCatchCom_sea_res<-stats_y_k(B_TotCatchCom_sea)
B_TotUnrep_river_res<-stats_y_k(B_TotUnrep_river)
B_TotDisSeal_MU_res<-stats_y_k(B_TotDisSeal_MU)
B_TotUnrep_MU_res<-stats_y_k(B_TotUnrep_MU)
B_TotCatch_MU_res<-stats_y_k(B_TotCatch_MU)
B_TotDis_dead_GND_res<-stats_y_k(B_TotDis_dead_GND)
B_TotDis_dead_LLD_res<-stats_y_k(B_TotDis_dead_LLD)
B_TotDis_dead_FYK_res<-stats_y_k(B_TotDis_dead_FYK)
B_TotDis_dead_MIS_res<-stats_y_k(B_TotDis_dead_MIS)
B_TotSeal_GND_res<-stats_y_k(B_TotSeal_GND)
B_TotSeal_LLD_res<-stats_y_k(B_TotSeal_LLD)
B_TotSeal_FYK_res<-stats_y_k(B_TotSeal_FYK)
B_TotSeal_MIS_res<-stats_y_k(B_TotSeal_MIS)




