
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




