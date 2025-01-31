stats_y<-function(variable){
  q5<-q50<-q95<-c()
  for(y in 1:23){
#    for(i in 1:dim(variable)[2]){ # loop over all monitored variables
      q50[y]<-quantile(variable[y,],0.5)
      q5[y]<-quantile(variable[y,],0.05)
      q95[y]<-quantile(variable[y,],0.95)
      
    }#}
  
  return(cbind(c(2001:2023),q50,q5,q95))
}



median_y_c<-function(variable){
    q50<-array(NA, dim=c(23,9))
  for(y in 1:23){
    for(j in 1:9){
      q50[y,j]<-quantile(variable[y,j,],0.5)
      }}
  
  l1<-cbind(c(2001:2023),round(q50,4))
  
  colnames(l1)<-c("year","FI","SE","DK","PL","LV","LT","DE","EE","RU")
  return(l1)
}



median_y_c_k<-function(variable){
q50<-array(NA, dim=c(23,9,2))
#variable<-Tdis
  for(y in 1:23){
    for(j in 1:9){
      for(k in 1:2){
#      for(i in 1:dim(variable)[4]){ # loop over all monitored variables
        q50[y,j,k]<-quantile(variable[y,j,k,],0.5)
        
      }}}#}

  l1<-cbind(c(2001:2023),round(q50[,,1],0))
  l2<-cbind(c(2001:2023),round(q50[,,2],0))

  colnames(l1)<-colnames(l2)<-c("year","FI","SE","DK","PL","LV","LT","DE","EE","RU")
  return(list(l1, l2))
}


stats_y_k<-function(variable){
  q5<-q50<-q95<-array(NA, dim=c(23,2))
  for(y in 1:23){
    for(k in 1:2){
     # for(i in 1:dim(variable)[3]){ # loop over all monitored variables
        q5[y,k]<-quantile(variable[y,k,],0.05)
        q50[y,k]<-quantile(variable[y,k,],0.5)
        q95[y,k]<-quantile(variable[y,k,],0.95)
        
      }}#}
  l1<-cbind(c(2001:2023),q50[,1],q5[,1],q95[,1])
  l2<-cbind(c(2001:2023),q50[,2],q5[,2],q95[,2])
  colnames(l1)<-colnames(l2)<-c("year","q50", "q5", "q95")
  return(list(l1, l2))
}

Tseal_res<-median_y_c_k(Tseal)
Tdis_res<-median_y_c_k(Tdis)
Dis_LLD_dead_res<-median_y_c_k(Dis_LLD_dead)

DisLL_res<-median_y_c(DisLL)

# T 2.2.1.1: Painot
# T 2.2.1.2: Kappaleet
#Estimated unreported catch		Estimated discarded catch		Total catch	
round(A_TotUnrep_BS_res,0)
round(A_TotDis_BS_res,0)
round(A_TotCatch_BS_res,0)

round(B_TotRepCom_sea_res,0)


A_TotCatch_BS_res<-stats_y(A_TotCatch_BS)
A_TotDis_BS_res<-stats_y(A_TotDis_BS)
A_TotUnrep_BS_res<-stats_y(A_TotUnrep_BS)
A_TotSeal_BS_res<-stats_y(A_TotSeal_BS)

B_TotRepCom_sea_res<-stats_y(B_TotRepCom_sea)
B_TotRecr_sea_res<-stats_y(B_TotRecr_sea)
B_TotMisr_sea_res<-stats_y(B_TotMisr_sea)





B_TotDis_alive_res<-stats_y_k(B_TotDis_alive)
B_TotDis_LLD_alive_res<-stats_y_k(B_TotDis_LLD_alive)
B_TotDis_FYK_alive_res<-stats_y_k(B_TotDis_FYK_alive)
B_TotDis_GND_alive_res<-stats_y_k(B_TotDis_GND_alive)


B_TotUnrepDis_sea_res<-stats_y_k(B_TotUnrepDis_sea)
B_TotCatch_sea_res<-stats_y_k(B_TotCatch_sea)
B_TotCatchCom_sea_res<-stats_y_k(B_TotCatchCom_sea)
B_TotRiver_res<-stats_y_k(B_TotRiver)
B_TotUnrep_sea_res<-stats_y_k(B_TotUnrep_sea)
B_TotUnrep_river_res<-stats_y_k(B_TotUnrep_river)
B_TotDisSeal_MU_res<-stats_y_k(B_TotDisSeal_MU)
B_TotUnrep_MU_res<-stats_y_k(B_TotUnrep_MU)
B_TotCatch_MU_res<-stats_y_k(B_TotCatch_MU)
B_TotDis_dead_res<-stats_y_k(B_TotDis_dead)
B_TotSeal_res<-stats_y_k(B_TotSeal)
B_TotDis_dead_GND_res<-stats_y_k(B_TotDis_dead_GND)
B_TotDis_dead_LLD_res<-stats_y_k(B_TotDis_dead_LLD)
B_TotDis_dead_FYK_res<-stats_y_k(B_TotDis_dead_FYK)
B_TotDis_dead_MIS_res<-stats_y_k(B_TotDis_dead_MIS)
B_TotSeal_GND_res<-stats_y_k(B_TotSeal_GND)
B_TotSeal_LLD_res<-stats_y_k(B_TotSeal_LLD)
B_TotSeal_FYK_res<-stats_y_k(B_TotSeal_FYK)
B_TotSeal_MIS_res<-stats_y_k(B_TotSeal_MIS)




