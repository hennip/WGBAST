# Table 2.2.1.2. Total catch: Nominal reported catches plus discards (incl. seal 
# damaged salmon), unreported, and misreported catches of Baltic 
# salmon in numbers from sea, coast, and river by country in 2014-2023 in Subdivisions 22-32

# Countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE and 9=RU																			  						

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/unrep-discards/unrep-and-discards.R")

rep_catch<-LLD_N[,,1]+LLD_N[,,2]+
  River_N[,,1]+River_N[,,2]+
  Recr_N[,,1]+Recr_N[,,2]+
  GND_N[,,1]+GND_N[,,2]+
  FYK_N[,,1]+FYK_N[,,2]+
  MIS_N[,,1]+MIS_N[,,2]
colnames(rep_catch)<-c("FI","SE","DK","PL","LV","LT","DE","EE","RU")

tmp<-cbind(rep_catch[,3],rep_catch[,8],rep_catch[,1],rep_catch[,7],rep_catch[,5],rep_catch[,6],rep_catch[,4],rep_catch[,9],rep_catch[,2])

tot<-c()
for(i in 1:NumYears){
  tot[i]<-sum(tmp[i,])
}
rep_catch<-cbind(tmp,tot)


# Estimated misreported catch 
misrep<-misr[,1]

# Estimated unreported catch (median & 90%PI)
As_TotUnrep_res<-stats_y(As_TotUnrep);unrep<-round(As_TotUnrep_res,0)
unrep<-as.data.frame(cbind(unrep[,2], str_c(unrep[,3], "-", unrep[,4])))


# Estimated discarded catch
As_TotDis_res<-stats_y(As_TotDis);dis<-round(As_TotDis_res,0)
dis<-as.data.frame(cbind(dis[,2], str_c(dis[,3], "-", dis[,4])))

# Total catch
As_TotCatch_res<-stats_y(As_TotCatch);totcatch<-round(As_TotCatch_res,0)
totcatch<-as.data.frame(cbind(totcatch[,2], str_c(totcatch[,3], "-", totcatch[,4])))

T2212<-cbind(2001:(2000+NumYears),rep_catch,misrep, unrep, dis, totcatch)
colnames(T2212)<-c("Year","DK", "EE", "FI", "DE", "LV","LT","PL","RU","SE","Reported total",
                   "Misreported","Unreported", "PI","Discarded","PI","Total","PI")
write_xlsx(T2212, "../../WGBAST_shared/flhm/2025/dat/der/T2212.xlsx")



#source("02-data/discards/unrep-and-discards.R")

# Table 2.2.1.1. Total catch: Nominal reported catches plus discards (incl. seal 
# damaged salmon), unreported, and misreported catches of Baltic salmon in tonnes 
# round fresh weight, from sea, coast, and river by country in 2014-2023 in 
# Subdivisions 22-32
# Countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE and 9=RU																			  						

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"W"
source("02-data/unrep-discards/unrep-and-discards.R")


rep_catch<-LLD_W[,,1]+LLD_W[,,2]+
  River_W[,,1]+River_W[,,2]+
  Recr_W[,,1]+Recr_W[,,2]+
  GND_W[,,1]+GND_W[,,2]+
  FYK_W[,,1]+FYK_W[,,2]+
  MIS_W[,,1]+MIS_W[,,2]
colnames(rep_catch)<-c("FI","SE","DK","PL","LV","LT","DE","EE","RU")

tmp<-cbind(rep_catch[,3],rep_catch[,8],rep_catch[,1],rep_catch[,7],rep_catch[,5],rep_catch[,6],rep_catch[,4],rep_catch[,9],rep_catch[,2])

tot<-c()
for(i in 1:NumYears){
  tot[i]<-sum(tmp[i,])
}
rep_catch<-cbind(tmp,tot)


# Estimated misreported catch 
misrep<-TMisr[,4,1]

# Estimated unreported catch (median & 90%PI)
As_TotUnrep_res<-stats_y(As_TotUnrep);unrep<-round(As_TotUnrep_res,0)
unrep<-as.data.frame(cbind(unrep[,2], str_c(unrep[,3], "-", unrep[,4])))


# Estimated discarded catch
As_TotDis_res<-stats_y(As_TotDis);dis<-round(As_TotDis_res,0)
dis<-as.data.frame(cbind(dis[,2], str_c(dis[,3], "-", dis[,4])))

# Total catch
As_TotCatch_res<-stats_y(As_TotCatch);totcatch<-round(As_TotCatch_res,0)
totcatch<-as.data.frame(cbind(totcatch[,2], str_c(totcatch[,3], "-", totcatch[,4])))

T2211<-cbind(2001:(2000+NumYears),rep_catch,misrep, unrep, dis, totcatch)
colnames(T2211)<-c("Year","DE", "EE", "FI", "DE", "LV","LT","PL","RU","SE","Reported total",
                   "Misreported","Unreported", "PI","Discarded","PI","Total","PI")
write_xlsx(T2211, "../../WGBAST_shared/flhm/2025/dat/der/T2211.xlsx")







