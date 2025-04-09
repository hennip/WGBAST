
# Table 12 in SD 22-31 advice sheet 
# Note! State clearly in the table text when the values are median or mean of 
# a distribution


skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/unrep-discards/unrep-and-discards.R")

# SD 32
tot_comm_sea<-array(NA, dim=c(NumYears, Nsim))
med_tot_comm_sea_dead<-med_tot_comm_sea<-c()
for(i in 1:NumYears){
  for(s in 1:Nsim){
    tot_comm_sea[i,s]<-sum(TcatchCom[i,1:9,2])+B_TotUnrepDis_sea[i,2,s] 
  }
  med_tot_comm_sea[i]<-median(tot_comm_sea[i,])
  med_tot_comm_sea_dead[i]<-median(tot_comm_sea[i,]-B_TotDis_alive[i,2,])
}
med_tot_comm_sea

T12_SD32<-cbind(
  c(2001:(2000+NumYears)),  
  B_TotRepCom_sea[,2], # Reported
  
  med_alive_dis[,2],
  med_dead_dis[,2],
  med_seal[,2],
  med_unrep_sea[,2],# unreported
  
  misr[,2],
  B_TotRecr_sea[,2],
  
  rep_river[,2],
  med_Runrep[,2],
  
  # tot comm sea
 med_tot_comm_sea,
  
  # tot comm dead sea
 med_tot_comm_sea_dead
  
)
T12_SD32
colnames(T12_SD32)<-c("year","reported", "alive dis", "dead dis", "seal dam", "unrep", "misrep", "recr",
                        "rep river", "unrep river", "tot_comm_sea", "tot_comm_sea_dead")
T12<-round(T12_SD32,0)

write_xlsx(as.data.frame(T12), "../../WGBAST_shared/flhm/2025/dat/der/Advice_SD32_T5.xlsx")



# SD 22-31

T12_SD2231<-cbind(
  c(2001:(2000+NumYears)),  
B_TotRepCom_sea[,1], # Reported

med_alive_dis[,1],
med_dead_dis[,1],
med_seal[,1],
med_unrep_sea[,1],# unreported

misr[,1],
B_TotRecr_sea[,1],

rep_river[,1],
med_Runrep[,1]

)
T12_SD2231
colnames(T12_SD2231)<-c("year","reported", "alive dis", "dead dis", "seal dam", "unrep", "misrep", "recr",
                        "rep river", "unrep river")
T12<-round(T12_SD2231[20:NumYears,],0)

write_xlsx(as.data.frame(T12), "../../WGBAST_shared/flhm/2025/dat/der/AdviceSD2231_T12.xlsx")


# Åland sea and Gulf of Bothnia (SD 29N-31)


# Save medians per area (SD22-31 and SD32) for further usage
tmp9<-tmp8<-tmp6<-tmp7<-tmp4<-tmp3<-tmp2<-tmp22<-tmp<-array(NA, dim=c(NumYears, 2, Nsim))

med_Runrep<-rep_river<-med_Tcatch<-med_unrep<-med_dis<-
  rep_recr_sea<- rep_catch_com<- med_alive_dis<-med_unrep_sea<-med_seal<-misr<-med_river<-med_dead_dis<-array(NA, dim=c(NumYears, 2))

for(i in 1:NumYears){
  for(k in 1:2){
    for(s in 1:Nsim){
      tmp[i,k,s]<-sum(Tdis[i, 1:2,k,s], na.rm=T)
      tmp22[i,k,s]<-sum(Sunrep[i, 1:2,k,s], na.rm=T)
      tmp3[i,k,s]<-sum(Tcatch[i,1:2,k,s], na.rm=T)
      tmp4[i,k,s]<-sum(Runrep[i, 1:2,k,s], na.rm=T)
      #tmp5[i,k,s]<-sum(TcatchCom[i, 1:2,k,s], na.rm=T)
  
      tmp6[i,k,s]<-sum(Tdis_alive[i,1:2,k,s])	
      tmp7[i,k,s]<-sum(Tdis[i,1:2,k,s])	
      tmp8[i,k,s]<-sum(Tseal[i,1:2,k,s])

    }
    med_dis[i,k]<-median(tmp[i,k,]) # Total discared
    med_unrep_sea[i,k]<-median(tmp22[i,k,]) #Total unreported
    med_Tcatch[i,k]<-median(tmp3[i,k,])
    med_Runrep[i,k]<-median(tmp4[i,k,])
    rep_catch_com[i,k]<-sum(TcatchCom[i,1:2,k])
    rep_recr_sea[i,k]<-sum(TRecrSea[i,1:2,k])
    
    med_dead_dis[i,k]<-median(tmp7[i,k,])
    med_alive_dis[i,k]<-median(tmp6[i,k,])
    misr[i,k]<-sum(TMisr[i,1:2,k])
    rep_river[i,k]<-sum(River[i,1:2,k])
    med_seal[i,k]<-median(tmp8[i,k,])
    med_recr_sea[i,k]<-median(tmp8[i,k,])
    
  }}

T12_SD29N31<-cbind(
  c(2001:(2000+NumYears)),  
  rep_catch_com[,1], # Reported
  
  med_alive_dis[,1], 
  med_dead_dis[,1],
  med_seal[,1],
  med_unrep_sea[,1],#ok?
  
  misr[,1], #ok?
  rep_recr_sea[,1],#B_TotRecr_sea[,1],
  
  rep_river[,1],#ok?
  med_Runrep[,1] #ok?
  
)
T12_SD29N31
colnames(T12_SD29N31)<-c("year","reported", "alive dis", "dead dis", "seal dam", "unrep", "misrep", "recr",
                        "rep river", "unrep river")
round(T12_SD29N31[20:NumYears,],0)

write_xlsx(as.data.frame(T12_SD29N31), "../../WGBAST_shared/flhm/2025/dat/der/AdviceSD29N31_T12.xlsx")



