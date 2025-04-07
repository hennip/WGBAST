
# Table 12 in SD 22-31 advice sheet 
# Note! State clearly in the table text when the values are median or mean of 
# a distribution


skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/discards/unrep-and-discards.R")

# SD 32

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
  med_Runrep[,2]
  
)
T12_SD32
colnames(T12_SD32)<-c("year","reported", "alive dis", "dead dis", "seal dam", "unrep", "misrep", "recr",
                        "rep river", "unrep river")
T12<-round(T12_SD32[20:NumYears,],0)

write_xlsx(as.data.frame(T12), "../../WGBAST_shared/flhm/2025/dat/der/AdviceSD32_T12.xlsx")



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










df_2231<-read_xlsx(str_c(pathIn, 
                        "WGBAST_2025_Catch_02.04.2025_Hennille.xlsx"), # Update!
                  range="A1:Q18593", # Update!
                  sheet="Catch data", col_names = T, guess_max = 10000, na=c("",".", "NaN", "NA"))%>%
  # filter(YEAR>2005)%>% # Include results only 2009 onwards, catch DB has only updates from those years 
  # mutate(NUMB=parse_double(NUMB))%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, 
         EFFORT, everything()) |> 
  mutate(TP_TYPE=ifelse(TP_TYPE=="QRT", "QTR", TP_TYPE)) |> 
  mutate(GEAR=ifelse(GEAR=="GNS", "MIS", GEAR)) |> 
  filter(SPECIES=="SAL", YEAR>2000) |> 
  filter(sub_div2=="22-31")


df_2231$F_TYPE

df_2231 |> filter(F_TYPE=="BMS") |> 
  group_by(YEAR) |> summarise(dis=sum(NUMB))

