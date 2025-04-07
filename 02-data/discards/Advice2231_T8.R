skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/discards/unrep-and-discards.R")

# SD22-31
############
# Commercial reported landings at sea
comm_rep_landings_sea<-B_TotRepCom_sea[,1]/1000

# Landings at sea
# Total reported landings including recreational catches. 
landings_sea<-(B_TotRepCom_sea[,1]+B_TotRecr_sea[,1])/1000


# Catch at sea
#^^ Estimated total catches including discards, misreported catch, and unreported catch.
catch_sea<-(B_TotRepCom_sea[,1]+B_TotRecr_sea[,1]+B_TotMisr_sea[,1]+
            med_dis[,1]+# discards
            med_unrep[,1]# unreported
            )/1000
catch_sea

# River catch
med_river

T8<-cbind(c(2001:(2000+NumYears)),comm_rep_landings_sea, landings_sea,catch_sea, med_river[,1]/1000)
colnames(T8)<-c("Year", "Comm rep landings", "Landings at sea",
               "Catch at sea", "River catch")
T8<-round(T8,0)

write_xlsx(as.data.frame(T8), "../../WGBAST_shared/flhm/2025/dat/der/Advice2231_T8.xlsx")


# SD32
############

# Commercial reported landings at sea
comm_rep_landings_sea<-B_TotRepCom_sea[,2]

# Landings at sea
# Total reported landings including recreational catches. 
landings_sea<-(B_TotRepCom_sea[,2]+B_TotRecr_sea[,2])


# Catch at sea
#^^ Estimated total catches including discards, misreported catch, and unreported catch.
catch_sea<-(B_TotRepCom_sea[,2]+B_TotRecr_sea[,2]+B_TotMisr_sea[,2]+
              med_dis[,2]+# discards
              med_unrep[,2]# unreported
)
catch_sea

# River catch
med_river

T8_32<-cbind(c(2001:(2000+NumYears)),comm_rep_landings_sea, landings_sea,catch_sea, med_river[,2])
colnames(T8_32)<-c("Year", "Comm rep landings", "Landings at sea",
                "Catch at sea", "River catch")
T8_32<-round(T8_32,0)

write_xlsx(as.data.frame(T8_32), "../../WGBAST_shared/flhm/2025/dat/der/Advice32_T8.xlsx")
