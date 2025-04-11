
# Figure 1 in SD 32 Advice
################################

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/unrep-discards/unrep-and-discards.R")

# river catch
dim(Bs_TotRiver)

med_unrep_sea<-med_disseal<-recr_sea<-river<-c()
unrep_sea<-array(NA, dim=c(NumYears,Nsim))
for(i in 1: NumYears){
  # river catch, reported and unreported
  river[i]<-median(Bs_TotRiver[i,2,])/1000
  
  # recr sea
  recr_sea[i]<-Bd_TotRecr_sea[i,2]/1000

  # dead discards (sea)
  med_disseal[i]<-median(Bs_TotDisSeal[i,2,])/1000

    # unrep (sea)
    # for(s in 1:Nsim){
    #   unrep_sea[i,s]<-sum(Sunrep[i,1:9,2,s])/1000 # = Ounrep + Cunrep
    # }
    med_unrep_sea[i]<-median(Bs_TotUnrep_OC[i,2,])/1000
}


# comm landings
Bd_TotRepCom_sea[,2]

F1<-cbind(c(2001:(2000+NumYears)), river, recr_sea, med_disseal,med_unrep_sea, Bd_TotRepCom_sea[,2]/1000)
colnames(F1)<-c("Year", "river", "recr_sea", "dead_dis_seal", "unrep_sea", "comm")
F1

write_xlsx(as.data.frame(F1), "../../WGBAST_shared/flhm/2025/dat/der/Advice_SD32_F1.xlsx")



