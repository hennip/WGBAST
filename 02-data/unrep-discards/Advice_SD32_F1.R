

skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/unrep-discards/unrep-and-discards.R")

# river catch
B_TotRiver[,2]

dim(B_TotRiver)

med_unrep_sea<-med_disseal<-recr_sea<-river<-c()
unrep_sea<-array(NA, dim=c(NumYears,Nsim))
for(i in 1: NumYears){
  # river catch, reported and unreported
  river[i]<-median(B_TotRiver[i,2,])/1000
  
  # recr sea
  recr_sea[i]<-sum(TRecrSea[i,1:9,2])/1000
    
  # dead discards (sea)
  med_disseal[i]<-median(B_TotDisSeal_MU[i,2,])/1000

    # unrep (sea)
    for(s in 1:Nsim){
      unrep_sea[i,s]<-sum(Sunrep[i,1:9,2,s])/1000 # = Ounrep + Cunrep
    }
    med_unrep_sea[i]<-median(unrep_sea[i,])
}


# comm landings
B_TotRepCom_sea[,2]

F1<-cbind(c(2001:(2000+NumYears)), river, recr_sea, med_disseal,med_unrep_sea, B_TotRepCom_sea[,2]/1000)
colnames(F1)<-c("Year", "river", "recr_sea", "dead_dis_seal", "unrep_sea", "comm")
F1

write_xlsx(as.data.frame(F1), "../../WGBAST_shared/flhm/2025/dat/der/Advice_SD32_F1.xlsx")



