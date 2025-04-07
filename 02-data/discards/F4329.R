skip<-T # Skips the lines at unrep-and-discards.R where number_or_weight is defined
number_or_weight<-"N"
source("02-data/discards/unrep-and-discards.R")

# Figure 4.3.2.9. Share of commercial and recreational catches at sea, river 
# catches (river catches include unreporting and some commercial fishing), and 
# discard/unreporting/misreporting of total sea catches in subdivisions 22-31 in 
# years 1987-2022. Numbers of salmon in the upper panel with corresponding 
# proportions of catch components in the lower panel.

F4329<-cbind(c(2001:(2000+NumYears)),
med_river[,1],
B_TotRecr_sea[,1],
med_dead_dis_seal[,1],
med_unrep[,1],
misr[,1], # Data, no uncertainty
B_TotRepCom_sea[,1]) # Data, no uncertainty

colnames(F4329)<-c("Year", "River", "Recr", "Dead discards", "Unrep", "Misrep", "Comm rep catch sea")
write_xlsx(as.data.frame(F4329), "../../WGBAST_shared/flhm/2025/dat/der/F4329.xlsx")




