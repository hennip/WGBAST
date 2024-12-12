###################################################
#   comparing MPS based on different selection    #
#   in this case difference between selecting     #
#   a different final historical year             #
###################################################
library(tidyverse);library(coda)

#Load the file containing stats
#   old


load(str_c(PathOut_Scen,"2024/output/ScenProj_2024_EScen1_RCzero23-35.RData"))
datW_old <- exp(-Mps_All) %>% as.mcmc %>%  as.data.frame
datR_old <- exp(-Mps_AllR) %>% as.mcmc %>%  as.data.frame


#   new
load(str_c(PathOut_Scen,"2024/output/ScenProj_2024_JAGS_Mps_EScen1_RCzero23-35.RData"))
datW_new <- exp(-Mps_All) %>% as.mcmc %>%  as.data.frame
datR_new <- exp(-Mps_AllR) %>% as.mcmc %>%  as.data.frame

rownames(datW_new)=rownames(datW_old)=
  rownames(datR_new)=rownames(datR_old) = c(1992:(1991+nrow(datW_new)))

rownames(datW_new) = c(1992:(1991+nrow(datW_new)))

#   selecting years from 2020-2023
W_new <- datW_new %>% 
  apply(., 1, quantile, c(0.05,0.5,0.95, 0.25, 0.75)) %>% as.data.frame

R_new <- datR_new %>% 
  filter(rownames(.) %in% c(2020:2023)) %>% 
  apply(., 1, quantile, c(0.05,0.5,0.95, 0.25, 0.75))

W_old <- datW_old %>% 
  filter(rownames(.) %in% c(2020:2023)) %>% 
  apply(., 1, quantile, c(0.05,0.5,0.95, 0.25, 0.75))

R_old <- datR_old %>% 
  filter(rownames(.) %in% c(2020:2023)) %>% 
  apply(., 1, quantile, c(0.05,0.5,0.95, 0.25, 0.75))

colnames(W_new)=colnames(W_old)=
  colnames(R_new)=colnames(R_old) = c(2020:2023)

rownames(W_new)=rownames(W_old)=
  rownames(R_new)=rownames(R_old) = c("min", "middle", "max", "lower", "upper")

rownames(W_new) = c("90%lower", "median", "90%upper", "lower", "upper")



df <- data.frame("Year"    = 2020:2023,
                 "Med_new" = W_new[2,], 
                 "Med_old" = W_old[2,],
                 "PI_new" = paste(round(W_new[1,], 4), "-", round(W_new[3,], 4), sep=""),
                 "PI_old" = paste(round(W_old[1,], 4), "-", round(W_old[3,], 4), sep=""),
                 
                 row.names = NULL)

W_new_m <- W_new[,1:34]

df <- data.frame("Year"    = colnames(W_new_m) %>% as.numeric,
                 "median"  = W_new_m[2,] %>% as.numeric,
                 "90%PI" = paste(round(W_new_m[1,], 4), "-", round(W_new_m[3,], 4), sep="")) 

library(openxlsx)

write.xlsx(df, "06-misc/MPS_table(1992-2024).xlsx")

W_new_m <- W_new %>% 
  t %>% 
  as.data.frame %>% 
  mutate(
    Year = c(paste(2020:2023))
  )


W_old_m <- W_old %>% 
  t %>% 
  apply(.,2,as.numeric) %>%
  as.data.frame %>% 
  mutate(
    Year = c(paste(2020:2023))
  )




ggplot()+
  theme_bw(base_size = 20)+
  geom_boxplot(
    data  = W_old_m ,
    aes(x=Year,
        ymin=min,
        lower=lower,
        middle=middle,
        upper=upper, 
        ymax=max),
    stat = "identity",
    col = "gray",
    alpha=0.8,
    lwd = 1
  )+
  geom_boxplot(
    data  = W_new_m ,
    aes(x=Year,
        ymin=min,
        lower=lower,
        middle=middle,
        upper=upper, 
        ymax=max),
    stat = "identity",
    col = "black",
    alpha=0.8,
    lwd = 1
  )+
  ggtitle("MPS comparison; gray final historical year 2022, black 2021")




# ===============================================================================

par(mfrow=c(2,1))
par(mar=c(4,5,3,1)+0.1)

year <- c(1992:LastPredYear)
datW <- exp(-Mps_All)
datR <- exp(-Mps_AllR)
medW<-lowW<-highW<-MeanW<-c()
medR<-lowR<-highR<-MeanR<-c()
for(i in 1:dim(datW)[1]){
  dat_sumW<-summary(as.mcmc(datW[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  lowW[i]<-dat_sumW$quantiles[1]
  medW[i]<-dat_sumW$quantiles[3]
  highW[i]<-dat_sumW$quantiles[5]
  MeanW[i]<-dat_sumW$statistics[1]
  
  dat_sumR<-summary(as.mcmc(datR[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  lowR[i]<-dat_sumR$quantiles[1]
  medR[i]<-dat_sumR$quantiles[3]
  highR[i]<-dat_sumR$quantiles[5]
  MeanR[i]<-dat_sumR$statistics[1]
}

plot(year-0.3,medR[1:length(year)], type="p", pch=19, col="red", ylim=c(0,0.6),
     main="Post-smolt survival", ylab = "Rate", xlab="Year")
segments(year-0.3, lowR[1:length(year)], year-0.3, highR[1:length(year)], col="red")
points(year,medW[1:length(year)], type="p", pch=19)
segments(year, lowW[1:length(year)], year, highW[1:length(year)])
legend("topright",c("wild","reared"), pch=c(19,19), col=c("black","red"),lty=c(1,1))


apu<-c()
for( i in 1:1000){
  apu[i]<-sum(exp(-Mps_All)[17:20,i])/4
}
summary(as.mcmc(apu))


dat <- 1-M74_All
med<-vector(); low<-vector(); high<-vector()
for(i in 1:dim(dat)[1]){
  dat_sum<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.2, 0.5, 0.8, 0.95))
  low[i]<-dat_sum$quantiles[1]
  med[i]<-dat_sum$quantiles[3]
  high[i]<-dat_sum$quantiles[5]
}

plot(year,med[1:length(year)], type="p", pch=19, ylim=c(0,1),main="M74 survival", ylab = "Rate",
     xlab="Year")
segments(year, low[1:length(year)], year, high[1:length(year)])
#med

p <- recordPlot();p

