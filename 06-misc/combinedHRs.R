# Combined harvest rates

# Obs! Run alongside extra-output.R



#########################
lastY<-30  # ADD ONE EACH YEAR!!!
#########################
year<-c(1992:LastPredYear)



# Combined offshore harvest rate (driftnet included in the history)
windows()
par(mfrow=c(1,2))
par(mar=c(3,4,4,2))

# Wild salmon
dat<-OffsW_HR[2,1:lastY,]# MSW salmon
med<-vector();low<-vector();high<-vector()
for(i in 1:lastY){  
  sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
  low[i]<-sumdat$quantiles[1]
  high[i]<-sumdat$quantiles[5]
  med[i]<-sumdat$quantiles[3]
}
cbind(med, low, high)
# add one for years so that estimates correspond the correct spring fishery 
# (model year 2007 = autumn 2007 + spring 2008 => calendar year 2008  

plot(year[1:length(med)]+1,med, pch=19, main="Combined offshore HR, MSW wild", 
     ylim=c(0,0.6), ylab ="Harvest rate" )
segments(year[1:length(med)]+1, low, year[1:length(med)]+1, high)

tab<-as.data.frame(cbind(year[1:lastY]+1,med, low, high))
write_xlsx(tab, paste0(PathOut_Scen_tmp,"CombinedOffsHR_wildMSW.xlsx"))


# Reared salmon
dat<-OffsR_HR[2,1:lastY,]# MSW salmon
med<-vector();low<-vector();high<-vector()
for(i in 1:lastY){
  sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
  low[i]<-sumdat$quantiles[1]
  high[i]<-sumdat$quantiles[5]
  med[i]<-sumdat$quantiles[3]
}
# add one for years so that estimates correspond the correct spring fishery 
# (model year 2007 = autumn 2007 + spring 2008 => calendar year 2008  
plot(year[1:length(med)]+1,med, pch=19, main= "Combined offshore HR, MSW reared",
     ylim=c(0,0.6), ylab ="Harvest rate")
segments(year[1:length(med)]+1, low, year[1:length(med)]+1, high)
tab<-as.data.frame(cbind(year[1:lastY]+1,med, low, high))
write_xlsx(tab, paste0(PathOut_Scen_tmp,"CombinedOffsHR_rearedMSW.xlsx"))


# Combined coastal harvest rate (coastal driftnet and gillnet 
# included in the history)
windows()
par(mfrow=c(1,2))
par(mar=c(3,4,4,2))

# wild salmon
dat<-CoastW_HR[2,1:lastY,1,]# MSW salmon, AU 1
med<-vector();low<-vector();high<-vector()
for(i in 1:(lastY)){
  sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
  low[i]<-sumdat$quantiles[1]
  high[i]<-sumdat$quantiles[5]
  med[i]<-sumdat$quantiles[3]
}
plot(year[1:length(med)],med, pch=19, main="Combined coastal HR, MSW wild, AU1", 
     ylim=c(0,1), ylab ="Harvest rate" )
segments(year[1:length(med)], low, year[1:length(med)], high)

tab<-as.data.frame(cbind(year[1:(lastY)],med, low, high))
write_xlsx(tab, paste0(PathOut_Scen_tmp,"CombinedCoastHR_wildMSW_AU1.xlsx"))


# Reared salmon
dat<-CoastR_HR[2,1:lastY,1,]# MSW salmon, AU 1
med<-vector();low<-vector();high<-vector()
for(i in 1:lastY){
  sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
  low[i]<-sumdat$quantiles[1]
  high[i]<-sumdat$quantiles[5]
  med[i]<-sumdat$quantiles[3]
}
plot(year[1:length(med)],med, pch=19, main="Combined coastal HR, MSW reared, AU1", 
     ylim=c(0,1), ylab = "Harvest rate" )
segments(year[1:length(med)], low, year[1:length(med)], high)
tab<-as.data.frame(cbind(year[1:(lastY)],med, low, high))
write_xlsx(tab, paste0(PathOut_Scen_tmp,"CombinedCoastHR_rearedMSW_AU1.xlsx"))

# 
# # Total HR, au 1
# windows()
# par(mfrow=c(1,2))
# par(mar=c(3,4,4,2))
# 
# # wild
# TotW_HR<-1-exp(-(-log(1-OffsW_HR)-log(1-CoastW_HR[,,1,])))
# 
# dat<-TotW_HR[2,1:lastY,]# MSW salmon
# med<-vector();low<-vector();high<-vector()
# for(i in 1:lastY){
#   sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
#   low[i]<-sumdat$quantiles[1]
#   high[i]<-sumdat$quantiles[5]
#   med[i]<-sumdat$quantiles[3]
# }
# plot(year[1:length(med)],med, pch=19, #main=paste(sep="","Scenario ",S), 
#      ylim=c(0,1), main = "Combined total HR, MSW wild AU1", ylab="Harvest rate")
# segments(year, low, year, high)
# 
# # reared                        
# TotR_HR<-1-exp(-(-log(1-OffsR_HR)-log(1-CoastR_HR[,,1,])))
# 
# dat<-TotR_HR[2,1:lastY,]# MSW salmon
# med<-vector();low<-vector();high<-vector()
# for(i in 1:lastY){
#   sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
#   low[i]<-sumdat$quantiles[1]
#   high[i]<-sumdat$quantiles[5]
#   med[i]<-sumdat$quantiles[3]
# }
# plot(year[1:length(med)],med, pch=19, #main=paste(sep="","Scenario ",S), 
#      ylim=c(0,1), main = "Combined total HR, MSW reared AU1", ylab="Harvest rate")
# segments(year, low, year, high)
# 
# mean(TotW_HR[2,1:23,1:10]-TotR_HR[2,1:23,1:10])
# # total HR is slightly higher for wild than for reared. This comes from the 
# # estimates of catchability, but is not biologically correct.
# 
# 

