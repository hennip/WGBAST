#   COMMENT OUT THE NEEDED PLOTS
#   FOR 2024 ADVICE LL AND TR PLOTS WERE NOT NEEDED


endyear<-Nyears

# Longlining, wild salmon
##########################
# 
# par(mfrow=c(1,2))
# par(mar=c(3,4,4,2))
# 
# #nrscen<-c(1)
# 
# #for(s in 1:2){ # Number of scenarios
# for(s in 1:length(scen_HR_LL)){
#   S<-scen_HR_LL[s]
#   File<-paste0(PathScen,"ScenProj_",Model,"_EScen",S,"_RCzero23-35.RData")
# 
#   load(File)
#   dim(WOLL_HR)
#   dat<-WOLL_HR[2,,1,] # index 2 == MSW salmon    
#   
#   med<-vector();low<-vector();high<-vector()
#   
#   for(i in 1:length(year)){
#     sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
#     low[i]<-sumdat$quantiles[1]
#     high[i]<-sumdat$quantiles[5]
#     med[i]<-sumdat$quantiles[3]
#   }
#   # add one for years so that estimates correspond the correct spring fishery 
#   # (model year 2007 = autumn 2007 + spring 2008 => calendar year 2008
#   plot(year+1,med, pch=19, main=paste(sep="","Scenario ",S), 
#        ylim=c(0,0.4), ylab = "LL HR for MSW wild")
#   segments(year+1, low, year+1, high)
# }
# #dev.off()
# p1 <- recordPlot();p1

# Trapnetting, wild salmon
###########################


par(mfrow=c(3,2))
par(mar=c(3,4,4,2))

for(s in 1:length(scen_HR_TN)){ # Number of scenarios
  S<-scen_HR_TN[s]
  #S<-4
  File<-paste0(PathScen,"ScenProj_",Model,"_EScen",S,"_RCzero23-35.RData")
  load(File)
  dat<-WCTN_HR[2,,1,]  # MSW salmon, assessment unit 1  
  
  med<-vector();low<-vector();high<-vector()
  for(i in 1:length(year)){
    sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
    low[i]<-sumdat$quantiles[1]
    high[i]<-sumdat$quantiles[5]
    med[i]<-sumdat$quantiles[3]
  }
  plot(year,med, pch=19, main=paste(sep="","Scenario ",S), 
       ylim=c(0,0.9), ylab = "TN HR for MSW wild in AU 1")
  segments(year, low, year, high)
}

summary(as.mcmc(WCTN_HR[2,1,1,]))

#dev.off()
p2 <- recordPlot();p2

# Trolling, wild salmon
###########################


# 
# par(mfrow=c(2,2))
# par(mar=c(3,4,4,2))
# 
# for(s in 1:length(scen_HR_tr)){ # Number of scenarios
#   S<-scen_HR_tr[s]
#   #S<-9
#   File<-paste0(PathScen,"ScenProj_",Model,"_EScen",S,"_RCzero23-35.RData")
#   load(File)
#   dat<-WTR_HR[2,,1,]
#   
#   med<-vector();low<-vector();high<-vector()
#   for(i in 1:length(year)){
#     sumdat<-summary(as.mcmc(dat[i,]),quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95))
#     low[i]<-sumdat$quantiles[1]
#     high[i]<-sumdat$quantiles[5]
#     med[i]<-sumdat$quantiles[3]
#   }
#   plot(year,med, pch=19, main=paste(sep="","Scenario ",S), 
#        ylim=c(0,0.12), ylab = "Trolling HR for MSW wild")
#   segments(year, low, year, high)
# }
# 
# summary(as.mcmc(WCTN_HR[2,1,1,]))
# 
# #dev.off()
# p3 <- recordPlot();p3
