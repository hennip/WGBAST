load(file="H:/FLR/WGBAST18/WGBAST_JAGS_SRorig.RData")
load(file="H:/FLR/WGBAST18/WGBAST_JAGS_new_SR.RData")

run1<-run

cbind(Years, 1:length(Years))

plot(run1, var="R0[1]")
summary(run1, var="R0[1]")

plot(run1, var="K[1]")
summary(run1, var="K[1]") # psrf 4

summary(run1, var="MW") #psrf 3.11 /3.8
summary(run1, var="MR")
plot(run1, var="MW") 
plot(run1, var="MR") 

summary(run1, var="MpsW")
plot(run1, var="MpsW[22]") #2008
plot(run1, var="MpsW[23]")#2009
plot(run1, var="MpsW[24]")#2010
plot(run1, var="MpsW[25]")#2011

summary(run1, var="Wprop")

plot(run1, var="Wprop[18,1]")# 2004
plot(run1, var="Wprop[23,1]") # 2009
plot(run1, var="Wprop[25,1]")# 2011

plot(run1, var="Wprop[18,2]")
plot(run1, var="Wprop[23,2]")
plot(run1, var="Wprop[25,2]")

summary(run1, var="LW")

plot(run1, var="LW[24,1]")
plot(run1, var="LW[25,1]")

plot(run1, var="LW[20,2]")
plot(run1, var="LW[26,2]")
plot(run1, var="LW[28,2]")

plot(run, var="tauL")
