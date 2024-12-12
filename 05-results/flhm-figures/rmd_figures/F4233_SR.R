# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.3, SR graphs

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


# Time
######################
Year<-c(1996:assessment_year) # Smolt years, add one each year
Nyears<-length(Year) 
#Nstocks<-17



######################

# Points
######################
#nsim <- 4000
E<-array(NA, dim=c(Nyears,Nstocks,nsim))
S<-array(NA, dim=c(Nyears,Nstocks,nsim))

if(nchains==1){
  for(r in 1:Nstocks){   
    for(y in 10:(10+Nyears-1)){ 
      S[y-9,r,]<-chains[,str_c("SmoltW[",y,",",r,"]")] 
    }
    for(y in 6:(6+Nyears-1)){ 
      E[y-5,r,]<-chains[,str_c("Eggstot_M74[",y,",",r,"]")]
    }
  }
}

if(nchains==2){
  for(r in 1:Nstocks){   
    for(y in 10:(10+Nyears-1)){ 
      S[y-9,r,]<-chains[,str_c("SmoltW[",y,",",r,"]")][[1]]  
    }
    for(y in 6:(6+Nyears-1)){ 
      E[y-5,r,]<-chains[,str_c("Eggstot_M74[",y,",",r,"]")][[1]] 
    }
  }
}



E_med<-array(NA, dim=c(Nyears,Nstocks)); E_low<-array(NA, dim=c(Nyears,Nstocks))
E_high<-array(NA, dim=c(Nyears,Nstocks));S_med<-array(NA, dim=c(Nyears,Nstocks))
S_low<-array(NA, dim=c(Nyears,Nstocks)); S_high<-array(NA, dim=c(Nyears,Nstocks))

for(r in 1:Nstocks){
  for(y in 1:Nyears){
    E_med[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[3]
    E_low[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[1]
    E_high[y,r]<-summary(as.mcmc(E[y,r,]))$quantiles[5]
    S_med[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[3]
    S_low[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[1]
    S_high[y,r]<-summary(as.mcmc(S[y,r,]))$quantiles[5]
  }
}

cbind(E_low[,1],E_med[,1],E_high[,1],S_low[,1],S_med[,1],S_high[,1])


# Curves
######################

a<-array(NA, dim=c(Nstocks,nsim))
b<-array(NA, dim=c(Nstocks,nsim))

# Load simulated values from files
if(nchains==1){
  for(i in 1:Nstocks){
    a[i,]<-chains[,str_c("alphaSR[",i,"]")]
    b[i,]<-chains[,str_c("betaSR[",i,"]")]  
  }
}
if(nchains==2){
  for(i in 1:Nstocks){
    a[i,]<-chains[,str_c("alphaSR[",i,"]")][[1]]  
    b[i,]<-chains[,str_c("betaSR[",i,"]")][[1]]  
  }
}

# Take a sample or use all nsim simulated values
samp<-1:nsim
a_samp<-array(NA, dim=c(Nstocks, length(samp)))
b_samp<-array(NA, dim=c(Nstocks, length(samp)))
for(r in 1:Nstocks){
  for(i in 1:length(samp)){
    a_samp[r,i]<-a[r,samp[i]]
    b_samp[r,i]<-b[r,samp[i]]
  }
}

# calculate number of smolts for each amount of eggs with B-H stock recruitment 
# function
# Note!! length(eggs)==length(eggs2) (or change the code)
eggs<-seq(0,nsim, by=2)
eggs2<-seq(0,nsim/100, by=0.02)  # For small rivers
length(eggs)==length(eggs2) # These must be equal so that the following will work
smolts<-array(NA, dim=c(Nstocks,length(eggs),length(samp)))
smolts2<-array(NA, dim=c(Nstocks, length(eggs),length(samp)))
for(r in 1:Nstocks){
  for(i in 1:length(eggs)){
    for(j in 1:length(samp)){
      ifelse(r<5|r==7|r==10|r==14|r==16,
             smolts[r,i,j]<-eggs[i]*nsim/(a_samp[r,j]+b_samp[r,j]*eggs[i]*nsim),
             smolts2[r,i,j]<-eggs2[i]*nsim/(a_samp[r,j]+b_samp[r,j]*eggs2[i]*nsim)
      )
    }
  }
}

############          
## Graphs ##
############


## ---- graphs-SRcurves



############

par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

col1<-rgb(0.5,0,0.5,0.03)
col2<-rgb(0,0.7,1,0.1)

col1<-rgb(1,0,0,0.03)
col2<-rgb(0,0,1,0.1)

cexLab<-1.2
cexAxis<-1.2
cexMain<-1.5

relx<-7 # relationship between x and y axis is 1:8

############
# Tornionjoki

#windows()

plot(eggs, smolts[1,,1], type="l",ylim=c(0,relx*500), xlim=c(0,800), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[1,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,1,]/nsim,S[i,1,], col=col2)
  #points(e1[i,1,]/nsim,s1[i,1,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[1,,j], type="l", col=col1)
}

############
# Simojoki

plot(eggs, smolts[2,,1], type="l",ylim=c(0,150), xlim=c(0,50), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[2,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,2,]/nsim,S[i,2,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[2,,j], type="l", col=col1)
}

############
# Kalix

plot(eggs, smolts[3,,1], type="l",ylim=c(0,relx*225), xlim=c(0,300), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[3,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,3,]/nsim,S[i,3,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[3,,j], type="l", col=col1)
}


############
# R?ne

plot(eggs, smolts[4,,1], type="l",ylim=c(0,relx*35), xlim=c(0,30), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[4,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,4,]/nsim,S[i,4,], col=col2)
}

for(j in 2:length(samp)){
  points(eggs, smolts[4,,j], type="l", col=col1)
}

#dev.off()
F4233_SR1 <- recordPlot()
F4233_SR1
 
################################################################################
 
par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

############
# Pite

plot(eggs2, smolts2[5,,1], type="l",ylim=c(0,relx*9), xlim=c(0,9), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[5,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,5,]/nsim,S[i,5,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[5,,j], type="l", col=col1)
}


############
# ?by

plot(eggs2, smolts2[6,,1], type="l",ylim=c(0,relx*7), xlim=c(0,10), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[6,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,6,]/nsim,S[i,6,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[6,,j], type="l", col=col1)
}

############
# Byske

plot(eggs, smolts[7,,1], type="l",ylim=c(0,relx*45), xlim=c(0,90), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[7,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,7,]/nsim,S[i,7,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[7,,j], type="l", col=col1)
}


############
# Rickle?n

plot(eggs2, smolts2[8,,1], type="l",ylim=c(0,relx*2.5), xlim=c(0,2.5), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[8,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,8,]/nsim,S[i,8,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[8,,j], type="l", col=col1)
}

F4233_SR2 <- recordPlot()
F4233_SR2
 

################################################################################

par(mfrow=c(2,2),mar=c(4.5,4,3.5,1))

############
# S?var?n

plot(eggs2, smolts2[9,,1], type="l",ylim=c(0,relx*3), xlim=c(0,3), col=col1, 
     ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivernames[9,], cex.lab=cexLab, 
     cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,9,]/nsim,S[i,9,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[9,,j], type="l", col=col1)
}


################################################################################
############
# Ume

plot(eggs, smolts[10,,1], type="l",ylim=c(0,relx*100), xlim=c(0,100), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[10,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,10,]/nsim,S[i,10,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[10,,j], type="l", col=col1)
}


############
# ?re

plot(eggs2, smolts2[11,,1], type="l",ylim=c(0,relx*5), xlim=c(0,5), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[11,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,11,]/nsim,S[i,11,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[11,,j], type="l", col=col1)
}


############
# L?gde

plot(eggs2, smolts2[12,,1], type="l",ylim=c(0,relx*6), xlim=c(0,6), col=col1, ylab="Smolts (thousands)",
     xlab="Eggs (millions)", main=Rivernames[12,], cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,12,]/nsim,S[i,12,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[12,,j], type="l", col=col1)
}


F4233_SR3 <- recordPlot()
F4233_SR3
 
################################################################################

par(mfrow=c(3,2),mar=c(4.5,4,3.5,1))

############
# K?ge?lven
plot(eggs, smolts[16,,1], type="l",ylim=c(0,relx*15), xlim=c(0,15), col=col1, 
     ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivernames[16,], 
     cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,16,]/nsim,S[i,16,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[16,,j], type="l", col=col1)
}

############
# Ljungan

plot(eggs2, smolts2[13,,1], type="l",ylim=c(0,relx*1.5), xlim=c(0,1.5), 
     col=col1, ylab="Smolts (thousands)",xlab="Eggs (millions)", main=Rivernames[13,], 
     cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,13,]/nsim,S[i,13,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[13,,j], type="l", col=col1)
}


############
# Testebo?n

plot(eggs2, smolts2[17,,1], type="l",ylim=c(0,20), xlim=c(0,6), 
     col=col1, ylab="Smolts (thousands)",  xlab="Eggs (millions)", main=Rivernames[17,], 
     cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in (smolt_year[17]-8):Nyears){
  points(E[i,17,]/nsim,S[i,17,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[17,,j], type="l", col=col1)
}

############
# M?rrum

plot(eggs, smolts[14,,1], type="l",ylim=c(0,150), xlim=c(0,100), col=col1, 
     ylab="Smolts (thousands)", xlab="Eggs (millions)", main=Rivernames[14,], 
     cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,14,]/nsim,S[i,14,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs, smolts[14,,j], type="l", col=col1)
}

############
# Em?n

plot(eggs2, smolts2[15,,1], type="l",ylim=c(0,20), xlim=c(0,10), 
     col=col1, ylab="Smolts (thousands)",  xlab="Eggs (millions)", main=Rivernames[15,], 
     cex.lab=cexLab, cex.axis=cexAxis, cex.main=cexMain)

for(i in 1:Nyears){
  points(E[i,15,]/nsim,S[i,15,], col=col2)
}
for(j in 2:length(samp)){
  points(eggs2, smolts2[15,,j], type="l", col=col1)
}

F4233_SR4 <- recordPlot()
F4233_SR4
 
## ---- graphs-SRcurves-tornereport



col2rgb("brown", alpha = FALSE)

col1<-rgb(1,0,0,0.03) # Viivat
col2<-rgb(0,0,1,0.3)  # 2015, sininen
col3<-rgb(0,0,0,0.3)  # 2016, harmaa
col4<-rgb(0.17,0.66,0.17,0.05)  # muut vuodet, vihre?

cexLab<-1.2
cexAxis<-1.2
cexMain<-1.5


 