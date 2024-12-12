# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.4

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*



tend<-years
yend<-1987+years-1

e_delay<-c(rep(4,times=13),3,3,4,3)
stocks1<-which(e_delay==4)
stocks2<-which(e_delay==3)

final_year<-years-1   

#Introduce the different names for the different salmon stocks
RiverNames<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
              ,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävarån"
              ,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
              ,"Emån", "Kågeälven","Testeboån")
stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)


# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
par(mfrow=c(2,3),mar=c(1,4,1.5,0.1),oma=c(4,1,1,1),font=2,font.lab=2,font.axis=2)

plot(density(Smolt_lim[1,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[1,]))),
     ylim=c(0,max(pretty(density(Smolt_lim[1,])$y)) +0.002))
lines(density(Smolt_MSY[1,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[1,]),xlab="",ylab="",lty=2)
legend("topleft", "Tornionjoki, Unit 1", bty="n") 




plot(density(Smolt_lim[2,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,100),
     ylim=c(0,max(pretty(density(Smolt_lim[2,])$y))+0.02))
lines(density(Smolt_MSY[2,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[2,]),xlab="",ylab="",lty=2)
legend("topleft", "Simojoki, Unit 1", bty="n")

plot(density(Smolt_lim[3,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[3,]))),
     ylim=c(0,max(pretty(density(Smolt_lim[3,])$y))+0.002))
lines(density(Smolt_MSY[3,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[3,]),xlab="",ylab="",lty=2)
legend("topleft", "Kalixälven, Unit 1", bty="n")

plot(density(Smolt_lim[4,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,150),ylim=c(0,max(pretty(density(Smolt_lim[4,])$y))))
lines(density(Smolt_MSY[4,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[4,]),xlab="",ylab="",lty=2)

legend("topleft", "Råneälven, Unit 1", bty="n")
plot(density(Smolt_lim[5,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[5,]))),
     ylim=c(0,max(pretty(density(Smolt_lim[5,])$y))+0.1))
lines(density(Smolt_MSY[5,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[5,]),xlab="",ylab="",lty=2)

legend("topleft", "Piteälven, Unit 2", bty="n")

plot(density(Smolt_lim[6,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,50),ylim=c(0,max(pretty(density(Smolt_lim[6,])$y))))
lines(density(Smolt_MSY[6,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[6,]),xlab="",ylab="",lty=2)
legend("topleft", "Åbyälven, Unit 2", bty="n")

mtext("Probability density", side = 2, line = -1, outer = TRUE, cex=0.9)
mtext("Smolts (thousands)", side = 1, line = 2, outer = TRUE, cex=0.9)

p1 <- recordPlot()
p1

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

par(mfrow=c(2,3),mar=c(1,4,1.5,0.1),oma=c(4,1,1,1),font=2,font.lab=2,font.axis=2)

plot(density(Smolt_lim[7,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[7,]))),ylim=c(0,max(pretty(density(Smolt_lim[7,])$y))))
lines(density(Smolt_MSY[7,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[7,]),xlab="",ylab="",lty=2)
legend("topleft", "Byskeälven, Unit 2", bty="n") 




plot(density(Smolt_lim[8,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[8,]))),ylim=c(0,max(pretty(density(Smolt_lim[8,])$y))))
lines(density(Smolt_MSY[8,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[8,]),xlab="",ylab="",lty=2)
legend("topleft", "Rickleån, Unit 2", bty="n")


plot(density(Smolt_lim[9,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,100),ylim=c(0,max(pretty(density(Smolt_lim[9,])$y))))
lines(density(Smolt_MSY[9,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[9,]),xlab="",ylab="",lty=2)
legend("topleft", "Sävarån, Unit 2", bty="n")


plot(density(Smolt_lim[10,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[10,]))),ylim=c(0,max(pretty(density(Smolt_lim[10,])$y))))
lines(density(Smolt_MSY[10,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[10,]),xlab="",ylab="",lty=2)
legend("topleft", "Ume/Vindelälven, Unit 2", bty="n")


plot(density(Smolt_lim[11,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,200),ylim=c(0,max(pretty(density(Smolt_lim[11,])$y))))
lines(density(Smolt_MSY[11,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[11,]),xlab="",ylab="",lty=2)
legend("topleft", "Öreälven, Unit 2", bty="n")



plot(density(Smolt_lim[12,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,200),ylim=c(0,max(pretty(density(Smolt_lim[12,])$y))))
lines(density(Smolt_MSY[12,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[12,]),xlab="",ylab="",lty=2)
legend("topleft", "Lögdeälven, Unit 2", bty="n")

mtext("Probability density", side = 2, line = -1, outer = TRUE, cex=0.9)
mtext("Smolts (thousands)", side = 1, line = 2, outer = TRUE, cex=0.9)

p2 <- recordPlot()
p2

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
par(mfrow=c(2,3),mar=c(1,4,1.5,0.1),oma=c(4,1,1,1),font=2,font.lab=2,font.axis=2)


plot(density(Smolt_lim[16,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,80),
     ylim=c(0,max(pretty(density(Smolt_lim[16,])$y))+0.02))
lines(density(Smolt_MSY[16,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[16,]),xlab="",ylab="",lty=2)
legend("topleft", "Kågeälven, Unit 2", bty="n") 

plot(density(Smolt_lim[13,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,10),ylim=c(0,max(pretty(density(Smolt_lim[13,])$y))))
lines(density(Smolt_MSY[13,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[13,]),xlab="",ylab="",lty=2)
legend("topleft", "Ljungan, Unit 3", bty="n") 

plot(density(Smolt_lim[17,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,8),ylim=c(0,max(pretty(density(Smolt_lim[17,])$y))))
lines(density(Smolt_MSY[17,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[17,]),xlab="",ylab="",lty=2)
legend("topleft", "Testeboån, Unit 3", bty="n")


plot(density(Smolt_lim[15,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,40),
     ylim=c(0,max(pretty(density(Smolt_lim[15,])$y))+0.05))
lines(density(Smolt_MSY[15,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[15,]),xlab="",ylab="",lty=2)
legend("topleft", "Emån, Unit 4", bty="n")


plot(density(Smolt_lim[14,]),xlab="",ylab="",main="",lwd=1,xlim=c(0,max(pretty(R0_all[14,]))),ylim=c(0,max(pretty(density(Smolt_lim[14,])$y))))
lines(density(Smolt_MSY[14,]),xlab="",ylab="",lwd=2)
lines(density(R0_all[14,]),xlab="",ylab="",lty=2)
legend("topleft", "Mörrumsån, Unit 4", bty="n")

mtext("Probability density", side = 2, line = -1, outer = TRUE, cex=0.9)
mtext("Smolts (thousands)", side = 1, line = 2, outer = TRUE, cex=0.9)

p3 <- recordPlot()
p3
