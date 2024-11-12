 

##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixälven"
#4 "Råneälven"
#5 "Piteälven"
#6 "Åbyälven"
#7 "Byskeälven"
#8 "Rickleån"
#9 "Sävarån"
#10 "Vindelälven"
#11 "Öreälven"
#12 "Logdeälven"
#13 "Ljungan"
#14 "Mörrumsån"
#15 "Emån"
#16 "Kågeälven"

setwd("C:/WGBAST15/")
source("plotfunctions.r")
source("parajags4.r")
source("//storage-dh.slu.se/home$/rewh0001/My Documents/ICES WGBAST/2018/box_functions.r")

library(R2jags)
library(runjags)
library(rjags)

#load("JAGS final 2018/new_SR_HRR2018-03-22.Rdata")  
load("Assessment results/FLHM_results_2019_select_chains.RData") 
d<-as.matrix(chains_new)

first_year<-1987
p_year<-1997         #first year in plot
assess_year<-2019
years<-length(first_year:assess_year)
maxage<-6
stock_indices<-1:17
stocks<-length(stock_indices)
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)

tend<-years+2
yend<-1987+years+1

R0_mult<-0.75
R0_mult1<-0.50

#get SmoltW variables into array
sm<-d[ ,grep("SmoltWW\\[",colnames(d))]  #34 x 16 for 2018 assess
smolts<-array(0,dim=c(years+2,stocks,dim(d)[1]))           
smolts_au<-array(0,dim=c(years+2,5,dim(d)[1]))     #AU 1-4, TOTAL

for(y in 1:(years+2)){  #year       
            for(s in 1:stocks){
                  smolts[y,s,]<-sm[ ,(s-1)*(years+2)+y]         
      }
}

for(y in 1:(years+2)){  #year
smolts_au[y,1,]<-apply(smolts[y,which(AU==1),],2,sum)
smolts_au[y,2,]<-apply(smolts[y,which(AU==2),],2,sum)
smolts_au[y,3,]<-apply(smolts[y,which(AU==3),],2,sum)
smolts_au[y,4,]<-apply(smolts[y,which(AU==4),],2,sum)
smolts_au[y,5,]<-apply(smolts[y,1:stocks,],2,sum)
}  

#R_zero<-array(NA,dim=c(2000,32,16))
#   for(s in 1:16){
#        for(y in 1:32){ 
#          tempr<-paste0("R0[",y,",",s,"]")
#
#          R_zero[,y,s]<-d[,grep(tempr,colnames(d),fixed=TRUE)]
#
#        }
#}

#R0_au<-array(NA,dim=c(5,2000))
#for(i in 1:4){
#R0_au[i ,]<-apply(R_zero[ ,years,which(AU==i)],1,sum)
#}
#
#R0_au[3,]<-R_zero[ ,years,which(AU==3)]
#R0_au[5,]<-apply(R_zero[ ,years,1:stocks],1,sum)

#2019 only

e_delay<-c(rep(4,times=13),3,3,4,3)
stocks1<-which(e_delay==4)
stocks2<-which(e_delay==3)

R_zero<-array(NA,dim=c(dim(d)[1],years,stocks))
   for(s in 1:stocks){
        for(y in 1:years){ 
          tempr<-paste0("R0[",y,",",s,"]")

          R_zero[1:dim(d)[1],y,s]<-d[,grep(tempr,colnames(d),fixed=TRUE)]

        }
}

R0_final<-matrix(0,nrow=dim(d)[1],ncol=stocks)
for(s in stocks1){ 
R0_final[,s]<-R_zero[,28,s] 
}
for(s in stocks2){ 
R0_final[,s]<-R_zero[,29,s] 
}
######################
R0_au<-array(NA,dim=c(5,2000))
for(i in 1:4){
R0_au[i ,]<-apply(R0_final[ ,which(AU==i)],1,sum)
}


R0_au[5,]<-apply(R0_final,1,sum)

#2019 only



windows(12,12)
par(mfrow=c(3,2),mar=c(3,4,2,0.1),oma=c(1,1,1,1),font=2,font.lab=2,font.axis=2)
bx90(smolts_au[,1,],1997,yend,1,0,4000,beg=11,end=tend,border="black",main="AU 1", cex.main=1);

abline(a = median(R0_au[1,]*R0_mult), b = 0,col="black")
abline(a = median(R0_au[1,]*R0_mult1), b = 0,col="black",lty=2)

bx90(smolts_au[,2,],1997,yend,1,0,800,beg=11,end=tend,border="black",main="AU 2", cex.main=1);

abline(a = median(R0_au[2,]*R0_mult), b = 0,col="black")
abline(a = median(R0_au[2,]*R0_mult1), b = 0,col="black",lty=2)

bx90(smolts_au[,3,],1997,yend,1,0,10,beg=11,end=tend,border="black",main="AU 3", cex.main=1);

abline(a = median(R0_au[3,]*R0_mult), b = 0,col="black")
abline(a = median(R0_au[3,]*R0_mult1), b = 0,col="black",lty=2)

bx90(smolts_au[,4,],1997,yend,1,0,120,beg=11,end=tend,border="black",main="AU 4", cex.main=1);

abline(a = median(R0_au[4,]*R0_mult), b = 0,col="black")
abline(a = median(R0_au[4,]*R0_mult1), b = 0,col="black",lty=2)

bx90(smolts_au[,5,],1997,yend,1,0,5000,beg=11,end=tend,border="black",main="AU 1-4 total", cex.main=1);

abline(a = median(R0_au[5,]*R0_mult), b = 0,col="black")
abline(a = median(R0_au[5,]*R0_mult1), b = 0,col="black",lty=2)

mtext("Smolt production (thousands)", side = 2, line = -1, outer = TRUE,cex=0.8)
#mtext("Year", side = 1, line = 2, outer = TRUE, cex=0.8)



