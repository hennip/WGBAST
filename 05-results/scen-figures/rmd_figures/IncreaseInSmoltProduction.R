# Draw density functions for river specific smolt production within a generation
# under different scenarios and compare with production in last year
# How is smolt production expected to change under different fishing pressures?

#rm(list=ls(all=TRUE))


#source("04-scenarios/paths_scens.r")
#source("04-scenarios/scens_stuff.r")

compyear<-31 # 2022
refyear<-37 # 2028 (ref year should be +6 for AU1-3 and +5 for AU4)
CompYear<-2022


SmoltList<-list()

#EffScen<-1

for(EffScen in 1:15){
  File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")
  load(File)
  SmoltList[[EffScen]]<-SmoltW
}

river<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
         ,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävåran"
         ,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
         ,"Emån", "Kågeälven", "Testeboån")

river<-c("Tornio", "Simo", "Kalix", "Råne", "Pite", "Åby", "Byske", "Rickleån",
"Sävarån", "Ume/Vindel", "Öre", "Lögde", "Ljungan", "Mörrumsån", "Emån", "Kåge")



MinSmolts<-c(
  700,10,100,0,
  10,0,30,0,
  0,50,0,0,
  0,0,0,0,0)
MaxSmolts<-c(
  3000,100,1500,150,
  50,20,350,15,
  30,400,80,50,
  2.5,80,20,70,8)

COL<-c(1,1,2,3,4,1,6,7,8,2,3,4,6,7,8,9)
LTY<-c(1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2)
LWD<-c(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

#windows(record=T)
par(mfrow=c(3,2),mar=c(4,2.5,4,1))
for(r in 1:Nstocks){
  # r<-1
  if(r==14 | r==15){ryear<-refyear-1}else{ryear<-refyear}
  
  plot(density(SmoltList[[1]][r,compyear,]), lwd=2, main=river[r], xlab="Smolt production",
       xlim=c(MinSmolts[r],MaxSmolts[r]), ylim = c(0, 
       max(density(SmoltList[[1]][r,compyear,])$y)*1.1)) # added this to make ylim adaptible
  abline(v=median(SmoltW[r,compyear,]), lwd=2)
  
  for(scen in 1:15){
    #scen<-6
    points(density(SmoltList[[scen]][r,ryear,]), lwd=LWD[scen+1], type="l", col=COL[scen+1], 
           lty=LTY[scen+1])
    abline(v=median(SmoltList[[scen]][r,ryear,]), col=COL[scen+1], lty=LTY[scen+1])
  } 
  legend("topright", col=COL, lwd=LWD, lty=LTY,
         legend=c(CompYear,"Scen1","Scen2","Scen3","Scen4","Scen5","Scen6","Scen7","Scen8","Scen9", "Scen10",
                  "Scen11","Scen12","Scen13","Scen14","Scen15"))
  
}  

p <- recordPlot();p


