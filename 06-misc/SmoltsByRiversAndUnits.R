# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Jokikohtaisten smolttim??rien tunnuslukujen talletus
# excel-tiedostoon. Aikajana kattaa sek? historiallisen osan (alkaen tosin
# vuodesta 92 vuoden 87 sijaan) ett? eteenp?in ennustavan osan.
# -------------------------
# ~*~ (C): Henni (9/2008) ~*~
# -------------------------
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

rm(list=ls(all=TRUE))

source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location

Model<-"2024_JAGS_Mps" # Assessment model version
nsim<-1000
LastHistYear<-2023
ymax<-350
LastPredYear<-LastHistYear+ymax


EffScen<-14

#Load the file containing stats
File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

#ScenProj_2024_JAGS_Mps_EScen1_RCzero23-35
File
load(File)


#! #############################################################################
################################################################################
Years<-c(1992:LastPredYear)
length(Years)

Nyears<-yBreak<-length(Years)
sims<-c(1:nsim)
Nstocks<-17; Nunits<-4 

# ls()

# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Lasketaan tunnusluvut eri joille, talletetaan excel-tiedostoon
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

River<-c("Torne","Simo","Kalix","Rane","Pite","Aby","Byske","Ricklean",
"Savaran","Ume/Vindel","Ore","Lodge","Ljungan","Morrumsan","Eman", "Kage", "Testebo")

dim(SmoltW)

for(r in 1:numStocks){
	Mean<-NULL; Sd<-NULL; Q5<-NULL; Median<-NULL; Q95<-NULL; PI<-NULL

	for(y in 1:length(Years)){
    
    sum_temp<-summary(as.mcmc(SmoltW[r,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
		
    Mean[y]<-sum_temp$statistics[1]
    Sd[y]<-sum_temp$statistics[2]
    Q5[y]<-sum_temp$quantiles[1]
		Median[y]<-sum_temp$quantiles[3]
		Q95[y]<-sum_temp$quantiles[5]

# This makes more decimals for the probability intervals of small rivers
		ifelse(r==8||r==9||r==13, 
		PI[y]<-paste(sep="", "'",round(Q5[y],1),"-",round(Q95[y],1)),
		PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0)))
	}

	t1<-rbind(rep(River[r],length(Years)),Years,Mean,Sd,Median,Q5,Q95)#,PI)

	t2<-rbind(rep(River[r],length(Years)),Years, PI)

	ifelse(r==1, table1<-t1, table1<-rbind(table1, t1))
	#ifelse(r==1, table2<-t2, table2<-rbind(table2, t2))
}

write_xlsx(as.data.frame(table1), paste0(PathOut_Scen,"SmoltsByRiver_",Model,"_EScen",EffScen,".xlsx"))


SmoltW_AU<-array(NA, dim=c(5,length(Years),nsim))

for(y in 1:length(Years)){
  for(s in 1:nsim){
    SmoltW_AU[1,y,s]<-sum(SmoltW[1:4,y,s])
    SmoltW_AU[2,y,s]<-sum(SmoltW[5:12,y,s])+SmoltW[16,y,s]
    SmoltW_AU[3,y,s]<-SmoltW[13,y,s]+SmoltW[17,y,s]
    SmoltW_AU[4,y,s]<-sum(SmoltW[14:15,y,s])
    SmoltW_AU[5,y,s]<-SmoltW_AU[1,y,s]+SmoltW_AU[2,y,s] # AU1-2!!!!!!!!!!!!!!!
  }  
}
    
for(u in 1:5){
	Mean<-NULL; Sd<-NULL; Q5<-NULL; Median<-NULL; Q95<-NULL; PI<-NULL

	for(y in 1:length(Years)){
		
		sum_temp<-summary(as.mcmc(SmoltW_AU[u,y,]), quantiles=c(0.05,0.1,0.5,0.8,0.95))
		
		Mean[y]<-sum_temp$statistics[1]
    Sd[y]<-sum_temp$statistics[2]    
		Q5[y]<-sum_temp$quantiles[1]
		Median[y]<-sum_temp$quantiles[3]
		Q95[y]<-sum_temp$quantiles[5]

# This makes more decimals for the probability intervals of small rivers
		ifelse(r==8||r==9||r==13, 
		PI[y]<-paste(sep="", "'",round(Q5[y],1),"-",round(Q95[y],1)),
		PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0)))
	}

	if(u<5){
	  t<-rbind(rep(paste0("AU",u),length(Years)),Years,Mean,Sd,Median,Q5,Q95)#,PI)
	  #t2<-rbind(rep(paste0("AU",u),length(Years)),Years, PI)
	}else{
	  t<-rbind(rep("AU1-2",length(Years)),Years,Mean,Sd,Median,Q5,Q95)#,PI)
	}
	
	
	ifelse(u==1, table<-t, table<-rbind(table, t))
	#ifelse(u==1, table2<-t2, table2<-rbind(table2, t2))
}

write_xlsx(as.data.frame(table), paste0(PathOut_Scen,"SmoltsByUnit_",Model,"_EScen",EffScen,".xlsx"))


