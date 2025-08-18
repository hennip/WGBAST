# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Calculates number of spawners per year and assessment unit/river,
# for wild, reared and in total
# saves output in two xlsx files 
# -------------------------
# ~*~ (C): Henni (2008/2011/2025) ~*~
# -------------------------
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#rm(list=ls(all=TRUE))

# Skip == T when running 05-results/workflow-extra-output 
if(exists("skip")==F){
  
source("run-this-first.R") # This file should be located at the root of the Rproject file. If not using Rstudio, pls define the location


Model<-"2024_JAGS_Mps" # Assessment model version
nsim<-1000
LastHistYear<-2023
ymax<-350
LastPredYear<-LastHistYear+ymax
Years<-c(1992:LastPredYear)
length(Years)

Nyears<-yBreak<-length(Years)
sims<-c(1:nsim)
Nstocks<-17; Nunits<-4 


EffScen<-14

#Load the file containing stats
File<-paste0(PathOut_Scen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

File
load(File)
}
#! #############################################################################
################################################################################


# Function stats calculates a set of statistics from a vector 
stats<-function(dat){

		summary_dat<-summary(as.mcmc(dat), quantiles=c(0.05,0.1,0.5,0.8,0.95))
		M<-mean(dat)
   	S<-sd(dat)
  	q5<-summary_dat$quantiles[1]
		q25<-summary_dat$quantiles[2]
		q50<-summary_dat$quantiles[3]
		q75<-summary_dat$quantiles[4]
		q95<-summary_dat$quantiles[5]
	
return(data.frame(M,S,q5,q50,q95))
}

# year: 92=1, 2007=16
NspW<-array(NA, dim=c(4,length(Years),nsim))
NspR<-array(NA, dim=c(4,length(Years),nsim))
for(y in 1:length(Years)){
  for(s in 1:nsim){
    NspW[1,y,s]<-sum(SpawnerW[1:4,y,s])
    NspW[2,y,s]<-sum(SpawnerW[5:12,y,s])+SpawnerW[16,y,s]
    NspW[3,y,s]<-SpawnerW[13,y,s]+SpawnerW[17,y,s]
    NspW[4,y,s]<-sum(SpawnerW[14:15,y,s])
    
    NspR[1,y,s]<-SpawnerR[1,y,s]
    NspR[2,y,s]<-SpawnerR[2,y,s]
    NspR[3,y,s]<-SpawnerR[3,y,s]
    NspR[4,y,s]<-SpawnerR[4,y,s]
  }
}

#*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
#   Calculate statistics separately for wild and reared by units and 
#   as totals per unit.
#*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

#*~*~*~*~*~*~*~**~*~*~*
#   ASSESSMENT UNITS  
#*~*~*~*~*~*~*~**~*~*~*
for(u in 1:4){
  if(u==1){data1<-as.mcmc(NspW[1,,]); data2<-as.mcmc(NspR[1,,])}
	if(u==2){data1<-as.mcmc(NspW[2,,]);	data2<-as.mcmc(NspR[2,,])}
	if(u==3){data1<-as.mcmc(NspW[3,,]);	data2<-as.mcmc(NspR[3,,])}
	if(u==4){data1<-as.mcmc(NspW[4,,]);	data2<-as.mcmc(NspR[4,,])}
	
	for(t in 1:2){
		Mean<-NULL; Q95<-NULL; Q5<-NULL; Sd<-NULL; Median<-NULL;
		if(t==1){dat<-data1}
		if(t==2){dat<-data2}

		for(y in 1:length(Years)){
	
			result<-stats(dat[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
	
		}
if(u==1&&t==1){t1w<-rbind(rep("AU1/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2&&t==1){t2w<-rbind(rep("AU2/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3&&t==1){t3w<-rbind(rep("AU3/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4&&t==1){t4w<-rbind(rep("AU4/Wild", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==1&&t==2){t1r<-rbind(rep("AU1/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2&&t==2){t2r<-rbind(rep("AU2/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3&&t==2){t3r<-rbind(rep("AU3/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4&&t==2){t4r<-rbind(rep("AU4/Reared", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
	}

# Totals per unit
	Mean<-NULL; Q95<-NULL; 
	Q5<-NULL; Sd<-NULL; Median<-NULL;

	dataTot<-data1+data2

		for(y in 1:length(Years)){
	
			result<-stats(dataTot[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
	 }

if(u==1){t1tot<-rbind(rep("AU1_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==2){t2tot<-rbind(rep("AU2_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==3){t3tot<-rbind(rep("AU3_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}
if(u==4){t4tot<-rbind(rep("AU4_Tot", length(Years)),
			Years,Mean,Sd,Median,Q5,Q95)}

}

#*~*~*~*~*~*~*~**~*~*~*
#   RIVERS  
#*~*~*~*~*~*~*~**~*~*~*


for(r in 1:Nstocks){
  Mean<-NULL;  Q95<-NULL;  Q5<-NULL; 
  PI<-NULL; Sd<-NULL; Median<-NULL;

  dat<-as.mcmc(SpawnerW[r,,])  
  
  for(y in 1:length(Years)){
    
			result<-stats(dat[y,])
	    Q5[y]<-summary(as.mcmc(dat[y,]))$quantiles[1]
	    Q95[y]<-summary(as.mcmc(dat[y,]))$quantiles[5]
	    Median[y]<-summary(as.mcmc(dat[y,]))$quantiles[3]
      Mean[y]<-summary(as.mcmc(dat[y,]))$statistics[1]
      Sd[y]<-summary(as.mcmc(dat[y,]))$statistics[2]
      PI[y]<-paste(sep="", "'",round(Q5[y],0),"-",round(Q95[y],0))

  }

if(r==1){t_tornio<-rbind(rep("Tornio", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==2){t_simo<-rbind(rep("Simo", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==3){t_kalix<-rbind(rep("Kalix", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==4){t_rane<-rbind(rep("Rane", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==5){t_pite<-rbind(rep("Pite", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==6){t_aby<-rbind(rep("Aby", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==7){t_byske<-rbind(rep("Byske", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==8){t_rick<-rbind(rep("Ricklean", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==9){t_savaran<-rbind(rep("Savaran", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==10){t_vindel<-rbind(rep("Ume/Vindel", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==11){t_orea<-rbind(rep("Ore", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==12){t_lodge<-rbind(rep("Lodge", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==13){t_ljungan<-rbind(rep("Ljungan", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==14){t_morrumsan<-rbind(rep("Morrum", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
if(r==15){t_eman<-rbind(rep("Eman", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
  if(r==16){t_kage<-rbind(rep("Kage", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
  if(r==17){t_test<-rbind(rep("Test", length(Years)),Years,Mean,Sd,Median,Q5,Q95)}
}

#########################
rows<-c("X", "Years", "Mean", "Sd", "Median", "q5", "q95")
t_units<-as.data.frame(cbind(rep(rows,12),
                             rbind(t1w,t2w,t3w,t4w,t1r,t2r,t3r,t4r,t1tot,t2tot,t3tot,t4tot)))

t_rivers<-as.data.frame(cbind(rep(rows,17),
  rbind(t_tornio, t_simo,t_kalix,t_rane,t_pite,t_aby,t_byske,t_rick,
t_savaran,t_vindel,t_orea,t_lodge,t_ljungan,t_morrumsan,t_eman, t_kage, t_test)))

write_xlsx(t_units,paste0(PathOut_Scen,"SpawnersByUnit_",Model,"_EScen",EffScen,".xlsx"))
write_xlsx(t_rivers,paste0(PathOut_Scen,"SpawnersByRiver_",Model,"_EScen",EffScen,".xlsx"))


