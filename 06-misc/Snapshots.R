# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Statistics for number of salmon at different points of time/space
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


rm(list=ls(all=TRUE))

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

##############################################################################
stats0<-function(var){
  q5<-q50<-q95<-c()
  
    tmp<-summary(as.mcmc(var), quantiles=c(0.05,0.5,0.95))
    q5<-tmp$quantiles[1]
    q50<-tmp$quantiles[2]
    q95<-tmp$quantiles[3]
    
  #res<-cbind(year[1]:year[length(year)],q50,q5,q95)
  res<-cbind(q50,q5,q95)
  return(res)
}


stats<-function(var){
  q5<-q50<-q95<-c()
  
  for(i in 1:dim(var)[1]){
    tmp<-summary(as.mcmc(var[i,]), quantiles=c(0.05,0.5,0.95))
    q5[i]<-tmp$quantiles[1]
    q50[i]<-tmp$quantiles[2]
    q95[i]<-tmp$quantiles[3]
  }
  #res<-cbind(year[1]:year[length(year)],q50,q5,q95)
  res<-cbind(q50,q5,q95)
  return(res)
}


stats2<-function(x, qs, oneAge){

  if(oneAge==T){ # One age group, e.g. smolts / sum over ages
    res<-cbind(stats(x)[,qs])
    if(length(qs)==1){colnames(res)<-c("q50")}
    if(length(qs)==3){colnames(res)<-c("q50","q5","q95")}
    
  }
  if(oneAge==F){ # Ages 2:6
    res<-cbind(stats(x[2,,])[,qs],stats(x[3,,])[,qs],stats(x[4,,])[,qs],
          stats(x[5,,])[,qs],stats(x[6,,])[,qs])
    if(length(qs)==3){colnames(res)<-c("1SW_q50","1SW_q5","1SW_q95","2SW_q50","2SW_q5","2SW_q95",
                     "3SW_q50","3SW_q5","3SW_q95","4SW_q50","4SW_q5","4SW_q95",
                     "5SW_q50","5SW_q5","5SW_q95")}
    if(length(qs)==1){colnames(res)<-c("1SW_q50","2SW_q50","3SW_q50","4SW_q50","5SW_q50")}
  }
  return(res)
}

sumAges<-function(x){
  sumx<-array(NA, dim=c(dim(x)[2],dim(x)[3]))
  for(i in 1:dim(x)[2]){
    for(j in 1:dim(x)[3]){
      sumx[i,j]<-sum(x[2:6,i,j])
    }
  }  
  return(sumx)
}

sumAges2<-function(x,z1,z2){
#  x<-MatW_1[,2:Nyears,,]
#  dim(x)
#z1<-1
#z2<-4
  sumx<-array(NA, dim=c(dim(x)[2],dim(x)[4]))
  for(i in 1:dim(x)[2]){
    for(j in 1:dim(x)[4]){
      sumx[i,j]<-sum(x[2:6,i,z1:z2,j])
    }
  }  
  return(sumx)
}

#sumAges2(MatW_1[,2:Nyears,,],1,4)
#####################################################

# Coastal mortality (%)
coast_MW<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
for(y in 1:Nyears){
  for(r in 1:Nstocks){
    for(a in 1:6){
      for(s in 1:nsim){
        coast_MW[a,y,r,s]<-1-(exp(-(MW[a,y,r,2,s]*(1/12)))* # May
                                exp(-(MW[a,y,r,2,s]*F_seal[y,a,AU[r]]*(2/12)))) # June, July
      }
    }
  }
}
#1-(exp(-(MW[,,1,2,1]*(1/12)))

# River mortality (%)
river_MW<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
river_MR<-array(NA, dim=c(6,Nyears,4,nsim))
for(y in 1:Nyears){
    for(a in 1:6){
      for(s in 1:nsim){
        for(r in 1:Nstocks){
          river_MW[a,y,r,s]<-1-exp(-(MW[a,y,r,2,s]*(2/12))) # August, September     
        }
        for(u in 1:4){
        river_MR[a,y,u,s]<-1-exp(-(MR[a,y,u,2,s]*(2/12))) # August, September     
        }
      }
  }
}

# Approximated number ascending to rivers, river catch + number of spawners (those dying for natural mortality in river are missing)
ascW<-RiverCatchW+MatW_3+river_MW
ascR<-RiverCatchR+MatR_3+river_MR

dim(SmoltW) # number of smolts
dim(May1stW) # Total abundance May 1st
dim(ImmW_1) # Immature at May 1st
dim(MatW_1) # Mature at May 1st
dim(WCTN_C) # Coastal catch
dim(coast_MW) # Coastal natural mortality
dim(MatW_2) # Number ascending to rivers, history currently NA
dim(RiverCatchW) # river catch
dim(MatW_3) # Spawners per age

#apply(PFAW[,,1,],1:2,stats0)

#Stock specific PFA without addition from AU5-6
#This takes a while, choose only the stocks you need
PFAW2<-array(NA, dim=c(6, Nyears, 2, nsim))
for(i in 1:nsim){
  for(r in 1:2){
    for(y in 1:Nyears){
      for(a in 1:6){
        PFAW2[a,y,r,]<-PFAW[a,y,r,]/PropCW[y-a+6]
      }
    }
  }
}
dim(PFAW2)

# Torne
pfa<-cbind(stats(PFAW2[1,,1,])[,1],stats(PFAW2[2,,1,])[,1],stats(PFAW2[3,,1,])[,1],stats(PFAW2[4,,1,])[,1],
      stats(PFAW2[5,,1,])[,1]) # Includes post-smolts, age 6=0 because no immature at that age (PFA is calculated from immature at may 1st)
colnames(pfa)<-c("1SW_q50","2SW_q50","3SW_q50","4SW_q50","5SW_q50")

# 
may1st<-sum_immW<-sum_pfa<-array(NA, dim=c(dim(PFAW2)[2],dim(PFAW2)[4]))
for(i in 1:dim(PFAW2)[2]){
  for(j in 1:dim(PFAW2)[4]){
    sum_pfa[i,j]<-sum(PFAW2[2:6,i,1,j]) #!!!!!!!!! # Add/remove grilse!
    sum_immW[i,j]<-sum(ImmW_1[2:6,i,1,j])
    may1st[i,j]<-sum(May1stW[3:6,i,1,j]) # grilse = age 2, add or remove
  }
}
stats(sum_pfa)[,1]

immw<-rbind(rep(NA, 6),cbind(stats(ImmW_1[1,2:Nyears,1,])[,1],stats(ImmW_1[2,2:Nyears,1,])[,1],stats(ImmW_1[3,2:Nyears,1,])[,1],
      stats(ImmW_1[4,2:Nyears,1,])[,1],stats(ImmW_1[5,2:Nyears,1,])[,1],stats(ImmW_1[6,2:Nyears,1,])[,1]))
colnames(immw)<-c("1SW_q50","2SW_q50","3SW_q50","4SW_q50","5SW_q50","6SW_q50")



# Välituloksia Atsolle, mediaanit:

#3/6/24

# Yksilömääräiset saaliit kalastuksittain, mediaanit

# Torne
df<-cbind(
  stats2(sumAges(WOLL_C[,,1,]),1,T),
  stats2(sumAges(WCTN_C[,,1,]),1,T),
  stats2(sumAges(RiverCatchW[,,1,]),1,T)
)
colnames(df)<-c("OLL_Catch","CoastalCatch", "RiverCatch")

Torne_catch<-as_tibble(df)%>%
  mutate(y=Years)%>%
  select(y, everything())

write_xlsx(Torne_catch,path=paste0(PathOut_Scen,"Catches_Torne_medians.xlsx"))

# Simo
df<-cbind(
  stats2(sumAges(WOLL_C[,,2,]),1,T),
  stats2(sumAges(WCTN_C[,,2,]),1,T),
  stats2(sumAges(RiverCatchW[,,2,]),1,T)
)
colnames(df)<-c("OLL_Catch","CoastalCatch", "RiverCatch")

Simo_catch<-as_tibble(df)%>%
  mutate(y=Years)%>%
  select(y, everything())

write_xlsx(Simo_catch,path=paste0(PathOut_Scen,"Catches_Simo_medians.xlsx"))




# Torne
Torne<-as_tibble(cbind(
stats2(SmoltW[1,,],1,T),
pfa,
stats2(May1stW[,,1,],1,F),
immw,
#rbind(rep(NA, 5),stats2(ImmW_1[,2:Nyears,1,],1,F)),
rbind(rep(NA, 5),stats2(MatW_1[,2:Nyears,1,],1,F)),
stats2(WCTN_C[,,1,],1,F),
stats2(coast_MW[,,1,],1,F),
#rbind(array(NA, dim=c(28,5)),stats2(MatW_2[,29:Nyears,1,],1,F)),
stats2(ascW[,,1,],1,F),
stats2(RiverCatchW[,,1,],1,F),
stats2(MatW_3[,,1,],1,F)
),.name_repair="unique")%>%
  mutate(y=Years)%>%
  select(y, everything())

write_xlsx(Torne,path=paste0(PathOut_Scen,"Snapshots_medians_Torne.xlsx"))

# Torne, sums over ages

df<-cbind(
  stats2(SmoltW[1,,],1,T),
  stats(sum_pfa)[,1],
  stats(may1st)[,1],
  #stats2(sumAges(May1stW[,,1,]),1,T),
  c(NA,stats2(sumAges(MatW_1[,2:Nyears,1,]),1,T)),
  stats2(sumAges(WCTN_C[,,1,]),1,T),
  stats2(sumAges(ascW[,,1,]),1,T),
  stats2(sumAges(RiverCatchW[,,1,]),1,T),
  stats2(sumAges(MatW_3[,,1,]),1,T))
colnames(df)<-c("Smolts","PFA","May1st", "Mature_may1st", "CoastalCatch", "Asc_river", "RiverCatch", "Spawners")


TorneSums<-as_tibble(df)%>%
  mutate(y=Years)%>%
  select(y, everything())

write_xlsx(TorneSums,path=paste0(PathOut_Scen,"Snapshots_medians_TorneSumOverAges.xlsx"))

# Simo
Simo<-as_tibble(cbind(
  stats2(SmoltW[2,,],1,T),
  stats2(May1stW[,,2,],1,F),
  rbind(rep(NA, 5),stats2(MatW_1[,2:Nyears,2,],1,F)),
  stats2(WCTN_C[,,2,],1,F),
  stats2(coast_MW[,,2,],1,F),
  #rbind(array(NA, dim=c(28,5)),stats2(MatW_2[,29:Nyears,2,],1,F)),
  stats2(ascW[,,2,],1,F),
  stats2(RiverCatchW[,,2,],1,F),
  stats2(MatW_3[,,2,],1,F)
))%>%mutate(y=year)

write_xlsx(Torne,path=paste0(PathOut_Scen,"Snapshots_medians_Simo2.xlsx"))

# Simo, sums over ages

df<-cbind(
  stats2(SmoltW[2,,],1,T),
  stats2(sumAges(May1stW[,,2,]),1,T),
  c(NA,stats2(sumAges(MatW_1[,2:Nyears,2,]),1,T)),
  stats2(sumAges(WCTN_C[,,2,]),1,T),
  stats2(sumAges(ascW[,,2,]),1,T),
  stats2(sumAges(RiverCatchW[,,2,]),1,T),
  stats2(sumAges(MatW_3[,,2,]),1,T))
colnames(df)<-c("Smolts","May1st", "Mature_may1st", "CoastalCatch","Asc_river", "RiverCatch", "Spawners")


SimoSums<-as_tibble(df)%>%
  mutate(y=year)%>%
  select(y, everything())

write_xlsx(SimoSums,path=paste0(PathOut_Scen,"Snapshots_medians_SimoSumOverAges.xlsx"))


# Välituloksia Petrille, mediaanit + 90% PI


# * Pohjanlahdelle nousevat (AU1, AU2, AU1+2, villit ja viljellyt erikseen)
# * Koko Pohjanlahden saaliit (AU1, AU2, AU1+2, villit ja viljellyt erikseen)
# * Tornionjokeen ja Kalixjokeen nousevat

# Torne & Kalix ascending to river, sums over ages

df<-cbind(
  stats2(sumAges(ascW[,,1,]),1:3,T),
  stats2(sumAges(ascW[,,3,]),1:3,T),
  stats2(sumAges(ascW[,,1,]+ascW[,,3,]),1:3,T)
)
colnames(df)<-c(rep("Torne",3),rep("Kalix",3),rep("Tot",3))

df<-as_tibble(df)%>%
  mutate(y=year)%>%
  select(y, everything())

write_xlsx(df,path=paste0(PathScen,"TorneKalix_ascendingRiver2.xlsx"))

                
# df<-cbind(
#   stats2(SmoltW[3,,],1:3,T),
#   stats2(sumAges(May1stW[,,3,]),1:3,T),
#   rbind(rep(NA,3),stats2(sumAges(MatW_1[,2:Nyears,3,]),1:3,T)),
#   stats2(sumAges(WCTN_C[,,3,]),1:3,T),
#   stats2(sumAges(coast_MW[,,3,]),1:3,T),
#   stats2(sumAges(ascW[,,3,]),1:3,T),
#   stats2(sumAges(RiverCatchW[,,3,]),1:3,T),
#   stats2(sumAges(MatW_3[,,3,]),1:3,T))
# colnames(df)<-c(rep("Smolts",3),rep("May1st",3),rep("Mature_may1st",3),rep("CoastalCatch",3),
#                 rep("CoastalMW",3),rep("Asc_river",3),rep("RiverCatch",3),rep("Spawners",3))
# 
# 
# KalixSums<-as_tibble(df)%>%
#   mutate(y=year)%>%
#   select(y, everything())

# Number migrating towards BB (Mature at May 1st)

# Mature AU1 wild
df<-cbind(
stats2(sumAges(MatW_1[,2:Nyears,1,]+MatW_1[,2:Nyears,2,]+
                 MatW_1[,2:Nyears,3,]+MatW_1[,2:Nyears,4,]),1:3,T),
#stats2(sumAges2(MatW_1[,2:Nyears,,],1,4),1:3,T)

# Mature AU2 wild
stats2(sumAges(MatW_1[,2:Nyears,5,]+MatW_1[,2:Nyears,6,]+MatW_1[,2:Nyears,7,]+
                 MatW_1[,2:Nyears,8,]+MatW_1[,2:Nyears,9,]+MatW_1[,2:Nyears,10,]+MatW_1[,2:Nyears,11,]+
               MatW_1[,2:Nyears,12,]+MatW_1[,2:Nyears,16,]),1:3,T),

# Mature AU1+AU2 wild
stats2(sumAges(MatW_1[,2:Nyears,1,]+MatW_1[,2:Nyears,2,]+MatW_1[,2:Nyears,3,]+MatW_1[,2:Nyears,4,]+
                 MatW_1[,2:Nyears,5,]+MatW_1[,2:Nyears,6,]+MatW_1[,2:Nyears,7,]+
                 MatW_1[,2:Nyears,8,]+MatW_1[,2:Nyears,9,]+MatW_1[,2:Nyears,10,]+MatW_1[,2:Nyears,11,]+
                 MatW_1[,2:Nyears,12,]+MatW_1[,2:Nyears,16,]),1:3,T),


# Mature AU1 reared
stats2(sumAges(MatR_1[,2:Nyears,1,]),1:3,T),

# Mature AU2 reared
stats2(sumAges(MatR_1[,2:Nyears,2,]),1:3,T),

# Mature AU1+AU2 reared
stats2(sumAges(MatR_1[,2:Nyears,1,]+MatR_1[,2:Nyears,2,]),1:3,T)
)
colnames(df)<-c(rep("Wild_AU1",3),rep("Wild_AU2",3),rep("Wild_AU1+2",3),
                rep("Reared_AU1",3),rep("Reared_AU2",3),rep("Reared_AU1+2",3))

df<-as_tibble(df)%>%
  mutate(y=year[2:Nyears])%>%
  select(y, everything())


write_xlsx(df,path=paste0(PathScen,"AU1-2_migrBB2.xlsx"))

# Bothnian Bay coastal catch


# Mature AU1 wild
df<-cbind(
  stats2(sumAges(WCTN_C[,,1,]+WCTN_C[,,2,]+
                   WCTN_C[,,3,]+WCTN_C[,,4,]),1:3,T),
  
  # Mature AU2 wild
  stats2(sumAges(WCTN_C[,,5,]+WCTN_C[,,6,]+WCTN_C[,,7,]+
                   WCTN_C[,,8,]+WCTN_C[,,9,]+WCTN_C[,,10,]+WCTN_C[,,11,]+
                   WCTN_C[,,12,]+WCTN_C[,,16,]),1:3,T),
  
  # Mature AU1+AU2 wild
  stats2(sumAges(WCTN_C[,,1,]+WCTN_C[,,2,]+WCTN_C[,,3,]+WCTN_C[,,4,]+
                   WCTN_C[,,5,]+WCTN_C[,,6,]+WCTN_C[,,7,]+
                   WCTN_C[,,8,]+WCTN_C[,,9,]+WCTN_C[,,10,]+WCTN_C[,,11,]+
                   WCTN_C[,,12,]+WCTN_C[,,16,]),1:3,T),
  
  
  # Mature AU1 reared
  stats2(sumAges(RCTN_C[,,1,]),1:3,T),
  
  # Mature AU2 reared
  stats2(sumAges(RCTN_C[,,2,]),1:3,T),
  
  # Mature AU1+AU2 reared
  stats2(sumAges(RCTN_C[,,1,]+RCTN_C[,,2,]),1:3,T)
)
colnames(df)<-c(rep("Wild_AU1",3),rep("Wild_AU2",3),rep("Wild_AU1+2",3),
                rep("Reared_AU1",3),rep("Reared_AU2",3),rep("Reared_AU1+2",3))

df<-as_tibble(df)%>%
  mutate(y=year)%>%
  select(y, everything())

write_xlsx(df,path=paste0(PathScen,"AU1-2_BBcatch.xlsx"))

# Bothnian Bay ascending to river


# Mature AU1 wild
dim(ascW)
df<-cbind(
  stats2(sumAges(ascW[,,1,]+ascW[,,2,]+
                   ascW[,,3,]+ascW[,,4,]),1:3,T),
  
  # Mature AU2 wild
  stats2(sumAges(ascW[,,5,]+ascW[,,6,]+ascW[,,7,]+
                   ascW[,,8,]+ascW[,,9,]+ascW[,,10,]+ascW[,,11,]+
                   ascW[,,12,]+ascW[,,16,]),1:3,T),
  
  # Mature AU1+AU2 wild
  stats2(sumAges(ascW[,,1,]+ascW[,,2,]+ascW[,,3,]+ascW[,,4,]+
                   ascW[,,5,]+ascW[,,6,]+ascW[,,7,]+
                   ascW[,,8,]+ascW[,,9,]+ascW[,,10,]+ascW[,,11,]+
                   ascW[,,12,]+ascW[,,16,]),1:3,T),

  # Mature AU1 reared
  stats2(sumAges(ascR[,,1,]),1:3,T),
  
  # Mature AU2 reared
  stats2(sumAges(ascR[,,2,]),1:3,T),
  
  # Mature AU1+AU2 reared
  stats2(sumAges(ascR[,,1,]+ascR[,,2,]),1:3,T)
)
colnames(df)<-c(rep("Wild_AU1",3),rep("Wild_AU2",3),rep("Wild_AU1+2",3),
                rep("Reared_AU1",3),rep("Reared_AU2",3),rep("Reared_AU1+2",3))

df<-as_tibble(df)%>%
  mutate(y=year)%>%
  select(y, everything())

write_xlsx(df,path=paste0(PathScen,"AU1-2_ascendingRiver.xlsx"))
