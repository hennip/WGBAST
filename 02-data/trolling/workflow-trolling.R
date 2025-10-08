# Model: Trolling_catch_all_countries_Tot_Landed_and_Released_separeated
# Model translates the triangular distributions as Lognormal distributions
# and sums the distributions in totals
# And outputs Mean and SD values for these
# Here used to compute the trolling cathes over all countries in three areas
# Monitor Tot_Landed, Tot_Released and Tot_Catch

# use Landed catch estimates for catch tables and graphs! 
# For country specific estimates monitor d[i,j,k,n] correspondingly -> samplesSet(d[,,,]) in script file
# country specific estimates has not been presented in WGBAST report -> focus in totals by area

# Model name: Trolling_catch_all_countries_Tot_Landed_and_Released_separated
# Sript name: script_Trolling_catch_all_countries_Tot_Landed_Released_separ

# Data: 1) MBasin.txt 2) SD29-31.txt 3) SD32.txt
#
# indexing [i,j,k,n]
#	i=year {1,...,n}; 1=1987,...
#	j=country {1,...,9}; 1=SWE, 2=DEN, 3=GER, 4=POL, 5=LIT, 6=LAT, 7=EST, 8=RUS, 9=FIN
#    k=catch category {1,2}; 1=retained catch, 2=released catch
#	n=fishery {1,2,3}; 1=Main Basin, 2=Northern Baltic sea and Gulf of Bothnia, 3=Gulf of Finland


source("../run-this-first-wgbast.R")
min_year<-1987
max_year<-2024
years<-min_year:max_year
NumYears<-length(years)

# Later the data should be included here
#source("02-data/catch-effort/read-in-wgbast-catch&effort.r")
#df_all<-wgbast_catch_data

# .. But for now it is collected in this excel on various sheets:
file_tr<-"../../WGBAST_shared/submodels/trolling/Baltic salmon trolling catches_expert elicitation ver_19-02-2025.xlsx"
col_nms<-c("min", "mode", "max")
r1<-"B5:D42"
r2<-"F5:H42"

se2228_land<-read_xlsx(file_tr, col_names = col_nms, sheet="SWE_SD22-28", range=r1)
se2228_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="SWE_SD22-28", range=r2)
se29_land<-read_xlsx(file_tr, col_names = col_nms, sheet="SWE_SD29", range=r1)
se29_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="SWE_SD29", range=r2)
dk_land<-read_xlsx(file_tr, col_names = col_nms, sheet="DEN", range=r1)
dk_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="DEN", range=r2)
ge_land<-read_xlsx(file_tr, col_names = col_nms, sheet="GER", range=r1)
ge_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="GER", range=r2)
po_land<-read_xlsx(file_tr, col_names = col_nms, sheet="POL", range=r1)
po_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="POL", range=r2)
li_land<-read_xlsx(file_tr, col_names = col_nms, sheet="LIT", range=r1)
li_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="LIT", range=r2)
la_land<-read_xlsx(file_tr, col_names = col_nms, sheet="LAT", range=r1)
la_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="LAT", range=r2)
ee_land<-read_xlsx(file_tr, col_names = col_nms, sheet="EST", range=r1)
ee_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="EST", range=r2)
ru_land<-read_xlsx(file_tr, col_names = col_nms, sheet="RUS", range=r1)
ru_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="RUS", range=r2)
fi2931_land<-read_xlsx(file_tr, col_names = col_nms, sheet="FIN_SD2931", range=r1)
fi2931_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="FIN_SD2931", range=r2)
fi32_land<-read_xlsx(file_tr, col_names = col_nms, sheet="FIN_SD32", range=r1)
fi32_rel<-read_xlsx(file_tr,col_names = col_nms, sheet="FIN_SD32", range=r2)

# Replace 0's in german data with small constants (don't know why this was not done in BUGS, dlnorm should not work there either with zeros
ge_land<-ge_land |> mutate(min=ifelse(min==0, 0.001, min)) |> 
  mutate(mode=ifelse(mode==0, 0.01, mode)) |> 
  mutate(max=ifelse(max==0, 0.1, max)) 

ge_rel<-ge_rel |> mutate(min=ifelse(min==0, 0.001, min)) |> 
  mutate(mode=ifelse(mode==0, 0.01, mode)) |> 
  mutate(max=ifelse(max==0, 0.1, max)) 

#
# ini_min<-0.001
# ini_mode<-0.01
# ini_max<-0.1

ini<-cbind(rep(0.001, NumYears), 
           rep(0.01, NumYears),
           rep(0.1, NumYears))
colnames(ini)<-col_nms

ini<-array(NA, dim=c(NumYears,2,2))

###############################################################
# Function to calculate mu and cv of a triangular distribution
###############################################################

stats<-function(g){
CV<-Var<-Mu<-c()
for( i in 1:NumYears){
  #mu[i,j,k,n]<-(Min[i,j,k,n]+Mod[i,j,k,n]+Max[i,j,k,n])/3
  #var[i,j,k,n]<-(pow(Min[i,j,k,n],2)+pow(Mod[i,j,k,n],2)+pow(Max[i,j,k,n],2)-Min[i,j,k,n]*Max[i,j,k,n]-Min[i,j,k,n]*Mod[i,j,k,n]-Mod[i,j,k,n]*Max[i,j,k,n])/18
  #cv[i,j,k,n]<-sqrt(var[i,j,k,n])/mu[i,j,k,n]
  Mu[i]<-sum(g[i,])/3
  Var[i]<-(g$min[i]^2+g$mode[i]^2+g$max[i]^2-g$min[i]*g$max[i]-g$min[i]*g$mode[i]-g$mode[i]*g$max[i])/18
  CV[i]<-sqrt(Var[i])/Mu[i]
}
return(cbind(Mu, CV))
}
#stats(ee_land)
#stats(as.data.frame(ini))

##############################
# Same model for all areas! 
##############################
M_trolling<-"
model{
  for (i in 1:NumYears){	# 38 = 2024
    Tot_Landed[i]<-sum(d[i,1:NumCountries,1]) 	# Total retained catch by area = estimate without mortality from released catch
    Tot_Released[i]<-sum(d[i,1:NumCountries,2])	# Total catch released back to sea by area
    Tot_Catch[i]<-Tot_Landed[i] + Tot_Released[i]   # Total catch by area, no mortality accounted for relesed salmon 
    Tot_Catch_Dead[i]<-Tot_Landed[i] + Tot_Released[i]*0.25 	# Total dead catch by area, 25% mortality accounted for released salmon
    
    for (j in 1:NumCountries){ #	j=country {1,...,9}; 1=SWE, 2=DEN, 3=GER, 4=POL, 5=LIT, 6=LAT, 7=EST, 8=RUS, 9=FIN
      for (k in 1:2){ # 1: retained catch; 2: released catch 
        M[i,j,k]<-log(mu[i,j,k])-0.5/tau[i,j,k]
        tau[i,j,k]<-1/log(pow(cv[i,j,k],2)+1)
        
        d[i,j,k]~dlnorm(M[i,j,k],tau[i,j,k])
      }
    }
  }
}"



####################
# SD32
####################
# 6 countries with no data (close to zero)
# -> only FI & EE in sd 32
ini<-array(NA, dim=c(NumYears,2,2))

mu32<-ini
mu32[,1,1]<-stats(ee_land)[,1]
mu32[,2,1]<-stats(fi32_land)[,1]
mu32[,1,2]<-stats(ee_rel)[,1]
mu32[,2,2]<-stats(fi32_rel)[,1]

cv32<-ini
cv32[,1,1]<-stats(ee_land)[,2]
cv32[,2,1]<-stats(fi32_land)[,2]
cv32[,1,2]<-stats(ee_rel)[,2]
cv32[,2,2]<-stats(fi32_rel)[,2]
cv32

datalist<-list(
  NumYears=NumYears,
  NumCountries=2,
  mu=mu32, cv=cv32
)


parnames<-c(
  "Tot_Landed", "Tot_Released", "Tot_Catch", "Tot_Catch_Dead")

run_sd32 <- run.jags(M1, monitor= parnames,
                  data=datalist,
                  n.chains = 2, method = 'parallel', thin=100,
                  burnin =10000, modules = "mix",
                  sample =1000, adapt = 10000,
                  keep.jags.files=F,
                  progress.bar=TRUE, jags.refresh=100)

summary(run_sd32)


####################
# SD29-31
####################
ini<-array(NA, dim=c(NumYears,6,2))

mu2931<-ini
mu2931[,1,1]<-stats(se29_land)[,1]
mu2931[,2,1]<-stats(fi2931_land)[,1]

mu2931[,1,2]<-stats(se29_rel)[,1]
mu2931[,2,2]<-stats(fi2931_land)[,1]

cv2931<-ini
cv2931[,1,1]<-stats(se29_land)[,2]
cv2931[,2,1]<-stats(fi2931_rel)[,2]

cv2931[,1,2]<-stats(se29_rel)[,2]
cv2931[,2,2]<-stats(fi2931_rel)[,2]
cv2931



datalist<-list(
  NumYears=NumYears,
  NumCountries=2,
  mu=mu2931, cv=cv2931
)


parnames<-c(
  "Tot_Landed", "Tot_Released", "Tot_Catch", "Tot_Catch_Dead")

run_sd32 <- run.jags(M1, monitor= parnames,
                     data=datalist,
                     n.chains = 2, method = 'parallel', thin=100,
                     burnin =10000, modules = "mix",
                     sample =1000, adapt = 10000,
                     keep.jags.files=F,
                     progress.bar=TRUE, jags.refresh=100)

summary(run_sd32)

####################
# Main basin
####################
ini<-array(NA, dim=c(NumYears,6,2))

muMB<-ini
muMB[,1,1]<-stats(se2228_land)[,1]
muMB[,2,1]<-stats(dk_land)[,1]
muMB[,3,1]<-stats(ge_land)[,1]
muMB[,4,1]<-stats(po_land)[,1]
muMB[,5,1]<-stats(li_land)[,1]
muMB[,6,1]<-stats(la_land)[,1]

muMB[,1,2]<-stats(se2228_rel)[,1]
muMB[,2,2]<-stats(dk_rel)[,1]
muMB[,3,2]<-stats(ge_rel)[,1]
muMB[,4,2]<-stats(po_rel)[,1]
muMB[,5,2]<-stats(li_rel)[,1]
muMB[,6,2]<-stats(la_rel)[,1]

cvMB<-ini
cvMB[,1,1]<-stats(se2228_land)[,2]
cvMB[,2,1]<-stats(dk_land)[,2]
cvMB[,3,1]<-stats(ge_land)[,2]
cvMB[,4,1]<-stats(po_land)[,2]
cvMB[,5,1]<-stats(li_land)[,2]
cvMB[,6,1]<-stats(la_land)[,2]

cvMB[,1,2]<-stats(se2228_rel)[,2]
cvMB[,2,2]<-stats(dk_rel)[,2]
cvMB[,3,2]<-stats(ge_rel)[,2]
cvMB[,4,2]<-stats(po_rel)[,2]
cvMB[,5,2]<-stats(li_rel)[,2]
cvMB[,6,2]<-stats(la_rel)[,2]
cvMB


#	j=country {1,...,9}; 1=SWE, 2=DEN, 3=GER, 4=POL, 5=LIT, 6=LAT, 7=EST, 8=RUS, 9=FIN

datalist<-list(
  NumYears=NumYears,
  NumCountries=6,
  mu=muMB, cv=cvMB
)


parnames<-c(
  "Tot_Landed", "Tot_Released", "Tot_Catch", "Tot_Catch_Dead")

run_MB <- run.jags(M1, monitor= parnames,
                     data=datalist,
                     n.chains = 2, method = 'parallel', thin=100,
                     burnin =10000, modules = "mix",
                     sample =100000, adapt = 10000,
                     keep.jags.files=F,
                     progress.bar=TRUE, jags.refresh=100)

summary(run_MB, var="Tot_Catch")




