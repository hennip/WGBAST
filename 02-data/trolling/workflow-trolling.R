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

source("02-data/catch-effort/read-in-wgbast-catch&effort.r")
df_all<-wgbast_catch_data

M1<-"
model{
  for (i in 1:NumYears){	# 38 = 2024
    for (n in 1:1){ 	#  run one area at the time and change this loop accordingly! Only this works with the present data construction
      
      # Nämä ilmeisesti tuloksia, d tuotetaan koodin avulla
      Tot_Landed[i,n]<-sum(d[i,1:9,1,n]) 	# Total retained catch by area = estimate without mortality from released catch
      Tot_Released[i,n]<-sum(d[i,1:9,2,n])	# Total catch released back to sea by area
      Tot_Catch[i,n]<-Tot_Landed[i,n] + Tot_Released[i,n]   # Total catch by area, no mortality accounted for relesed salmon 
      Tot_Catch_Dead[i,n]<-Tot_Landed[i,n] + Tot_Released[i,n]*0.25 	# Total dead catch by area, 25% mortality accounted for released salmon
      
      for (j in 1:9){
        for (k in 1:2){
          
          mu[i,j,k,n]<-(Min[i,j,k,n]+Mod[i,j,k,n]+Max[i,j,k,n])/3
          var[i,j,k,n]<-(pow(Min[i,j,k,n],2)+pow(Mod[i,j,k,n],2)+pow(Max[i,j,k,n],2)-Min[i,j,k,n]*Max[i,j,k,n]-Min[i,j,k,n]*Mod[i,j,k,n]-Mod[i,j,k,n]*Max[i,j,k,n])/18
          cv[i,j,k,n]<-sqrt(var[i,j,k,n])/mu[i,j,k,n]
          
          M[i,j,k,n]<-log(mu[i,j,k,n])-0.5/tau[i,j,k,n]
          tau[i,j,k,n]<-1/log(pow(cv[i,j,k,n],2)+1)
          
          d[i,j,k,n]~dlnorm(M[i,j,k,n],tau[i,j,k,n])
        }
      }
    }
  }
}"
