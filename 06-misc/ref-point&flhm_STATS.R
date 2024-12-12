library(tidyverse)
library(runjags)
library(coda)
#################################################################
#   writing stats file from flhm results and ref points ATSO    #
#################################################################


#   custom round function

round_m <- function(vec, lim=3){
  rvec <- c()
  for(x in vec){
    #print(x)
    if(is.na(x)){rvec <- c(rvec, NA)}
    else if(abs(x) <= lim){
      r <- round(x, 1)
      #print(r)
      rvec <- c(rvec, paste(sprintf("%.1f", r)))
      
    }
    else{
      r <- round(x)
      #print(r)
      rvec <- c(rvec, paste(r))
    }
  }
  return(rvec)
}


#   loading results
PathModel_FLHM
load(str_c(PathModel_FLHM,"flhm/2024/output/CR_2024_selected_chain.Rdata"))

headtext<-c("Varname","mean","sd", "cv","5%","50%","95%", "90%PI")
statsfile<-"nodeStats24.xlsx"
d <- chains
#write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)
df <- data.frame()
for(i in 1:dim(d)[2]){
  name <- colnames(d)[i]
  mean <- mean(d[,i])
  sd <- sd(d[,i])
  cv <- sd/mean
  q005 <- quantile(d[,i],0.05)
  q050 <- quantile(d[,i],0.50)
  q095 <- quantile(d[,i],0.95)
  PI <- paste(q005 %>% round_m(5), "-", q095 %>% round_m(5), sep= "")
  printtxt<-c(name,mean,sd, cv,
              q005,q050,q095,PI)
  df <- rbind(df, printtxt)
  if(i %% 1000 == 0) print(paste(round(100*i/dim(d)[2], 3), "%", " done", sep=""))
  #write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
  #print(printtxt)
}
colnames(df) <- headtext


df <- map_df(d, t)

is.numeric(df[,2])

round_m(rnorm(10, 0, 10000))

df %>% 
  mutate(
    mean = as.numeric(mean) %>% round(3),
    sd = as.numeric(sd) %>% round(3),
    cv = as.numeric(cv) %>% round(3),
    `5%` = as.numeric(`5%`) %>% round(3),
    `50%` = as.numeric(`50%`) %>% round(3),
    `95%` = as.numeric(`95%`) %>% round(3)
  )

write.xlsx(df, file =statsfile )

#   referencepoints

#   functionf or calculating ref points stats

calcStat <- function(x){
  name <- colnames(x)
  mean <- mean(x) %>% round(4)
  sd <- sd(x) %>% round(4)
  cv <- (sd/mean) %>% round(4)
  q005 <- quantile(x,0.05) %>% round(4)
  q050 <- quantile(x,0.50) %>% round(4)
  q095 <- quantile(x,0.95) %>% round(4)
  PI <- paste(q005, "-", q095, sep= "")
  printtxt<-c(name,mean,sd, cv,
              q005,q050,q095,PI)
  
  return(printtxt)
  
}

RiverNames<-c("Tornionjoki","Simojoki","Kalixälven","Råneälven"
              ,"Piteälven","Åbyälven","Byskeälven","Rickleån","Sävarån"
              ,"Vindelälven","Öreälven","Lögdeälven","Ljungan","Mörrumsån"
              ,"Emån", "Kågeälven","Testeboån")
PathOut_Scen
#   loading ref points
load(str_c(PathOut_Scen, "2024/Ref pts/ref_pts_2024_JAGS_Mps.RData"))

load(str_c(PathOut_Scen, "2024/Ref pts/eqm_distns_Mps.RData"))

headtext<-c("mean","sd", "cv","5%","50%","95%", "90%PI")

 
EggsStats <- apply(Eggs_MSY, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(EggsStats) = RiverNames;rownames(EggsStats) = headtext

MSYStats <- apply(MSY, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(MSYStats) = RiverNames;rownames(MSYStats) = headtext

smoltlimStats<-apply(Smolt_lim, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(smoltlimStats) = RiverNames;rownames(smoltlimStats) = headtext

smoltMSYStats<-apply(Smolt_MSY, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(smoltMSYStats) = RiverNames;rownames(smoltMSYStats) = headtext

R0stats<-apply(R0_all, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(R0stats) = RiverNames;rownames(R0stats) = headtext

S0stats<-apply(S0_all, MARGIN = 1, calcStat) %>% as.data.frame()
colnames(S0stats) = RiverNames;rownames(S0stats) = headtext

library(openxlsx)
#   writing the table
list_stats <- list(
  "Eggs MSY"  = EggsStats,
  "MSY"       = MSYStats,
  "Smolt lim" = smoltlimStats,
  "Smolt MSY" = smoltMSYStats,
  "R0 all"    = R0stats,
  "S0_all"    = S0stats
)
write.xlsx(list_stats, file="06-misc/ref_point_stats.xlsx", rowNames =T)
