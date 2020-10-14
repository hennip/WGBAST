
library(coda)
library(xlsx)


load(file="C:/output/wgbast/flhm/FLHM_results_2019_extended2019-04-11.RData"); modelname<-"FLHM_2019_extended"
#load(file="C:/output/wgbast/flhm/FLHM_2019_trolling_LL_DN.RData"); modelname<-"FLHM_2019_trolling_LL_DN"
#load(file="C:/output/wgbast/flhm/FLHM_2020.RData"); modelname<-"FLHM_2020"

chains<-as.mcmc.list(run)

#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]   200 16500 # dimensions: iterations x number of variables

headtext<-c("mean","sd","cv","5%","50%","95%","90%PI","grdPE", "grdUCI", "Varname")
statsfile<-paste0("C:/output/wgbast/flhm/stats_",modelname,".csv")

write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)

for(i in 1:dim(d)[2]){ # loop over all monitored variables
  m<-mean(d[,i])
  s<-sd(d[,i])
  cv<-s/m
  q5<-quantile(d[,i],0.05)
  q50<-quantile(d[,i],0.50)
  q95<-quantile(d[,i],0.95)
  PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 in round() if decimals needed
  grdPE<-gelman.diag(chains[,i])$psrf[1]
  grdUCI<-gelman.diag(chains[,i])$psrf[2]
  
  printtxt<-c(m,s,cv,q5,q50,q95,PI90,grdPE, grdUCI, colnames(d)[i])
  write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
}





###############

headtext<-c("Varname","mean","sd","cv","5%","50%","95%","90%PI")

for(i in 1:dim(d)[2]){ # loop over variables
  #i<-1
  m<-mean(d[,i])
  s<-sd(d[,i])
  cv<-s/m
  q5<-quantile(d[,i],0.05)
  q50<-quantile(d[,i],0.50)
  q95<-quantile(d[,i],0.95)
  PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 if decimals needed

  printtxt<-c(colnames(d)[i],m,s,cv,q5,q50,q95,PI90)
  if(i==1){df<-t(as.data.frame(printtxt))}
  else{df<-rbind(df,t(as.data.frame(printtxt)))}
}

df
colnames(df)<-headtext
df<-as.tibble(df)%>%
  mutate(mean=parse_double(mean),sd=parse_double(sd),cv=parse_double(cv),
         q5=parse_double(q5),q50=parse_double(q50), q95=parse_double(q95))%>%
  select(Varname, mean, sd, cv)

tmp<-df
tmp%>%mutate(x=str_split(Varname, "\\["))  



View(df)

x1<-"SmoltWW[1,1]"
x2<-str_split(x1, c("\\[",","))

str_split(x1, "\\[")%>%.[[1]]


length(x2)


