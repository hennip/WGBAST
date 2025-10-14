
source("../run-this-first-wgbast.R")


#load(file=paste0(PathOut_FLHM,"chain_cleaned_2025_base4.RData"));modelname<-"base4_cleaned"
#load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2025_base4_data2025.RData"));modelname<-"base4_uncleaned"
load(file=paste0(PathOut_FLHM,"FLHM_JAGS_2025_base4_data2025.RData"));modelname<-"base4_long"
chains<-as.mcmc.list(run)
# plot(run, var="Mps")
# plot(run, var="mucL")
# plot(run, var="delta")
# plot(run, var="MW")
# 

chains<-window(chains, start=600000)
nchains<-2
plot(chains[, "MpsW[16]"])
plot(chains[, "MpsR[17]"])
plot(chains[, "HtW[22,2]"])
plot(chains[, "LR[19,1]"])
plot(chains[, "mucL"])
plot(chains[, "K[1]"])


#print stats to file
d<-as.matrix(chains)
dim(d)
#[1]   2000 22434 # dimensions: iterations x number of variables


statsfile<-paste0("../../stats_",modelname,".csv")

if(nchains==1){
  headtext<-c("mean","sd","cv","5%","50%","95%","90%PI", "Varname")
  
  write.table(t(as.matrix(headtext)),file=statsfile,sep=',',row.names=F, col.names=F)

    for(i in 1:dim(d)[2]){ # loop over all monitored variables
    m<-mean(d[,i])
    s<-sd(d[,i])
    cv<-s/m
    q5<-quantile(d[,i],0.05)
    q50<-quantile(d[,i],0.50)
    q95<-quantile(d[,i],0.95)
    PI90<-paste0("'",round(q5,0),"-",round(q95,0))  # change 0 in round() if decimals needed
    #grdPE<-gelman.diag(chains[,i])$psrf[1]
    #grdUCI<-gelman.diag(chains[,i])$psrf[2]
    
    printtxt<-c(m,s,cv,q5,q50,q95,PI90,colnames(d)[i])
    write.table(t(as.matrix(printtxt)),statsfile,sep=",",row.names=F, col.names=F,append=T)
  }
}

#C:\Users\03080932\OneDrive - Valtion\output\wgbast\flhm
if(nchains==2){
  headtext<-c("mean","sd","cv","5%","50%","95%","90%PI","grdPE", "grdUCI", "Varname")
 # statsfile<-paste0(paste0(pathMain,"output/wgbast/flhm/2023/stats_",modelname,".csv"))
  
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


