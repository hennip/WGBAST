# Compare results

#source("models-select.R")

## ---- load-mps

# Model 1: 
# =========

survMpsW<-array(NA, dim=c(nsims1,length(YearsB)))
survMpsR<-array(NA, dim=c(nsims1,length(YearsB)))
ratio<-array(NA, dim=c(nsims1,length(YearsB)))

for(y in 1:(length(YearsB))){
#  if(nchains1==1){
    survMpsW[,y]<-exp(-as.matrix(chains1[,str_c("MpsW[",y,"]")]))
    survMpsR[,y]<-exp(-as.matrix(chains1[,str_c("MpsR[",y,"]")]))
#  } 
  # if(nchains1==2){
  #   survMpsW[,y]<-exp(-as.matrix(chains1[,str_c("MpsW[",y,"]")][[1]]))
  #   survMpsR[,y]<-exp(-as.matrix(chains1[,str_c("MpsR[",y,"]")][[1]]))
  # }
  
  ratio[,y]<-survMpsR[,y]/survMpsW[,y]
}
  
summary(survMpsW)

dfW<-boxplot.bugs.df(survMpsW, 1:(length(YearsB)))%>%
    mutate(Type="Wild")
dfR<-boxplot.bugs.df(survMpsR, 1:(length(YearsB)))%>%
    mutate(Type="Reared")
df_ratio<-boxplot.bugs.df(ratio, 1:(length(YearsB)))%>%
    mutate(Type="Ratio")
  
df<-full_join(dfW,dfR, by=NULL)
df<-full_join(df,df_ratio, by=NULL)
  
df.1<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
df.1
  
# adult M
dW<-as.matrix(chains1[,"MW"])
dR<-as.matrix(chains1[,"MR"])
M<-cbind(dW,dR)

M<-exp(-M)
summary(as.mcmc(M))
M<-as_tibble(M)%>%
  setNames(c("Wild", "Reared"))%>%
  mutate(Ratio=Reared/Wild)
  
  dfM1<-boxplot.bugs.df(M, 1:3) #Both types in same!
  dfM.1<-as_tibble(setNames(dfM1,c("Type","q5","q25","q50","q75","q95")))%>%
    mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))
  


# Model 2: 
# =========

#summary(chains[ ,regexpr("Mps",varnames(chains))>0])
#summary(chains[,"MpsW[1]"][[1]])
#summary(exp(-chains[,"MpsW[1]"][[1]]))


  survMpsW<-array(NA, dim=c(nsims2,length(Years)))
  survMpsR<-array(NA, dim=c(nsims2,length(Years)))
  ratio<-array(NA, dim=c(nsims2,length(Years)))
  for(y in 1:(length(Years))){
# if(nchains2==1){
       survMpsW[,y]<-exp(-as.matrix(chains[,str_c("MpsW[",y,"]")]))
    survMpsR[,y]<-exp(-as.matrix(chains[,str_c("MpsR[",y,"]")]))
# } 
 #    if(nchains2==2){
 #   survMpsW[,y]<-exp(-as.matrix(chains[,str_c("MpsW[",y,"]")][[2]]))
 #   survMpsR[,y]<-exp(-as.matrix(chains[,str_c("MpsR[",y,"]")][[2]]))
 # }
        ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
# }


dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years)))%>%
  mutate(Type="Wild")
dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years)))%>%
  mutate(Type="Reared")
df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years)))%>%
  mutate(Type="Ratio")

df<-full_join(dfW,dfR, by=NULL)
df<-full_join(df,df_ratio, by=NULL)


df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
mutate(Year=Year+1986)
df.2

# adult M
dW<-as.matrix(chains[,"MW"])
dR<-as.matrix(chains[,"MR"])
M2<-cbind(dW,dR)
M2<-exp(-M2)
summary(as.mcmc(M2))
#1. Empirical mean and standard deviation for each variable,
#plus standard error of the mean:
  
#  Mean      SD  Naive SE Time-series SE
#var1 0.9159 0.01598 0.0003573       0.003978
#var1 0.7513 0.03554 0.0007948       0.001674

#2. Quantiles for each variable:
  
#  2.5%    25%    50%    75%  97.5%
#var1 0.8848 0.9041 0.9151 0.9270 0.9484
#var1 0.7070 0.7236 0.7434 0.7717 0.8380

M2<-as_tibble(M2)%>%
  setNames(c("Wild", "Reared"))%>%
  mutate(Ratio=Reared/Wild)

dfM2<-boxplot.bugs.df(M2, 1:3) #Both types in same!
dfM.2<-as_tibble(setNames(dfM2,c("Type","q5","q25","q50","q75","q95")))%>%
  mutate(Type= fct_recode(factor(Type), "Wild"="1", "Reared"="2", "Ratio"="3"))


# Draw boxplots to compare
# ==========================

## ---- graphs-mps
df1<-df.1
df2<-df.2

  ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  labs(x="Year", y="Survival", title="Post-smolt survival")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_grid(Type~., scales = "free")

# AdultM
df1<-dfM.1
df2<-dfM.2

ggplot(df2, aes(Type))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 4))+
  labs(x="Year", y="Survival", title="Adult survival")
  

## ---- graphs-mortality-traces



# Model 1 traces, if JAGS
# windows()
#par(mfrow=c(2,3))
#for(i in 1:length(Years)){
#    gd<-gelman.diag(chains1[,str_c("MpsW[",i,"]")])
#    if(gd$psrf[2]>1.5){
#      #print(c(i, gd$psrf))
#      traceplot(chains1[,str_c("MpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
#    }
#}
if(GR==T){

par(mfrow=c(2,3))
for(i in 1:length(Years)){
  # if(JAGSversion=="old"){
  #   gd<-gelman.diag(chains[,str_c("survMpsW[",i,"]")])
  #   if(gd$psrf[2]>1.5){
  #     traceplot(chains[,str_c("survMpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
  #   }
  # }else{
     gd<-gelman.diag(chainsGR[,str_c("MpsW[",i,"]")])
    if(gd$psrf[2]>1.2){
      #print(c(i, gd$psrf))
      traceplot(chainsGR[,str_c("MpsW[",i,"]")], main=str_c("MpsW ",df.2$Year[i]))
    }
}
 par(mfrow=c(2,3))
  for(i in 1:length(Years)){
       gd<-gelman.diag(chainsGR[,str_c("MpsR[",i,"]")])
     if(gd$psrf[2]>1.2){
       #print(c(i, gd$psrf))
       traceplot(chainsGR[,str_c("MpsR[",i,"]")], main=str_c("MpsR ",df.2$Year[i]))
     }
     
       # }
}

traceplot(chainsGR[,"MW"], main="MW")
traceplot(chainsGR[,"MR"], main="MR")

#par(mfrow=c(2,1))
#traceplot(exp(-chains[,"MR"]))
#traceplot(as.mcmc(M$Reared))

}

