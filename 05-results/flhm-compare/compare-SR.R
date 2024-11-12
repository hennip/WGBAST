# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-SR

# Model 1: 
# =========
    
  # if(SRnew=="no"){
  #   df1<-boxplot.jags.df(chains1, "R0[", 1:nstocks)%>%
  #     mutate(par="R0")
  #   
  #   df2<-boxplot.jags.df(chains1, "z[", 1:nstocks)%>%
  #     mutate(par="z")
  #   df5<-df1%>%
  #     mutate(par="R0_rep")
  #   df<-full_join(df1,df2)
  #   
  # }
df1<-boxplot.jags.df(chains1, "K[", 1:nstocks)%>%
  mutate(par="K")
    
df2<-boxplot.jags.df(chains1, paste0("z[",length(YearsB),","), 1:nstocks)%>%
  mutate(par="z")

df<-full_join(df1,df2)
    
#df5<-boxplot.jags.df(chains1, paste0("K[",length(YearsB),","), 1:nstocks)%>%
#  mutate(par="K")
    

df3<-boxplot.jags.df(chains1, "alphaSR[", 1:nstocks)%>%
  mutate(par="alpha")
df4<-boxplot.jags.df(chains1, "betaSR[", 1:nstocks)%>%
  mutate(par="beta")
  
df<-full_join(df,df3)
df<-full_join(df,df4)
#df<-full_join(df,df5)
  
df.1<-as_tibble(setNames(df,c("stock","q5","q25","q50","q75","q95","par")))
df.1


# Model 2: 
# =========

# if(SRnew=="no"){
#   df1<-boxplot.jags.df(chains, "R0[", 1:nstocks)%>%
#     mutate(par="R0")
#   
#   df2<-boxplot.jags.df(chains, "z[", 1:nstocks)%>%
#     mutate(par="z")
#   df5<-df1%>%
#     mutate(par="R0_rep")
#   df<-full_join(df1,df2)
#   
# }
df1<-boxplot.jags.df(chains, "K[", 1:nstocks)%>%
 mutate(par="K")
  
df2<-boxplot.jags.df(chains, paste0("z[",length(Years),","), 1:nstocks)%>%
  mutate(par="z")

df<-full_join(df1,df2)

#df5<-boxplot.jags.df(chains, paste0("K[",length(Years),","), 1:nstocks)%>%
#  mutate(par="K")

df3<-boxplot.jags.df(chains, "alphaSR[", 1:nstocks)%>%
  mutate(par="alpha")

df4<-boxplot.jags.df(chains, "betaSR[", 1:nstocks)%>%
  mutate(par="beta")

df<-full_join(df,df3)
df<-full_join(df,df4)
#df<-full_join(df,df5)

df.2<-as.tibble(setNames(df,c("stock","q5","q25","q50","q75","q95","par")))
df.2

# Draw boxplots to compare
# ==========================

## ---- graphs-SR
#   adding ume to filter
df1<-filter(df.1, par=="K", stock!=1 & stock!=3 & stock !=10)
df2<-filter(df.2, par=="K", stock!=1 & stock!=3 & stock !=10)

#   filtering to 4 plots
#   first all
#   second (small) 
#   third 2,4,6,7,11,12,14,15,16 (medium)



#   changing ylim to 200
ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="K", title="Asymptotic smolt production capacity")+
  coord_cartesian(ylim=c(0,200))+
  scale_x_continuous(breaks = c(1:17), labels=Rivername)

#   chaing the filter for torne, kalix, ume
# df1<-filter(df.1, par=="K", stock<4 & stock!=2)
# df2<-filter(df.2, par=="K", stock<4 & stock!=2)
df1<-filter(df.1, par=="K", stock %in% c(1,3,10))
df2<-filter(df.2, par=="K", stock %in% c(1,3,10))


ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="K", title="Asymptotic smolt production capacity")+
  #coord_cartesian(ylim=c(0,450))+
  #scale_x_continuous(breaks = c(1:10), labels=Rivername[1:10])
  scale_x_continuous(breaks = c(1,3,10), labels=Rivername[c(1,3,10)])
  


df1<-filter(df.1, par=="z")
df2<-filter(df.2, par=="z")

ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="z", title="Steepness")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = c(1:17), labels=Rivername)

df1<-filter(df.1, par=="alpha")
df2<-filter(df.2, par=="alpha")

ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="alpha", title="alpha SR")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = c(1:17), labels=Rivername)


# ljungan and testeboan seperate 13, 17
df1<-filter(df.1, par=="beta", stock != 13 & stock != 17)
df2<-filter(df.2, par=="beta", stock != 13 & stock != 17)

ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="beta", title="beta SR")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = c(1:17), labels=Rivername)

df1<-filter(df.1, par=="beta", stock %in% c(13,17))
df2<-filter(df.2, par=="beta", stock %in% c(13,17))

ggplot(df2, aes(stock, group=stock))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Stock", y="beta", title="beta SR")+
  #coord_cartesian(ylim=c(0,2500))+
  scale_x_continuous(breaks = c(13,17), labels=Rivername[c(13,17)])



## ---- graphs-SR-traces
if(GR==T){
  
par(mfrow=c(2,3))
for(i in 1:nstocks){
  #i<-1
   gd<-gelman.diag(chainsGR[,str_c("K[",i,"]")])
  if(gd$psrf[2]>1.1){
    #print(c(i, gd$psrf))
    traceplot(chainsGR[,str_c("K[",i,"]")], main=str_c("K ",Rivername[i]))
  }
}

for(i in 1:nstocks){
  gd<-gelman.diag(chainsGR[,str_c("alphaSR[",i,"]")])
  if(gd$psrf[2]>1.1){
    traceplot(chainsGR[,str_c("alphaSR[",i,"]")], main=str_c("alphaSR ",Rivername[i]))
  }
}

for(i in 1:nstocks){
  gd<-gelman.diag(chainsGR[,str_c("betaSR[",i,"]")])
  if(gd$psrf[2]>1.1){
    traceplot(chainsGR[,str_c("betaSR[",i,"]")], main=str_c("betaSR ",Rivername[i]))
  }
}
}

