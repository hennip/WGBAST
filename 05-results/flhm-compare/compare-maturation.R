# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-mat

# Model 1: 
# =================

  for(a in 1:4){
    dfW<-boxplot.jags.df2(chains1, "LW[",str_c(a,"]"),1:length(YearsB))%>%
      mutate(Age=a, Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.jags.df2(chains1, "LR[",str_c(a,"]"),1:length(YearsB))%>%
      mutate(Age=a, Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  #df<-dfW2 # if reared is missing
  
  df.1<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
    select(Age, Type, everything())%>%
    mutate(Year=Year+1986)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1",
                          "2SW"= "2",
                          "3SW"= "3",
                          "4SW"= "4"))
  df.1
  
  


# Model 2: 
# =================
#summary(chains[ ,regexpr("LR",varnames(chains))>0])

for(a in 1:4){
  dfW<-boxplot.jags.df2(chains, "LW[",str_c(a,"]"),1:length(Years))%>%
    mutate(Age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

  dfR<-boxplot.jags.df2(chains, "LR[",str_c(a,"]"),1:length(Years))%>%
    mutate(Age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)
#df<-dfW2 # if reared is missing

df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                                 "Grilse"= "1",
                                 "2SW"= "2",
                                 "3SW"= "3",
                                 "4SW"= "4"))
df.2

# Draw boxplots to compare
# ==========================

## ---- graphs-mat

# Wild
df1<-filter(df.1, Type=="Wild")
df2<-filter(df.2, Type=="Wild")

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
  labs(x="Year", y="Proportion per age group", title="Homing rates, wild")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

df1<-filter(df.1, Type=="Reared")
df2<-filter(df.2, Type=="Reared")

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
  labs(x="Year", y="Proportion per age group", title="Homing rates, reared")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)



## ---- graphs-mat-traces
if(GR==T){
  

par(mfrow=c(2,3))
for(i in 1:length(Years)){
  for(a in 1:4){
#i<-1
#a<-4
        gd<-gelman.diag(chainsGR[,str_c("LW[",i,",",a,"]")])
    if(gd$psrf[2]>1.1){
      #print(c(a,i, gd$psrf))
      traceplot(chainsGR[,str_c("LW[",i,",",a,"]")], main=str_c("LW age=",a," ",df.2$Year[i]))
    }
  }
}

par(mfrow=c(2,3))
for(i in 1:length(Years)){
  for(a in 1:4){
    #i<-1
    #a<-4
    gd<-gelman.diag(chainsGR[,str_c("LR[",i,",",a,"]")])
    if(gd$psrf[2]>1.1){
      #print(c(a,i, gd$psrf))
      traceplot(chainsGR[,str_c("LR[",i,",",a,"]")], main=str_c("LR age=",a," ",df.2$Year[i]))
    }
  }
}

}





