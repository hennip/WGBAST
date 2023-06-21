# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-wprop

tmp1<-read.table(str_c(PathData_FLHM, "Scale.txt"), header=T)[,1]
tmp2<-read.table(str_c(PathData_FLHM, "Scale.txt"), header=T)[,3]

add<-2 # 1 or 2
Year<-c(1987:(Years[length(Years)]+add))

tmp1<-as_tibble(cbind(tmp1,Year))%>%mutate(Type="2SW")
tmp2<-as_tibble(cbind(tmp2,Year))%>%mutate(Type="3SW")
colnames(tmp1)<-c("obs_prop", "Year","Type")
colnames(tmp2)<-c("obs_prop", "Year","Type")

obs<-full_join(tmp1, tmp2, by=NULL)


# Model 1: 
# =========

  df_2sw<-boxplot.jags.df2(chains1, "Wprop[", "1]", 6:(length(YearsB)-fix1))%>%
    mutate(Type="2SW")
  df_3sw<-boxplot.jags.df2(chains1, "Wprop[", "2]", 6:(length(YearsB)-fix1))%>%
    mutate(Type="3SW")
  
  df<-full_join(df_2sw,df_3sw, by=NULL)
  
  df.1<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  
  df.1<-full_join(df.1,obs, by=NULL)
  
  df.1

# Model 2: 
# =========
  
#summary(chains[ ,regexpr("Wprop",varnames(chains))>0])

df_2sw<-boxplot.jags.df2(chains, "Wprop[", "1]", 6:(length(YearsB)-fix2))%>%
  mutate(Type="2SW")
df_3sw<-boxplot.jags.df2(chains, "Wprop[", "2]", 6:(length(YearsB)-fix2))%>%
  mutate(Type="3SW")

df<-full_join(df_2sw,df_3sw, by=NULL)

df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)

df.2<-full_join(df.2,obs, by=NULL)

df.2

# Draw boxplots to compare
# ==========================

## ---- graphs-wprop


df1<-filter(df.1, is.na(Year)==F)
df2<-filter(df.2, is.na(Year)==F)

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="proportion", title="Wild proportion")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x=Year, y=obs_prop))+
  facet_grid(Type~.)


## ---- graphs-wprop-traces

if(GR==T){
  
par(mfrow=c(1,2))
for(i in 6:length(Years)){
  for(a in 1:2){
    #i<-1
    #a<-1
    gd<-gelman.diag(chainsGR[,str_c("Wprop[",i,",",a,"]")])
    if(gd$psrf[2]>1.2){
      print(c(a,i, gd$psrf))
      traceplot(chainsGR[,str_c("Wprop[",i,",",a,"]")], main=str_c("Wprop age=",a," ",df.2$Year[i]))
    }
  }
}

}


