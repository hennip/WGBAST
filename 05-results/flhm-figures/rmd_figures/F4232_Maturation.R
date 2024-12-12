#   one year less!!!

for(a in 1:4){
  #a<-1
  #dfW<-boxplot.jags.df2(chains, "LW[",str_c(a,"]"),1:length(Years), nchains)%>%
  dfW<-boxplot.jags.df2(chains, "LW[",str_c(a,"]"),1:length(Years_m))%>%
    mutate(Age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  #dfR<-boxplot.jags.df2(chains, "LR[",str_c(a,"]"),1:length(Years), nchains)%>%
  dfR<-boxplot.jags.df2(chains, "LR[",str_c(a,"]"),1:length(Years_m))%>%
    mutate(Age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1",
                        "2SW"= "2",
                        "3SW"= "3",
                        "4SW"= "4"))
df.2

## ---- F4232

df1<-filter(df.2, Type=="Reared")
df2<-filter(df.2, Type=="Wild")

p <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_line(colour = "gray88"))+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey90")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.4))+
  labs(x="Year", y="Proportion", title="Maturation rates")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)+
  coord_cartesian(ylim=c(0,1))

plot(p)

