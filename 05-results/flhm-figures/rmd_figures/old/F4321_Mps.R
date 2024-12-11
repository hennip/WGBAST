# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.3.2.1, Mps

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


#   one year less

if(nchains==1){
  survMpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"]),length(Years_m)))
  survMpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"]),length(Years_m)))
  ratio<-array(NA, dim=c(length(chains[,"MpsR[1]"]),length(Years_m)))
  for(y in 1:(length(Years_m))){
    survMpsW[,y]<-exp(-chains[,str_c("MpsW[",y,"]")])
    survMpsR[,y]<-exp(-chains[,str_c("MpsR[",y,"]")])
    ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
}
if(nchains==2){
  survMpsW<-array(NA, dim=c(length(chains[,"MpsW[1]"][[1]]),length(Years_m)))
  survMpsR<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years_m)))
  ratio<-array(NA, dim=c(length(chains[,"MpsR[1]"][[1]]),length(Years_m)))
  for(y in 1:(length(Years_m))){
    survMpsW[,y]<-exp(-chains[,str_c("MpsW[",y,"]")][[2]])
    survMpsR[,y]<-exp(-chains[,str_c("MpsR[",y,"]")][[2]])
    ratio[,y]<-survMpsR[,y]/survMpsW[,y]
  }
}


dfW<-boxplot.bugs.df(survMpsW, 1:(length(Years_m)))%>%
  mutate(Type="Wild")
dfR<-boxplot.bugs.df(survMpsR, 1:(length(Years_m)))%>%
  mutate(Type="Reared")
df_ratio<-boxplot.bugs.df(ratio, 1:(length(Years_m)))%>%
  mutate(Type="Ratio")

df<-full_join(dfW,dfR, by=NULL)
df<-full_join(df,df_ratio, by=NULL)

df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.2

## ---- F4231
df1<-filter(df.2, Type=="Reared")
df2<-filter(df.2, Type=="Wild")

#for(i in 1:2){}
gg1 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 15)+
  theme(panel.grid = element_line(colour = "gray88"))+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="gray90")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.4))+
  labs(x="Year", y="Survival", title="Post-smolt survival")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))#+
#facet_grid(Type~.)

## --- ratio


plot(gg1)
