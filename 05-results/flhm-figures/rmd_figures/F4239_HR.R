
nsample<-nsim
# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hrR<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hcW.au1<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hcR.au1<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hdcW<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hdcR<-array(NA, dim=c(2,length(Years_m)-2, nsample))
for(y in 3:(length(Years_m))){ 
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #hrW[grilse:MSW, ]
    hrW[a-1,y-2,]<-chains[,str_c("HrW[",y-(a-1),",",a,"]")]
    hrR[a-1,y-2,]<-chains[,str_c("HrR[",y-(a-1),",",a,"]")]
    
    #hcW[grilse:MSW, ] AU1
    hcW.au1[a-1,y-2,]<-chains[,str_c("HcW[",y-(a-1),",",a,",1]")]
    hcR.au1[a-1,y-2,]<-chains[,str_c("HcR[",y-(a-1),",",a,",1]")]
    
    #hdcW[grilse:MSW, ]
    hdcW[a-1,y-2,]<-chains[,str_c("HdcW[",y-(a-1),",",a,"]")]
    hdcR[a-1,y-2,]<-chains[,str_c("HdcR[",y-(a-1),",",a,"]")]
    
  }}

hdoW<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hdoR<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hlW<-array(NA, dim=c(2,length(Years_m)-2, nsample))
hlR<-array(NA, dim=c(2,length(Years_m)-2, nsample))
for(y in 3:(length(Years_m))){ 
  for(a in 1:2){ # 1SW & 2SW (=MSW)
    
    #hdoW[grilse:MSW, ]
    hdoW[a,y-2,]<-chains[,str_c("HdoW[",y-a,",",a,"]")]
    hdoR[a,y-2,]<-chains[,str_c("HdoR[",y-a,",",a,"]")]
    
    #hlW[grilse:MSW, ]
    hlW[a,y-2,]<-chains[,str_c("HlW[",y-a,",",a,"]")]
    hlR[a,y-2,]<-chains[,str_c("HlR[",y-a,",",a,"]")]
  }}

htW<-htR<-array(NA, dim=c(2,length(Years_m)-2, nsample))
for(y in 3:(length(Years_m))){ 
  for(a in 1:2){ # 1SW, 2SW=MSW
    # hlW[a,y-2,]<-chains[,str_c("HlW[",y-a,",",a,"]")]
    # hlR[a,y-2,]<-chains[,str_c("HlR[",y-a,",",a,"]")]
    htW[a,y-2,]<-chains[,str_c("HtW[",y-a,",",a,"]")]
    htR[a,y-2,]<-chains[,str_c("HtR[",y-a,",",a,"]")]
  }}



# wrangle
##########################

# River fishery

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="River", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="River", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hr<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  #mutate(Year=Year+1993)%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        
df.2.Hr


# COASTAL FISHERIES (TRAPNET&GILLNET)

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type,
         everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        

df.2.Hc


# COASTAL DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="CDN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdc<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"="1", "2SW"="2"))
df.2.Hdc


# OFFSHORE DRIFTNET

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="ODN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdo<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "2SW"= "2"))
df.2.Hdo

# OFFSHORE LONGLINE

# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){
  dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="OLL", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hl<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"="1",
                        "MSW"= "2"))
df.2.Hl


# OFFSHORE (RECREATIONAL) TROLLING
for(a in 1:2){
  dfW<-boxplot.bugs.df2(htW, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="Trolling", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(htR, a ,1:(length(Years_m)-2))%>%
    mutate(age=a, Fishery="Trolling", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Ht<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1989)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "MSW"="2"))
df.2.Ht



## ---- F42311a

df2<-filter(df.2.Hdo, Type=="Wild")

ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 


df2<-filter(df.2.Hl, Type=="Wild")

ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

df2<-filter(df.2.Ht, Type=="Wild")

ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 


## ---- F42311b

# coastal TN & GN

df2<-filter(df.2.Hdo, Type=="Wild")

gp1 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

plot(gp1)

df2<-filter(df.2.Hl, Type=="Wild")

gp2 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

plot(gp2)

df2<-filter(df.2.Ht, Type=="Wild")

gp3 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

plot(gp3)

## ---- F42311b

# coastal TN & GN

df2<-filter(df.2.Hc, Type=="Wild")

gp4 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Coastal HR AU1"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)+
  coord_cartesian(ylim = c(0, 1))

plot(gp4)

df2<-filter(df.2.Hdc, Type=="Wild")

gp5 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Coastal driftnet HR"))+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age, scales="free") 

plot(gp5)

## ---- F42311c

# river wild
df2<-filter(df.2.Hr, Type=="Wild")

gp6 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River HR")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

plot(gp6)
