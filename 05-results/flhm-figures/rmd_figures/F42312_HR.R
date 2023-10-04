# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.12, HR graphs

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

nsim<-length(chains[,str_c("HrW[",2,",",2,"]")])

# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years)-2, nsim))
hrR<-array(NA, dim=c(2,length(Years)-2, nsim))
hcW.au1<-array(NA, dim=c(2,length(Years)-2, nsim))
hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsim))
hdcW<-array(NA, dim=c(2,length(Years)-2, nsim))
hdcR<-array(NA, dim=c(2,length(Years)-2, nsim))
for(y in 3:(length(Years))){ 
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #    y<-3;a<-2
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

hdoW<-array(NA, dim=c(2,length(Years)-2, nsim))
hdoR<-array(NA, dim=c(2,length(Years)-2, nsim))
hlW<-array(NA, dim=c(2,length(Years)-2, nsim))
hlR<-array(NA, dim=c(2,length(Years)-2, nsim))
for(y in 3:(length(Years))){ 
  for(a in 1:2){ # Grilse & 2SW (=MSW)
    
    #hdoW[grilse:MSW, ]
    hdoW[a,y-2,]<-chains[,str_c("HdoW[",y-a,",",a,"]")]
    hdoR[a,y-2,]<-chains[,str_c("HdoR[",y-a,",",a,"]")]
    
    #hlW[grilse:MSW, ]
    hlW[a,y-2,]<-chains[,str_c("HlW[",y-a,",",a,"]")]
    hlR[a,y-2,]<-chains[,str_c("HlR[",y-a,",",a,"]")]
  }}



# from cohort+age to calendar years
hr_coastW<-array(NA, dim=c(2,length(Years)-2, nsim))
hr_coastR<-array(NA, dim=c(2,length(Years)-2, nsim))       #AU1

hr_coastW<- 1-exp(-(-log(1-hcW.au1)+ (-log(1-hdcW))))
hr_coastR<- 1-exp(-(-log(1-hcR.au1)+ (-log(1-hdcR))))       #AU1

hr_seaW<-array(NA, dim=c(2,length(Years)-2, nsim))
hr_seaR<-array(NA, dim=c(2,length(Years)-2, nsim))

hr_seaW<- 1-exp(-(-log(1-hdoW)+ -log(1-hlW)))
hr_seaR<- 1-exp(-(-log(1-hdoR)+ -log(1-hlR)))    
# wrangle
##########################
# Combined HR coast
# boxplot.bugs.df2 is used since chains-structure is not anymore in hrW
for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hr_coastW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coastal_tot", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hr_coastR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coastal_tot", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.ctot<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))  



#print(as_tibble(df.2.ctot), n=120)


# Combined HR sea
for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hr_seaW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Sea_tot", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hr_seaR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Sea_tot", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.stot<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))  

labels <- c(Coastal_tot = "Combined coastal HR, AU1", Sea_tot = "Combined offshore HR")


## ---- F42312

#total hr

df1<-filter(df.2.stot, Type=="Wild", Age=="MSW")
df2<-filter(df.2.ctot, Type=="Wild", Age=="MSW")
df<-full_join(df1,df2, by=NULL)%>% 
  mutate(Fishery = fct_relevel(Fishery, "Sea_tot","Coastal_tot")) 

gp1 <- ggplot(df, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    data=df,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=" ")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(limits = c(0,0.9))+
  facet_wrap(~Fishery, labeller=labeller(Fishery = labels)) 

plot(gp1)
