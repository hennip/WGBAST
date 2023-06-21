# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-HR

# Model 1: 
# =================

#summary(chains1[ ,regexpr("HrW",varnames(chains))>0])
#summary(chains1[,"HrW[2,2]"])


  # from cohort+age to calendar years
  hrW<-hrR<-hcW.au1<-hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsims1))
  hdcW<-hdcR<-array(NA, dim=c(3,length(Years)-3, nsims1))
  for(y in 3:(length(Years))){ 
    for(a in 2:3){ # Grilse & 2SW (=MSW)
       #hrW[grilse:MSW, ]
      hrW[a-1,y-2,]<-chains1[,str_c("HrW[",y-(a-1),",",a,"]")]
      hrR[a-1,y-2,]<-chains1[,str_c("HrR[",y-(a-1),",",a,"]")]
      
      #hcW[grilse:MSW, ] AU1
      hcW.au1[a-1,y-2,]<-chains1[,str_c("HcW[",y-(a-1),",",a,",1]")]
      hcR.au1[a-1,y-2,]<-chains1[,str_c("HcR[",y-(a-1),",",a,",1]")]
     }
   }   
    for(y in 4:(length(Years))){ 
      for(a in 2:4){ # Grilse, 2SW, 3SW=MSW
      #hdcW[grilse:MSW, ]
      hdcW[a-1,y-3,]<-chains1[,str_c("HdcW[",y-(a-1),",",a,"]")]
      hdcR[a-1,y-3,]<-chains1[,str_c("HdcR[",y-(a-1),",",a,"]")]
      
    }}
  
  hdoW<-hdoR<-array(NA, dim=c(4,length(Years)-4, nsims1))
  for(y in 5:(length(Years))){ 
    for(a in 1:4){ # PS, Grilse, 2SW, 3SW=MSW
      hdoW[a,y-4,]<-chains1[,str_c("HdoW[",y-a,",",a,"]")]
      hdoR[a,y-4,]<-chains1[,str_c("HdoR[",y-a,",",a,"]")]
    }}  

  if(trolling1==T){
    htW<-htR<-array(NA, dim=c(3,length(Years)-3, nsims1))
  }
  hlW<-hlR<-array(NA, dim=c(3,length(Years)-3, nsims1))
  for(y in 4:(length(Years))){ 
    for(a in 1:3){ # PS, 2SW, 3SW=MSW
      hlW[a,y-3,]<-chains1[,str_c("HlW[",y-a,",",a,"]")]
      hlR[a,y-3,]<-chains1[,str_c("HlR[",y-a,",",a,"]")]
      if(trolling1==T){ 
        htW[a,y-3,]<-chains1[,str_c("HtW[",y-a,",",a,"]")]
        htR[a,y-3,]<-chains1[,str_c("HtR[",y-a,",",a,"]")]
      }
    }}
  #summary(chains[ ,regexpr("HdoW",varnames(chains))>0])  
  #summary(chains1[ ,regexpr("HdoW",varnames(chains1))>0])  

  # wrangle
  ##########################
  
  # River fishery
  
  for(a in 1:2){ # grilse vs msw
    dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="River", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years)-2))%>%
      mutate(age=a, Fishery="River", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hr<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1993)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  df.1.Hr
  
  
  # COASTAL FISHERIES (TRAPNET&GILLNET)
  
  for(a in 1:2){
    dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years)-2))%>%
      mutate(age=a, Fishery="Coast", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years)-2))%>%
      mutate(age=a, Fishery="Coast", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hc<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type,
           everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1","MSW"= "2"))                        
  
  df.1.Hc
  
  
  # COASTAL DRIFTNET
  
  for(a in 1:3){
    dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years)-3))%>%
      mutate(age=a, Fishery="CDN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years)-3))%>%
      mutate(age=a, Fishery="CDN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdc<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1988)%>%
    mutate(Age=fct_recode(factor(Age),
                          "Grilse"= "1",
                          "2SW"= "2",
                          "MSW"= "3"))
  df.1.Hdc
  
  
  # OFFSHORE DRIFTNET
  
  for(a in 1:4){ 
      dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years)-4))%>%
      mutate(age=a, Fishery="ODN", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years)-4))%>%
      mutate(age=a, Fishery="ODN", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hdo<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1989)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"= "1",
                          "2SW"= "2",
                          "3SW"= "3",
                          "MSW"= "4"))
  df.1.Hdo
  
  # OFFSHORE LONGLINE
  
  for(a in 1:3){
    dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years)-3))%>%
      mutate(age=a, Fishery="OLL", Type="Wild")
    ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
    
    dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years)-3))%>%
      mutate(age=a, Fishery="OLL", Type="Reared")
    ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
  }
  
  df<-full_join(dfW2,dfR2, by=NULL)
  
  df.1.Hl<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
    select(Age, Fishery, Type, everything())%>%
    mutate(Year=Year+1989)%>%
    mutate(Age=fct_recode(factor(Age),
                          "1SW"= "1",
                          "2SW"="2",
                          "MSW"="3"))
  df.1.Hl


  # OFFSHORE (RECREATIONAL) TROLLING
  if(trolling1==T){
    for(a in 1:3){
      dfW<-boxplot.bugs.df2(htW, a ,1:(length(Years)-3))%>%
        mutate(age=a, Fishery="Trolling", Type="Wild")
      ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
      
      dfR<-boxplot.bugs.df2(htR, a ,1:(length(Years)-3))%>%
        mutate(age=a, Fishery="Trolling", Type="Reared")
      ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
    }
    
    df<-full_join(dfW2,dfR2, by=NULL)
    
    df.1.Ht<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
      select(Age, Fishery, Type, everything())%>%
      mutate(Year=Year+1989)%>%
      mutate(Age=fct_recode(factor(Age),
                            "1SW"= "1",
                            "2SW"="2",
                            "MSW"="3"))
    df.1.Ht
    
  }
  
# OFFSHORE COMBINED
# missing for now


# Model 2: JAGS
# =================
#
#
#  summary(chains[ ,regexpr("HtW",varnames(chains))>0])
#length(chains[, "HrW[1,1]"])
#summary(chains[ ,regexpr("HcW",varnames(chains))>0])
#length(chains[, "HcW[1,1]"])
#summary(chains[ ,regexpr("HdcW",varnames(chains))>0])
#summary(chains[ ,regexpr("HdoW",varnames(chains))>0])
#summary(chains[ ,regexpr("HlW",varnames(chains))>0])

if(nchains2==1){

# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years)-2, nsims2))
hrR<-array(NA, dim=c(2,length(Years)-2, nsims2))
hcW.au1<-array(NA, dim=c(2,length(Years)-2, nsims2))
hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsims2))
hdcW<-hdcR<-array(NA, dim=c(3,length(Years)-3, nsims2))

for(y in 3:(length(Years))){ 
  #y<-3
  #a<-2
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #hrW[grilse:MSW, ]
    hrW[a-1,y-2,]<-chains[,str_c("HrW[",y-(a-1),",",a,"]")]
    hrR[a-1,y-2,]<-chains[,str_c("HrR[",y-(a-1),",",a,"]")]
    
    #hcW[grilse:MSW, ] AU1
    hcW.au1[a-1,y-2,]<-chains[,str_c("HcW[",y-(a-1),",",a,",1]")]
    hcR.au1[a-1,y-2,]<-chains[,str_c("HcR[",y-(a-1),",",a,",1]")]
  }} 
for(y in 4:(length(Years))){ 
  for(a in 2:4){ # Grilse, 2SW, 3SW=MSW
    #hdcW[grilse:MSW, ]
    hdcW[a-1,y-3,]<-chains[,str_c("HdcW[",y-(a-1),",",a,"]")]
    hdcR[a-1,y-3,]<-chains[,str_c("HdcR[",y-(a-1),",",a,"]")]
    
  }
}

hdoW<-hdoR<-array(NA, dim=c(4,length(Years)-4, nsims2))
for(y in 5:(length(Years))){ 
  for(a in 1:4){ # PS, Grilse, 2SW, 3SW=MSW
    hdoW[a,y-4,]<-chains[,str_c("HdoW[",y-a,",",a,"]")]
    hdoR[a,y-4,]<-chains[,str_c("HdoR[",y-a,",",a,"]")]
  }}  

if(trolling2==T){
  htW<-htR<-array(NA, dim=c(3,length(Years)-3, nsims2))
}
hlW<-hlR<-array(NA, dim=c(3,length(Years)-3, nsims2))
for(y in 4:(length(Years))){ 
  for(a in 1:3){ # PS, Grilse, 2SW=MSW
    hlW[a,y-3,]<-chains[,str_c("HlW[",y-a,",",a,"]")]
    hlR[a,y-3,]<-chains[,str_c("HlR[",y-a,",",a,"]")]
    if(trolling2==T){ 
      htW[a,y-3,]<-chains[,str_c("HtW[",y-a,",",a,"]")]
      htR[a,y-3,]<-chains[,str_c("HtR[",y-a,",",a,"]")]
    }
  }}


}
  
if(nchains2==2){

# from cohort+age to calendar years
hrW<-array(NA, dim=c(2,length(Years)-2, nsims2/2))
hrR<-array(NA, dim=c(2,length(Years)-2, nsims2/2))
hcW.au1<-array(NA, dim=c(2,length(Years)-2, nsims2/2))
hcR.au1<-array(NA, dim=c(2,length(Years)-2, nsims2/2))
hdcW<-hdcR<-array(NA, dim=c(3,length(Years)-3, nsims2/2))

for(y in 3:(length(Years))){ 
  #y<-3
  #a<-2
  for(a in 2:3){ # Grilse & 2SW (=MSW)
    #hrW[grilse:MSW, ]
    hrW[a-1,y-2,]<-chains[,str_c("HrW[",y-(a-1),",",a,"]")][[1]]
    hrR[a-1,y-2,]<-chains[,str_c("HrR[",y-(a-1),",",a,"]")][[1]]
    
    #hcW[grilse:MSW, ] AU1
    hcW.au1[a-1,y-2,]<-chains[,str_c("HcW[",y-(a-1),",",a,",1]")][[1]]
    hcR.au1[a-1,y-2,]<-chains[,str_c("HcR[",y-(a-1),",",a,",1]")][[1]]
}} 
  for(y in 4:(length(Years))){ 
    for(a in 2:4){ # Grilse, 2SW, 3SW=MSW
      #hdcW[grilse:MSW, ]
    hdcW[a-1,y-3,]<-chains[,str_c("HdcW[",y-(a-1),",",a,"]")][[1]]
    hdcR[a-1,y-3,]<-chains[,str_c("HdcR[",y-(a-1),",",a,"]")][[1]]
    
  }
  }

hdoW<-hdoR<-array(NA, dim=c(4,length(Years)-4, nsims2/2))
for(y in 5:(length(Years))){ 
  for(a in 1:4){ # PS, Grilse, 2SW, 3SW=MSW
    hdoW[a,y-4,]<-chains[,str_c("HdoW[",y-a,",",a,"]")][[1]]
    hdoR[a,y-4,]<-chains[,str_c("HdoR[",y-a,",",a,"]")][[1]]
  }}  

if(trolling2==T){
  htW<-htR<-array(NA, dim=c(3,length(Years)-3, nsims2/2))
}
hlW<-hlR<-array(NA, dim=c(3,length(Years)-3, nsims2/2))
for(y in 4:(length(Years))){ 
  for(a in 1:3){ # PS, Grilse, 2SW=MSW
    hlW[a,y-3,]<-chains[,str_c("HlW[",y-a,",",a,"]")][[1]]
    hlR[a,y-3,]<-chains[,str_c("HlR[",y-a,",",a,"]")][[1]]
    if(trolling2==T){ 
      htW[a,y-3,]<-chains[,str_c("HtW[",y-a,",",a,"]")][[1]]
      htR[a,y-3,]<-chains[,str_c("HtR[",y-a,",",a,"]")][[1]]
    }
  }}

}
  
# wrangle
##########################

# River fishery

for(a in 1:2){ # grilse vs msw
  dfW<-boxplot.bugs.df2(hrW, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hrR, a ,1:(length(Years)-2))%>%
    mutate(age=a, Fishery="River", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hr<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1993)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        
df.2.Hr


# COASTAL FISHERIES (TRAPNET&GILLNET)

for(a in 1:2){
  dfW<-boxplot.bugs.df2(hcW.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hcR.au1, a, 1:(length(Years)-2))%>%
    mutate(age=a, Fishery="Coast", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hc<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type,
         everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1","MSW"= "2"))                        

df.2.Hc


# COASTAL DRIFTNET

for(a in 1:3){
  dfW<-boxplot.bugs.df2(hdcW, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="CDN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdcR, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="CDN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdc<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1988)%>%
  mutate(Age=fct_recode(factor(Age),
                        "Grilse"= "1",
                        "2SW"= "2",
                        "MSW"= "3"))
df.2.Hdc


# OFFSHORE DRIFTNET

for(a in 1:4){ 
  dfW<-boxplot.bugs.df2(hdoW, a ,1:(length(Years)-4))%>%
    mutate(age=a, Fishery="ODN", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hdoR, a ,1:(length(Years)-4))%>%
    mutate(age=a, Fishery="ODN", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hdo<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1989)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "2SW"= "2",
                        "3SW"= "3",
                        "MSW"= "4"))
df.2.Hdo

# OFFSHORE LONGLINE

for(a in 1:3){
  dfW<-boxplot.bugs.df2(hlW, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="OLL", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(hlR, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="OLL", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Hl<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1989)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "2SW"="2",
                        "MSW"="3"))
df.2.Hl

# OFFSHORE (RECREATIONAL) TROLLING
if(trolling2==T){
for(a in 1:3){
  dfW<-boxplot.bugs.df2(htW, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="Trolling", Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)
  
  dfR<-boxplot.bugs.df2(htR, a ,1:(length(Years)-3))%>%
    mutate(age=a, Fishery="Trolling", Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)

df.2.Ht<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Fishery","Type")))%>%
  select(Age, Fishery, Type, everything())%>%
  mutate(Year=Year+1989)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "2SW"="2",
                        "MSW"="3"))
df.2.Ht

}
# Draw boxplots to compare
# ==========================

## ---- graphs-Hdo
df1<-filter(df.1.Hdo, Type=="Wild" & Age!="PS")
df2<-filter(df.2.Hdo, Type=="Wild" & Age!="PS")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,0.5))+
  facet_wrap(~Age)#, scales="free") 


df1<-filter(df.1.Hdo, Type=="Reared")
df2<-filter(df.2.Hdo, Type=="Reared")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore driftnet HR reared"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,0.5))+
  facet_wrap(~Age)#, scales="free") 



## ---- graphs-Hl

df1<-filter(df.1.Hl, Type=="Wild")
df2<-filter(df.2.Hl, Type=="Wild")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,0.5))+
  facet_wrap(~Age)#, scales="free") 


df1<-filter(df.1.Hl, Type=="Reared")
df2<-filter(df.2.Hl, Type=="Reared")

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
  geom_line(data=df1,aes(Year,q50),colour="grey")+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore longline HR reared"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,0.5))+
  facet_wrap(~Age)#, scales="free") 

## ---- graphs-Ht

if(trolling1==F &trolling2==T){
df2<-filter(df.2.Ht, Type=="Wild")

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR wild"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales="free") 
}

if(trolling1==F &trolling2==T){
df2<-filter(df.2.Ht, Type=="Reared")

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR reared"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales="free") 

}

if(trolling1==T & trolling2==T){
  df1<-filter(df.1.Ht, Type=="Wild")
  df2<-filter(df.2.Ht, Type=="Wild")
  
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
    geom_line(data=df1,aes(Year,q50),colour="grey")+
    geom_line(aes(Year,q50))+
    labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR wild"))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    #coord_cartesian(ylim=c(0,0.5))+
    facet_wrap(~Age)#, scales="free") 
}
if(trolling1==T & trolling2==T){
  df1<-filter(df.1.Ht, Type=="Reared")
  df2<-filter(df.2.Ht, Type=="Reared")
  
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
    geom_line(data=df1,aes(Year,q50),colour="grey")+
    geom_line(aes(Year,q50))+
    labs(x="Year", y="Harvest rate", title=str_c("Offshore trolling HR reared"))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
    #coord_cartesian(ylim=c(0,0.5))+
    facet_wrap(~Age)#, scales="free") 
  
}


## ---- graphs-Hc

# coastal TN & GN

df1<-filter(df.1.Hc, Type=="Wild", Year<2017)
df2<-filter(df.2.Hc, Type=="Wild")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal TN & GN, wild AU1")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)

df1<-filter(df.1.Hc, Type=="Reared", Year<2017)
df2<-filter(df.2.Hc, Type=="Reared")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal TN & GN, reared AU1")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)
#  facet_grid(AU~Age)


## ---- graphs-Hdc

# coastal DN
df1<-filter(df.1.Hdc, Type=="Wild")
df2<-filter(df.2.Hdc, Type=="Wild")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal DN, wild")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)#, scales = "free")

df1<-filter(df.1.Hdc, Type=="Reared")
df2<-filter(df.2.Hdc, Type=="Reared")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="Coastal DN, reared")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #coord_cartesian(ylim=c(0,1))+
  facet_wrap(~Age)#, scales = "free")

## ---- graphs-Hr

# river wild
df1<-filter(df.1.Hr, Type=="Wild")
df2<-filter(df.2.Hr, Type=="Wild")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
    geom_boxplot(
      data=df2,
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River, wild")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

# river reared
df1<-filter(df.1.Hr, Type=="Reared")
df2<-filter(df.2.Hr, Type=="Reared")

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey", fill="grey95")+
  geom_boxplot(
    data=df2,
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.1))+
  geom_line(data=df2,aes(Year,q50))+
  labs(x="Year", y="Harvest rate", title="River, reared")+
  geom_line(aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)

