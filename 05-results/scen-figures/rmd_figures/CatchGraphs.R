# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# CatchGraphs.R
# 
# Makes the graphs of the amount of estimated and predicted (future) catches 
# for coastal, offshore and river fisheries and in total.
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ 


scen_nr<-c(7,9,10,12:15)

for(s in 1:length(scen_nr)){
  #s<-5  
  EffScen<-scen_nr[s]
  #Load the file containing stats
  File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")
  
  File
  load(File)
  

  # # ===============================================================================
  
  cbind(c(1992:LastPredYear),c(1:length(year)))
  
  
  C_OLL<-array(NA, c(Nyears,nsim))
  C_CTN<-array(NA, c(Nyears,nsim))
  C_TR<-array(NA, c(Nyears,nsim))
  CalC_tot<-array(NA, c(Nyears,nsim))
  CalC_rep_coast<-array(NA, c(nsim,Nyears))
  CalC_rep_offs<-array(NA, c(nsim,Nyears))
  for(i in 1:nsim){
    for(y in 1:Nyears){
      C_OLL[y,i]<-sum(WOLL_C[2:6,y,1:Nstocks,i])+sum(ROLL_C[2:6,y,1:4,i])
      C_TR[y,i]<-sum(WTR_C[2:6,y,1:Nstocks,i])+sum(RTR_C[2:6,y,1:4,i])
      C_CTN[y,i]<-sum(WCTN_C[2:6,y,1:Nstocks,i])+sum(RCTN_C[2:6,y,1:4,i])
      if(y>1){
        #COASTAL FISHING IS IN CALENDAR /ADVICE YEAR BUT LONGLINE IS IN THE PREVIOUS ONE
        CalC_tot[y,i]<-C_OLL[y-1,i]+C_CTN[y,i]+C_TR[y-1,i]
        #CalC_rep_coast[i,y]<-CalC_tot[y,i]*p_rep_c
        #CalC_rep_offs[i,y]<-CalC_tot[y,i]*p_rep_o
        CalC_rep_coast[i,y]<-C_CTN[y,i]#*p_rep_c
        CalC_rep_offs[i,y]<-C_OLL[y-1,i]+C_TR[y-1,i]#*p_rep_o
      }
    }
  }
  
  # CalC_tot is the commercial sea removal, containing misreported, unreported and discards
  dfC<-boxplot.bugs.df(CalC_rep_coast, 1:Nyears)%>%
    mutate(Type="Coast")
  dfO<-boxplot.bugs.df(CalC_rep_offs, 1:Nyears)%>%
    mutate(Type="Offshore")
  
  tmp<-full_join(dfC,dfO)
  
  tmp<-as_tibble(setNames(tmp,c("Year","q5","q25","q50","q75","q95", "Type")))%>%
    mutate(Year=Year+1991)%>%
    mutate(Scen=s)
  if(s>1){df<-full_join(df,tmp)}else{df<-tmp}
  #if(s>3){df<-full_join(df,tmp)}else{df<-tmp}
  
} 


df<-df%>%mutate(Scen2= as.character(Scen))%>%
  mutate(Scen3= recode(Scen2, "1"="7", "2"="9", "3"="10", "4"="12", 
                       "5"="13", "6"="14", "7"="15"))%>%
  mutate(Scen4= as.integer(Scen3))

#scen_nr<-c(7,9,10,12:15)



df<-filter(df, Year>2009 & Year<2033)

df1<-filter(df, Type=="Coast")
df2<-filter(df, Type=="Offshore")


gp <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    data=df1,
    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",
    colour="grey50", fill="grey85")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.4), color=rgb(0,0,0,0.7))+
  labs(x="Year", y="Catch (1000's)", title="Sea catches")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))+
  coord_cartesian(xlim=c(2009,2033))+
  facet_wrap(~Scen4)


plot(gp)

