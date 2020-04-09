# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
# CatchGraphs.R
# 
# Makes the graphs of the amount of estimated and predicted (future) catches 
# for coastal, offshore and river fisheries and in total.
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ 
rm(list=ls(all=TRUE))


source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")
source("C:/Rprojects/WGBAST/04-scenarios/scens_stuff.r")


E_OLL_hist<-read.table(str_c(PathData,"EffortOLL_ICES.txt"))
E_OLL_hist<-rbind(E_OLL_hist, c(0,0,0.77,2.83,5.34))
E_OLL_hist$Total<-rowSums(E_OLL_hist)
tmp<-(E_OLL_hist$Trolling/E_OLL_hist$Total)
prop_recr<-tmp[6:length(tmp)] # skip 1987-1991


#! Effort 
EffScen<-1

scen_nr<-c(1:4,5,6,7,8)

for(s in 1:8){
#s<-1  
  EffScen<-scen_nr[s]
  #Load the file containing stats
  File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,".RData")
  
  File
  load(File)
  
  Coef1<-0.7
  CoefTrollingF<-0.763 # this coef produces the 26.7k of recr catch when commercial fisheries are removed
  if(EffScen==1){Coef2<-2.249} # previous advice approach
  if(EffScen==2){Coef2<-2.78} #+20%
  if(EffScen==3){Coef2<-1.761} #-20%
  if(EffScen==8){Coef2<-5.237} #+100% 
  if(EffScen==4){Coef2<-2.753} # Updated target 
  if(EffScen==5){Coef2<-0} # no fishing
  if(EffScen==6){Coef2<-0} # No comm fishing
  if(EffScen==7){Coef2<-2.21} # no recr fishing
  
  Coef<-Coef1*Coef2
  
  # These according to ProjEffort-file
  E_OLL_DEN<-0.77*Coef
  E_OLL_PL<-2.83*Coef
  E_OLL_TROLLING<-5.34*CoefTrollingF # this covers all recr fishing
   
  prop_recr[(yBreak+2):Nyears]<-E_OLL_TROLLING/(E_OLL_DEN+E_OLL_PL+E_OLL_TROLLING) #2020 assessment
  #prop_recr[(yBreak+1):Nyears]<-E_OLL_TROLLING/(E_OLL_DEN+E_OLL_PL+E_OLL_TROLLING)
  prop_recr<-as.matrix(prop_recr)
  rownames(prop_recr)<-year
  p_wantCRep<-0.83 # share of wanted catch reported in commercial removal
  
  # ===============================================================================
  
  cbind(c(1992:LastPredYear),c(1:length(year)))
  
  C_OLL<-array(NA, c(1000,Nyears))
  C_CTN<-array(NA, c(1000,Nyears))
  CalC_comm<-array(NA, c(1000,Nyears))
  CalC_recr<-array(NA, c(1000,Nyears))
  for(i in 1:1000){
    for(y in 1:Nyears){
      C_OLL[i,y]<-sum(WOLLCtot[2:6,y,1:Nstocks,i], na.rm=T)+
                  sum(ROLLCtot[2:6,y,1:4,i])    
      C_CTN[i,y]<-sum(WCTNCtot[2:6,y,1:Nstocks,i], na.rm=T)+
                  sum(RCTNCtot[2:6,y,1:4,i])
      
      # p_wantCRep is the proportion of wanted catch reported out of all commercial, see T4_3_2_1_workfile.xlsx 
      
      if(y>1){
        if(s<10){
          CalC_recr[i,y]<-C_OLL[i,y-1]*prop_recr[y-1]
          CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*p_wantCRep
        }
        #if(y>(yBreak+1) && s==7){
          if(y>(yBreak+2) && s==7){ # 2020 assessment
            CalC_recr[i,y]<-0 #C_OLL[i,y-1]*prop_recr[y-1]
          CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*p_wantCRep
        }
        #else{
        #  if(y<=(yBreak+1)){ # until 2018 (yBreak+1 is the interim year, fishing scenarios affect coastal but not yet offshore fisheries)
        #    CalC_recr[i,y]<-C_OLL[i,y-1]*prop_recr[y-1]
        #    CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*0.532
        #  }
        #  if(y>(yBreak+1) && s==6 ){ #2019->
        #    CalC_comm[i,y]<-0
        #    CalC_recr[i,y]<-C_OLL[i,y-1]
        #  }
        #  if(y>(yBreak+1) && s==7 ){ #2019->
        #    CalC_comm[i,y]<-(C_OLL[i,y-1]*(1-prop_recr[y-1])+C_CTN[i,y])*0.532
        #    CalC_recr[i,y]<-0
        #  }
        #}
      }
    }
  }
  
  # CalC_tot is the commercial sea removal, containing misreported, unreported and discards
  dfC<-boxplot.bugs.df(CalC_comm, 1:Nyears)%>%
    mutate(Type="Comm")
  dfR<-boxplot.bugs.df(CalC_recr, 1:Nyears)%>%
    mutate(Type="Recr")
  
  tmp<-full_join(dfC,dfR)
  
  tmp<-as_tibble(setNames(tmp,c("Year","q5","q25","q50","q75","q95", "Type")))%>%
    mutate(Year=Year+1991)%>%
    mutate(Scen=s)
  if(s>1){df<-full_join(df,tmp)}else{df<-tmp}
}

df<-df%>%mutate(Scen2=as.factor(Scen))
#View(df)
# ============================================

# Observations
tmp<-read_tsv(str_c(PathData, "Catch_TrollingSeparated.txt"))
colnames(tmp)<-c("river", "coast", "offs","trolling")

obs_r<-tmp[,1]%>%mutate(Type="River", Year=1987:(LastHistYear+1), obs_catch=river)%>%select(-river)
obs_c<-tmp[,2]%>%mutate(Type="Coast", Year=1987:(LastHistYear+1), obs_catch=coast)%>%select(-coast)
obs_o<-tmp[,3]%>%mutate(Type="Offshore", Year=1987:(LastHistYear+1), obs_catch=offs)%>%select(-offs)
obs_tr<-tmp[,4]%>%mutate(Type="Trolling", Year=1987:(LastHistYear+1), obs_catch=trolling)%>%select(-trolling)

obs<-full_join(obs_r,obs_c, by=NULL)
obs<-full_join(obs,obs_o, by=NULL)
obs<-full_join(obs,obs_tr, by=NULL)

obs_t<-obs%>%group_by(Year)%>%
  summarise(obs_catch=sum(obs_catch))%>%
  mutate(Type="Total")

obs<-full_join(obs, obs_t, by=NULL)

df<-full_join(df, obs_o, by=NULL)

df<-full_join(df, obs_tr, by=NULL)

# ============================================

df<-filter(df, Year>2009 & Year<2027)

df1<-filter(df, Type=="Recr")
df2<-filter(df, Type=="Comm")

tiff(paste0(PathScen,"F4325.tiff"),  width=1600, height=1600, res=200)

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
  labs(x="Year", y="Catch (1000's)", title="Sea catches")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))+
  coord_cartesian(xlim=c(2009,2027))+
  facet_wrap(~Scen)

dev.off()


#filter(df,Scen==5, Year==2018)






  