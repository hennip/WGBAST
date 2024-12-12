# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.6, catch graphs

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

# Catch data (reported catches)
# =================
tmp<-read_tsv(str_c(PathData,"Catch_TrollingSeparated.txt"))
colnames(tmp)<-c("river", "coast", "offs", "trolling")
Years_m <- Years[-length(Years)]


# correction coefs for unreporting:
tmp2<-read_tsv(str_c(PathData,"unrep_coefs.txt"))
unrep<-tmp2%>%mutate(Year=Years_m)%>%select(-...4)


fix<-0 # or 1


obs_r<-tmp[,1]%>%
  mutate(Type="River", Year=Years_m[1:(length(Years_m))-fix], obs_catch=river)%>%select(-river)%>%
  full_join(unrep%>%select(coef_r,Year), by=NULL)%>%
  mutate(catchWunrep=obs_catch*coef_r)%>%select(catchWunrep, Year, Type)
obs_c<-tmp[,2]%>%
  mutate(Type="Coast", Year=Years_m[1:(length(Years_m)-fix)], obs_catch=coast)%>%select(-coast)%>%
  full_join(unrep%>%select(coef_c,Year), by=NULL)%>%
  mutate(catchWunrep=obs_catch*coef_c)%>%select(catchWunrep, Year, Type)
obs_o<-tmp[,3]%>%
  mutate(Type="Offshore", Year=Years_m[1:(length(Years_m)-fix)], obs_catch=offs)%>%select(-offs)%>%
  full_join(unrep%>%select(coef_o,Year), by=NULL)%>%
  mutate(catchWunrep=obs_catch*coef_o)%>%select(catchWunrep, Year, Type)
obs_t<-tmp[,4]%>%
  mutate(Type="Trolling", Year=Years_m[1:(length(Years_m)-fix)], obs_catch=trolling)%>%select(-trolling)%>%
  full_join(unrep%>%select(coef_o,Year), by=NULL)%>%
  mutate(catchWunrep=obs_catch*coef_o)%>%select(catchWunrep, Year, Type)

obs<-full_join(obs_r,obs_c, by=NULL)%>%
  full_join(obs_o, by=NULL)%>%
  full_join(obs_t, by=NULL)

obs_tot<-obs%>%group_by(Year)%>%
  summarise(catchWunrep=sum(catchWunrep))%>%
  mutate(Type="Total")

obs<-full_join(obs, obs_tot, by=NULL)



# Model estimates
# =================
if(nchains==1){
  catch_tot<-array(NA, dim=c(length(chains[,"ncr_ObsTotX[1]"]),length(Years_m)-0))
  dim(catch_tot)
  for(y in 1:(length(Years_m)-0)){
    catch_tot[,y]<-chains[,str_c("ncr_ObsTotX[",y,"]")]+
      chains[,str_c("ncc_ObsTotX[",y,"]")]+
      chains[,str_c("nco_ObsTotX[",y,"]")]
  }
}
if(nchains==2){
  catch_tot<-array(NA, dim=c(length(chains[,"ncr_ObsTotX[1]"][[1]]),length(Years_m)-0))
  dim(catch_tot)
  for(y in 1:(length(Years_m)-0)){
    catch_tot[,y]<-chains[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
      chains[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
      chains[,str_c("nco_ObsTotX[",y,"]")][[1]]
  }
}
dfr<-boxplot.jags.df(chains, "ncr_ObsTotX[", 1:(length(Years_m)-0))%>%
  mutate(Type="River")

dfc<-boxplot.jags.df(chains, "ncc_ObsTotX[", 1:(length(Years_m)-0))%>%
  mutate(Type="Coast")

dfo<-boxplot.jags.df(chains, "nco_ObsTotX[", 1:(length(Years_m)-0))%>%
  mutate(Type="Offshore")

dft<-boxplot.jags.df(chains, "nct_ObsTotX[", 1:(length(Years_m)-0))%>%
  mutate(Type="Trolling")

dftot<-boxplot.bugs.df(catch_tot, 1:(length(Years_m)-0))%>%
  mutate(Type="Total", x=y)%>%select(-y)

df<-full_join(dfr,dfc,by=NULL)%>%
  full_join(dfo,by=NULL)%>%
  full_join(dft,by=NULL)%>%
  full_join(dftot,by=NULL)

df.2<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.2

df.2<-full_join(df.2,obs,by=NULL)


## ---- F4238

df.2<-filter(df.2, Year>1991)

for(i in 1:5){
  #i<-4
  if(i==1){ df2<-filter(df.2, Type=="River")}
  if(i==2){ df2<-filter(df.2, Type=="Coast")}
  if(i==3){ df2<-filter(df.2, Type=="Offshore")}
  if(i==4){ df2<-filter(df.2, Type=="Trolling")}
  if(i==5){ df2<-filter(df.2, Type=="Total")}
  
  plot<-ggplot(df2, aes(Year, group=Year))+
    theme_bw(base_size = 15)+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
    labs(x="Year", y="Catch (in thousands)", title="")+
    geom_line(aes(Year,q50))+
    geom_point(aes(Year,catchWunrep), col="red")+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    facet_grid(Type~.)
  
  #print(plot)
  if(i==1){plot1<-plot}
  if(i==2){plot2<-plot}
  if(i==3){plot3<-plot}
  if(i==4){plot4<-plot}
  if(i==5){plot5<-plot}
  
}


grid.arrange(plot1, plot2, plot3, plot4, plot5, nrow=3, ncol=2)


