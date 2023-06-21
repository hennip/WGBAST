# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-catches

# unrep coefs (needed to adjust BUGS but not JAGS)
# =================
# # coef_r<-c(rep(NA,5),rep(1.24,9), rep(1.22,7), rep(1.23,(length(YearsB)-5)-16))
# # coef_c<-c(rep(NA,5),rep(1.33,9), rep(1.21,7),rep(1.2,5), rep(1.11,(length(YearsB)-5)-21))
# # coef_o<-c(rep(NA,5),rep(1.18,9), rep(1.15,7),rep(1.16,5), rep(1.12,(length(YearsB)-5)-21))
# # cbind(YearsB,coef_r,coef_c,coef_o)
# 
# ureport_r=c(rep(NA,times=5),rep(1.24,times=9),rep(1.22,times=7),rep(1.23,times=11))
# ureport_c=c(rep(NA,times=5),rep(1.33,times=9),rep(1.21,times=7),rep(1.20,times=5),rep(1.11,times=6))
# ureport_o=c(rep(NA,times=5),rep(1.18,times=9),rep(1.15,times=7),rep(1.16,times=4),rep(1.12,times=7))
# cbind(YearsB,ureport_r,ureport_c,ureport_o)


# Catch data (reported catches)
# =================
# Note! If trolling is included as a separate fishery, estimates of nct_ObsTotX needs to be added 
# as a separate graph. Be sure to use corresponding Catch.txt file

if(trolling2==T){
tmp<-read_tsv(str_c(PathData_FLHM, "Catch_TrollingSeparated.txt"), show_col_types = FALSE)
colnames(tmp)<-c("river", "coast", "offs", "trolling")
}else{
tmp<-read_tsv(str_c(PathData_FLHM, "Catch.txt"), show_col_types = FALSE)
colnames(tmp)<-c("river", "coast", "offs")
}

obs_r<-tmp[,1]%>%
  mutate(Type="River", Year=Years[1:length(Years)], obs_catch=river)%>%select(-river)
obs_c<-tmp[,2]%>%
  mutate(Type="Coast", Year=Years[1:length(Years)], obs_catch=coast)%>%select(-coast)
obs_o<-tmp[,3]%>%
  mutate(Type="Offshore", Year=Years[1:length(Years)], obs_catch=offs)%>%select(-offs)

obs<-full_join(obs_r,obs_c, by=NULL)
obs<-full_join(obs,obs_o, by=NULL)
if(trolling2==T){
  obs_tr<-tmp[,4]%>%
  mutate(Type="Trolling", Year=Years[1:length(Years)], obs_catch=trolling)%>%select(-trolling)
  obs<-full_join(obs,obs_tr, by=NULL)
}

# Total catch, including trolling if separated
obs_tot<-obs%>%group_by(Year)%>%
  summarise(obs_catch=sum(obs_catch))%>%
  mutate(Type="Total")

obs<-full_join(obs, obs_tot, by=NULL)

obs2<-obs

#View(obs)

# Model 1:
# =================
  
  catch_tot<-array(NA, dim=c(length(chains1[,"ncr_ObsTotX[1]"][[1]]),length(YearsB)-0))
  dim(catch_tot)
  # for(y in 1:length(YearsB)){
  #   catch_tot[,y]<-chains1[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
  #     chains1[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
  #     chains1[,str_c("nco_ObsTotX[",y,"]")][[1]]+
  #     ifelse(trolling1==T,
  #            chains[,str_c("nct_ObsTotX[",y,"]")][[1]],0)
  #   
  # }
  
  if(nchains1==1){
    catch_tot<-array(NA, dim=c(nsims1,length(YearsB)))
    dim(catch_tot)
    for(y in 1:(length(YearsB))){
      catch_tot[,y]<-chains1[,str_c("ncr_ObsTotX[",y,"]")]+
        chains1[,str_c("ncc_ObsTotX[",y,"]")]+
        chains1[,str_c("nco_ObsTotX[",y,"]")]+
        chains1[,str_c("nct_ObsTotX[",y,"]")]
        # ifelse(trolling1==T,
        #        chains1[,str_c("nct_ObsTotX[",y,"]")],0)
    }
  }  
  if(nchains1==2){
    catch_tot<-array(NA, dim=c(nsims1,length(YearsB)-fix1))
    dim(catch_tot)
    for(y in 1:(length(YearsB)-fix1)){
      catch_tot[,y]<-chains1[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
        chains1[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
        chains1[,str_c("nco_ObsTotX[",y,"]")][[1]]+
        chains1[,str_c("nct_ObsTotX[",y,"]")][[1]]
        # ifelse(trolling1==T,
        #        chains1[,str_c("nct_ObsTotX[",y,"]")][[1]],0)
    }
  }
  
  
  dfr<-boxplot.jags.df(chains1, "ncr_ObsTotX[", 1:(length(YearsB)-fix1))%>%
    mutate(Type="River")
  dfc<-boxplot.jags.df(chains1, "ncc_ObsTotX[", 1:(length(YearsB)-fix1))%>%
    mutate(Type="Coast")
  dfo<-boxplot.jags.df(chains1, "nco_ObsTotX[", 1:(length(YearsB)-fix1))%>%
    mutate(Type="Offshore")
  if(trolling1==T){
    dftr<-boxplot.jags.df(chains1, "nct_ObsTotX[", 1:(length(YearsB)-fix1))%>%
      mutate(Type="Trolling")
  }
  dft<-boxplot.bugs.df(catch_tot, 1:(length(YearsB)-fix1))%>%
    mutate(Type="Total", x=y)%>%select(-y)
  
  df<-full_join(dfr,dfc,by=NULL)%>%
    full_join(dfo,by=NULL)%>%
  #if(trolling1==T){df<-full_join(df,dftr,by=NULL)}
  full_join(dftr,by=NULL)%>%
    full_join(dft,by=NULL)
  
  df.1<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
    mutate(Year=Year+1986)
  df.1
  
  df.1<-full_join(df.1,obs,by=NULL)
  

# Model 2: 
# =================

#summary(chains[ ,regexpr("ncr_ObsTotX",varnames(chains))>0])
#summary(chains[ ,regexpr("nct_ObsTotX",varnames(chains))>0])
if(nchains2==1){
  catch_tot<-array(NA, dim=c(nsims2,length(Years)))
  dim(catch_tot)
  for(y in 1:(length(Years))){
    catch_tot[,y]<-chains[,str_c("ncr_ObsTotX[",y,"]")]+
      chains[,str_c("ncc_ObsTotX[",y,"]")]+
      chains[,str_c("nco_ObsTotX[",y,"]")]+
      chains[,str_c("nct_ObsTotX[",y,"]")]
      # ifelse(trolling2==T,
      #        chains[,str_c("nct_ObsTotX[",y,"]")],0)
  }
}  
if(nchains2==2){
  catch_tot<-array(NA, dim=c(nsims2,length(Years)))
  dim(catch_tot)
  for(y in 1:(length(Years))){
    catch_tot[,y]<-chains[,str_c("ncr_ObsTotX[",y,"]")][[1]]+
      chains[,str_c("ncc_ObsTotX[",y,"]")][[1]]+
      chains[,str_c("nco_ObsTotX[",y,"]")][[1]]+
      chains[,str_c("nct_ObsTotX[",y,"]")][[1]]
      # ifelse(trolling2==T,
      #        chains[,str_c("nct_ObsTotX[",y,"]")][[1]],0)
  }
}

dfr<-boxplot.jags.df(chains, "ncr_ObsTotX[", 1:(length(Years)))%>%
  mutate(Type="River")
dfc<-boxplot.jags.df(chains, "ncc_ObsTotX[", 1:(length(Years)))%>%
  mutate(Type="Coast")
dfo<-boxplot.jags.df(chains, "nco_ObsTotX[", 1:(length(Years)))%>%
  mutate(Type="Offshore")
#if(trolling2==T){
  dftr<-boxplot.jags.df(chains, "nct_ObsTotX[", 1:(length(Years)))%>%
  mutate(Type="Trolling")
#}
dft<-boxplot.bugs.df(catch_tot, 1:length(Years))%>%
  mutate(Type="Total", x=y)%>%select(-y)

df<-full_join(dfr,dfc,by=NULL)%>%
  full_join(dfo,by=NULL)%>%
  full_join(dftr,by=NULL)%>%
  full_join(dft,by=NULL)

df.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)
df.2

df.2<-full_join(df.2,obs2,by=NULL)


#View(df.2)
#View(df.1)


# Draw boxplots to compare
# ==========================

## ---- graphs-catches

df.1<-filter(df.1, Year>1991)
df.2<-filter(df.2, Year>1991)

for(i in 1:4){
  #i<-3
  if(i==1){ df1<-filter(df.1, Type=="River");df2<-filter(df.2, Type=="River")}
  if(i==2){ df1<-filter(df.1, Type=="Coast");df2<-filter(df.2, Type=="Coast")}
  if(i==3){ df1<-filter(df.1, Type=="Offshore");df2<-filter(df.2, Type=="Offshore")}
  if(i==4){ df1<-filter(df.1, Type=="Total");df2<-filter(df.2, Type=="Total")}
  
plot<-
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
  labs(x="Year", y="Catch (in thousands)", title="")+
  geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+
  geom_point(data=df1,aes(Year,obs_catch), col="red")+
  geom_point(data=df2,aes(Year,obs_catch), col="blue")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_grid(Type~.)
  
#print(plot)
if(i==1){plot1<-plot}
if(i==2){plot2<-plot}
if(i==3){plot3<-plot}
if(i==4){plot4<-plot}

}

#windows()
#par(mfrow=c(3,1))
grid.arrange(plot1, plot2, plot3, plot4, nrow=2, ncol=2)

df1<-filter(df.1, Type=="Trolling")
df2<-filter(df.2, Type=="Trolling")
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
  labs(x="Year", y="Catch (in thousands)", title="Offshore trolling")+
  geom_line(aes(Year,q50))+
  # geom_line(data=df1,aes(Year,q50),col="grey")+
  # geom_point(data=df1,aes(Year,obs_catch), col="red")+
  geom_point(data=df2,aes(Year,obs_catch), col="blue")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))



#summary(chains[ ,regexpr("nct",varnames(chains))>0])


par(mfrow=c(2,3))
for(i in 6:(length(Years)-1)){
  gd<-gelman.diag(chains[,str_c("nct_ObsTotX[",i,"]")])
  #print(gd)
  
  if(gd$psrf[2]>1.2){
    #print(c(i, gd$psrf))
    traceplot(chainsGR[,str_c("nct_ObsTotX[",i,"]")], main=str_c("nct_ObsTotX ",df.2$Year[i]))
  }
  
  # }
}


