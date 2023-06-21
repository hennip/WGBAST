
source("01-submodels/M74/data-M74.r")
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Input mean2_M74 and mean_M74 codas from one file (note! all rivers and 
# years are in the same chain) 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

mean1<-read_tsv(str_c(pathM74,"/prg/output/coda_mean_M74_22.txt"), col_names = c("s", "mean1"))
mean2<-read_tsv(str_c(pathM74,"/prg/output/coda_mean2_M74_22.txt"), col_names = c("s", "mean2"))

# change format, otherwise the loops below won't work 
mean1<-as.data.frame(mean1)
mean2<-as.data.frame(mean2)

Years<-c(1985:2021) #spawning years, add +1 each year
Rivers<-c(1:14)

length(Rivers)*length(Years)

mean2_M74<-array(NA, dim=c(length(Rivers),length(Years),1000))
mean_M74<-array(NA, dim=c(length(Rivers),length(Years),1000))

for(y in 1:(length(Years))){
  for(r in 1:length(Rivers)){
    # all chains are in one, so first 1000 river=1, year=1, then
    # next 1000 river=2, year=1 etc... River changes after evey thousand
    # draws and year in every 14000 draws. 
    mean2_M74[r,y,]<-mean2[(1+14000*(y-1)+1000*(r-1)):(14000*(y-1)+1000*r),2]
    mean_M74[r,y,]<-mean1[(1+14000*(y-1)+1000*(r-1)):(14000*(y-1)+1000*r),2]
    
  }
}


#mean2_M74[,1,]

# F 4.2.2.2
#######################

for(r in 1:length(Rivers)){
  df<-boxplot.bugs(mean_M74, r ,1:length(Years))%>%
    mutate(stock=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}

df.bugs<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","stock")))%>%
  select(stock, everything())%>%
  mutate(YEAR=Year+1984)  #%>%
#  mutate(river=as.factor(stock))


df1<-full_join(df.bugs, dfM74)%>%
  arrange(stock)%>% # Arranges rivers into ascending order
  mutate(river=as.factor(stock))%>%
  mutate(rivername=fct_recode(river,
                              Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
                              Luleälven="5",Skellefteälven="6",Umeälven="7",Ångermanälven="8",
                              Indalsälven="9",Ljungan="10",Ljusnan="11",Dalälven="12",
                              Morrumsån="13",`Unsampled stock`="14"))



windows()

  ggplot(df1, aes(x=YEAR,group=YEAR,
                  ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95))+
    geom_boxplot(stat = "identity")+
    theme_bw()+
    labs(x="Year", y="Proportion", title="")+
    geom_point(aes(YEAR,ysfm), shape=2)+
    geom_point(aes(YEAR,propM74), shape=1)+
    facet_wrap(~rivername)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    theme(title = element_text(size=15), 
          axis.text = element_text(size=12), strip.text = element_text(size=15))
  
  # Torne & Simo only  
  
  # ggplot(filter(df1, stock<3), aes(x=YEAR,group=YEAR,
  #                 ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95))+
  #   geom_boxplot(stat = "identity")+
  #   theme_bw()+
  #   labs(x="Vuosi", y="Osuus", title="")+
  #   geom_point(aes(YEAR,ysfm), shape=2)+
  #   geom_point(aes(YEAR,propM74), shape=1)+
  #   facet_wrap(~rivername)+
  #   scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  #   theme(title = element_text(size=15), 
  #         axis.text = element_text(size=12), strip.text = element_text(size=15))
  # 

# F 4.2.2.3
#######################

for(r in 1:length(Rivers)){
  df<-boxplot.bugs(mean2_M74, r ,1:length(Years))%>%
    mutate(stock=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}

  

df.bugs<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1984)  %>%
  mutate(river=as.factor(River))%>%
  mutate(rivername=fct_recode(river,
                              Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
                              Lulealven="5",Skelleftealven="6",Umealven="7",Angermanalven="8",
                              Indalsalven="9",Ljungan="10",Ljusnan="11",Dalalven="12",
                              Morrumsan="13",`Unsampled stock`="14"))

df.bugs


df1<-filter(df.bugs, River==1 | River ==2 | River==14, Year>1990)

ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion", title="Proportion of M74 affected offspring that dies")+
  #geom_line(aes(Year,q50))+
  facet_grid(rivername~.)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))





