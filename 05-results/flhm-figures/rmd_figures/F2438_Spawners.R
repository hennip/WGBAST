
rivernames<-c("Torne","Simo","Kalix","Rane"
              ,"Pite","Aby","Byske","Rickle","Savaran"
              ,"Ume","Ore","Logde","Ljungan","Morrum"
              ,"Eman", "Kage", "Test")


for(r in 1:Nstocks){
  #r<-1
  df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:(length(Years_m)+1))
  #df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:(length(Years)+1),1)
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as.tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.2



# Spawner count datasets
# =================

counts<-read_tsv(str_c(PathData,"spawner_counts.txt"),skip=9,col_names=T, na="NA") %>% 
  as.data.frame()

colnames(counts)<-rivernames
rownames(counts) <-  1986+1:nrow(counts)
counts_m <- t(counts) %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "rivername") %>% 
  mutate(id  = 1:17) 


counts_long <- counts_m %>% 
  melt(id.vars = c("id", "rivername")) %>% 
  select(-rivername) %>% 
  transmute(
    River = id,
    Year = variable %>% as.character() %>% as.numeric(),
    Count = value/1000
  )
  

counts2<-read_tsv(str_c(PathData,"spawner_counts_notInJAGS.txt"),col_names=T, na="NA") %>% 
  as.data.frame()

rownames(counts2) <- counts2$Year
counts2_r <- counts2[,-1]

counts2_m <- t(counts2_r) %>% 
  as.data.frame() %>% 
  mutate(id = c(6,7,8,11))

counts2_long <- counts2_m %>% 
  melt(id.vars = c("id")) %>% 
  transmute(
    River = id,
    Year = variable %>% as.character() %>% as.numeric(),
    Count2 = value/1000
  )

#counts<-full_join(counts, counts2, by=NULL)
counts<-full_join(counts_long, counts2_long, by=c("Year", "River"))
#View(counts2)


df.2<-left_join(df.2,counts, by=c("River", "Year"))

df.2<-df.2%>%
  mutate(Rivername=as.factor(River))%>%#, levels=NULL))%>%
  mutate(Rivername=fct_recode(Rivername, "Torne"="1", "Simo"="2", "Kalix"= "3", "Råne"="4",
                              "Pite"="5", "Åby"="6", "Byske"="7", "Rickleån"="8", "Sävärån"="9",
                              "Ume"="10", "Öre"="11", "Lögde"="12", "Ljungan"="13", "Mörrum"="14", "Emån"="15",
                              "Kåge"="16", "Testeboån"="17" ))




## ---- F42310



df2<-filter(df.2, Year>1991, River<10)

gp3 <-list()
for(riv_id in df2$Rivername %>% unique()){
  
  gp_r = ggplot(data = df2 %>% filter(Rivername == riv_id),
                aes(Year, group=Year))+
    theme_bw(base_size = 10)+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
    #labs(x="Year", y="Number of spawners (1000s)", title="")+
    labs(x="", y="", title=riv_id)+
    geom_line(aes(Year,q50))+
    geom_point(aes(Year, Count),col="red")+
    geom_point(aes(Year, Count2),col="blue", shape=17)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

  gp3[[riv_id]] = gp_r
}

pg3 = plot_grid(plotlist = gp3,
          ncol = 3, nrow = 3
          ) 

pg3 <- ggdraw(pg3)+
  draw_label("Number of spawners (1000s)", 
                       x = 0.01, angle = 90)+
  draw_label("Year",
             x = 0.5, y = 0.01)

plot(pg3)



df2<-filter(df.2, Year>1991,River>9)

gp4 <-list()
for(riv_id in df2$Rivername %>% unique()){
  
  gp_r = ggplot(data = df2 %>% filter(Rivername == riv_id),
                aes(Year, group=Year))+
    theme_bw(base_size = 10)+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity",fill=rgb(1,1,1,0.6))+
    #labs(x="Year", y="Number of spawners (1000s)", title="")+
    labs(x="", y="", title=riv_id)+
    geom_line(aes(Year,q50))+
    geom_point(aes(Year, Count),col="red")+
    geom_point(aes(Year, Count2),col="blue", shape=17)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    ggtitle(riv_id)
  if(riv_id == "Ljungan"){
    gp_r = gp_r+ylim(0,2)
  }
  gp4[[riv_id]] = gp_r
}


pg4 = plot_grid(plotlist = gp4,
                ncol = 3, nrow = 3
) 

pg4 <- ggdraw(pg4)+
  draw_label("Number of spawners (1000s)", 
             x = 0.01, angle = 90)+
  draw_label("Year",
             x = 0.5, y = 0.01)

plot(pg4)
