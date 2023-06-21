
# Compare BUGS/JAGS results

#source("models-select.R")


## ---- load-nsp

# Model 1: 
# =========  # Number of spawners per river
  
for(r in 1:nstocks){
    #r<-1
    df<-boxplot.jags.df2(chains1, "NspWtot[",str_c(r,"]"),1:length(YearsB)+1)
    #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
    df<-mutate(df, River=r)
    ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
  }
  df.1<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
    select(River, everything())%>%
    mutate(Year=Year+1986)
  df.1
  #View(df.1)


# Model 2: 
# =========
  
# Number of spawners per river
for(r in 1:nstocks){
#r<-1
  df<-boxplot.jags.df2(chains, "NspWtot[",str_c(r,"]"),1:(length(Years)+1))
  #df<-boxplot.jags.df2(dsub, "NspWtot[",str_c(r,"]"),1:length(Years))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.2
#View(df.2)


# Spawner count datasets
# =================

counts1<-read_tsv(str_c(PathData_FLHM,"spawner_counts.txt"),skip=8,col_names=T, na="NA")

colnames(counts1)<-Rivername
counts1<-counts1%>%
  mutate(Year=c(1:(length(Years)+1)))%>%
  mutate(Year=Year+1986)%>%
  select(Torne, Simo, Kalix, Ume, Year)%>%
  gather(key="River", value="Count", `Torne`:`Ume`)%>%
  mutate(River=fct_recode(River,
                          "1"="Torne",
                          "2"="Simo",
                          "3"="Kalix",
                          "10"="Ume"))%>%
  mutate(River=as.character(River))%>%
  mutate(Count=Count/1000)
counts1
#View(counts)

counts2<-read_tsv(str_c(PathData_FLHM,"spawner_counts_notInJAGS.txt"),col_names=T, na="NA")
counts2<-counts2%>%
  gather(key="River", value="Count2", `Rane`:`Ore`)%>%
  mutate(River=fct_recode(River,
                        "4"="Rane","5"="Pite",
                        "6"="Aby","7"="Byske", "8"="Rickle", "11"="Ore"))%>%
  mutate(River=as.character(River))%>%
  mutate(Count2=Count2/1000)


counts<-full_join(counts1, counts2, by=NULL)%>%
  mutate(River=as.numeric(River))
#View(counts)


df.2<-left_join(df.2,counts, by=NULL)
#View(df.1)

## ---- graphs-nsp


# Draw boxplots to compare
# ==========================


for(r in 1:17){
#r<-1
df1<-filter(df.1, River==r, Year>1991)
df2<-filter(df.2, River==r, Year>1991)
#df1<-filter(df.1, Year>1991)
#df2<-filter(df.2, Year>1991)
print(
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
  labs(x="Year", y="Number of spawners (1000s)", 
       #title=Rivername_long[r])+
       title=Rivername[r])+
       geom_line(aes(Year,q50))+
  geom_line(data=df1,aes(Year,q50),col="grey")+  
  geom_point(data=df2, aes(Year, Count),col="red")+
  geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))#+
  #facet_wrap(~River, scales="free") # Facet if you like to have all graphs together, downside is you cannot easily control ylim and scales are very different
  
)
}

## ---- graphs-nsp-report


# Draw boxplots to compare
# ==========================

#df1<-filter(df.1, Year>1991)
#df2<-filter(df.2, Year>1991)

plots<-list()
for(r in 1:17){
  #r<-1
  df1<-filter(df.1, River==r, Year>1991)
  df2<-filter(df.2, River==r, Year>1991)
  plot<-ggplot(df2, aes(Year, group=Year))+
          theme_bw()+
          geom_boxplot(
            aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
            stat = "identity",fill=rgb(1,1,1,0.6))+
          labs(x="Year", y="1000s spawners", title=Rivername[r])+
          geom_line(aes(Year,q50))+
          geom_point(data=df2, aes(Year, Count),col="red")+
          geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  plots[[r]]<-plot
}

# res <- 6
# name_figure <- "spawners1.png"
# png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
# 
# grid.arrange(plots[[1]],plots[[2]], plots[[3]],plots[[4]], 
#              plots[[5]],plots[[6]], plots[[7]],plots[[8]], 
#              plots[[9]],ncol=3)
# 
# dev.off()
# 
# res <- 6
# name_figure <- "spawners2.png"
# png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)
# 
# grid.arrange(plots[[10]], plots[[11]],plots[[12]], 
#              plots[[13]],plots[[14]], plots[[15]],plots[[16]],ncol=3)
# 
# dev.off()
