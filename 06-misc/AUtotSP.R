


source("run-this-first.R")
source(paste0(PathBasics,"boxplot-functions.R"))
source(paste0(PathBasics,"plotfunctions.R"))
source(paste0(PathBasics,"tidy-functions_2chains.R"))



load("../../WGBAST_shared/flhm/2024/output/CR_2024_selected_chain.Rdata")
ch24_r <- as.mcmc(chains)
chains24 = ch24_r[
  seq(1,nrow(ch24_r), by = 7)
  ,] %>% tail(1000)


load("../../WGBAST_shared/flhm/2025/output/FLHM_JAGS_2025_base4_data2025.Rdata")
ch25_r <- as.mcmc(run)

chains25 = ch25_r[
  seq(1,nrow(ch25_r), by = 4)
  ,] %>% tail(1000)
data <- run$data %>% list.format()

rivernames<-c("Torne","Simo","Kalix","Rane"
              ,"Pite","Aby","Byske","Rickle","Savaran"
              ,"Ume","Ore","Logde","Ljungan","Morrum"
              ,"Eman", "Kage", "Test")
Nstocks <-  17
years <- Years <- c(1987:2024)
#   deletes last element from years
Years_m <- Years[-length(Years)]

for(r in 1:Nstocks){
  df24<-boxplot.jags.df2(chains24, "NspWtot[",str_c(r,"]"),1:(length(Years_m)+1))
  df25<-boxplot.jags.df2(chains25, "NspWtot[",str_c(r,"]"),1:(length(Years_m)+2))
  
  df24<-mutate(df24, River=r, AU = data$AU[r])
  df25<-mutate(df25, River=r, AU = data$AU[r])
  # df24<-mutate(df24, River=r)
  # df25<-mutate(df25, River=r)
  
  ifelse(r>1, df24_2<-bind_rows(df24_2,df24),df24_2<-df24)
  ifelse(r>1, df25_2<-bind_rows(df25_2,df25),df25_2<-df25)
}
df24.2<-as.tibble(setNames(df24_2,c("Year","q5","q25","q50","q75","q95","River", "AU")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)

df25.2<-as.tibble(setNames(df25_2,c("Year","q5","q25","q50","q75","q95","River", "AU")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)

df24_AU <- 
  df24.2 %>% 
  group_by(AU, Year) %>% 
  reframe(Year, q5 = sum(q5), q25 = sum(q25), q50 = sum(q50), q75 = sum(q75), q95 = sum(q95)) %>% 
  unique()

df25_AU <- 
  df25.2 %>% 
  group_by(AU, Year) %>% 
  reframe(Year, q5 = sum(q5), q25 = sum(q25), q50 = sum(q50), q75 = sum(q75), q95 = sum(q95)) %>% 
  unique()


# Spawner count datasets
# =================

counts<-read_tsv(str_c(PathData,"spawner_counts.txt"),skip=8,col_names=T, na="NA")
colnames(counts)<-rivernames
counts<-counts%>%
  mutate(Year=c(1:(length(Years_m)+1)))%>%
  #mutate(Year=c(1:(length(Years))))%>%
  mutate(Year=Year+1986)%>%
  select(Torne, Simo, Kalix, Pite, Ume, Morrum,Year)%>%
  #gather(key="River", value="Count", `Torne`:`Ume`)%>%
  gather(key="River", value="Count", `Torne`:`Morrum`)%>%
  mutate(river=fct_recode(River,
                          "1"="Torne",
                          "2"="Simo",
                          "3"="Kalix",
                          "5"="Pite",
                          "10"="Ume",
                          "14"="Morrum"))%>%
  mutate(River=parse_integer(as.character(river)))%>%
  mutate(Count=Count/1000)

counts2<-read_tsv(str_c(PathData,"spawner_counts_notInJAGS.txt"),col_names=T, na="NA")
counts2<-counts2%>%
  gather(key="River", value="Count2", `Rane`:`Ore`)%>%
  mutate(river=fct_recode(River,
                          "4"="Rane",
                          "6"="Aby","7"="Byske", "8"="Rickle", "11"="Ore"))%>%
  mutate(River=parse_integer(as.character(river)))%>%
  mutate(Count2=Count2/1000)


counts<-full_join(counts, counts2, by=NULL)


df.2<-left_join(df.2,counts, by=NULL)

df.2<-df.2%>%
  mutate(Rivername=as.factor(River))%>%#, levels=NULL))%>%
  mutate(Rivername=fct_recode(Rivername, "Torne"="1", "Simo"="2", "Kalix"= "3", "Råne"="4",
                              "Pite"="5", "Åby"="6", "Byske"="7", "Rickleån"="8", "Sävärån"="9",
                              "Ume"="10", "Öre"="11", "Lögde"="12", "Ljungan"="13", "Mörrum"="14", "Emån"="15",
                              "Kåge"="16", "Testeboån"="17" ))




## ---- F42310



df242<-filter(df24_AU, Year>1991)
df252<-filter(df25_AU, Year>1991)
gp1 <- ggplot(df242, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    #stat = "identity",fill=rgb(1,1,1,0.6))+
    stat = "identity",colour="grey", fill="grey95")+
  geom_boxplot(
    data = df252, 
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
    #stat = "identity")+
  labs(x="Year", y="Number of spawners (1000s)", title="")+
  #geom_point(data=df2, aes(Year, Count),col="red")+
  #geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~AU, scales="free") 


plot(gp1)

df2<-filter(df.2, Year>1991,River>9)
gp2 <- ggplot(df2, aes(Year, group=Year))+
  theme_bw(base_size = 13)+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Number of spawners (1000s)", title="")+
  geom_line(aes(Year,q50))+
  geom_point(data=df2, aes(Year, Count),col="red")+
  geom_point(data=df2, aes(Year, Count2),col="blue", shape=17)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Rivername, scales="free") 

plot(gp2)

