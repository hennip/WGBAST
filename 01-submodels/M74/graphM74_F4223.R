
#source("01-submodels/M74/data-M74.r")
source("run-this-first.R")
source("00-basics/packages.r")
source("00-basics/boxplot-functions.r")
## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Input mean2_M74 and mean_M74 codas from one file (note! all rivers and 
# years are in the same chain) 
# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


res <- read_rds(str_c(pathMain,"01-PROJECTS/M74/2024/JAGS/res/res24M74.rds"))

mcmc_r <- as.mcmc(res)
mcmc <- mcmc_r[seq(1,nrow(mcmc_r), by = res$thin),] %>% tail(1000) %>% as.data.frame()

mean1 = mcmc %>% 
  select(starts_with("mean_M74"))

mean2 = mcmc %>% 
  select(starts_with("mean2_M74"))

# t<- mean2 %>% select(
#   ends_with(
#     paste("[", y, ",", r, "]", sep = "")
#   )) %>% c()

Years<-c(1985:2022) #spawning years, add +1 each year (lags one behind)
Rivers<-c(1:14)

mean2_M74<-array(NA, dim=c(length(Rivers),length(Years),1000))
mean_M74<-array(NA, dim=c(length(Rivers),length(Years),1000))

for(y in 1:(length(Years))){
  #   1 index
  for(r in 1:length(Rivers)){
    #   2 index
    # all chains are in one, so first 1000 river=1, year=1, then
    # next 1000 river=2, year=1 etc... River changes after evey thousand
    # draws and year in every 14000 draws. 
    #mean2_M74[r,y,]<-mean2[(1+14000*(y-1)+1000*(r-1)):(14000*(y-1)+1000*r),2]
    #mean_M74[r,y,]<-mean1[(1+14000*(y-1)+1000*(r-1)):(14000*(y-1)+1000*r),2]
    m2 <- mean2 %>% select(
      ends_with(
        paste("[", y, ",", r, "]", sep = "")
      )) %>% c()
    
    mean2_M74[r,y,] <- m2[[1]]
    
    m1 <- mean1 %>% select(
      ends_with(
        paste("[", y, ",", r, "]", sep = "")
      )) %>% c()
    mean_M74[r,y,] <- m1[[1]]
    
  }
}
#   sanity check
length(Rivers)*length(Years)



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



M74fi <- read.table(str_c(pathMain,"01-PROJECTS/M74/2024/JAGS/input/M74dataFI23.txt"),
                    header = T)
colnames(M74fi) <- c("Eggs", "year", "ss", "sur_eggs", "j", "k")



Mfi <- M74fi %>% 
  mutate(
    year = year %>% as.numeric,
    Eggs = Eggs %>% as.numeric,
    die_eggs = Eggs - sur_eggs,
    m74fem  = ifelse(k>j, NA, k),
    fem = ifelse(k>j, NA, j),
    #propM74 = 1-k/j,
    ysfm = die_eggs/Eggs) %>% 
  group_by(year, ss) %>% 
  mutate(
    ysfm = ysfm %>% mean(na.rm=T),
    propM74 = (sum(j-1)/n())
  ) %>% 
  select(Year = year, stock = ss, Eggs, ysfm, propM74) %>% unique


M74se <- read.table(str_c(pathMain,"01-PROJECTS/M74/2024/JAGS/input/M74dataSE23.csv"),
                    header = T, sep = ",")
colnames(M74se) <- c("year","ss","Females",  "m74fem")

Mse <- M74se %>% 
  mutate(
    year = year %>% as.numeric,
    propM74 = m74fem/Females) %>% 
  group_by(year, ss) %>% 
  mutate(
    propM74 = propM74 %>% mean(na.rm=T)
  ) %>% 
  select(Year = year, stock = ss, Females, propM74) %>% unique




M74 <- full_join(Mfi, Mse)
#df1<-full_join(df.bugs, dfM74)%>%
df1<-full_join(df.bugs, M74, by = c("Year", "stock"))%>%
  arrange(stock)%>% # Arranges rivers into ascending order
  mutate(river=as.factor(stock))%>%
  mutate(rivername=fct_recode(river,
                              Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
                              Luleälven="5",Skellefteälven="6",Umeälven="7",Ångermanälven="8",
                              Indalsälven="9",Ljungan="10",Ljusnan="11",Dalälven="12",
                              Morrumsån="13",`Unsampled stock`="14"))


# 
# windows()
# df1
# ggplot(df1, aes(x=YEAR,group=YEAR,
#                 ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95))+
#   geom_boxplot(stat = "identity")+
#   theme_bw()+
#   labs(x="Year", y="Proportion", title="")+
#   geom_point(aes(YEAR,ysfm), shape=2)+
#   geom_point(aes(YEAR,propM74), shape=1)+
#   facet_wrap(~rivername)+
#   scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
#   theme(title = element_text(size=15), 
#         axis.text = element_text(size=12), strip.text = element_text(size=15))

lgp <- list()
lwd = 0.4
size = 0.75
for(i in 1:14){
  dm <- df1 %>% filter(stock==i)
  gp <- ggplot(dm, aes(x=YEAR,group=YEAR,
                  ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95))+
    geom_boxplot(stat = "identity", lwd = lwd)+
    theme_bw()+
    labs(x=NULL, y=NULL, title=NULL)+
    geom_point(aes(YEAR,ysfm), shape=2, size = size)+
    geom_point(aes(YEAR,propM74), shape=1, size = size)+
    facet_wrap(~rivername)+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
    theme(title = element_text(size=10), 
          axis.text = element_text(size=8), strip.text = element_text(size=10))
  #plot(gp)
  lgp[[i]] = gp
}
grid.arrange(grobs=lgp, left = "Proportion", bottom = "Year")
#library(cowplot)
#m74g <- plot_grid(plotlist = lgp, nrow = 4, ncol = 4, vjust = -10)

# m74g <- ggdraw(m74g)+
#   draw_label("Proportion", 
#              x = 0.01, angle = 90)+
#   draw_label("Year",
#              x = 0.5, y = 0.01)

png(filename = "05-results/flhm-figures/figures/F4_2_2_2.png", res = 300, 
    width = 23, height =24, units = "cm")
grid.arrange(grobs=lgp, left = "Proportion", bottom = "Year", )
dev.off()
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

gp3 <- ggplot(df1, aes(Year, group=Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion", title="Proportion of M74 affected offspring that dies")+
  #geom_line(aes(Year,q50))+
  facet_grid(rivername~.)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme(title = element_text(size=15), axis.text = element_text(size=12), strip.text = element_text(size=15))

png(filename = "05-results/flhm-figures/figures/F4_2_2_3.png", res = 300, 
    width = 23, height =24, units = "cm")
plot(gp3)
dev.off()




