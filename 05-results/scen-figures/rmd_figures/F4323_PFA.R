
EffScen<-15
#Load the file containing stats
File<-paste0(PathScen,"ScenProj_",Model,"_EScen",EffScen,"_RCzero23-35.RData")

File
load(File)

nsim<-1000
Nstocks
# ===============================================================================


year<-c(1992:LastPredYear)

dim(PFAW)

PFAW[,,1:Nstocks,1:3]

PFAW2<-array(NA, dim=c(6,length(year),nsim))
PFAW2_MSW<-array(NA, dim=c(length(year),nsim))
PFAR2<-array(NA, dim=c(6,length(year),nsim))
PFAR2_MSW<-array(NA, dim=c(length(year),nsim))
PFA<-array(NA, dim=c(6,length(year),nsim))
PFA_MSW<-array(NA, dim=c(length(year),nsim))
PFAall<-array(NA, dim=c(length(year),nsim))
PFAWall<-array(NA, dim=c(length(year),nsim))
for(y in 1:length(year)){
  for(s in 1:nsim){
    PFAWall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,s], na.rm=T)
    PFAall[y,s]<-sum(PFAW[1:6,y,1:Nstocks,s], na.rm=T)+sum(PFAR[1:6,y,1:4,s])
    PFA_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,s], na.rm=T)+sum(PFAR[2:6,y,1:4,s])
    PFAR2_MSW[y,s]<-sum(PFAR[2:6,y,1:4,s])
    PFAW2_MSW[y,s]<-sum(PFAW[2:6,y,1:Nstocks,s], na.rm=T)
    
    for(a in 1:6){
      PFAW2[a,y,s]<-sum(PFAW[a,y,1:Nstocks,s], na.rm=T)
      PFAR2[a,y,s]<-sum(PFAR[a,y,1:4,s])
      PFA[a,y,s]<-sum(PFAW[a,y,1:Nstocks,s], na.rm=T)+sum(PFAR[a,y,1:4,s])
    }
  }}

df1<-boxplot.bugs.df(t(PFAW2[1,,]), 1:Nyears)%>%mutate(Type="Wild 1SW")%>%mutate(Year=year+1)
df2<-boxplot.bugs.df(t(PFA[1,,]), 1:Nyears)%>%mutate(Type="Wild & reared 1SW")%>%mutate(Year=year+1)
df3<-boxplot.bugs.df(t(PFAW2_MSW), 1:Nyears)%>%mutate(Type="Wild MSW")%>%mutate(Year=year+1)
df4<-boxplot.bugs.df(t(PFA_MSW), 1:Nyears)%>%mutate(Type="Wild & reared MSW")%>%mutate(Year=year+1)
df5<-boxplot.bugs.df(t(PFAall), 1:Nyears)%>%mutate(Type="All")%>%mutate(Year=year+1)

df<-full_join(df1,df2)%>%
  full_join(df3)%>%
  full_join(df4)%>%
  full_join(df5)


#   looping plots to list and plot_grid
pls <- list()
for(i in unique(df$Type) ){
  gp <- 
    ggplot(df %>% filter(Type == i), aes(Year, group=Year))+
    theme_bw(base_size = 13)+
    geom_boxplot(
      aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
      stat = "identity")+
    labs(x=NULL)+
    xlim(1990, 2040)+
    facet_wrap(~Type, scales="free", ncol = 2)
  
  if(i=="All"){ gp <- gp + ylim(0,4000)}
  else{ gp <- gp+ylim(0,2000)}
  
  pls[[paste(i)]] <- gp
}
  
y_lab <- textGrob("Abundance (in 1000's)", rot = 90,
                  gp=gpar(fontsize=15))
x_lab <- textGrob("Year",
                  gp=gpar(fontsize=15))
title <- textGrob("Pre-fishery abundance",
                  gp=gpar(fontsize=20))

grid.arrange(pls$All, pls$`Wild & reared 1SW`, pls$`Wild & reared MSW`,
             pls$`Wild 1SW`, pls$`Wild MSW`, ncol = 2, 
             left = y_lab,
             bottom = x_lab,
             top = title
                )


  



