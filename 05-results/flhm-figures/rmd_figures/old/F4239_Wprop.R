# ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*
# Project: 		 Baltic salmon stock assessment (WGBAST)

# Contents:		 produce figure F4.2.3.9, Wprop

## ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*


# Scale data
# =================

tmp1<-read.table(str_c(PathData, "Scale.txt"), header=T)[,1]
tmp2<-read.table(str_c(PathData, "Scale.txt"), header=T)[,3]


fix<-2 # or 1

tmp1<-as.tibble(tmp1)%>%
  mutate(Type="2SW", Year=c(1987:(Years_m[length(Years_m)]+fix)))
tmp2<-as.tibble(tmp2)%>%
  mutate(Type="3SW", Year=c(1987:(Years_m[length(Years_m)]+fix)))

obs<-full_join(tmp1, tmp2, by=NULL)
colnames(obs)<-c("obs_prop", "Type", "Year")

# Model estimates
# =================

df_2sw<-boxplot.jags.df2(chains, "Wprop[", "1]", 6:(length(Years_m)-1))%>%
  mutate(Type="2SW")
df_3sw<-boxplot.jags.df2(chains, "Wprop[", "2]", 6:(length(Years_m)-1))%>%
  mutate(Type="3SW")

df<-full_join(df_2sw,df_3sw, by=NULL)

df.2<-as.tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Type")))%>%
  mutate(Year=Year+1986)

df.2<-full_join(df.2,obs, by=NULL)

df.2


## ---- F4239


df2<-df.2 %>% filter(Year>=1992)

gp1 <- ggplot(df2, aes(Year, group = Year))+
  theme_bw()+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion", title="Wild proportion")+
  geom_line(aes(Year,q50))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  coord_cartesian(ylim=c(0,1))+
  geom_point(aes(x=Year, y=obs_prop))+
  facet_grid(Type~.)

plot(gp1)