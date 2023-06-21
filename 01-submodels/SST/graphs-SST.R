############################
# Graphs for SST data and estimates
###############################

# Workflow:

# 1: Run data-SST.R
# 2: Input data to BUGS (data-SST.odc) and run model-SST.odc 
# 3: Run graphs-SST.R for figures
# 4: Produce input data for the full life history model (SSTinputToJAGS.xlsx)


## ---- load-sst-april


# Plot SST data (this graph not currently needed anywhere, but nice to check out )
df.plot<-df.bugs %>% 
   group_by(year, Month)%>%
   summarise(sst=mean(sst.station), # mean SST per month
             sd.sst=sd(sst.station, na.rm=T))%>%
  mutate(month=factor(Month, levels=c(1:4)))%>%
  mutate(Year=year+1991)

df.plot
ggplot(df.plot) + 
  geom_line(aes(x = Year, y = sst, color=month), linewidth=1)+
  geom_errorbar(aes(x=Year, ymin=sst-sd.sst, ymax=sst+sd.sst, color=month)) +
  labs(x="Year", y="Sea surface temperature", color="Month")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

####################


# Expected April temperatures (median & 95% PI) from the model
(april<-read_xlsx(str_c(pathIn,"output-aprilSST.xlsx"),skip=3))
april<-mutate(april,Year=year)%>%
select(-year)

df.obs<-filter(df.plot, Month==4)
df.april<-full_join(df.obs, april)
#View(df.april)

## ---- graphs-sst-april

ggplot(data = df.april) +
  theme_bw()+
  geom_line(aes(x=Year, y=med), col="red")+
  geom_line(aes(x=Year, y=low), col="red")+
  geom_line(aes(x=Year, y=high), col="red")+
  geom_line(aes(x=Year, y=sst), col="blue")+
  geom_errorbar(aes(x=Year, ymin=sst-sd.sst, ymax=sst+sd.sst), col="blue")+
  labs(x="Year", y="Temperature (C)",title="April SST", col=c("Prediction", "Observation"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))


