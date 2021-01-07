# Compare results

#source("models-select.R")

## ---- load-q

# Model 1: 
# =========



# Model 2: 
# =========

# Catchability per smolt cohort

#summary(chains[ ,regexpr("qlW",varnames(chains))>0])
#summary(chains[ ,regexpr("qdW",varnames(chains))>0])

# longline
for(a in 1:4){
  dfW<-boxplot.jags.df2(chains, "qlW[",str_c(a,"]"),1:(length(Years)+1))%>%
    mutate(Age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

  dfR<-boxplot.jags.df2(chains, "qlR[",str_c(a,"]"),1:(length(Years)+1))%>%
    mutate(Age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)
#df<-dfW2 # if reared is missing

df.ql.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "2SW"= "2",
                        "3SW"= "3"))
df.ql.2


# driftnet
for(a in 1:6){
  dfW<-boxplot.jags.df2(chains, "qdW[",str_c(a,"]"),1:(length(Years)+1))%>%
    mutate(Age=a, Type="Wild")
  ifelse(a>1, dfW2<-bind_rows(dfW2,dfW),dfW2<-dfW)

  dfR<-boxplot.jags.df2(chains, "qdR[",str_c(a,"]"),1:(length(Years)+1))%>%
    mutate(Age=a, Type="Reared")
  ifelse(a>1, dfR2<-bind_rows(dfR2,dfR),dfR2<-dfR)
}

df<-full_join(dfW2,dfR2, by=NULL)
#df<-dfW2 # if reared is missing

df.qd.2<-as_tibble(setNames(df,c("Year","q5","q25","q50","q75","q95","Age","Type")))%>%
  select(Age, Type, everything())%>%
  mutate(Year=Year+1986)%>%
  mutate(Age=fct_recode(factor(Age),
                        "1SW"= "1",
                        "2SW"= "2",
                        "3SW"= "3",
                        "4SW"= "4",
                        "5SW"= "5",
                        "6SW"="6"))
df.qd.2


# Draw boxplots to compare
# ==========================

## ---- graphs-q

# Wild ql
df2<-filter(df.ql.2, Type=="Wild", Age!=4)

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
#  geom_boxplot(
#    data=df1,
#    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
 #   stat = "identity",
 #   colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion per age group", title="Catchability, longline, wild")+
  geom_line(aes(Year,q50))+
#  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales="free")

# reared ql
df2<-filter(df.ql.2, Type=="Reared", Age!=4)

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  #  geom_boxplot(
  #    data=df1,
  #    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #   stat = "identity",
  #   colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion per age group", title="Catchability, longline, reared")+
  geom_line(aes(Year,q50))+
  #  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales="free")



# Wild qd
df2<-filter(df.qd.2, Type=="Wild", Age=="1SW"| Age=="2SW"| Age=="3SW"| Age=="4SW")

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  #  geom_boxplot(
  #    data=df1,
  #    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #   stat = "identity",
  #   colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion per age group", title="Catchability, driftnet, wild")+
  geom_line(aes(Year,q50))+
  #  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales = "free")

# Reared qd
df2<-filter(df.qd.2, Type=="Reared", Age=="1SW"| Age=="2SW"| Age=="3SW"| Age=="4SW")

ggplot(df2, aes(Year, group=Year))+
  theme_bw()+
  #  geom_boxplot(
  #    data=df1,
  #    mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
  #   stat = "identity",
  #   colour="grey", fill="grey95")+
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
    stat = "identity",fill=rgb(1,1,1,0.6))+
  labs(x="Year", y="Proportion per age group", title="Catchability, driftnet, reared")+
  geom_line(aes(Year,q50))+
  #  geom_line(data=df1,aes(Year,q50),col="grey")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  facet_wrap(~Age)#, scales = "free")


## ---- graphs-q-traces

par(mfrow=c(2,2))
traceplot(chains[,"sd_qd"], main="sd_qd")
traceplot(chains[,"phi_qd"], main="phi_qd")
traceplot(chains[,"mean_qdW[2]"], main="mean_qdW[2]")
traceplot(chains[,"mean_qdW[3]"], main="mean_qdW[3]")

par(mfrow=c(2,2))
traceplot(chains[,"sd_ql"], main="sd_ql")
traceplot(chains[,"phi_ql"], main="phi_ql")
traceplot(chains[,"mean_qlW"], main="mean_qlW")

par(mfrow=c(2,2))
traceplot(chains[,"sd_tr"], main="sd_tr")
traceplot(chains[,"phi_tr"], main="phi_tr")
traceplot(chains[,"mean_trW"], main="mean_trW")



