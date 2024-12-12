
# Compare BUGS/JAGS results

#source("models-select.R")

## ---- load-smolts


# Model 1:
# =========

  for(r in 1:nstocks){
    #r<-1
    df<-boxplot.jags.df2(chains1, "SmoltW[",str_c(r,"]"),1:(length(YearsB)+3))
    #df<-boxplot.jags.df2(chains1, "SmoltWW[",str_c(r,"]"),1:(length(Years)+3))
    df<-mutate(df, River=r)
    ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
  }
  df.1<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
    select(River, everything())%>%
    mutate(Year=Year+1986)
  df.1

# Model 2: 
# =========
 # summary(chains[ ,regexpr("SmoltW",varnames(chains))>0])

for(r in 1:nstocks){
  #r<-1
  df<-boxplot.jags.df2(chains, "SmoltW[",str_c(r,"]"),1:(length(Years)+3))
  df<-mutate(df, River=r)
  ifelse(r>1, df2<-bind_rows(df2,df),df2<-df)
}
df.2<-as_tibble(setNames(df2,c("Year","q5","q25","q50","q75","q95","River")))%>%
  select(River, everything())%>%
  mutate(Year=Year+1986)
df.2


## ---- graphs-smolts


# Draw boxplots to compare
# ==========================

#df1<-df.1
#df2<-df.2

for(r in 1:16){
  #r<-1
  df1<-filter(df.1, River==r, Year>1991)
  df2<-filter(df.2, River==r, Year>1991)
  gp <- ggplot(df2, aes(Year, group=Year))+
      theme_bw()+
      geom_boxplot(
        data=df1,
        mapping= aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
        stat = "identity",
        colour="grey", fill="grey95")+
      geom_boxplot(
        aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
        stat = "identity",fill=rgb(1,1,1,0.1))+
      labs(x="Year", y="Number of smolts (1000s)", title=Rivername[r])+
      geom_line(aes(Year,q50))+
      geom_line(data=df1,aes(Year,q50),col="grey")+  
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  if(r == 13) gp <- gp+ylim(0, 25)
  print(gp)
  
}

# Testeboån
r<-17
df1<-filter(df.1, River==r, Year>1991)
df2<-filter(df.2, River==r, Year>1991)
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
        labs(x="Year", y="Number of smolts (1000s)", title=Rivername[r])+
        geom_line(aes(Year,q50))+
        geom_line(data=df1,aes(Year,q50),col="grey")+  
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  ylim(0,20)

