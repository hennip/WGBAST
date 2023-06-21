

# Formerly called boxplot.bugs.df2
boxplot.bugs<-function(param, R, Y){ # variable name, values to x-axis
  # note: length of x and dim variable need to match
  
#param<-LR
#r<-4
#Y<-1:30
  Q5<-Q25<-Q50<-Q75<-Q95<-c()
  n<-length(Y)
  
  for(i in 1:n){
    x<-as.mcmc(param[R,i,])
    if(is.na(x[1])==F){      
      tmp<-summary(x,quantiles=c(0.05,0.25,0.5,0.75,0.95)) 
      Q5[i] = tmp$quantiles[1]
      Q25[i] = tmp$quantiles[2]
      Q50[i] = tmp$quantiles[3]
      Q75[i] = tmp$quantiles[4]
      Q95[i] = tmp$quantiles[5]
    }else{
      Q5[i]<-Q25[i]<-Q50[i]<-Q75[i]<-Q95[i]<-NA}
  }
  
  df<-data.frame(
    y<-Y,
    q5=Q5,
    q25=Q25,
    q50=Q50,
    q75=Q75,
    q95=Q95
  )
  colnames(df)<-c("y","q5","q25","q50","q75","q95")
df
}

