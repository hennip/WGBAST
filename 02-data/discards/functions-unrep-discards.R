stats_y<-function(variable){
  q5<-q50<-q95<-c()
  for(y in 1:23){
    #for(i in 1:dim(variable)[2]){ # loop over all monitored variables
    q50[y]<-quantile(variable[y,],0.5)
    q5[y]<-quantile(variable[y,],0.05)
    q95[y]<-quantile(variable[y,],0.95)
    
  }#}
  
  return(cbind(c(2001:2023),q50,q5,q95))
}

median_y_c<-function(variable){
  q50<-array(NA, dim=c(23,9))
  for(y in 1:23){
    for(j in 1:9){
      q50[y,j]<-quantile(variable[y,j,],0.5)
    }}
  
  l1<-cbind(c(2001:2023),round(q50,4))
  
  colnames(l1)<-c("year","FI","SE","DK","PL","LV","LT","DE","EE","RU")
  return(l1)
}



median_y_c_k<-function(variable){
  q50<-array(NA, dim=c(23,9,2))
  #variable<-Tdis
  for(y in 1:23){
    for(j in 1:9){
      for(k in 1:2){
        #      for(i in 1:dim(variable)[4]){ # loop over all monitored variables
        q50[y,j,k]<-quantile(variable[y,j,k,],0.5)
        
      }}}#}
  
  l1<-cbind(c(2001:2023),round(q50[,,1],0))
  l2<-cbind(c(2001:2023),round(q50[,,2],0))
  
  colnames(l1)<-colnames(l2)<-c("year","FI","SE","DK","PL","LV","LT","DE","EE","RU")
  return(list(l1, l2))
}


stats_y_k<-function(variable){
  q5<-q50<-q95<-array(NA, dim=c(23,2))
  for(y in 1:23){
    for(k in 1:2){
      q5[y,k]<-quantile(variable[y,k,],0.05)
      q50[y,k]<-quantile(variable[y,k,],0.5)
      q95[y,k]<-quantile(variable[y,k,],0.95)
      
    }}
  l1<-cbind(c(2001:2023),q50[,1],q5[,1],q95[,1])
  l2<-cbind(c(2001:2023),q50[,2],q5[,2],q95[,2])
  colnames(l1)<-colnames(l2)<-c("year","q50", "q5", "q95")
  return(list(l1, l2))
}




# Replace NA's with 0's
rpl<-function(var){
  ifelse(is.na(var)==T, 0, var)
}



# calculate catch per country in number or in weight
func_country_catches<-function(df, numb_or_weight){
  #numb_or_weight<-2
    # GND, LLD, FYK & MIS
  ###############################################################################
  GND<-array(0, dim=c(23,9,2))
  FYK<-array(0, dim=c(23,9,2))
  LLD<-array(0, dim=c(23,9,2))
  MIS<-array(0, dim=c(23,9,2))
  for(i in 1:9){
#      i<-1
    tmp<-df |> filter(country_nr==i, FISHERY!="R", F_TYPE=="COMM")|> 
      group_by(sub_div2,YEAR,GEAR) |> 
      summarise(catch_tot=ifelse(numb_or_weight==1,
             round(sum(NUMB, na.rm = T),0),
             round(sum(WEIGHT, na.rm = T),0))
             )
    
    piv_catch<-pivot_wider(tmp, id_cols=c(sub_div2, YEAR), names_from=GEAR, values_from=catch_tot) |>  
      full_join(yrs) 
    
    GND[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                           arrange(YEAR)|>select(GND))
    GND[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                           arrange(YEAR)|> select(GND))
    if(i<8){
      LLD[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                             arrange(YEAR)|>select(LLD))
      LLD[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                             arrange(YEAR)|>select(LLD))
    }
    if(i!=3){
      FYK[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                             arrange(YEAR)|> select(FYK))
      FYK[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                             arrange(YEAR)|> select(FYK))
    }
    MIS[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                           arrange(YEAR)|>select(MIS))
    MIS[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                           arrange(YEAR)|> select(MIS))
    
  }
  
  
  
  # Recr, i.e. estimated offshore trolling catch, all countries
  ###############################################################################
  tmp<-df |> filter(FISHERY!="R", F_TYPE=="RECR")|> 
    group_by(sub_div2,YEAR,country_nr) |> 
    summarise(catch_tot=ifelse(numb_or_weight==1,
                               round(sum(NUMB, na.rm = T),0),
                               round(sum(WEIGHT, na.rm = T),0)))|>
    full_join(yrs)
  piv_catch<-pivot_wider(tmp, id_cols=c(sub_div2, YEAR), names_from=country_nr, values_from=catch_tot)
  #View(piv_catch)
  Recr<-array(0, dim=c(23,9,2))
  Recr[,1:8,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31") |> 
                            arrange(YEAR)|>select(order(colnames(piv_catch))) |> 
                            select(-sub_div2, -YEAR))
  Recr[,1:8,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32") |> 
                            arrange(YEAR)|>select(order(colnames(piv_catch))) |> 
                            select(-sub_div2, -YEAR))
  
  
  
  
  # River catches
  ###############################################################################
  tmp<-df |> filter(FISHERY=="R")|> 
    group_by(sub_div2,country_nr,YEAR) |> 
    summarise(catch_tot=ifelse(numb_or_weight==1,
                               round(sum(NUMB, na.rm = T),0),
                               round(sum(WEIGHT, na.rm = T),0))) |> 
    full_join(yrs)
  piv_catch<-pivot_wider(tmp, id_cols=c(sub_div2, YEAR), names_from=country_nr, values_from=catch_tot)
  #View(piv_catch)
  River<-array(0, dim=c(23,9,2))
  piv_catch2<-piv_catch |> ungroup() |> add_column(`3`=rep(0,46))|> add_column(`7`=rep(0,46))
  
  River[,1:9,1]<-as.matrix(piv_catch2 |> ungroup() |>  filter(sub_div2=="22-31") |> 
                             arrange(YEAR)|> 
                             select(order(colnames(piv_catch2))) |> 
                             select(-sub_div2, -YEAR))
  River[,1:9,2]<-as.matrix(piv_catch2 |> ungroup() |>  filter(sub_div2=="32") |> 
                             arrange(YEAR)|> 
                             select(order(colnames(piv_catch2))) |> 
                             select(-sub_div2, -YEAR))
  

  for(k in 1:2){
    for(i in 1:23){
      for(j in 1:9){
        River[i,j,k]<-rpl(River[i,j,k])
        Recr[i,j,k]<-rpl(Recr[i,j,k])
        GND[i,j,k]<-rpl(GND[i,j,k])
        LLD[i,j,k]<-rpl(LLD[i,j,k])
        FYK[i,j,k]<-rpl(FYK[i,j,k])
        MIS[i,j,k]<-rpl(MIS[i,j,k])
        
      }}
  }
  
 res<-list(River, Recr, GND, LLD, FYK, MIS)
return(res)
 }
  


# calculate seal damages and other discards per country in number or in weight
func_country_sealdam<-function(dfX, numb_or_weight){
  # numb_or_weight<-1
 # dfX<-df3
  # GND, LLD, FYK & MIS
  ###############################################################################
  SealGND<-array(0, dim=c(23,9,2))
  SealFYK<-array(0, dim=c(23,9,2))
  SealLLD<-array(0, dim=c(23,9,2))
  SealMIS<-array(0, dim=c(23,9,2))
  for(i in 1:9){
#          i<-5
    tmp<-dfX |> filter(country_nr==i)|> 
      group_by(sub_div2,YEAR,GEAR) |> 
      summarise(catch_tot=ifelse(numb_or_weight==1,
                                 round(sum(NUMB, na.rm = T),0),
                                 round(sum(WEIGHT, na.rm = T),0))
      )

    piv_catch<-pivot_wider(tmp, id_cols=c(sub_div2, YEAR), names_from=GEAR, values_from=catch_tot) |>  
      full_join(yrs)
    
    if("GND" %in% colnames(piv_catch)){
      SealGND[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                               arrange(YEAR)|>select(GND))
      SealGND[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                               arrange(YEAR) |> select(GND))
    }

    if("LLD" %in% colnames(piv_catch)){
      SealLLD[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                               arrange(YEAR)|>select(LLD))
      SealLLD[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                               arrange(YEAR)|>select(LLD))
    }
    if("FYK" %in% colnames(piv_catch)){
      SealFYK[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> 
                                 arrange(YEAR)|> select(FYK))
      SealFYK[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> 
                                 arrange(YEAR)|> select(FYK))
    }
    if("MIS" %in% colnames(piv_catch)){
      SealMIS[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|>
                               arrange(YEAR) |> select(MIS))
      SealMIS[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|>
                               arrange(YEAR) |> select(MIS))
    }

  }
  for(k in 1:2){
    for(i in 1:23){
      for(j in 1:9){
        SealGND[i,j,k]<-rpl(SealGND[i,j,k])
        SealLLD[i,j,k]<-rpl(SealLLD[i,j,k])
        SealFYK[i,j,k]<-rpl(SealFYK[i,j,k])
        SealMIS[i,j,k]<-rpl(SealMIS[i,j,k])
        
      }}
  }
  
  res<-list(SealGND, SealLLD, SealFYK, SealMIS)
  return(res)
}


# calculate other discards per country in number or in weight
func_country_discards<-function(dfX, numb_or_weight){
 #  numb_or_weight<-1
 #dfX<-df4
  # GND, LLD, FYK & MIS
  ###############################################################################
  Dis<-array(0, dim=c(23,9,2))
  for(i in 1:9){
  #   i<-2
  #   dfX |> group_by(country_nr, GEAR) |> summarise(n=n())
    tmp<-dfX |> filter(country_nr==i)|> 
      group_by(sub_div2,YEAR) |> 
      summarise(catch_tot=ifelse(numb_or_weight==1,
                                 round(sum(NUMB, na.rm = T),0),
                                 round(sum(WEIGHT, na.rm = T),0))
      )
    
  piv_catch<-full_join(tmp,yrs) 
    
    Dis[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31") |>  
                           arrange(YEAR) |> select(catch_tot))
    Dis[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|>  
                           arrange(YEAR) |> select(catch_tot))
  }

  for(k in 1:2){
    for(i in 1:23){
      for(j in 1:9){
        Dis[i,j,k]<-rpl(Dis[i,j,k])
      }}
    }
  
  return(Dis)
}

