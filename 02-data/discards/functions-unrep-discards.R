
# Replace NA's with 0's
rpl<-function(var){
  ifelse(is.na(var)==T, 0, var)
}



# calculate catch per country in CATCHer or in weight
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
    
    GND[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|>select(GND))
    GND[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> select(GND))
    if(i<8){
      LLD[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|>select(LLD))
      LLD[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|>select(LLD))
    }
    if(i!=3){
      FYK[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|> select(FYK))
      FYK[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> select(FYK))
    }
    MIS[,i,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31")|>select(MIS))
    MIS[,i,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32")|> select(MIS))
    
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
  Recr[,1:8,1]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="22-31") |>select(order(colnames(piv_catch))) |> 
                            select(-sub_div2, -YEAR))
  Recr[,1:8,2]<-as.matrix(piv_catch |> ungroup() |>  filter(sub_div2=="32") |>select(order(colnames(piv_catch))) |> 
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
                             select(order(colnames(piv_catch2))) |> 
                             select(-sub_div2, -YEAR))
  River[,1:9,2]<-as.matrix(piv_catch2 |> ungroup() |>  filter(sub_div2=="32") |> 
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
  