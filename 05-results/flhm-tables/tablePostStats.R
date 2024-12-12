#   table of posterior riverstats
source("05-results/flhm-tables/functions.R")
#   getting chains

load("C:/Users/03195892/OneDrive - Valtion/WGBAST_shared/flhm/2024/output/CR_2024_selected_chain.Rdata")
#   result are stored in run named object
selCH = T

if(selCH){
  chains_r <- chains
  chains = chains_r[
                    seq(1, nrow(chains_r), length.out = 1000) %>% round
                    ,] 
}else{
  chains = run %>% as.mcmc() %>% window(thin = run$thin*4)
}
dim(chains)


SmoltW <- ch_ss(chains, "SmoltW[")


  rivnam <- c("Tornio", "Simo", "Kalix", "Råne", 
           "Pite", "Åby", "Byske", "Rickleån",
           "Sävarån", "Ume/Vindell", "Öre", "Lögde", "Ljungan", "Mörrumsån",
           "Emån", "Kåge", "Testeboån")
  
#   orfer of rivers in table
  tind <-list(
    c(2,1,3,4),#    AU1
    c(5,6,7,8,9,10,11,12,16),
    c(13, 17),
    c(15,14)
  ) 
#   names of pointestimated rivers
  pestnam <- c("Pärnu","Salaca","Vitrupe","Peterupe","Gauja","Daugava","Irbe",
  "Venta","Saka","Uzava","Barta","Nemunas river basin",
  
  "Kymijoki","Neva","Luga","Gladyshevka","Purtse","Kunda","Selja",
  "Loobu","Pirita","Vasalemma","Keila","Valgejõgi","Jägala","Vääna")

  #   reading pointestimates
    pest <- read_excel("05-results/flhm-tables/YoungFishDatabase/WGBAST_2024_Young_fish.xlsx",
                     range = c("C5:AP82"), sheet = "T4.3.1.")%>% 
    filter(river%in%pestnam)
  
  #   reading lithuainnia smolts
  
  litsm <- read_excel("05-results/flhm-tables/LithuSmolts/WGBAST_Lithuania_salmon_smolt_production_2000-2023.xlsx", 
                      range = "A2:B25", 
                      col_names = c("Year", "Smolts")) %>% 
    mutate( Smolts = Smolts/1000 )
  #   adding lithuania smolts to pest
  #   selecthing rivernames and years 2000: 2024
  #pest_m <- pest[,c(1,15:39)]
  pest_m <- pest %>%  
    select(c(1,15:40)) %>%  
    rbind(c("Nemunas river basin", litsm$Smolts, rep(NA,2))) 
  
  #   getting needed length
  #   years wanted are 2000 -> +2 future
  #   year 2000 is index 21 in timeseries
  
  #   from full life history model
  srs_m <- list()
  for(i in 1:17){
    ind <- paste(",",i, "]", sep = "")
    riv <- SmoltW %>% select(ends_with(ind))
    #   getting indexes
    #   if käge,start from 2008, index = 22
    if(i == 16){start=22}
    else{
    #   start from year 2000, index 14, first is 1986  
    start = 14
    }
    end <- ncol(riv)
    #   chopping
    riv_ss <- riv[,start:end]
    
    median_r <- riv_ss %>% map_dbl(quantile, 0.5)
    median <- median_r %>% round_m()
    #print("kissa")
    Q5  <- riv_ss %>% map_dbl(quantile, 0.05)
    Q95 <- riv_ss %>% map_dbl(quantile, 0.95)
    
    PI <- paste(round_m(Q5), "-", round_m(Q95), sep = "" )
    
    srs_m[[rivnam[i]]] <- data.frame(median, median_r, PI, Q5, Q95)
    
    
  }
  
  
  #   from yong fish database
  #   medians
  #pest_m
  yfdb <- list()
  for(i in 1:nrow(pest_m)){
    nam <- pest_m$river[i]
    
    median_r <- pest_m[i,] %>% select(-river) %>% as.numeric()
    median_r <- pest_m[i,2:ncol(pest_m)] %>% 
      t %>% 
      as.data.frame %>% 
      rownames_to_column(var = "Year") %>% 
      mutate(Year = as.numeric(Year))
    yTor <- data.frame(Year = 1999+(1:nrow(srs_m$Tornio)))
    temp = full_join(yTor, median_r)
    
    median_r = temp$V1 %>% as.numeric() 
    #while(length(median_r)< nrow(srs_m$Tornio)){
    #  median_r <- c(NA,median_r)
    #}
    median <- round_m(median_r)
    yfdb[[nam]] <- data.frame(median, median_r)
  }
  
  
  #   adding NA to kåge
  while(nrow(srs_m$Kåge) < nrow(srs_m$Tornio)){
    srs_m$Kåge <- rbind(rep(NA, 5), srs_m$Kåge)
  }
  
  #   summing rivers based on AUs (different from basic statistics)
  AUids <- list(
    c(2,1,3,4),#    AU1
    c(5,6,7,8,9,10,11,12,16), #   AU2
    c(13, 17),#   AU3
    c(15,14),#   AU4
    c(1,5,7,8,4,3,9,6,11,10,2,26), #   AU5 26 is the nemunar basin from antanas
    c(22,25,24,23,12,13,14,15,16,17,18,21,19,20)#   AU6
     
  )
  
  AUls <- list()
  for(i in 1:4){
    nam <- paste("AU", i)
    ids <- AUids[[i]]
    #   getting all into one data frame
    rivs <- srs_m[c(ids)] %>% reduce(data.frame)
    
    #   summing median
    median_r <- rivs %>% select(starts_with("median_r")) %>% 
      rowSums(na.rm=T)
    median <- median_r  %>% round_m()
    #   Q5
    Q5 <- rivs %>% select(starts_with("Q5")) %>% 
      rowSums(na.rm=T)
    #   Q95
    Q95 <- rivs %>% select(starts_with("Q95")) %>% 
      rowSums(na.rm=T)
    #   PI
    PI <- paste(round_m(Q5), "-", round_m(Q95), sep = "" )
    
    #   creating list of AUS
    
    AUls[[nam]] <- data.frame(median_r, median, PI, Q5, Q95)
    
  }
  
  for(i in 5:6){
    nam <- paste("AU", i)
    ids <- AUids[[i]]
    #   getting all into one data frame
    rivs <- yfdb[c(ids)] %>% reduce(data.frame) %>% modify(as.numeric)
    
    #   summing median
    median_r <- rivs %>% select(starts_with("median_r")) %>% 
      rowSums(na.rm=T) 
    median =  median_r %>% round_m()
    
    
    AUls[[nam]] <- data.frame(median_r, median, "Q5" = median, "Q95" = median)
  }
  
  
  #   calculating totals gulf_of_b, main_b, grand
  totnam <- c("gulf_of_b", "main_b", "grand")
  totind <- list(
    c(1,2,3),
    c(4,5),
    c(1,2,3,4,5)
  )
  totls <- list()
  for(i in 1:3){
    inds <- totind[[i]]
    AUs <- AUls[inds] %>% reduce(data.frame) %>% modify(as.numeric)
    
    median = AUs %>% select(starts_with("median_r")) %>% 
      rowSums(na.rm=T) %>% round_m()
    
    Q5 <- AUs %>% select(starts_with("Q5")) %>% 
      rowSums(na.rm=T)
    #   Q95
    Q95 <- AUs %>% select(starts_with("Q95")) %>% 
      rowSums(na.rm=T)
    #   PI
    PI <- paste(round_m(Q5), "-", round_m(Q95), sep = "" )
    
    totls[[totnam[i]]] <- data.frame(median, PI, Q5, Q95)
  }
  
  
  

  ###############################
  #     FORMING THE BIG TABLE   #
  ###############################
  nlist <- srs_m
  
  ltbl <- data.frame(
    
    #   2,1,3,4     northern rivers             AU1
    #   Simo
    nlist[[2]]$median,
    nlist[[2]]$PI,
    #   Tornio
    nlist[[1]]$median,
    nlist[[1]]$PI,
    #   Kalix
    nlist[[3]]$median,
    nlist[[3]]$PI,
    #   Råne
    nlist[[4]]$median,
    nlist[[4]]$PI,
    #   AU1
    AUls[[1]]$median,
    AUls[[1]]$PI,
    
    #   5,6,7,8,9,10,11,12,16   northern rivers   AU2
    #   Pite
    nlist[[5]]$median,
    nlist[[5]]$PI,
    #   Åby
    nlist[[6]]$median,
    nlist[[6]]$PI,
    #   Byske
    nlist[[7]]$median,
    nlist[[7]]$PI,
    #   Rickleån
    nlist[[8]]$median,
    nlist[[8]]$PI,
    #   Sävarån
    nlist[[9]]$median,
    nlist[[9]]$PI,
    #   Ume/Vindell
    nlist[[10]]$median,
    nlist[[10]]$PI,
    #   Öre
    nlist[[11]]$median,
    nlist[[11]]$PI,
    #   Lägde
    nlist[[12]]$median,
    nlist[[12]]$PI,
    #   Kåge
    nlist[[16]]$median,
    nlist[[16]]$PI,
    #   AU2
    AUls[[2]]$median,
    AUls[[2]]$PI,
    
    #   13, 17    3 is from southern rivers     AU3
    #   Ljungan
    nlist[[13]]$median,
    nlist[[13]]$PI,
    #   Testeboån
    nlist[[17]]$median,
    nlist[[17]]$PI,
    #   AU3
    AUls[[3]]$median,
    AUls[[3]]$PI,
    
    #   Total Gulf of B
    totls[[1]]$median,
    totls[[1]]$PI,
    
    
    #   15,14   from southern river
    #   Emån
    nlist[[15]]$median,
    nlist[[15]]$PI,
    #   Mörrumsån
    nlist[[14]]$median,
    nlist[[14]]$PI,
    #   AU4
    AUls[[4]]$median,
    AUls[[4]]$PI,
    
    #   1, 5, 7, 8, 4, 3, 9, 6, 11, 10, 2 from yfdb
    #   Pärnu
    yfdb[[1]]$median,
    #yfdb$Pärnu[["median"]],
    #   Salaca
    yfdb[[5]]$median,
    #yfdb$Salaca[["median"]],
    #   Vitrupe
    yfdb[[7]]$median,
    #   Peterupe
    yfdb[[8]]$median,
    #   Gauja
    yfdb[[4]]$median,
    #   Daugava
    yfdb[[3]]$median,
    #   Irbe
    yfdb[[9]]$median,
    #   Venta
    yfdb[[6]]$median,
    #   Saka
    yfdb[[11]]$median,
    #   Uzava
    yfdb[[10]]$median,
    #   Barta
    yfdb[[2]]$median,
    #   Nemunas river basin
    #rep(NA, length(yfdb[[2]]$median)),
    yfdb[[26]]$median,
    #   AU5
    AUls[[5]]$median,
    #rep(NA, length(yfdb[[2]]$median)),
    
    #   Total Main B
    totls[[2]]$median,
    totls[[2]]$PI,
    
    #   22, 25, 24, 23, 12, 13, 14, 15, 16, 17, 18, 21, 19, 20 yfdb
    #   Kymijoki
    yfdb[[22]]$median,
    #   Neva
    yfdb[[25]]$median,
    #   Luga
    yfdb[[24]]$median,
    #   Gladyshevka
    yfdb[[23]]$median,
    #   Purtse
    yfdb[[12]]$median,
    #   Kunda
    yfdb[[13]]$median,
    #   Selja
    yfdb[[14]]$median,
    #   Loobu
    yfdb[[15]]$median,
    #   Pirita
    yfdb[[16]]$median,
    #   Vaselemma
    yfdb[[17]]$median,
    #   Keila
    yfdb[[18]]$median,
    #   Valgejoki
    yfdb[[21]]$median,
    #   Jägala
    yfdb[[19]]$median,
    #   Vääna
    yfdb[[20]]$median,
    #   AU6
    AUls[[6]]$median,
    #rep(NA, length(yfdb[[2]]$median))
    
    #   Grand total
    totls[[3]]$median,
    totls[[3]]$PI
    
  )
  


  #   getting genereal info from 2021 table
  namecareinf <- read_excel("05-results/flhm-tables/T4_2_3_3(2021).xlsx", range = "B12:E106", 
                       col_names = c("Names", "Category", "Reprod.a", "PSPC")) %>% 
    mutate(id = 1:nrow(.)) %>% 
    drop_na(Names) %>% 
    filter(!startsWith(Names,"Assessment unit,")) %>% 
    filter(!startsWith(Names,"Finl")) %>% 
    filter(!startsWith(Names,"Swed")) %>% 
    filter(!startsWith(Names,"Lithu")) %>% 
    filter(!startsWith(Names,"Latvi")) %>% 
    filter(!startsWith(Names,"Esto")) %>%
    filter(!startsWith(Names,"Russi"))
    
    
  #   correcting latvian reprod areas
  la_rpr <- read_excel("05-results/flhm-tables/T4_2_3_3_LV.xlsx", range = "B67:E76",
                       col_names = c("Names", "Category", "Reprod.a", "PSPC")) %>% 
    mutate(
      Reprod.a = round_m(Reprod.a),
      PSPC = round_m(PSPC)
    )
  #   to get same names 
  la_rpr$Names <- namecareinf[46:55,]$Names
  
  #   manually updating values
  namecareinf[46:55,c(1,2,3,4)] <- la_rpr
  #   purtse pspc to 12.8
  namecareinf[64,4] <- "12.8"
  
  
  
  #   getting method of estimation
  estinf <- read_excel("05-results/flhm-tables/T4_2_3_3(2021).xlsx", range = "AD12:AE106", 
                       col_names = c("Pot.p", "Pres.p")) %>% 
    mutate(id = 1:nrow(.))
  #   joining to one
  tabinf <- left_join(namecareinf, estinf, by="id")
  

  #   getting updated pspc for flhm results
  #load("C:/Users/03195892/OneDrive - Valtion/WGBAST_shared/Ref pts/eqm_distns.RData")
  load("C:/Users/03195892/OneDrive - Valtion/WGBAST_shared/scen/2024/Ref pts/ref_pts_2024_JAGS_Mps.RData")
  load("C:/Users/03195892/OneDrive - Valtion/WGBAST_shared/scen/2024/Ref pts/eqm_distns_Mps.RData")
  pspc1_17 <- pspc_calc(R0_all, tind) %>% 
    tibble("Names" = namecareinf$Names[1:44], "PSPC"=.)
  
  #   getting rest of the PSPC from 2021 table and updating them
  #   
  #pspc18_e <- tabinf[45:nrow(tabinf), c(1,4)]
  pspc18_e <- tabinf[45:nrow(tabinf),] %>% 
    select(Names, PSPC) %>% 
    mutate(
      PSPC_r = asnum_parse(PSPC)
    )
  #   manually fixing  PSPC
  #   AU5 total, TotalmainB, AU6 total and grand total
  #   AU5 total
  pspc18_e[13,]$PSPC <- sum(pspc18_e$PSPC_r[1:12]) %>% as.character()
  #   Total main B
  pspc18_e[14,]$PSPC <- 
  sum(pspc1_17[43,]$PSPC %>% as.numeric(), pspc18_e[13,]$PSPC %>% as.numeric()) %>% 
    as.character()
  #   AU6 total
  pspc18_e[30,]$PSPC <-sum(pspc18_e[16:29,]$PSPC_r, 80,na.rm=T) %>% as.character()
  #   grand total
  pspc18_e[31,]$PSPC <- 
  sum(
    pspc1_17[37,]$PSPC %>% as.numeric(), # total gulf of b
    pspc18_e[14,]$PSPC %>% as.numeric(), # total main b
    pspc18_e[30,]$PSPC %>% as.numeric() # AU6 total
    
  ) %>% 
    as.character()
  #   combining pspc
  
  PSPC <- rbind(pspc1_17, pspc18_e[,1:2])
  
  names(ltbl) = tabinf$Names
  rownames(ltbl) = 2000:2026
  
  ftbl <- rbind("Category"=tabinf$Category, 
                "Reprod.a" = tabinf$Reprod.a,
                "PSPC" = PSPC$PSPC,
                ltbl[4:nrow(ltbl),] %>% replace(is.na(.),"na"), #    start from 2003
                "Pot.p" = tabinf$Pot.p, 
                "Pres.p" = tabinf$Pres.p)

library(openxlsx)

#write.xlsx(ftbl, "05-results/flhm-tables/T4_2_3_3V2(raw)2024.xlsx", showNA = F, rowNames = T)



  