source("code/functions.R")
library("xlsx")
#   reading median an quantile information from results excel

#res <- readRDS("results/2023/runs/rfix2V2.rds")
res <- readRDS("results/2024/rNoLjunganAbyUpdtData_fix.rds")

#   adding ljungan from the swedish results to the 



chs <- ch_ss(res, "S[")
#   calculating wanted values river based, with -15% ume
rs <- river_stats(chs, rivers = 12)

#   because no ljunganm Kåge is 12 -> into 13
rownames(rs$river12) <- str_replace(rownames(rs$river12), "12]", "13]") 
rs$river13 <- rs$river12

#   Mörrumsån, Emån, Testeboån, Ljungan
smu_r <- read_excel("results/2024/swe/Southern river model results 2024_hierarchical MP0.xlsx", range = "G3:J43",
                    sheet = "Southern river model estimates") %>% as.data.frame

stau_r <- read_excel("results/2024/swe/Southern river model results 2024_hierarchical MP0.xlsx", range = "W3:Z43",
                     sheet = "Southern river model estimates") %>% as.data.frame

sq_r <- read_excel("results/2024/swe/Southern river model results 2024_hierarchical MP0.xlsx", range = "O3:V43",
                     sheet = "Southern river model estimates") %>% as.data.frame

#   deleting first 2 rows and repeating last one twice

smu_m <- rbind(smu_r, rep(NA,ncol(smu_r)), rep(NA,ncol(smu_r)))[-2:-1,] 
stau_m <- rbind(stau_r, rep(NA,ncol(stau_r)), rep(NA,ncol(stau_r)))[-2:-1,] 
sq_m <- rbind(sq_r, rep(NA,ncol(sq_r)), rep(NA,ncol(sq_r)))[-2:-1,] 

#   adding ljungan mu and tau to nstats so the FLHM_input can be used
rs$river12 <- data.frame(
  median = exp(smu_m[,4]),
  qt0.05 = sq_m[,7],
  qt0.95 = sq_m[,8],
  logmedian = smu_m[,4],
  tau = stau_m[,4]
)


#   removing ljungan from smu and stau
smu <- smu_m[,-4]
stau <- stau_m[,-4]
sqt = sq_m[,-8:-7]


round_m <- function(vec, lim=3){
  rvec <- c()
  for(x in vec){
    #print(x)
  if(is.na(x)){rvec <- c(rvec, NA)}
  else if(abs(x) <= lim){
    r <- round(x, 1)
    #print(r)
    rvec <- c(rvec, paste(sprintf("%.1f", r)))
    
  }
  else{
    r <- round(x)
    #print(r)
    rvec <- c(rvec, paste(r))
    }
  }
  return(rvec)
}

#round_m(ver)

#ver <- rnorm(10, 0, 10)


#round(ver[1])

tblrmStats <- function(nstats, smedian, sqt, yrs, summ = F, save = F){
  
  #   RIVER ORDERS USED BY SCRIPT
  #   NORTHERN
  #   Tornio,Kalix,Simo,Råne,Åby,Byske,Ume/Vindel,Rickleån,Sävarån,Öre,Lögde,Ljungan,Kåge
  #   SOUTHERN
  #   Mörrumsån,Emån,Testeboån
  nnam <- c("Tornio","Kalix","Simo","Råne","Åby","Byske","Ume/Vindel","Rickleån","Sävarån","Öre","Lögde","Ljungan","Kåge")
  snam <- c("Mörrumsån","Emån","Testeboån")
  #   getting needed length
  n = length(yrs)
  
  
  nlist <- list()
  slist <- list()
  #   getting median and qt005, qt095 from nothern rivers
  for(i in 1:13){
    len <- nrow(nstats[[i]])
    med_c <- nstats[[i]]$median; med_c <- c(rep(NA, n-len), med_c)
    med <- med_c%>% round_m()
    Q5 <- nstats[[i]]$qt0.05; Q5 <- c(rep(NA, n-len), Q5)%>% round()
    Q95 <- nstats[[i]]$qt0.95; Q95 <- c(rep(NA, n-len), Q95)%>% round()
    QI <- paste(Q5, "-", Q95, sep = "")
    #   saving to list 
    nlist[[i]] <- data.frame(med, med_c, Q5, Q95, QI)
  }
  #   same for soutgern rivers
  for(i in 1:ncol(smedian)){
    len = nrow(smedian)
    med_c <- smedian[,i]; med_c <- c(rep(NA, n-len), med_c)
    med <- med_c%>% round_m()
    j = i*2-1
    Q5 = sqt[,j]; Q5 <- c(rep(NA, n-len), Q5)%>% round()
    Q95 = sqt[,j+1]; Q95 <- c(rep(NA, n-len), Q95)%>% round()
    QI <- paste(Q5, "-", Q95, sep = "")
    #   saving to list 
    slist[[i]] <- data.frame(med, med_c, Q5, Q95, QI)
  }  
  
  #   summing rivers based on AUs (different from basic statistics)
  AUids <- list(
    c(1,3,2,4),#    northern rivers             AU1
    c(5,6,13,8,9,7,10,11),#   northern rivers   AU2
    c(12, 3),#    3 is from southern rivers     AU3
    c(1,2)#   from southern rivers              AU4
  )
  #return(nlist)
  
  #   summs
  #   AU1
  AU1 <- data.frame(
    med = round_m(nlist[[1]]$med_c+nlist[[3]]$med_c+nlist[[2]]$med_c+nlist[[4]]$med_c),
    Q5 = round(nlist[[1]]$Q5+nlist[[3]]$Q5+nlist[[2]]$Q5+nlist[[4]]$Q5),
    Q95 = round(nlist[[1]]$Q95+nlist[[3]]$Q95+nlist[[2]]$Q95+nlist[[4]]$Q95)
    #QI <- paste(Q5, "-", Q95, sep = "")
      ) %>% 
    mutate(
      QI <- paste(Q5, "-", Q95, sep = "")
    )
  
  #   AU2
  kåge <-nlist[[13]][,1:4]
  kåge[which(is.na(kåge), arr.ind=T)] = 0
  
  
  AU2 <- data.frame(
    med = round_m(nlist[[5]]$med_c+nlist[[6]]$med_c+kåge$med_c+nlist[[8]]$med_c+nlist[[9]]$med_c+
          nlist[[7]]$med_c+nlist[[10]]$med_c+nlist[[11]]$med_c),
        
    Q5 = round(nlist[[5]]$Q5+nlist[[6]]$Q5+kåge$Q5+nlist[[8]]$Q5+nlist[[9]]$Q5+
      nlist[[7]]$Q5+nlist[[10]]$Q5+nlist[[11]]$Q5),
    
    Q95 = round(nlist[[5]]$Q95+nlist[[6]]$Q95+kåge$Q95+nlist[[8]]$Q95+nlist[[9]]$Q95+
      nlist[[7]]$Q95+nlist[[10]]$Q95+nlist[[11]]$Q95)
    
    #QI <- paste(Q5, "-", Q95, sep = "")
  ) %>% 
    mutate(
      QI <- paste(Q5, "-", Q95, sep = "")
    )
  
  #   AU3
  #   setting testeboån NA to 0
  #return(slist)
  #nlist[[12]] <- x[is.na(x)] <- 0
  slist[[3]][is.na(slist[[3]])]<-0
  
  AU3 <- data.frame(
    med = round_m(nlist[[12]]$med_c+slist[[3]]$med_c),
    Q5 = round(nlist[[12]]$Q5+slist[[3]]$Q5),
    Q95 = round(nlist[[12]]$Q95+slist[[3]]$Q95)
    #QI <- paste(Q5, "-", Q95, sep = "")
  ) %>%
    mutate(
      QI <- paste(Q5, "-", Q95, sep = "")
  )
  
  #   AU4
  AU4 <- data.frame(
    med = round_m(slist[[1]]$med_c+slist[[2]]$med_c),
    Q5 = round(slist[[1]]$Q5+slist[[2]]$Q5),
    Q95 = round(slist[[1]]$Q95+slist[[2]]$Q95)
    #QI <- paste(Q5, "-", Q95, sep = "")
  ) %>% 
    mutate(
      QI <- paste(Q5, "-", Q95, sep = "")
    )
    
  #return(nlist)
  ###############################
  #     FORMING THE BIG TABLE   #
  ###############################
  
  ltbl <- data.frame(
    
    #   1,3,2,4     northern rivers             AU1
    #   Tornio
    nlist[[1]]$med,
    nlist[[1]]$QI,
    #   Simo
    nlist[[3]]$med,
    nlist[[3]]$QI,
    #   Kalix
    nlist[[2]]$med,
    nlist[[2]]$QI,
    #   Råne
    nlist[[4]]$med,
    nlist[[4]]$QI,
    #   AU1
    AU1$med,
    AU1$QI,
    
    #   5,6,13,8,9,7,10,11   northern rivers   AU2
    #   Pite
    rep(NA, n),
    rep("-", n),
    #   Åby
    nlist[[5]]$med,
    nlist[[5]]$QI,
    #   Byske
    nlist[[6]]$med,
    nlist[[6]]$QI,
    #   Kåge
    nlist[[13]]$med,
    nlist[[13]]$QI,
    #   Rickleån
    nlist[[8]]$med,
    nlist[[8]]$QI,
    #   Sävarån
    nlist[[9]]$med,
    nlist[[9]]$QI,
    #   Umee
    nlist[[7]]$med,
    nlist[[7]]$QI,
    #   Öre
    nlist[[10]]$med,
    nlist[[10]]$QI,
    #   Lögde
    nlist[[11]]$med,
    nlist[[11]]$QI,
    #   AU2
    AU2$med,
    AU2$QI,
    
    #   12, 3    3 is from southern rivers     AU3
    #   Ljungan
    nlist[[12]]$med,
    nlist[[12]]$QI,
    #   Testeboån
    slist[[3]]$med,
    slist[[3]]$QI,
    #   AU3
    AU3$med,
    AU3$QI,
    
    #   1,2   from southern river
    #   Emån
    slist[[1]]$med,
    slist[[1]]$QI,
    #   Mörrumsån
    slist[[2]]$med,
    slist[[2]]$QI,
    #   AU3
    AU4$med,
    AU4$QI
    
  )
  
  #   creating colnames 
  rnames <- c(nnam[AUids[[1]]], "AU1", 
          "Pite", nnam[AUids[[2]]], "AU2", 
          nnam[AUids[[3]][1]], snam[AUids[[3]][2]], "AU3", 
          snam[AUids[[4]]], "AU4")
  
  frow <- rep(NA, 2*length(rnames))
  frow[seq(1, length(frow), by = 2)] = rnames
  frow[seq(2, length(frow), by = 2)] = "90% PI"
 
  names(ltbl) = frow
  rownames(ltbl) = yrs

  #   saving table if wanted so
  if(save == T){
    library(openxlsx)
    date <- format(Sys.time(), "%Y")
    #write.table(t(ltbl), sep = ",", file = paste0("au", date, ".csv"), col.names = F)
    write.xlsx(ltbl, paste("T_4_2_2_1", date, ".xlsx"))
    
  }
  
  
  return(ltbl)
  
  
}

date
format(Sys.time(), "%Y")

k <- tblrmStats(rs, 
          read.table("results/2023/southern/medians.txt", header = T),
          read.table("results/2023/southern/quantiles.txt", header = T),
          1987:2025
          )






#   southern starts from 1985!!!!!!!!
#   slice 2 rows and add 2 NA rows 
smed_r <- read.table("results/2023/southern/medians.txt", header = T)
sqt_r <- read.table("results/2023/southern/quantiles.txt", header = T)

smed <- smed_r %>% slice(c(-1,-2)) %>% rbind(rep(NA, 3))%>% rbind(rep(NA, 3))
sqt <- sqt_r %>% slice(c(-1,-2)) %>% rbind(rep(NA, 6))%>% rbind(rep(NA, 6))

tb <- kissa[[3]]
tb[is.na(tb)] <- 0
kissa[[3]][is.na(kissa[[3]])]<-0

kissa <- tblrmStats(rs, 
           exp(smu),
           sqt,
           1987:2026)
           


tblrmStats(rs, 
           exp(smu),
           sqt,
           1987:2026,
           save =T)
