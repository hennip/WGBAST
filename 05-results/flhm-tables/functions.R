#   FUNCTIONS 
library(runjags);library(coda);library(rjags);library(stringr);library(tidyverse)
library(qpcR);library(readxl);library(nimble);library(conflicted)
conflict_prefer("select", "dplyr");conflict_prefer("filter", "dplyr")
#   reading data from untransdormed excel
#   WGDATA_river model input -2021.xlsx
#   reading data strigt from excel in R

WGdataRead <- function(data2, rivers = 13){
  
  minput <- data2
  # getting highest and lowest value for a year
  y_max <- max(minput[,1], na.rm = T)
  y_min <- min(minput[,1], na.rm = T)
  y_maxl <- which(minput[,1] == y_max)
  y_minl <- which(minput[,1] == y_min)
  #   dropping year from minput (first column)
  minput <- minput[,-1]
  #   getting columnnames
  hder <- minput[1:y_minl[1],]
  hder <- hder[which(startsWith(hder[,1], "IS")),]
  cn <- str_remove_all(hder[!is.na(hder)], "[\\.\\]\\[]")
  cn <- str_replace_all(cn, ",", "_")
  #   turning to numeric and splitting the excel
  minput <- apply(minput, 2, as.numeric)
  data_r <- minput[y_minl[1]:y_maxl[1],]
  colnames(data_r) <- cn
  #   data r is the rawdata, which needs to be transformed 
  return(data_r)
}

#   this does the required transformations for rawdata
WGdataTrans <- function(data_r, rivers = 13){
  ip <- c()
  cn <- colnames(data_r)
  
  for( i in 1:rivers){
    #   separate columns with same ending
    endin <- paste0("_", i)
    rivdat <- data_r[, which(endsWith(colnames(data_r), endin))]
    n_i <- rivdat[,ncol(rivdat)]
    cols <- rivdat[,1:ncol(rivdat)-1]
    #   adding transfoirmed colums to rvr table  
    rvr <- c()
    for(j in 1:ncol(cols)){
      if(j == 1){
        #   for first column log 
        newcol <- log( cols[,j] * ( cols[,j+1]^2 + 1) )
      }
      else if( j == 2){
        #   second column stays the same
        newcol <- cols[,j]
      }
      else{
        #   rest are multiplied
        newcol <- cols[,j]*n_i*5
      }
      rvr <- cbind(rvr, newcol)
    }
    rvr <- cbind(rvr, n_i)
    ip <- cbind(ip, rvr)
    
  }
  colnames(ip) <- cn
  #   this is the transformed dataframe
  data_t <- as.data.frame(ip)
  
  return(data_t)
  
}


#   combining original data1 and just made data_t (previous data2)
#   into one data suitable for jags
WGdataJAGS <- function(data_t, data1){
  cn <- colnames(data_t)
  #   preparing data_t for list format
  IS   <- as.matrix(data_t[,startsWith(cn, "IS")])
  CIS  <- as.matrix(data_t[,startsWith(cn, "CIS")])
  IP2  <- as.matrix(data_t[,startsWith(cn, "IP2")])
  IP1  <- as.matrix(data_t[,startsWith(cn, "IP1")])
  IOP1 <- as.matrix(data_t[,startsWith(cn, "IOP1")])
  IP0  <- as.matrix(data_t[,startsWith(cn, "IP0")])
  n    <- as.matrix(data_t[,startsWith(cn, "n")])
  #   creating list from data_t
  data_l <- list(IS = IS, CIS = CIS, IP2 = IP2, IP1 = IP1, IOP1 = IOP1, IP0 = IP0, n = n)
  #   combining data1 and data_l to one data suitable to be used in jags
  data <- append(data1, data_l)
  return(data)
  
}



#   function for getting stats from mcmc chains
resStats <- function(result, rivers = 13){
  chains <- as.data.frame(as.mcmc(result))
  
  chains_l <- list()
  for( i in 1:rivers){
    if(i == 13){
      sel <- 29:(length(chains[i,])/rivers)
    }
    else{sel <- 8:(length(chains[i,])/rivers)}
    
    nam <- paste0("river", i)
    chains_l[[paste0(nam)]] <- chains[, endsWith(colnames(chains), paste0(",", i, "]"))][sel]
    
  }
  
  riv_stats <- list()
  for(i in 1:rivers){
    nam <- paste0("river", i)
    if(i == 7){m = 0.85}else{m = 1}
    
    mean   <- apply(chains_l[[i]], 2, mean)*m
    sd     <- apply(chains_l[[i]], 2, sd)*m
    median <- apply(chains_l[[i]], 2, median)*m
    qt0.05 <- apply(chains_l[[i]], 2, quantile, probs =c(0.05))*m
    qt0.95 <- apply(chains_l[[i]], 2, quantile, probs =c(0.95))*m
    cv <- sd/mean
    
    logmedian <- log(median)
    tau <- 1/(log(cv^2+1))
    
    
    riv_stats[[nam]]$mean   <- mean
    riv_stats[[nam]]$sd     <- sd
    riv_stats[[nam]]$median <- median
    riv_stats[[nam]]$qt0.05 <- qt0.05
    riv_stats[[nam]]$qt0.95 <- qt0.95
    riv_stats[[nam]]$cv     <- cv
    riv_stats[[nam]]$logmed <- logmedian
    riv_stats[[nam]]$tau    <- tau
    
  }
  return(riv_stats)
}

resStats2 <- function(result, rivers = 13, nimb =F){
  chains <- as.data.frame(as.mcmc(result))
  #chains <- result
  chains_l <- list()
  for( i in 1:rivers){
    if(i == rivers){
      sel <- 29:(length(chains[i,])/rivers)
    }
    else{sel <- 8:(length(chains[i,])/rivers)}
    
    nam <- paste0("river", i)
    if(nimb == T){
      chains_l[[paste0(nam)]] <- chains[, endsWith(colnames(chains), paste0(", ", i, "]"))][sel]
    }
    else{
      chains_l[[paste0(nam)]] <- chains[, endsWith(colnames(chains), paste0(",", i, "]"))][sel]  
    }
    
    
  }
  
  riv_stats <- list()
  for(i in 1:rivers){
    nam <- paste0("river", i)
    if(i == 7){m = 0.85}else{m = 1}
    #if(i == 7){m = 1}else{m = 1}
    mean   <- apply(chains_l[[i]], 2, mean)*m
    sd     <- apply(chains_l[[i]], 2, sd)*m
    median <- apply(chains_l[[i]], 2, median)*m
    qt0.05 <- apply(chains_l[[i]], 2, quantile, probs =c(0.05))*m
    qt0.95 <- apply(chains_l[[i]], 2, quantile, probs =c(0.95))*m
    cv <- sd/mean
    
    logmedian <- log(median)
    tau <- 1/(log(cv^2+1))
    
    df <- data.frame(mean, sd, median, qt0.05, qt0.95, cv, logmedian, tau)
    riv_stats[[nam]] <- df
    
  }
  return(riv_stats)
}


resEX <- function(stats, rivers = 13){
  res_df <- c()
  nrsdf <- c()
  for(i in 1:rivers){
    mu <- stats[[i]]$logmed
    if(length(mu) != length(stats[[1]]$mean)){
      mu <- c(rep(NA, length(stats[[1]]$mean) - length(mu)), mu)
    }
    tau <- stats[[i]]$tau
    if(length(tau) != length(stats[[1]]$mean)){
      tau <- c(rep(NA, length(stats[[1]]$mean) - length(tau)), tau)
    }
    res_df <- cbind(res_df, mu)
    res_df <- cbind(res_df, tau)
    
    nmu <- paste0("mu_SmoltW[,", i, "]")
    ntau <- paste0("tau_SmoltW[,", i, "]")
    nrsdf <- c(nrsdf, nmu, ntau)
    
  }
  colnames(res_df) <- nrsdf
  rownames(res_df) <- NULL
  res_df <- as.data.frame(res_df)
  return(res_df)
}

resEX_m <- function(stats, rivers = 17){
  res_df <- c()
  nrsdf <- c()
  ind <- c(1,3,2,4,NA,5,6,8,9,7,10,11,12,NA,NA,13,NA)
  
  for(i in 1:rivers){
    j = 0
    if(i != c(5,14,15,17)){
      j = ind[i]
    
      mu <- stats[[j]]$logmed
      if(length(mu) != length(stats[[1]]$mean)){
        mu <- c(rep(NA, length(stats[[1]]$mean) - length(mu)), mu)
      }
      tau <- stats[[j]]$tau
      if(length(tau) != length(stats[[1]]$mean)){
        tau <- c(rep(NA, length(stats[[1]]$mean) - length(tau)), tau)
      }
    }
    else{
      mu <- rep(NA, length(stats[[1]]$mean))
      tau <- rep(NA, length(stats[[1]]$mean))
    }
    res_df <- cbind(res_df, mu)
    res_df <- cbind(res_df, tau)
    
    nmu <- paste0("mu_SmoltW[,", i, "]")
    ntau <- paste0("tau_SmoltW[,", i, "]")
    nrsdf <- c(nrsdf, nmu, ntau)
    
  }
  colnames(res_df) <- nrsdf
  rownames(res_df) <- NULL
  res_df <- as.data.frame(res_df)
  return(res_df)
}


#   Function for plotting rivermodel results

plotRM <- function(res, yrs = 1980:2024, 
                   rivernam = c("Tornio", "Kalix", "Simo", "Råne", "Åby", "Byske", "Ume/Vindel", 
                                                       "Rickleån", "Sävarån", "Öre", "Lögde", "Ljungan", "Kåge"),
                   colors = c("Median" = "blue", "95%" = "black", "5%" = "black"),
                   save =F)
{
  piclist <- list()
  for(i in 1:length(res)){
    #sel <-  -(length(yrs)-lengths(res[[i]])[1]):-1
    sel <-  -(length(yrs)-nrow(res[[i]])):-1
    jdata <- as.data.frame(res[[i]])
    
    data <- as.data.frame(cbind(jdata$median, jdata$qt0.05, jdata$qt0.95, yrs[sel]))
    colnames(data) <- c("jmedian","j05", "j95", "yrs")
    #print(jdata)
    riverp <- ggplot(data = data , aes(x = yrs)) + 
      #   name for plot
      ggtitle(rivernam[i]) +
      #   adding jags results
      geom_line(aes(y = jmedian, color = "Median"), size = 1.2) +
      geom_line(aes(y = j05, color = "5%"), size = 1.2, linetype =2) +
      geom_line(aes(y = j95, color = "95%"), size = 1.2, linetype =2) 
    
    riverp <- riverp +
      #   graphical tweaking
      theme(axis.text = element_text(angle = 90)) +
      scale_y_continuous(n.breaks = 5) +
      scale_x_continuous(n.breaks = 10) +
      labs(x = "Years", y = "Smolts (thousands)", color = "Legend") +
      scale_color_manual(values = colors)
    
    if(save){
      piclist[[i]] <- riverp
    }
    else{
      plot(riverp)
    }
  }
  if(save){return(piclist)}
  
}

plotAU <- function(res, yrs = 1980:2025, 
                   rivernam = c("AU1", "AU2", "AU3"),
                   colors = c("Median" = "blue", "95%" = "black", "5%" = "black"),
                   save=F)
{
  piclist <- list()
  
  for(i in 1:length(res)){
    #sel <-  -(length(yrs)-lengths(res[[i]])[1]):-1
    #   deciding which rivers to drop
    sel <-  -(length(yrs)-nrow(res[[i]])):-1
    jdata <- as.data.frame(res[[i]])
    
    data <- as.data.frame(cbind(jdata$median, jdata$qt0.05, jdata$qt0.95, yrs[sel]))
    colnames(data) <- c("jmedian","j05", "j95", "yrs")
    #print(jdata)
    riverp <- ggplot(data = data , aes(x = yrs)) + 
      #   name for plot
      ggtitle(rivernam[i]) +
      #   adding jags results
      geom_line(aes(y = jmedian, color = "Median"), size = 1.2) +
      geom_line(aes(y = j05, color = "5%"), size = 1.2, linetype =2) +
      geom_line(aes(y = j95, color = "95%"), size = 1.2, linetype =2) 
    
    riverp <- riverp +
      #   graphical tweaking
      theme(axis.text = element_text(angle = 90)) +
      scale_y_continuous(n.breaks = 5) +
      scale_x_continuous(n.breaks = 10) +
      labs(x = "Years", y = "Smolts (thousands)", color = "Legend") +
      scale_color_manual(values = colors)
    
    if(save){
      piclist[[i]] <- riverp
    }
    else{
      plot(riverp)
    }
  }
  if(save){return(piclist)}
  
}

plotTOT <- function(res, yrs = 1980:2025, 
                   rivernam = c("Total"),
                   colors = c("Median" = "blue", "95%" = "black", "5%" = "black"))
{
  for(i in 1:length(res)){
    sel <-  -(length(yrs)-lengths(res[[i]])[1]):-1
    
    jdata <- as.data.frame(res[[i]])
    
    data <- as.data.frame(cbind(jdata$median, jdata$qt05, jdata$qt95, yrs[sel]))
    colnames(data) <- c("jmedian","j05", "j95", "yrs")
    #print(jdata)
    riverp <- ggplot(data = data , aes(x = yrs)) + 
      #   name for plot
      ggtitle(rivernam[i]) +
      #   adding jags results
      geom_line(aes(y = jmedian, color = "Median"), size = 1.2) +
      geom_line(aes(y = j05, color = "5%"), size = 1.2, linetype =2) +
      geom_line(aes(y = j95, color = "95%"), size = 1.2, linetype =2) 
    
    riverp <- riverp +
      #   graphical tweaking
      theme(axis.text = element_text(angle = 90)) +
      scale_y_continuous(n.breaks = 5) +
      scale_x_continuous(n.breaks = 10) +
      labs(x = "Years", y = "Smolts (thousands)", color = "Legend") +
      scale_color_manual(values = colors)
    
    plot(riverp)
  }
  
}

#   Function for comparing different rivermodel results

plotRM_comp <- function(res, res2, yrs = 1980:2024, 
                        rivernam = c("Tornio", "Kalix", "Simo", "Råne", "Åby", "Byske", "Ume/Vindel", 
                                     "Rickleån", "Sävarån", "Öre", "Lögde", "Ljungan", "Kåge")
                        , list = F, colors=c("With Öre smolt count median" = "black", "With Öre smolt count 0.95" = "gold", "With Öre smolt count 0.05" = "gold",
                                                "Without Öre smolt count median" = "red", "Without Öre smolt count 0.05" = "blue", "Without Öre smolt count 0.95" = "blue"))
{
  
  #colors <- c("Orig_data median" = "black", "Orig_data 0.95" = "gold", "Orig_data 0.05" = "gold",
  #            "Alt_data median" = "red", "Alt_data 0.05" = "blue", "Alt_data 0.95" = "blue")
  
  #colors <- c("BUGS (2022) median" = "black", "BUGS (2022) 0.95" = "gold", "BUGS (2022) 0.05" = "gold",
  #            "JAGS (2023) median" = "red", "JAGS (2023) 0.05" = "blue", "JAGS (2023) 0.95" = "blue")
  
   colors <- c("Orig data median" = "black", "Orig data 0.95" = "gold", "Orig data 0.05" = "gold",
               "Updt data median" = "red", "Updt data 0.05" = "blue", "Updt data 0.95" = "blue")
  
  pl <- list()
  for(i in 1:length(res)){
    #sel <-  -(length(yrs)-lengths(res[[i]])[1]):-1
    sel <-  -(length(yrs)-nrow(res[[i]])):-1
    odata <- as.data.frame(res[[i]])
    adata <- as.data.frame(res2[[i]])
    
    data <- as.data.frame(cbind(odata$median, odata$qt0.05, odata$qt0.95,
                                adata$median, adata$qt0.05, adata$qt0.95, yrs[sel]))
    colnames(data) <- c("omedian","o05", "o95",
                        "amedian","a05", "a95","yrs")
    
    riverp <- ggplot(data = data , aes(x = yrs)) + 
      #   name for plot
      ggtitle(rivernam[i]) +
      #   adding original results
      geom_line(aes(y = omedian, color = "Orig data median"), size = 1) +
      geom_line(aes(y = o05, color = "Orig data 0.05"), size = 1, linetype =1) +
      geom_line(aes(y = o95, color = "Orig data 0.95"), size = 1, linetype =1) +
      #   adding altered results
      geom_line(aes(y = amedian, color = "Updt data median"), size = 1, linetype =2) +
      geom_line(aes(y = a05, color = "Updt data 0.05"), size = 1, linetype =2) +
      geom_line(aes(y = a95, color = "Updt data 0.95"), size = 1, linetype =2) 
    
    
    riverp <- riverp +
      #   graphical tweaking
      theme(axis.text = element_text(angle = 90)) +
      scale_y_continuous(n.breaks = 5) +
      scale_x_continuous(n.breaks = 10) +
      labs(x = "Years", y = "Smolts (thousands)", color = "Legend") +
      scale_color_manual(values = colors)
    
    
    if(list == T){pl[[rivernam[i]]] = riverp}
    else{plot(riverp)}
  }
  if(list == T){return(pl)}
  
}

#   function to transfrom stats into format used in  WGBAST 2020
#   Annex 5: Smolts and PSPC per AU for HELCOM salmon indicator

tblStats <- function(stats, oneline = F, summ = F){
  rivers <- length(stats)
  riv_stats <- stats
  ltbl <- list()
  tbl <- c()
  medS <- Q5S <- Q95S <- rep(0, length(riv_stats[[1]]$median))
  
  for(i in 1:rivers){
    tbl <- c()
    med <- round(riv_stats[[i]]$median)
    Q5 <- round(riv_stats[[i]]$qt0.05)
    Q95 <- round(riv_stats[[i]]$qt0.95)
    if(oneline == F){
      tbl <- rbind(med, Q5)
      tbl <- rbind(tbl, Q95)
    }
    else{
    ci <- paste0(Q5 , "-", Q95)
    tbl <- rbind(tbl, med)
    tbl <- rbind(tbl, ci)
    }
    
    medS <- colSums(rbind(medS, med), na.rm =T)
    Q5S <- colSums(rbind(Q5S, Q5), na.rm =T)
    Q95S <- colSums(rbind(Q95S, Q95), na.rm=T)
    rownames(tbl) <- NULL
    ltbl[[paste0("river", i)]] <- tbl
  }
  
  if(summ == T){
    ciS <- paste0(Q5S , "-", Q95S)
    ltbl[["Total"]] <- rbind(medS, ciS)
  }
 

  
  return(ltbl)
  
}


tblStats2 <- function(stats, oneline = F, summ = F, save = F){
  rivers <- length(stats)
  riv_stats <- stats
  mlen <- max(sapply(stats, nrow))
  medS <- Q5S <- Q95S <- rep(0, mlen)
  
  
  for(i in 1:rivers){
    len <- length(riv_stats[[i]]$median)
    med <- riv_stats[[i]]$median; med <- c(rep(NA, mlen-len), med)
    Q5 <- riv_stats[[i]]$qt0.05; Q5 <- c(rep(NA, mlen-len), Q5)
    Q95 <- riv_stats[[i]]$qt0.95; Q95 <- c(rep(NA, mlen-len), Q95)
    if(oneline == F){
      tbl <- data.frame("Median" = round(med), "Q5" = round(Q5), "Q95" = round(Q95))
      
    }
    else{
      ci <- paste0(round(Q5) , "-", round(Q95))
      tbl <- data.frame("Median" = round(med), "CI" = ci)
      
    }
    
    if(summ == T){
      medS <- round(colSums(rbind(medS, med), na.rm =T))
      Q5S <- round(colSums(rbind(Q5S, Q5), na.rm =T))
      Q95S <- round(colSums(rbind(Q95S, Q95), na.rm=T))
    }

      if(i == 1){ltbl <- tbl}
    
     else{ltbl <- data.frame(ltbl,tbl)}
  }
  
  if(summ == T){
    ciS <- paste0(Q5S , "-", Q95S)
    Total = data.frame("Tot.median" = medS, "Tot.ci" = ciS)
    ltbl <- data.frame(ltbl,Total)
  }
  
  
  if(save == T){
    date <- format(Sys.time(), "%H%M%S")
    write.table(t(ltbl), sep = ",", file = paste0("au", date, ".csv"), col.names = F)
    
  }
  return(ltbl)
  
  
}

#   function for addition with na.rm argument

addit <- function(objlist, na.rm = F){
  n <- length(objlist)
  for(i in 1:n){
    obj <- objlist[[i]]
    if(na.rm==T){
      obj[is.na(obj)] <- 0
    }
    if(i == 1){sum <- obj}
    else{sum <- sum+obj}
  }
  sum <- na_if(sum, 0)
  return(sum)
}


#   function for running nimble with simulations etc

rnimb <- function(code, constants = list(), data = list(), monitors,
                  niter = 10000, nburnin = 2000, thin = 1,
                  summ = T, simdat = F , ab =F, ad = T){
  
  model <- nimbleModel(code, constants = constants, data = data)
  model$simulate(includeData = simdat)
  
  compmodel <- compileNimble(model)
  #confmodel <- configureMCMC(compmodel, autoBlock = ab, control = list(adaptive = F))
  confmodel <- configureMCMC(compmodel, autoBlock = ab, control = list(adaptive = ad))
  
  confmodel$setMonitors(monitors)
  
  mcmcconfmodel <- buildMCMC(confmodel)
  cmodelmcmc <- compileNimble(mcmcconfmodel)
  
  samples <- runMCMC(cmodelmcmc, niter = niter, nburnin = nburnin, 
                     thin = thin, nchains = 1, summary =summ, 
                     samplesAsCodaMCMC = T) 
  
  return(samples)
  
  
  
}

sumcomp <- function(res1, res2, q, ind = T){
  if(ind){rown <- paste0(q, "\\[")}
  else{rown <- paste0(q, "$")}
  sum1_r <- summary(as.mcmc(res1))
  sum2_r <- summary(as.mcmc(res2))
  #print("summaryt")
  
  sum1 <- as.data.frame(sum1_r$statistics)
  sum2 <- as.data.frame(sum2_r$statistics)
  #print("dataframet")
  
  
  ss1 <- filter(sum1, grepl(rown, rownames(sum1)))
  ss2 <- filter(sum2, grepl(rown, rownames(sum2)))
  #print("filtterit")
  
  print(cbind(ss1$Mean, ss2$Mean))
  print(cbind(ss1$SD, ss2$SD))
  
  
}

ch_ss <- function(chains, spc ){
  
  n <- length(spc)
  pred <- chains %>% 
    as.mcmc() %>% 
    as.data.frame()  %>% 
    select(., starts_with(spc))
  
  return(pred)
}


FLHM_input <- function(nstats, smu, stau, pitemu , pitetau,
                       order = c(1,3,2,4,4,5,6,8,9,7,10,11,12,1,2,13,3),
                       len = 39){
  # processing northern model stats to get mus and taus
  #   goingin thru evry river individually
  for(i in 1:13){
    #   selecting mu and tau
    mu <- nstats[[i]]$logmedian 
    tau <- nstats[[i]]$tau
    #   if length is not enough, beginnignfill with NA 
    if(length(mu) != len){
      mu = c(rep(NA, (len-length(mu))), mu)
      tau = c(rep(NA, (len-length(tau))), tau)
    }
    #   creating matrices nmu, ntau at first iteration
    if(i == 1){
      nmu = mu
      ntau = tau
    }
    #   iff allreaby crated hust bind new columns
    else{
      nmu = cbind(nmu, mu)
      ntau = cbind(ntau, tau)
    }
  }
  
  #   placing results in correct order and giving apporpriate names
  #   sveriges rivers in 5, 14, 15, 17
  svei <- c(5,14,15,17)
  res = c()
  cn = c()
  for(i in 1:17){
    #cheking index
    j = order[i]
    #   swedish rivers
    if(i %in% svei){
      #    pite
      if(j == 4){
        mu = c(pitemu, rep(NA, len- length(pitemu)))
        tau = c(pitetau, rep(1, len- length(pitetau)))
      }
      #   normal operation
      #   selecting mua nd tau
      else{
        mu = smu[,j]
        tau = stau[,j]
      }
    }
    #   finnish rivers
    else{
      mu = nmu[,j]
      tau = ntau[,j]
    }
    #   binginf them to res
    res = cbind(res, mu)
    res = cbind(res, tau)
    
    #   creating colnames
    nmus <- paste0("mu_SmoltW[,", i, "]")
    ntaus <- paste0("tau_SmoltW[,", i, "]")
    cn <- c(cn, nmus, ntaus)
  }
  
  colnames(res) = cn
  #   changin testeboån to na????
  res[1:13, 33] <- rep(-10, 13)
  res[1:13, 34] <- rep(10, 13)
  #   kåge
  res[1:21, 31] <- rep(-10, 21)
  res[1:21, 32] <- rep(10, 21)
  
  return(as.data.frame(res))
  
}





river_stats <- function(result, rivers = 13, AU=F){
  chains <- as.data.frame(as.mcmc(result))
  #chains <- result
  chains_l <- list()
  riv_stats <- list()
  for( i in 1:rivers){
    #   iff kåge, start from 2008
    if(rivers<=13){
      if(i == rivers){
      sel <- 29:(length(chains[i,])/rivers)
    }
    #   else start from 1987
    else{sel <- 8:(length(chains[i,])/rivers)}
    }
    
    else if(rivers==17){
      if(i == 16){
        sel <- 29:(length(chains[i,])/rivers)
      }
      #   else start from 1987
      else{sel <- 8:(length(chains[i,])/rivers)}
    }
    #   listing river specific chains together
    #   and nming accorgingly
    nam <- paste0("river", i)
    #   if rivers is ume/vindell (==7), times 0.85 to delete extra turbine mortality
    if(i == 7){m = 0.85}else{m = 1}
    chsel <- chains[, endsWith(colnames(chains), paste0(",", i, "]"))][sel]*m 
    chains_l[[i]] <- chsel
    #   if this is used calculatign au based or other sums, return chains
    }
    #   calculating needed values
  if(AU==T){return(chains_l)}
  for(i in 1:rivers){ 
    nam <- paste0("river", i)
    mean   <- apply(chains_l[[i]], 2, mean)
    sd     <- apply(chains_l[[i]], 2, sd)
    median <- apply(chains_l[[i]], 2, median)
    qt0.05 <- apply(chains_l[[i]], 2, quantile, probs =c(0.05))
    qt0.95 <- apply(chains_l[[i]], 2, quantile, probs =c(0.95))
    cv <- sd/mean
    
    logmedian <- log(median)
    tau <- 1/(log(cv^2+1))
    #   adding dataframe to list
    df <- data.frame(mean, sd, median, qt0.05, qt0.95, cv, logmedian, tau)
    riv_stats[[nam]] <- df
    
  }
  #   returning created list
  return(riv_stats)
}

AU_stats <- function(res, chain=river_stats(res, AU=T), TOT=F, nL =F){
  #   getting chains 
  #chain <- river_stats(res, AU=T)
  #   summing chains accorgindly
  #   AU1
  AU1 <- chain[[1]]+chain[[2]]+chain[[3]]+chain[[4]]
  #   AU2
  AU2 <- chain[[5]]+chain[[6]]+chain[[7]]+chain[[8]]+chain[[9]]+chain[[10]]+chain[[11]]
  #   adding kåge to au2
  if(nL){
    AU2[,22:ncol(AU2)] <- AU2[,22:ncol(AU2)]+chain[[12]]

  }
  else{
    AU2[,22:ncol(AU2)] <- AU2[,22:ncol(AU2)]+chain[[13]]
    #   AU3
    AU3 <- chain[[12]]
  }
  
  
  chains_AU = list(
    AU1=AU1,
    AU2=AU2)
  if(!nL){
    chains_AU$AU3 = AU3
  }
  
  if(TOT==T){return(chains_AU)}
  aus <- c("AU1", "AU2")
  if(!nL){aus <- c(aus, "AU3")}
  AU_stats <- list()
  for(i in 1:length(aus)){  
    mean   <- apply(chains_AU[[i]], 2, mean)
    sd     <- apply(chains_AU[[i]], 2, sd)
    median <- apply(chains_AU[[i]], 2, median)
    qt0.05 <- apply(chains_AU[[i]], 2, quantile, probs =c(0.05))
    qt0.95 <- apply(chains_AU[[i]], 2, quantile, probs =c(0.95))
    cv <- sd/mean
    
    logmedian <- log(median)
    tau <- 1/(log(cv^2+1))
    #   adding dataframe to list
    df <- data.frame(mean, sd, median, qt0.05, qt0.95, cv, logmedian, tau)
    AU_stats[[aus[i]]] <- df
    
  }
  #   returning created list
  return(AU_stats)

  
  }

TOT_stats <- function(chs_AU, nL=F){
    #   summing all chains totether
  tot <- chs_AU[[1]]+chs_AU[[2]]#+chs_AU[[3]]
  if(!nL){tot <- tot+chs_AU[[3]]}
  #   calculating needed values
  mean   <- apply(tot, 2, mean)
  sd     <- apply(tot, 2, sd)
  median <- apply(tot, 2, median)
  qt0.05 <- apply(tot, 2, quantile, probs =c(0.05))
  qt0.95 <- apply(tot, 2, quantile, probs =c(0.95))
  cv <- sd/mean
  
  logmedian <- log(median)
  tau <- 1/(log(cv^2+1))
  #   adding dataframe to list
  df <- data.frame(mean, sd, median, qt0.05, qt0.95, cv, logmedian, tau)
  
  #   returning created list
  return(df)
}

#   rounding 
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

#   calculating pspc from R0 values
pspc_calc_simp <- function(R0, rivorder){
  #   chaing river order to suit the table
  pspc <- R0[rivorder,]
  #   calculating neeeded quantiles for vector
  pspc_vec <- c()
  for(i in 1:nrow(R0)){
    med <- quantile(pspc[i,], 0.5)
    q5  <- quantile(pspc[i,], 0.05)
    q95 <- quantile(pspc[i,], 0.95)
    #   adding calues to vector
    pspc_vec <- c(pspc_vec, round_m(med), 
                  paste(round_m(q5), "-", round_m(q95), sep = "" ) )
  }
  
  return(pspc_vec)
}

#   calculating pspc from R0 values
#   riv order is list of indexes grouped on AUs
pspc_calc <- function(R0, rivorder){
  #   chaing river order to suit the table
  #pspc <- R0[unlist(rivorder),]
  pspc <- R0
  #   calculating neeeded quantiles for vector
  pspc_vec <- c()
  AU_med_v <- AU_05_v <- AU_95_v <- c()
  for(i in 1:length(tind)){
    #   selecting rivers
    ind <- rivorder[[i]]
    #   vectors for Au summing
    med_v <- q5_v <- q95_v <- PI_v <- c()
    for(j in ind){
      #print(j)
      #   caluculatin ind rivers
      med <- quantile(pspc[j,], 0.5) ;med_v <- c(med_v, med)
      q5  <- quantile(pspc[j,], 0.05);q5_v  <- c(q5_v, q5)
      q95 <- quantile(pspc[j,], 0.95);q95_v <- c(q95_v, q95)
      
      PI <- paste(round_m(q5), "-", round_m(q95), sep = "" )
      PI_v <- c(PI_v, PI)
      #   addign to vec
      pspc_vec <- c(pspc_vec,round_m(med), PI)
    }
    #   calculating AU based on vecs
    AU_med <- sum(med_v); AU_med_v <- c(AU_med_v, AU_med)
    AU_05  <- sum(q5_v); AU_05_v <- c(AU_05_v, AU_05)
    AU_95  <- sum(q95_v); AU_95_v <- c(AU_95_v, AU_95)
    AU_PI <- paste(round_m(AU_05), "-", round_m(AU_95), sep = "" )
    #   addign AU t0 pspc vec
    pspc_vec <-c(pspc_vec, round_m(AU_med), AU_PI)
    
    #   if AU is 3, making Total sum gulf B
    if(i == 3){
      tot_m <- sum(AU_med_v) %>% round_m() 
      tot_PI <- paste(round_m(sum(AU_05_v)), "-", round_m(sum(AU_95_v)), sep = "" )
      pspc_vec <- c(pspc_vec, tot_m, tot_PI)
    }
  }
  
  return(pspc_vec)
}

#   parsing unregular string input to numeric
asnum_parse <- function(ch){
  num_vec <- c()
  for(i in 1:length(ch)){
    val <- ch[i] %>% 
      str_split(.,pattern = "") %>% 
      unlist() %>% 
      recode(","= ".")
    
    ind = 1
    flag =T
    num = NA
    while(flag){
      num_r <- paste(val[1:ind], sep ="", collapse ="")
      
      #print(num_r)
      test <- as.numeric(num_r)
      if(!is.na(test)){
        num <- num_r
        ind = ind+1
      }
      else{flag=F}
    }
    num_vec <- c(num_vec, num)
    
  }
  return(as.numeric(num_vec))
}

