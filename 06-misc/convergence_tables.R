library(runjags);library(coda);library(tidyverse);library(lubridate)
#####################################################################
#     write Gelman diagnostics for all variables to a .csv file     #
#####################################################################



#   defining outputfiles
outfile<-"06-misc/Gelman_Rubin_diagnostics_2024_CRModel_run8.csv"
outfile_buinoff<-"06-misc/Gelman_Rubin_diagnostics_2024_buinoff.csv"

#   loading wanted runs
load(str_c(PathOut_FLHM,"FLHM_JAGS_2024_orig_data2024_run8.RData"))
load(str_c(PathOut_FLHM,"CR_2024_selected_chain.Rdata"))




headtxt<-c("parname","Point est","95% CI")

timetaken <- seconds_to_period(run$timetaken)
iter <- run$sample*run$thin+run$burnin


#   function for taking of extra burnin

buin_off <- function(res, nBI=NA, pBI=NA){
  #   getting orig BI and thin
  BI <- res$burnin
  thin <- res$thin
  n <- res$sample
  #   converting into mcmc.list
  #   to make subsetting possible
  mcmc <- as.mcmc.list(res)
  #   calculating starttpoint for new posterior
  if(is.na(nBI))nBI <- round(pBI*n)
  start = BI+thin*nBI
  #   subsetting posterior
  mcmc_m <- window(mcmc, start = start)
  #   adding new chains to res
  res$mcmc <- mcmc_m
  return(res)
  
}


#   getting chains
chains <- as.mcmc.list(run)
chains_m <- as.mcmc.list(buin_off(run,pBI = 0.3))



#   calculating diagnostics (burnin of chain)
df_buinoff <- data.frame()
for(i in 1:dim(chains_m[[1]])[2]){
  df_row <- c(varnames(chains_m)[i],
              gelman.diag(chains_m[ ,varnames(chains_m)[i]])$psrf[1],
              gelman.diag(chains_m[ ,varnames(chains_m)[i]])$psrf[2])
  df_buinoff <- rbind(df_buinoff, df_row)
}

colnames(df_buinoff) <- headtxt


#   calculating diagnostics
df <- data.frame()
for(i in 1:dim(chains[[1]])[2]){
  df_row <- c(varnames(chains)[i],
              gelman.diag(chains[ ,varnames(chains)[i]])$psrf[1],
              gelman.diag(chains[ ,varnames(chains)[i]])$psrf[2])
  df <- rbind(df, df_row)
}
colnames(df) <- headtxt

#   calculating percentage of unconverged variables based on psrf
df_psrf <- df %>% 
  filter(`Point est`>1.1) %>% 
  filter(`Point est` != NaN) %>% 
  arrange(desc(`Point est`%>% as.numeric))

converged_df <- 1-nrow(df_psrf)/nrow(df)


df %>% 
  filter(`Point est`>1.1) %>% 
  filter(`Point est` != NaN) %>% 
  arrange(desc(`Point est` %>% as.numeric))


#   writing tables

write.table(df, file = outfile, sep = ",", row.names = F) 
write.table(df_buinoff, file = outfile_buinoff, sep = ",", row.names = F) 





