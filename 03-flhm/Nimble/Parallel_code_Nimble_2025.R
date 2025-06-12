#truncated M, early_Mps priors, truncated cv_SR and alpha SR priors

run_wgbastCode <- function(seed,wdata,wconsts,winits,wmonitor,mfile) {
library(nimble)

source(mfile)
#parallel
wgbastModel <- nimbleModel(code = WGBASTCode,
                          data = wdata,
                          constants = wconsts,
                          inits = winits,calculate=FALSE)    #,buildDerivs = TRUE
                          
stnodes <- wgbastModel$getNodeNames(stochOnly = TRUE, includeData = FALSE)
allvars<-wgbastModel$getVarNames(nodes = stnodes)
mvars<-allvars[!(grepl("lifted",allvars))]  

wgbastModel$simulate(wgbastModel$getDependencies(stnodes))
wgbastModel$calculate()
#if(is.na(calc)){
#  stop("calculate returned NaN")
#}


#nimbleOptions(MCMCenableWAIC = TRUE)
nimbleOptions(MCMCenableWAIC = TRUE)
wConf <- configureMCMC(wgbastModel, print=TRUE, useConjugacy = FALSE, monitors = wmonitor)
wMCMC <- buildMCMC(wConf) # uncompiled MCMC



Cwgbast <- compileNimble(wgbastModel,get_mu_tau,showCompilerOutput = TRUE)

CwgbastMCMC <- compileNimble(wMCMC, project = wgbastModel)
#CwgbastMCMC <- compileNimble(wHMC, project = wgbastModel)

#results <- runMCMC(CwgbastMCMC, niter = 50000, thin=25, setSeed = seed,WAIC=TRUE)  #test
results <- runMCMC(CwgbastMCMC, niter = 1000000, nburnin = 600000, thin=400, setSeed = seed,WAIC=TRUE)  

return(results)
}

#cL bL stuck (inits)

  
