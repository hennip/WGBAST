
source("../run-this-first-wgbast.r")

load(paste0(PathOut_FLHM, "Nimble_base4_2025.RData"))


#samplet ketjuista
v1 <- mcmc(chain_output[[1]]$samples)
v2 <- mcmc(chain_output[[2]]$samples)

chains<-mcmc.list(list(v1,v2))

#nimet
a<-chain_output[[1]]
b<-a$samples

cs<-attributes(b)$dimnames[[2]]
length(cs) #23689

pdf("traces2.pdf")
par(mfrow=c(3,3))

for( i in cs){
  cat("\n===Käsittelee ",i)
  gd<-gelman.diag(chains[,i])
  if(is.na(gd$psrf[2])==F){
  if(gd$psrf[2]>1.1){
    traceplot(chains[,i],main=paste(i))
  }
  }
}
dev.off()



