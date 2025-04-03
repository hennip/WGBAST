
MX<-"model{
coefDS_prior<-coefDS_tmp+1   # To ensure Simo count is always overestimation  
coefDS_tmp~dlnorm(-2.029014,1.1211)

}"

run0 <- run.jags(MX, monitor= "coefDS_prior",
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10000, adapt = 10000,
                 keep.jags.files=F,
                 progress.bar=TRUE, jags.refresh=100)

plot(run0)


chains_coefDS<-as.mcmc(run0)

saveRDS(chains_coefDS, "../../WGBAST_shared/flhm/2025/output/chains_coefDS_prior.rds")




chains_coefDS<-readRDS("../../WGBAST_shared/flhm/2025/output/chains_coefDS_prior.rds")

plot(density(chains_coefDS[,"coefDS_prior"]), xlim=c(1,1.3), ylim=c(0,22),
     main="Coef for Simojoki overcount of spawners", xlab="")
lines(density(chains[,"coefDS"][[1]]), lwd=2)
legend("topright", c("prior", "posterior"),lwd=c(1,2) )


