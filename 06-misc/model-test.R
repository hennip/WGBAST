

#source("00-Functions/packages-and-paths.r")





M1<-"
model{

x~dnegbin(p,r)
p~dbeta(2,2)
r~dunif(1,10)

}"

modelName<-"test"


Mname<-str_c("03-Model/",modelName, ".txt")
cat(M1,file=Mname)

data<-list(
  # s=df$Schools,
  flow=df$Flow,
  Nobs=df$Smolts,                     
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years)
)


inits<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
            list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))


var_names<-c(
  "p","r","x")


#nb of samples = samples * thin, burnin doesn't take into account thin
# sample on tässä lopullinen sample, toisin kuin rjagsissa!!!

t1<-Sys.time();t1
run1 <- run.jags(M1, 
                 monitor= var_names,#data=data,inits = inits,
                 n.chains = 2, method = 'parallel', thin=300, burnin =0, 
                 modules = "mix",keep.jags.files=T,sample =1000, adapt = 100, 
                 progress.bar=TRUE)
t2<-Sys.time()
difftime(t2,t1)
# 17h

run<-run1
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run2 <- extend.jags(run1, combine=F, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?

run<-run2
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run3 <- extend.jags(run2, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.2d?
run<-run3
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run4 <- extend.jags(run3, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#1.6d
run<-run4
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))

t1<-Sys.time();t1
run5 <- extend.jags(run4, combine=T, sample=4000, thin=300, keep.jags.files=T)
t2<-Sys.time()
difftime(t2,t1)
#2.1d

run<-run5
save(run, file=str_c(pathOut,modelName,"_",dataName,"_run.RData"))


run<-run3

summary(run, var="D")
summary(run, var="P")
summary(run, var="B")
summary(run, var="Ntot")
summary(run, var="eta_alphaN")
summary(run, var="sum")


plot(run, var="D")
plot(run, var="P")
plot(run, var="B")
plot(run, var="Ntot")
plot(run, var="eta_alphaN")
#plot(run, var="sum")

chains<-as.mcmc.list(run)
chains<-window(chains,start=1000000)
#save(run, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_etaB_0714_run.RData")
save(chains, file="H:/Projects/ISAMA/prg/output/Utsjoki-smolts/Smolts_etaB_0714_chains.RData")


gelman.diag(chains[,"Ntot[1]"])
gelman.diag(chains[,"Ntot[2]"])
gelman.diag(chains[,"Ntot[3]"])
gelman.diag(chains[,"Ntot[4]"])
gelman.diag(chains[,"Ntot[5]"])

gelman.diag(chains[,"aP"])
gelman.diag(chains[,"bP"])
gelman.diag(chains[,"aD"])
gelman.diag(chains[,"cvD"])
gelman.diag(chains[,"cvmuD"])
