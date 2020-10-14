# FLHM_2020X: sd-priorit lisätty AR-malleille, HrW korjattu, qdW ja qdR lisätty (ero 2SW kalaan)

traceplot(chains[,"eff_qdW[1]"])
traceplot(chains[,"eff_qdW[2]"])
traceplot(chains[,"eff_qdR[1]"])
traceplot(chains[,"eff_qdR[2]"])

traceplot(chains[,"phi_qd"])
traceplot(chains[,"sd_qd"])


traceplot(chains[,"mean_qdW[2]"])
traceplot(chains[,"mean_qdW[3]"])
traceplot(chains[,"mean_qdW[4]"])

traceplot(chains[,"phi_ql"])
traceplot(chains[,"sd_ql"])
traceplot(chains[,"mean_qlW"])


traceplot(chains[,"phi_tr"])
traceplot(chains1[,"phi_tr"])

traceplot(chains[,"mean_trW"])
traceplot(chains[,"mean_trR"])

traceplot(chains[,"mucL"])
traceplot(chains[,"taucL"])


# Jos ongelmat sd_qL:ssä ei poistu, pitäisikö laittaa yhteinen sd verkko- ja siimakalastukselle?

x<-combine.mcmc(chains)
chaincol<-c(rep(1,100),rep(2,100))

x1<-as.matrix(x[,"sd_qd"])
x2<-as.matrix(x[,"sd_ql"])

par(mfrow=c(2,2))

x1<-as.matrix(x[,"sd_qd"])
x2<-as.matrix(x[,"sd_ql"])
plot(x1,x2, ylab="sd_ql", xlab="sd_qd", col=chaincol)


x1<-as.matrix(x[,"sd_ql"])
x2<-as.matrix(x[,"Wprop[25,1]"])
plot(x1,x2, ylab="Wprop[25,1]", xlab="sd_ql",col=chaincol)

x1<-as.matrix(x[,"sd_ql"])
x2<-as.matrix(exp(-x[,"MpsW[24]"]))
plot(x1,x2, ylab="exp(-MpsW[24])", xlab="sd_ql",col=chaincol)


x1<-as.matrix(x[,"sd_ql"])
x2<-as.matrix(x[,"LW[25,1]"])
plot(x1,x2, ylab="LW[25,1]", xlab="sd_ql",col=chaincol)

x1<-as.matrix(exp(-x[,"MpsW[24]"]))
x2<-as.matrix(x[,"LW[25,1]"])
plot(x1,x2, ylab="LW[25,1]", xlab="exp(-MpsW[24])",col=chaincol)

