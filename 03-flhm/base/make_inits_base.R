# Initial values
alpha_detect<-c(18,18,rep(10,times=15))
beta_detect<-c(2,2,rep(10,times=15))

iql<-rep(0.001,years+proj_years+4)
iqd<-array(c(rep(NA,(years+proj_years+5)),rep(0.01,(years+proj_years+5)*2)), dim=c((years+proj_years+5),3))

iqcW<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),1),
            dim=c(years+proj_years+3,4,1))
iqcR<-array(rep(array(c(rep(NA,(years+proj_years+3)),rep(0.01,(years+proj_years+3)*3)),dim=c(years+proj_years+3,4)),3),
            dim=c(years+proj_years+3,4,3))

logit_mu_spawn<-rnorm(stocks,mu_mu_sp,sqrt(1/(tau_mu_sp*5)))

logit_CV_spawn<-rnorm(stocks,mu_CV_sp,sqrt(1/(tau_CV_sp*5)))

logit_mu_spawn[1]<-runif(1,0.85,3) #lower bound 0.70
logit_CV_spawn[1]<-runif(1,-3,-1.75) 

logit_mu_spawn[3]<-runif(1,0,3) #lower bound 0.70
logit_CV_spawn[3]<-runif(1,-3,-0.4) 


mu_a<-c(rnorm(1,-2.784,0.50),rnorm(1,-2.784,0.50))
sd_a<-c(rlnorm(1,-0.2653,0.20),rlnorm(1,-0.2653,0.20))


inits.fn<-function() {
  list(fec=c(exp(8),exp(9),exp(9.5),exp(9.5),exp(9.7)),
       K=rlnorm(length(stock_indices),M_K[stock_indices]*1.1,0.50),
       logit_pdetect=matrix(rnorm((years+5)*stocks,logit_mu_spawn,rep(0.20,times=stocks)),nrow=(years+5),byrow=T),    
       
       
       
       MpsW=rlnorm(years+proj_years,-1.2,0.3),
       a_slope=rnorm(stocks,mu_a[SR_unit[1:stocks]],sd_a[SR_unit[1:stocks]]),
       logit_qlW=log(iql/(1-iql)),     
       logit_qdW=log(iqd/(1-iqd)),     
       MW=rlnorm(1,-2.3,0.15),MR=rlnorm(1,-2,0.15),
       early_MpsW=rlnorm(1,0.23,0.15),
       CV_MpsW=rbeta(1,30,70),Reff_mu=rbeta(1,15,35),Reff_eta=rbeta(1,10,10)*0.5,CV_ladder=rlnorm(1,-3,0.20),
       cv_SR=rlnorm(1,-1.5,0.50))  #,CV_ladder=rlnorm(1,-3,0.20)
}
