

# Becky:
# ===============
#PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios

# Note! CR_2024_selected_chain.RData uploaded into 2025 flhm output folder (PathOut_FLHM)
# Another option would be to set different path in run-this-first-file for previous year output
PathSim<-PathOut_FLHM

#assessment_year<-2022
#years<-length(seq(1987:assessment_year))
#proj_years<-0
#
#maxage<-6
#rstocks<-2 #Lule, Dal
#AUS<-4
#stocknames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
#              "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")


AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)
e_delay<-c(rep(4,times=12),3,3,3,4,3)

load(paste0(PathSim,"CR_2024_selected_chain.Rdata"))
d<-as.matrix(chains)

first.word <- function(my.string){
    unlist(strsplit(my.string, "[",fixed=T))[1]
}

get.dims <- function(my.string){
    tempstr<-unlist(strsplit(my.string, "[",fixed=T))[2]
    matches <- regmatches(tempstr, gregexpr("[[:digit:]]+",tempstr))
as.numeric(unlist(matches))
}

get_trunc_gamma<-function(shape,rate,xmin,xmax){
x<-rgamma(1,shape=shape,rate=rate)
while(x<xmin | x>xmax){
x<-rgamma(1,shape=shape,rate=rate)
}
return(x)
}

get_q_init<-function(hr,E){
return(log(-log(1-hr)/E))
}
 

make.inits<-function(){

cnames<-colnames(d)
vnames<-sapply(cnames,first.word)
colnames(vnames)<-NULL
vnames<-unique(vnames)

#remove posterior predictive nodes
pp<-grep("X",vnames)
vnames<-vnames[-pp]

vnames<-sort(vnames)

inits_list<-vector("list", length = length(vnames))
names(inits_list)<-vnames

for(i in 1:length(vnames)){
#dsub<-d[s,grep(paste0(vnames[i],"["),colnames(d),fixed=TRUE)]
dsub<-d[,grep(vnames[i],colnames(d))]
#find names containing "[" (arrays and vectors)
if(length(grep(paste0(vnames[i],"["),colnames(dsub),fixed=TRUE))>0){
dsub1<-d[,grep(paste0("^",vnames[i],"\\["),colnames(d))]
nstring<-colnames(dsub1)[dim(dsub1)[2]]
vdims<-get.dims(nstring) #get the dims
inits_list[[i]]<-array(apply(dsub1,2,mean),dim=vdims)
}else if(length(grep(paste0(vnames[i],"["),colnames(dsub),fixed=TRUE))==0){
#else if(length(grep("[",nstring,fixed=T))==0){
dsub1<-d[,grep(paste0("^",vnames[i],"$"),colnames(d))]
names(dsub1)<-NULL
inits_list[[i]]<-mean(dsub1)
}
}

for(i in 1:length(inits_list)){
tempdims<-dim(inits_list[[i]])
#print(length(tempdims))          #max 3
if(any(tempdims %in% c((years-2):(years+4)))){  #find year dims
ydim<-which(tempdims %in% c((years-2):(years+4)))
#print(paste(i," ", ydim))     #year is always the first dim...good
   inits.add<-asub(inits_list[[i]],tempdims[tempdims %in% c((years-2):(years+4))],ydim)       #asub(x,index (of dim),dim)
   inits_list[[i]]<-abind(inits_list[[i]],inits.add,inits.add,along=ydim)
}

}



#add inits already earlier in WGBAST code
#source(paste0("flhm/make_JAGS_data_",assessment_year,".r"))


alpha_detect<-c(18,18,rep(10,times=15))
beta_detect<-c(2,2,rep(10,times=15))


inits.fn<-function() {
list(early_MpsW=rlnorm(1,0.23,0.15),CV_MpsW=rbeta(1,30,70),Reff_mu=rbeta(1,15,35),Reff_eta=rbeta(1,10,10)*0.5,kE=array(rlnorm(years*6*6,0,0.0001),dim=c(years,6,6)),kEc=array(rlnorm(years*6,0,0.0001),dim=c(years,6)),kEdc=array(rlnorm(years*6,0,0.0001),dim=c(years,6)),mu_a=rnorm(2,-2.8,0.50),sd_a=rlnorm(2,log(0.20),0.20),a_slope=rnorm(17,-2.8,0.50),error_SR=array(rnorm(years*stocks,0,0.50),dim=c(years,stocks)),cv_SR=rlnorm(1,-1.5,0.20),Temp=rnorm(length(muTemp),muTemp,0.20),Parr=rbind(array(rlnorm(years*stocks,mu_Parr,sqrt(1/tau_Parr)),dim=c(years,stocks)),array(NA,dim=c(2,stocks)))) }

inits.old<-inits.fn()
initsall<-c(inits_list,inits.old)

muLW_tmp<-array(0,dim=c(years+5,4))
muLR_tmp<-array(0,dim=c(years+5,4))  
for (i in 1:(years+5)){ # calendar years 1987-present+5
	for (j in 1:4){ # sea ages (1=1SW, 2=2SW etc.)
		muLW_tmp[i,j]<-initsall$cL[i]+initsall$bL[j]+initsall$delta[j]*initsall$Temp[i] # temperature effect differs on age groups
    muLR_tmp[i,j]<-initsall$cL[i]+initsall$bL[j]+initsall$LReffect[j]+initsall$delta[j]*initsall$Temp[i]
  }
}
   

initsall$zz<-array(rnsbeta(years*6*6, 2, 2, min = 0.01, max = 0.04),dim=c(years,6,6))
initsall$zzr<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.04),dim=c(years,6))
initsall$zzc<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.04),dim=c(years,6))
initsall$zzdc<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.04),dim=c(years,6))

initsall$lw<-array(rnorm((years+5)*4,muLW_tmp,0.30),dim=c(years+5,4))  
initsall$lr<-array(rnorm((years+5)*4,muLR_tmp,0.30),dim=c(years+5,4))

initsall$RMps<-rbeta(years,initsall$Ra,initsall$Rb)
initsall$cvDS<-rlnorm(1,-2.37,0.10)
initsall$SCR<-rbeta(1,3,16)
initsall$SCC<-rbeta(1,3,16)
initsall$SCO<-rbeta(1,3,16)
initsall$SCT<-rbeta(1,3,16)
initsall$SCRW<-rbeta(1,3,16)

initsall$tauqd<-get_trunc_gamma(10,1,1,10000)   #shape, rate, min, max
initsall$tauql<-get_trunc_gamma(50,1,1,10000)
initsall$tauqr<-get_trunc_gamma(10,1,1,10000)
initsall$tauqcgn<-get_trunc_gamma(10,1,1,10000)
initsall$tauqctn<-get_trunc_gamma(10,1,1,10000)

initsall$rrW<-runif(6,50,150)
initsall$rrRsp<-runif(6,50,150)
initsall$rcW<-runif(6,50,150)
initsall$rcR<-runif(6,50,150)
initsall$rdW<-runif(6,50,150)
initsall$rdR<-runif(6,50,150)
initsall$rlW<-runif(6,50,150)
initsall$rlR<-runif(6,50,150)

initsall$corr_msw<-runif(stocks,0.10,0.40)

initsall$mu_spawn<-rbeta(stocks,(mu_sp_alpha+1),(mu_sp_beta+1))
initsall$CV_spawn<-rbeta(stocks,(CV_sp_alpha+1),(CV_sp_beta+1))

initsall$muTrap<-c(rbeta(1,4,4),rbeta(1,150,400))
initsall$etaTrap<-c(rlnorm(1,10,0.20),rlnorm(1,3.7,0.20))

initsall$Usmolt<-runif(1,0.85,1.45) 		# Uncertainty factor


#HARVEST RATES!
#CATCHABILITIES?


initsall$HRR<-c(rbeta(1,5,100),rbeta(5,5,25)) #,rbeta(5,25,5)
initsall$qrW<-rlnorm(1,get_q_init(initsall$HRR[1],Er[1,1]),0.20)
initsall$qrR<-c(rlnorm(1,get_q_init(initsall$HRR[1],Er[1,1]),0.20),rep(NA,times=5))

initsall$HRL<-c(rbeta(1,5,100),rbeta(10,25,5))
initsall$HRD<-c(rbeta(1,5,100),rbeta(10,25,5))

initsall$qlW1<-rlnorm(1,get_q_init(initsall$HRL[1],El[1,1]),0.20)
initsall$qlR1<-rlnorm(1,get_q_init(initsall$HRL[1],El[1,1]),0.20)

initsall$qdW1<-rlnorm(1,get_q_init(initsall$HRD[1],Edo[1,1]),0.20)
initsall$qdR1<-rlnorm(1,get_q_init(initsall$HRD[1],Edo[1,1]),0.20)

initsall$HRCTN<-matrix(c(rbeta(3,5,100),rbeta(3,10,12.5),rbeta(12,10,25)),nrow=6,ncol=3,byrow=T)
initsall$HRCGN<-matrix(c(rbeta(3,5,100),rbeta(3,10,12.5),rbeta(12,10,25)),nrow=6,ncol=3,byrow=T) 

initsall$qctnW<-matrix(c(rlnorm(1,get_q_init(initsall$HRCTN[1,1],Ectn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCTN[2,1],Ectn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCTN[3,1],Ectn[1,1,1]),0.20),NA,NA,NA,NA,NA,NA,NA,NA,rlnorm(1,get_q_init(initsall$HRCTN[6,1],Ectn[1,1,1]),0.20),NA,NA),nrow=6,ncol=3,byrow=T)
                    
initsall$qctnR<-matrix(c(rlnorm(1,get_q_init(initsall$HRCTN[1,1],Ectn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCTN[2,1],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[2,2],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[2,3],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[3,1],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[3,2],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[3,3],Ectn[1,1,1]),0.20),NA,NA,NA,NA,NA,NA,rlnorm(1,get_q_init(initsall$HRCTN[6,1],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[6,2],Ectn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCTN[6,3],Ectn[1,1,1]),0.20)),nrow=6,ncol=3,byrow=T)

initsall$qcgnW<-matrix(c(rlnorm(1,get_q_init(initsall$HRCGN[1,1],Ecgn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCGN[2,1],Ecgn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCGN[3,1],Ecgn[1,1,1]),0.20),NA,NA,NA,NA,NA,NA,NA,NA,rlnorm(1,get_q_init(initsall$HRCGN[6,1],Ecgn[1,1,1]),0.20),NA,NA),nrow=6,ncol=3,byrow=T)
                    
initsall$qcgnR<-matrix(c(rlnorm(1,get_q_init(initsall$HRCGN[1,1],Ecgn[1,1,1]),0.20),NA,NA,rlnorm(1,get_q_init(initsall$HRCGN[2,1],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[2,2],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[2,3],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[3,1],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[3,2],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[3,3],Ecgn[1,1,1]),0.20),NA,NA,NA,NA,NA,NA,rlnorm(1,get_q_init(initsall$HRCGN[6,1],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[6,2],Ecgn[1,1,1]),0.20),rlnorm(1,get_q_init(initsall$HRCGN[6,3],Ecgn[1,1,1]),0.20)),nrow=6,ncol=3,byrow=T)

initsall$sd_tr<-runif(1,0.05,1)
initsall$phi_tr<-runif(1,0.1,0.9) 
initsall$mean_trW<-rnorm(1,-5,0.2)  

initsall$logitHtW2<-numeric(years+5)
initsall$mu_trW<-numeric(years+5)

initsall$logitHtW2[1]<-rnorm(1,-5,0.2) 
initsall$mu_trW[1]<-1

for(i in 1:(years+4)){ # i: calendar year

  initsall$mu_trW[i+1]<-initsall$phi_tr*initsall$logitHtW2[i]+(1-initsall$phi_tr)*initsall$mean_trW
  initsall$logitHtW2[i+1]<-rnorm(1,initsall$mu_trW[i+1],0.01)
}

#########################

initsall$sd_ql<-runif(1,0.05,1)
initsall$phi_ql<-runif(1,0.1,0.9) 
initsall$mean_qlW<-rnorm(1,-5,0.2) 

initsall$logit_qlW<-numeric(years+4)
initsall$mu_qlW<-numeric(years+4)

initsall$logit_qlW[1]<-rnorm(1,-5,0.2) 
initsall$mu_qlW[1]<-1 #not used anywhere

for(i in 1:(years+3)){ # i: calendar year

  initsall$mu_qlW[i+1]<-initsall$phi_ql*initsall$logit_qlW[i]+(1-initsall$phi_ql)*initsall$mean_qlW
  initsall$logit_qlW[i+1]<-rnorm(1,initsall$mu_qlW[i+1],0.01)
}

#############################

initsall$sd_qd<-runif(1,0.05,1)
initsall$phi_qd<-runif(1,0.1,0.9) 
initsall$eff_qd<-rbeta(1,20,10)

initsall$mean_qdW<-numeric(3)

initsall$mean_qdW[1]<-NA
initsall$mean_qdW[2]<-rnorm(1,-5,0.2)  # 2SW
initsall$mean_qdW[3]<-initsall$mean_qdW[2]*initsall$eff_qd

#*************************** 

initsall$logit_qdW<-array(NA,dim=c(years+5,maxage))  
initsall$mu_qdW<-array(NA,dim=c(years+5,maxage))

for(j in 2:3){

 # implies uniform[0,1] prior for initial catchability
  initsall$logit_qdW[1,j]<-rnorm(1,-5,0.2) 
  initsall$mu_qdW[1,j]<-1 
  
  for(i in 1:(years+4)){
    # mean reverting AR(1) (index i is year)
    initsall$mu_qdW[i+1,j]<-initsall$phi_qd*initsall$logit_qdW[i,j]+(1-initsall$phi_qd)*initsall$mean_qdW[j]
    initsall$logit_qdW[i+1,j]<-rnorm(1,initsall$mu_qdW[i+1,j], 0.01)

    }
}

initsall$HtW<-rbind(initsall$HtW,initsall$HtW[(years-3):years,])
initsall$HtR<-rbind(initsall$HtR,initsall$HtR[(years-3):years,]) 

initsall$qdW<-NULL
initsall$qdR<-NULL
initsall$qlW<-NULL
initsall$qlR<-NULL

initsall$eLW<-rnorm(years+5,0,0.5)
initsall$eLR<-rnorm(years+5,0,0.5)

initsall$SmoltW[1:10,]<-initsall$SmoltW[1:10,]*2
#initsall$K<-initsall$K*10

initsall$CV_HrW<-runif(1,0.01,1)             
initsall$logit_deltaHRW<-rnorm(1,0.85,0.25)
initsall$mu_HrW<-rnorm(stocks,-1.3862944 ,0.3)
initsall$HrW_autoc<-runif(stocks,0.1,0.99)

initsall$HrW<-NULL

initsall$logit_HrW<-array(NA,dim=c((years+3),6,stocks))
initsall$logit_HrW[,3,]<-rnorm((years+3)*stocks,-1.4,0.50)
initsall$coefDS_tmp<-rlnorm(1,-2.029014,0.1211)
 
initsall$CV_ladder<-rlnorm(1,-3,0.20)
initsall$mu_CVSR<-rnorm(1,-1.5,0.20)
initsall$tau_CVSR<-runif(1,5,30)
initsall$cv_SR<-rlnorm(stocks,-1.5,0.50)
#initsall$cv_SR<-rlnorm(1,-1.5,0.50) 

initsall$logit_mu_spawn<-rnorm(stocks,mu_mu_sp,sqrt(1/(tau_mu_sp*5)))
initsall$logit_CV_spawn<-rnorm(stocks,mu_CV_sp,sqrt(1/(tau_CV_sp*5)))

initsall$logit_mu_spawn[1]<-runif(1,0.85,3) #lower bound 0.70
initsall$ logit_CV_spawn[1]<-runif(1,-4,-2.5) 

initsall$logit_mu_spawn[3]<-runif(1,0,3) #lower bound 0.70
initsall$ logit_CV_spawn[3]<-runif(1,-3,-0.4) 

initsall$logit_pdetect<-matrix(rnorm((years+5)*stocks,initsall$logit_mu_spawn,rep(0.20,times=stocks)),nrow=(years+5),byrow=T)   
    
initsall$M74<-NULL
initsall$M74<-array(NA,dim=c((years),stocks))
for(s in 1:stocks){
initsall$M74[,s]<-rbeta(years,M74_alpha[,s]*2,M74_beta[,s]*2)
}

initsall$logit_M74<-matrix(rnorm(years*stocks,M74_mu,sqrt(1/M74_tau)),nrow=years)
return(initsall)
 
}

#
# # parameters of the gamma
# x <- seq(0,1,by=0.001)
#  jagsgamma <- function(x, r, mu) {(mu^r*x^(r-1)*exp(-mu*x))/gamma(r)}
# jagsr=2
# jagsmu=50
# # plot the density using the formula of jags
#    plot(x, jagsgamma(x, jagsr, jagsmu), type="l", xlab="x", ylab="Density")
#    #plot the density using the dgamma of r
#    par(new=TRUE)
#     plot(x, dgamma(x, shape=jagsr, rate=jagsmu), type="l", axes=FALSE, 
#     col="red", xlab="", ylab="")
#
#
#gvars<-rgamma(1000,shape=jagsr,rate=jagsmu)
#gvars<-gvars[gvars<0.5/12]
#
#bvars<-rnsbeta(1000, 2, 2, min = 0, max = 0.5/12)

