

# Becky:
# ===============

#PathSim<-"C:/WGBAST15/Assessment results/" # results from the simulation model and output from scenarios


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

load(paste0(PathSim,"CR_2024_selected_chain.RData"))
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
   inits_list[[i]]<-abind(inits_list[[i]],inits.add,along=ydim)
}

}

Ratmp<-inits_list$Ra
Rbtmp<-inits_list$Rb

parentvars<-c("fec",           "MW",            "MR",            "HrW",          
"HRR",           "rrW",           "rrRsp",         "rrR",          
"rcW",           "rcR",           "rdW",           "rdR",          
"rlW",           "rlR",           "Tretain",       "reportc",      
"reportrR",      "reportrW",      "reportd",       "reportl",      
"SCR",           "SCC",           "SCO",           "SCT",          
"p.rel",         "eLW",           "eLR",           "delta",        
"LReffect",      "bL",            "tauL",          "p.ladder",     
"surv_migr",     "corr_msw",      "CV_ladder",     "cvDS",         
"coefDS_tmp",    "Usmolt",        "p.mort",        "K",            
"logit_M74",     "Parr",          "SmoltW",        "Temp",         
"RMps",          "qlW1",          "qlR1",          "qdW1",         
"qdR1",          "qrW",           "qrR",           "qcgnW",        
"qcgnR",         "qctnW",         "qctnR",         "cL",           
"pTrap",         "a_slope",       "error_SR",      "MpsW",         
"logitHtW2",     "logit_qlW",     "logit_qdW",     "logit_pdetect",
"kE",            "kEdc",          "kEc") 

inits_list<-inits_list[names(inits_list) %in% parentvars]        

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
   

initsall$zz<-array(rnsbeta(years*6*5, 2, 2, min = 0.01, max = 0.5/12),dim=c(years,6,5))
initsall$zzr<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.5/12),dim=c(years,6))
initsall$zzc<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.5/12),dim=c(years,6))
initsall$zzdc<-array(rnsbeta(years*6, 2, 2, min = 0.01, max = 0.5/12),dim=c(years,6))


initsall$RMps<-rbeta(years,Ratmp,Rbtmp)
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

initsall$HRL<-rbeta(1,5,100)
initsall$HRD<-rbeta(1,5,100)

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
mu_trWtmp<-numeric(years+5)

initsall$logitHtW2[1]<-rnorm(1,-5,0.2) 
mu_trWtmp[1]<-NA

for(i in 1:(years+4)){ # i: calendar year

  mu_trWtmp[i+1]<-initsall$phi_tr*initsall$logitHtW2[i]+(1-initsall$phi_tr)*initsall$mean_trW
  initsall$logitHtW2[i+1]<-rnorm(1,mu_trWtmp[i+1],0.01)
}

#########################

initsall$sd_ql<-runif(1,0.05,1)
initsall$phi_ql<-runif(1,0.1,0.9) 
initsall$mean_qlW<-rnorm(1,-5,0.2) 

initsall$logit_qlW<-numeric(years+4)
mu_qlWtmp<-numeric(years+4)

initsall$logit_qlW[1]<-rnorm(1,-5,0.2) 
mu_qlWtmp[1]<-NA #not used anywhere

for(i in 1:(years+3)){ # i: calendar year

  mu_qlWtmp[i+1]<-initsall$phi_ql*initsall$logit_qlW[i]+(1-initsall$phi_ql)*initsall$mean_qlW
  initsall$logit_qlW[i+1]<-rnorm(1,mu_qlWtmp[i+1],0.01)
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

initsall$logit_qdW<-array(NA,dim=c((years+5),3))  
mu_qdWtmp<-array(NA,dim=c((years+5),3))

for(j in 2:3){

 # implies uniform[0,1] prior for initial catchability
  initsall$logit_qdW[1,j]<-rnorm(1,-5,0.2) 
  mu_qdWtmp[1,j]<-NA 
  
  for(i in 1:(years+4)){
    # mean reverting AR(1) (index i is year)
    mu_qdWtmp[i+1,j]<-initsall$phi_qd*initsall$logit_qdW[i,j]+(1-initsall$phi_qd)*initsall$mean_qdW[j]
    initsall$logit_qdW[i+1,j]<-rnorm(1,mu_qdWtmp[i+1,j], 0.01)

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
initsall$K<-initsall$K*10

initsall$HrW<-array(rbeta((years+3)*maxage,16,64),dim=c((years+3),maxage))
initsall$HrW[,1]<-NA
initsall$HrW[,4:6]<-NA

initsall$logit_survMps<-array(runif(years*Mps_stocks,-2,-0.3),dim=c(years,Mps_stocks))
initsall$sigma_stock<-rlnorm(Mps_stocks,-1.61,0.10)
initsall$corY<-rbeta((Mps_stocks-1),2,2)
initsall$corZ<-array(rnorm(Mps_stocks*Mps_stocks,0,0.5),dim=c(Mps_stocks,Mps_stocks))
initsall$coefDS_tmp<-rlnorm(1,-2.029014,0.1211)

initsall$MpsW<-NULL
initsall$EPR<-NULL
initsall$EPR_M74<-NULL  
initsall$Eggstot<-NULL  
initsall$Eggstot_M74<-NULL  
initsall$HcR<-NULL  
initsall$HcW<-NULL  
initsall$HdcR<-NULL  
initsall$HdcW<-NULL
initsall$HdoR<-NULL  
initsall$HdoW<-NULL
initsall$HlR<-NULL  
initsall$HlW<-NULL
initsall$HrR<-NULL  
initsall$HrW<-NULL
initsall$HtR<-NULL  
initsall$HtW<-NULL
initsall$LR<-NULL  
initsall$LW<-NULL
initsall$MpsR<-NULL


	initsall$eLW<-rnorm(years+5,0,0.20)
	initsall$eLR<-rnorm(years+5,0,0.20)

for(s in 1:stocks){
    for(i in smolt_year[s]:(years+2)){
          initsall$SmoltW[i,s]<-NA
        
        }
    }

initsall$mean_qdW[3]<-NA
initsall$pTrap[1:5,1:2]<-NA
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

