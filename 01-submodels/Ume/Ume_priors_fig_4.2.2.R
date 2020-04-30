#Ume-Vindel?lven mark recapture submodel
#inputs now include numbers tagged, numbers that passed the counter from mark-recap experiemt, as well as an expert opinion on the reduction in migration success for tagged salmon. (p.migr)
#parameter p from the sub-model goes to the FLHM

library(R2jags)
library(runjags)
library(rriskDistributions)

# Henni
source("C:/Rprojects/WGBAST/04-scenarios/paths_scens.r")



assessment_year<-2020     #update                     
first_year<-1996
years<-assessment_year-first_year
model_years<-assessment_year-1987



#priors for reduced migration success after tagging
ss_migr<-numeric(years)
alpha_migr<-numeric(years)
beta_migr<-numeric(years)
migr_lo<-numeric(years)
migr_hi<-numeric(years)

migr_modes<-read.table(
#"H:/Projects/WGBAST/FLHM/2020/dat/orig/Ume_Vindel stuff/Becky19/Reduced migration success tagged UmeVindel.txt")
"H:/Projects/WGBAST/FLHM/2020/dat/orig/Ume_Vindel stuff/Reduced migration success tagged UmeVindel.txt")
migr_lo[migr_modes$V3==1]<-migr_modes$V2[migr_modes$V3==1]-0.1       #expert opinions   
migr_hi[migr_modes$V3==1]<-migr_modes$V2[migr_modes$V3==1]+0.1

migr_lo[migr_modes$V3==0]<-migr_modes$V2[migr_modes$V3==0]-0.2       #interpolated modes more uncertain
migr_hi[migr_modes$V3==0]<-migr_modes$V2[migr_modes$V3==0]+0.2       #these are not used...

migr_lo[migr_lo<=0]<-0.01
migr_hi[migr_hi>=1]<-0.99

expert_migr<-data.frame(migr_modes$V2,migr_lo,migr_hi)

#1) Find the sample size (k=a+b) using 90% PI 
#2) a = mode * (k-2) + 1 
#   b = (1-mode) * (k-2) + 1

for(i in 1:years){
ss_migr[i]<-sum(get.beta.par(c(0.025,0.975),c(expert_migr[i,2],expert_migr[i,3])))
alpha_migr[i]<-expert_migr[i,1]*(ss_migr[i]-2)+1
beta_migr[i]<-(1-expert_migr[i,1])*(ss_migr[i]-2)+1
}
alpha_migr[migr_modes$V3==0]<-NA

Ume_model<-"

model{

for(i in 1:years){    
    
    N.migr.obs[i]~dbin(p.migr[i],ss.tagged[i])
    N.migr[i]<-round(Ntagged[i]*p.migr[i])

    Npassed[i]~dbin(p[i],N.migr[i])       #Observation model 
    #Npassed[i]~dbin(p[i],Ntagged[i])       #Observation model

     
    p.migr[i]~dbeta(a_migr[i],b_migr[i]) #expert opinion migration success after handling
    p[i]~dbeta(a[i],b[i])      #pars of this Beta distn p -> FLHM        
    
   a[i]<-mu[i]*eta[i]
	 b[i]<-(1-mu[i])*eta[i]
	 mu[i]~dbeta(amu,bmu) # uninformative priors
	 eta[i]~dlnorm(M_eta, tau_eta)
   
   a_migr[i]<-mu_migr[i]*eta_migr[i]
	 b_migr[i]<-(1-mu_migr[i])*eta_migr[i]
	 mu_migr[i]~dbeta(amu_migr,bmu_migr) # uninformative priors
	 eta_migr[i]~dlnorm(M_eta_migr, tau_eta_migr)
   
}   

amu~dlnorm(1,2)
bmu~dlnorm(1,2)

amu_migr~dlnorm(1,2)
bmu_migr~dlnorm(1,2)

M_eta~dunif(0.69,7)
tau_eta<-1/log(cv_eta*cv_eta+1)
cv_eta~dunif(0.001,1)

M_eta_migr~dunif(0.69,7)
tau_eta_migr<-1/log(cv_eta_migr*cv_eta_migr+1)
cv_eta_migr~dunif(0.001,1)


}"

cat(Ume_model,file="model.txt")

Ume_data<-list(years=years,Ntagged=c(484,
55,
100,
60,
100,
70,
493,
391,
503,
450,
100,
100,
100,
94,
100,
100,
100,
364,
159, #2014
100,  #2015
100,
400,  #2017
160,
100   #2019 (Tagging study not representative, so should not be used)
),
Npassed=c(72, # 1996
12, #1997
NA,
17, #1999
NA,
10, #2001
181, #2002
114, # 2003
63, # 2004
169, # 2005
NA,
NA,
NA,
30, # 2009
NA,
NA,
NA,
187, #2013
32, # 2014
NA,
NA,
1,  #2017
24,
0,
NA
),N.migr.obs=round(alpha_migr),ss.tagged=round(ss_migr))
#a.migr=alpha_migr,b.migr=beta_migr,

inits<-list(M_eta=4,amu=2,bmu=2,eta=rep(10,times=years),p.migr=migr_modes$V2)       
initsall<-list(inits,inits)

parnames<-c("mu","eta","p","p.migr","N.migr","Npassed")

jm<-jags.model("model.txt",data=Ume_data,n.chain=2,inits=initsall)
#system.time(update(jm,n.iter=10000))
chains<-coda.samples(jm,parnames,n.iter=10000)

summary(chains[ ,grep("migr",varnames(chains))])
v<-as.matrix(chains)
dev.new()
bx3gnl(v,0.75,22.75,1,"p[","]",0.15,ylim=c(0,0.8),border="springgreen2",xlab="Year",ylab="Probability")
posts<-apply(v[,grep("p",colnames(v))][,1:23],2,median)


dev.new()
bx3gnl(v,0.75,22.75,1,"p.migr[","]",0.15,ylim=c(0,0.8),xlab="Year",ylab="Probability")
points(1:23,migr_modes$V2,pch=23)

ps<-v[,grep("p",colnames(v))][,1:23]
p_par<-array(NA,dim=c(years,2))
#
##Get Beta distribution parameters for p
for(y in 1:years){
      
           tmp.p<-paste(sep="","p[",y,"]")
           q_p<-quantile(v[,grep(tmp.p,colnames(v),fixed=TRUE)],probs=c(0.025,0.50,0.975))
           p_par[y, ]<-get.beta.par(c(0.025,0.5,0.975),q_p)
}

write.table(p_par,file="Ume_p.ladder.txt",sep="\t",row.names=F)

##################################################################################
#Get Beta prior parameters for mortality after counting (expert opinion)
#mortality after the counter from Table in FW: Norrfors email 12.02.2018
expert_surv<-array(NA,dim=c(model_years,3))    #mode, 5%, 95%

expert_surv[9,]<-c(0.5,0.3,0.7)   #up to including 2013  #note these are used in the FLHM, not here
expert_surv[18,]<-c(0.5,0.3,0.7) 

expert_surv[28,]<-c(0.5,0.3,0.7)    #2014             #updated Feb 2019
expert_surv[29,]<-c(0.8,0.6,0.9)   #2015
expert_surv[30,]<-c(0.7,0.50,0.90)   #2016
expert_surv[31,]<-c(0.25,0.10,0.50)   #2017
expert_surv[32,]<-c(0.5,0.3,0.7)   #2018
expert_surv[33,]<-c(0.75,0.5,0.9)   #2019
na_inds<-which(is.na(expert_surv[,1]))
for(i in 1:length(na_inds)){
expert_surv[na_inds[i],]<-c(0.99,0.98,0.999999999)  #other years
}

ss_surv<-numeric(model_years)
alpha_surv<-numeric(model_years)
beta_surv<-numeric(model_years)

#1) Find the sample size (k=a+b) using 90% PI 
#2) a = mode * (k-2) + 1 
#   b = (1-mode) * (k-2) + 1

for(i in 1:model_years){
ss_surv[i]<-sum(get.beta.par(c(0.025,0.975),c(expert_surv[i,2],expert_surv[i,3])))
alpha_surv[i]<-expert_surv[i,1]*(ss_surv[i]-2)+1
beta_surv[i]<-(1-expert_surv[i,1])*(ss_surv[i]-2)+1
}

#setwd("C:/WGBAST15/data_2019/")


print.text<-cbind(alpha_surv,beta_surv)
write.table(print.text,"Vindel_survival_after_counting.txt",sep="\t",col.names=F,row.names=F)
###################################################################################
#plotting Figure 4.2.2.1



##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalix?lven"
#4 "R?ne?lven"
#5 "Pite?lven"
#6 "?by?lven"
#7 "Byske?lven"
#8 "Rickle?n"
#9 "S?var?n"
#10 "Vindel?lven"
#11 "?re?lven"
#12 "Logde?lven"
#13 "Ljungan"
#14 "M?rrums?n"
#15 "Em?n"
#16 "K?ge?lven"

setwd("C:/WGBAST15/")
source("parajags4.r")
source("box_functions_Ume.r")

library(R2jags)
library(runjags)
library(rjags) 

first_year<-1987
p_year<-1996
assess_year<-2020
years<-length(first_year:assess_year)
maxage<-6
stock_indices<-1:17
stocks<-length(stock_indices)
AU<-c(1,1,1,1,2,2,2,2,2,2,2,2,3,4,4,2,3)

tend<-years
yend<-1987+years-1


load("Assessment results/FLHM_results_2019_select_chains.Rdata")  
d<-as.matrix(chains_new)

#get p.ladder variables into array
pl<-d[ ,grep("p.ladder",colnames(d))]  #32 x 16 for 2018 assess
pladd<-array(0,dim=c((years+5),stocks,dim(d)[1]))           

for(y in 1:(years+5)){  #year       
            for(s in 1:16){
                  pladd[y,s,]<-pl[ ,(s-1)*(years+5)+y]         
      }
}

obs<-Ume_data$Npassed/Ume_data$Ntagged  #1996:2017

windows(10,10)
par(mfrow=c(1,1),mar=c(4,4,1,1),font=2,font.lab=1,font.axis=1) 

bx3gnl(v,0.75,22.75,1,"p[","]",0.15,ylim=c(0,0.8),border="springgreen2",xlab="Year",ylab="Probability")

bx90nl(pladd[,10,],0.8,21.8,1,0,0.8,0.15,beg=10,end=tend-1,border="black",add=TRUE);
axis(1,1:(length(p_year:assess_year)-1),as.character(p_year:(assess_year-1)))
points(1:(length(p_year:assess_year)-1),obs,pch=17,col="red")
par(cex=0.8)
legend(c(-0.6,9),c(0.7,0.83), c("Prior", "Posterior", "Mark-recapture study"), pch=c(NA,NA,17),lty=c(1,1,NA),col=c("springgreen2","black","red"))

save(chains,file="Assessment results/Ume_mark_recap_posteriors_2019.RData")

