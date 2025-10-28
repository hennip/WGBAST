
#river migration survival by stock/AU - currently wild stocks only (check) 
#Only Ume/Vindel currently has reduced spawner survival in some years
#Note that if extra mortality is added for wild stocks 1-4 in years with tagging data, 
#TxWs need to move into a loop where s is stock index, not AU - do something about this - use average surv_migr over stocks in each AU?
#In this version river is survrR or survrW and surv are re-numbered

#Now HrW instead of HrR for reared spawners (Torne and Simo) in population dynamics (tagged and untagged)
#Parr added to NrRsp not NrW (wild)

#flexible indexing not complete!!

source("../run-this-first-wgbast.R")

assessment_year<-2025
years<-length(seq(1987:assessment_year))

proj_years<-0
maxage<-6
rstocks<-2 #Lule, Dal
stock_indices<-c(1:17)
#NB if run with Torne and Simo they should have positions 1 and 2 because of exceptions for these rivers
stocks<-length(stock_indices)
allstocks<-17 #number of stocks in data files
AUS<-4
trolling<-1

##POPULATION DYNAMICS

#stock codes
#1 "Tornionjoki"
#2 "Simojoki"
#3 "Kalixalven"
#4 "Ranealven"
#5 "Pitealven"
#6 "Abyalven"
#7 "Byskealven"
#8 "Ricklean"
#9 "Savaran"
#10 "Vindelalven"
#11 "Orealven"
#12 "Logdealven"
#13 "Ljungan"
#14 "Morrumsan"
#15 "Eman"
#16 "Kagealven"
#17 "Testeboan"

# Assessment model 2025
#modelName<-"FLHM_JAGS_2025_base4"  #variant with trolling C&R " 
#modelName<-"FLHM_JAGS_2025_base2"  # base 2
#modelName<-"FLHM_JAGS_2025_base2_NoProcErrors"  # No process errors
#modelName<-"FLHM_JAGS_2025_base2_NoCarlin"  # No Carlin tag-recapture data from wild salmon
modelName<-"FLHM_JAGS_2025_base2_simplePE2"  # No process errors

source(paste0(PathModel_FLHM,modelName,".R"))

source(paste0(PathModel_FLHM,"make_JAGS_data_",assessment_year,".R"))

runName<-modelName
# if(RaneCount==F & full_sp_count==T){runName<-str_c(modelName, "_withoutRane")}
# if(RaneCount==T & full_sp_count==F){runName<-str_c(modelName, "_2yOffUT")}
# print(runName)

#CR<-ifelse(grepl("CR",modelName),T,F) #boolean to read correct version of catch data file

# data, initial values, parameters to monitor
source("03-flhm/setup_FLHM_2025.R")

initsall<-list(inits.fn(),inits.fn())

print(paste0(runName,"_data", assessment_year))

##Quick test
##cat(WGBAST_model,file="wgbast_model.txt")
##jm<-jags.model("wgbast_model.txt",n.adapt=100,
##data=datalist,inits=inits.fn())
##
##chains<-coda.samples(jm,variable.names=parnames,n.iter=100,thin=10)
##v<-as.matrix(chains)

# Burn-in
t01<-Sys.time();print(t01)
run0 <- run.jags(WGBAST_model, monitor= parnames,
                 data=datalist,inits = initsall,
                 n.chains = 2, method = 'parallel', thin=1,
                 burnin =10000, modules = "mix",
                 sample =10, adapt = 10000,
                 keep.jags.files=paste0(runName, assessment_year),
                 progress.bar=TRUE, jags.refresh=100)
t02<-Sys.time();print(t02)
print("run0 done");print(difftime(t02,t01))
print("--------------------------------------------------")

t1<-Sys.time();print(t1)
run1 <- extend.jags(run0, combine=F, sample=10000, thin=100, keep.jags.files=T)
t2<-Sys.time();print(t2)
print("run1 done"); print(difftime(t2,t1))
print("--------------------------------------------------")
run<-run1
save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData"))

# t3<-Sys.time();print(t3)
# run2 <- extend.jags(run1, combine=T, sample=1000, thin=100, keep.jags.files=T)
# t4<-Sys.time();print(t4)
# print("run2 done");print(difftime(t4,t3))
# print("--------------------------------------------------")
# run<-run2
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData"))
# 
# t5<-Sys.time();print(t5)
# run3 <- extend.jags(run2, combine=T, sample=5000, thin=100, keep.jags.files=T)
# t6<-Sys.time();print(t6)
# print("run3 done");print(difftime(t6,t5))
# print("--------------------------------------------------")
# run<-run3
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData"))
# 
# t7<-Sys.time();print(t7)
# run4 <- extend.jags(run3, combine=T, sample=1000, thin=100, keep.jags.files=T)
# t8<-Sys.time();print(t8)
# print("run4 done");print(difftime(t8,t7))
# print("--------------------------------------------------")
# 
# run<-run4
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData"))
# 
# t9<-Sys.time();print(t9)
# run5 <- extend.jags(run4, combine=T, sample=1000, thin=100, keep.jags.files=T)
# t10<-Sys.time();print(t10)
# print("run5 done");print(difftime(t9,t10))
# print("--------------------------------------------------")
# run<-run5
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData"))
# 
# t11<-Sys.time();print(t11)
# run6 <- extend.jags(run5, combine=T, sample=1000, thin=100, keep.jags.files=T)
# t12<-Sys.time();print(t12)
# print("run6 done");print(difftime(t11,t12))
# print("--------------------------------------------------")
# run<-run6
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData")) 			   
# 		


#t13<-Sys.time();print(t13)
# run7 <- extend.jags(run6, combine=T, sample=1000, thin=350, keep.jags.files=T)
# t14<-Sys.time();print(t14)
# print("run6 done");print(difftime(t13,t14))
# run<-run7
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData")) 			   
				
#t15<-Sys.time();print(t15)
# run8 <- extend.jags(run6, combine=T, sample=2000, thin=350, keep.jags.files=T)
# t16<-Sys.time();print(t16)
# print("run8 done");print(difftime(t16,t15))
# run<-run8
# save(run, file=paste0(PathOut_FLHM,runName, "_data",assessment_year,".RData")) 			   

		  
														  


