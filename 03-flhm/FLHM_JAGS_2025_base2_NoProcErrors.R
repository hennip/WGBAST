WGBAST_model<-"

model{ 

#here i index denotes current year
   pmat[1,1]<-0 
   pimm[1,1]<-1
   
   for(j in 2:6){
        pmat[1,j]<-LW[1,(j-1)]*pimm[1,(j-1)]      
        pimm[1,j]<-(1-LW[1,(j-1)])*pimm[1,(j-1)]

    }
    
for(i in 2:(m+proj_years)){
    pmat[i,1]<-0
    pimm[i,1]<-1

    for(j in 2:6){
        pmat[i,j]<-LW[i,(j-1)]*pimm[(i-1),(j-1)]     
        pimm[i,j]<-(1-LW[i,(j-1)])*pimm[(i-1),(j-1)]
  
    }
}

for(s in 1:stocks){
#here i index denotes smolt year

   simm[1,1,s]<-exp(-(11*MpsW[1]/Tstep))*exp(-(MpsW[1]*sealMort[1,1,1]/Tstep))     #note AU 1 sealMort for all change later
   smat[1,1,s]<-1    #not used anywhere

   for(j in 2:6){
        simm[1,j,s]<-exp(-(12*MW/Tstep))
        smat[1,j,s]<-exp(-(3*MW/Tstep))*exp(-(2*MW*sealMort[1,j,AU[s]]/Tstep))*p.ladder[(1+j-1),s]*surv_migr[(1+j-1),s]
    }
    
for(i in 2:(m+proj_years)){
      
    simm[i,1,s]<-exp(-(11*MpsW[i]/Tstep))*exp(-(MpsW[i]*sealMort[i,1,1]/Tstep))   #note AU 1 sealMort for all change later
    smat[i,1,s]<-1    #not used anywhere
    
    for(j in 2:6){
        simm[i,j,s]<-exp(-(12*MW/Tstep))
        smat[i,j,s]<-exp(-(3*MW/Tstep))*exp(-(2*MW*sealMort[i,j,AU[s]]/Tstep))*p.ladder[(i+j-1),s]*surv_migr[(i+j-1),s]
    }
}

#EPR calculation

for(i in 1:5){

         EPR[i,s]<-0
         EPR_M74[i,s]<-EPR[i,s]*(1-M74[i,s])
							  
         z[i,s]<-(slope[s]*EPR_M74[i,s])/(4+slope[s]*EPR_M74[i,s])	  
}

for(i in 6:(m+proj_years)){    
           
    EPR[i,s]<-pmat[i,2]*prop_fem[(i-1),1,s]*fec[1]*simm[(i-1),1,s]*smat[(i-1),2,s]        #1SW
    +pmat[i,3]*prop_fem[(i-2),2,s]*fec[2]*simm[(i-2),1,s]*simm[(i-1),2,s]*smat[(i-2),3,s]                                   #2SW
    +pmat[i,4]*prop_fem[(i-3),3,s]*fec[3]*simm[(i-3),1,s]*simm[(i-2),2,s]*simm[(i-1),3,s]*smat[(i-3),4,s]                         #3SW
    +pmat[i,5]*prop_fem[(i-4),4,s]*fec[4]*simm[(i-4),1,s]*simm[(i-3),2,s]*simm[(i-2),3,s]*simm[(i-1),4,s]*smat[(i-4),5,s]               #4SW
    +pmat[i,6]*prop_fem[(i-5),5,s]*fec[5]*simm[(i-5),1,s]*simm[(i-4),2,s]*simm[(i-3),3,s]*simm[(i-2),4,s]*simm[(i-1),5,s]*smat[(i-5),6,s]     #5SW
    
         EPR_M74[i,s]<-EPR[i,s]*(1-M74[i,s])
																
         z[i,s]<-(slope[s]*EPR_M74[i,s])/(4+slope[s]*EPR_M74[i,s])  
    }
}

# Northern stocks
# prior: mean(slope)=0.04, sd_slope=0.017
mu_a[1]~dnorm(-2.784,2.388)
sd_a[1]~dlnorm(-0.2653,3.1529)
tau_a[1]<-1/(sd_a[1]*sd_a[1])

# Southern stocks
# prior: mean(slope)=0.04, sd_slope=0.017
mu_a[2]~dnorm(-2.784,2.388)
sd_a[2]~dlnorm(-0.2653,3.1529)
tau_a[2]<-1/(sd_a[2]*sd_a[2])

# From eggs to smolts	
# Total egg production (Eggstot) is a function of sex ratio, fecundity and spawner abundance		
for (s in 1:stocks){

    a_slope[s]~dnorm(mu_a[SR_unit[s]],tau_a[SR_unit[s]])
    logit(slope[s])<-a_slope[s]
    alphaSR[s] <- 1/slope[s]				
  	K[s]~dlnorm(M_R0[s],tau_R0[s])     #M_R0 and tau_R0 come from the data file K_prior
    betaSR[s] <- 1/K[s]
    for(i in 1:(6+e_delay[s]-1)){
        smoltPred[i,s]<-0
    }
    
    for(i in 1:5){ 
       
        NspWtot[i,s]<-0
        NrW_msw[i,s]<-0
        NrW_tot[i,s]<-0
        NladderW_tot[i,s]<-0
        NrRsp_msw[i,s]<-0
        NrRsp_tot[i,s]<-0
        NrAll_msw[i,s]<-0
        NrAll_tot[i,s]<-0  
       
        PropWgrilse[i,s]<-0
        PropWMSW[i,s]<-0
        
        mean_sp[i,s]<-0
        var_sp[i,s]<-0
        cv_sp[i,s]<-0
        tau_sp[i,s]<-0
        mu_sp[i,s]<-0
        sp_countX[i,s]<-0
        
        mean_la[i,s]<-0
        var_la[i,s]<-0
        cv_la[i,s]<-0
        tau_la[i,s]<-0
        mu_la[i,s]<-0
        
        eta_star[i,s]<-0
		    alpha_msw[i,s]<-1
        beta_msw[i,s]<-1
        NrWmsw[i,s] <-0
        NrWtot[i,s]<-0 
       	probMSW[i,s] <-0
              
        Eggstot[i,s]<-0
        Eggstot_M74[i,s]<-0
       
        M74[i,s]~dbeta(M74_alpha[i,s], M74_beta[i,s])   #M74 alpha and beta from M74 data file
        error_SR[i,s] ~dnorm(mu_SR,tau_SR)
         

    }
}

for(s in 1:2){     #Torne, Simo
    for (i in 6:(m+proj_years)){
    
        Eggstot[i,s] <- (NspW[(i-1),2,s]+NspRsp[(i-1),2,s])*prop_fem[(i-1),1,s] * fec[1] +(NspW[(i-2),3,s]+NspRsp[(i-2),3,s])* prop_fem[(i-2),2,s] * fec[2] +(NspW[(i-3),4,s]+NspRsp[(i-3),4,s])* prop_fem[(i-3),3,s] * fec[3] + (NspW[(i-4),5,s]+NspRsp[(i-4),5,s]) * prop_fem[(i-4),4,s] * fec[4] + (NspW[(i-5),6,s]+NspRsp[(i-5),6,s])* prop_fem[(i-5),5,s] * fec[5]
        
        Eggstot_M74[i,s]<- Eggstot[i,s] * (1-M74[i,s])	  #Eggs surviving after M74
 
        # Beverton Holt SR model for predicted smolt abundance
        #In AU 1-3 it takes 4 years from egg to smolt (1st BH smolts in yr 10)		
        # In AU 4 it takes 3 years from egg to smolt (1st BH smolts in yr 9)
        
        smoltPred[(i+e_delay[s]),s] <-log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]
 
        M74[i,s]~dbeta(M74_alpha[i,s], M74_beta[i,s])   #M74 alpha and beta from M74 data file
        error_SR[i,s] ~dnorm(mu_SR,tau_SR)
        
        NspWtot[i,s] <- NspW[(i-1), 2,s]+NspW[(i-2), 3,s] +NspW[(i-3), 4,s] +NspW[(i-4), 5,s] + NspW[(i-5),6,s]             +NspRsp[(i-1), 2,s]+NspRsp[(i-2), 3,s] +NspRsp[(i-3), 4,s] + NspRsp[(i-4), 5,s] +NspRsp[(i-5), 6,s] 
                  	
        NrW_msw[i,s] <- NrW[(i-2),3,s] +NrW[(i-3),4,s] +NrW[(i-4),5,s] +NrW[(i-5),6,s]
        NrW_tot[i,s] <- NrW[(i-1),2,s]+NrW_msw[i,s] 
        NladderW_tot[i,s]<- (NladderW[(i-1),2,s]+NladderW[(i-2),3,s]+NladderW[(i-3),4,s]+NladderW[(i-4),5,s]+NladderW[(i-5),6,s])*1000
        
        NrRsp_msw[i,s]<-NrRsp[(i-2),3,s]+ NrRsp[(i-3),4 ,s]+NrRsp[(i-4),5,s]+NrRsp[(i-5), 6,s]  
        NrRsp_tot[i,s]<-NrRsp[(i-1),2,s]+NrRsp_msw[i,s]
        
        NrAll_msw[i,s] <- NrW_msw[i,s]+NrRsp_msw[i,s] 
        NrAll_tot[i,s] <- NrW_tot[i,s]+NrRsp_tot[i,s]  #NrWtot1 winbugs (1000s)
         
        PropWgrilse[i,s]<-NrW[(i-1), 2,s] /(NrW[(i-1), 2,s]+NrRsp[(i-1), 2,s])   #propn of grilse that are wild
        PropWMSW[i,s]<-NrW_msw[i,s]/NrAll_msw[i,s]   #propn MSW wild
         	
        NrWtot[i,s] <- round((NrW_tot[i,s]+NrRsp_tot[i,s])*1000)+1  
        #doesn't make a diff for Ume here if NrW or NladderW used to calc below ratio as p.ladder same all ages 
 	      probMSW[i,s] <- NrAll_msw[i,s]/NrAll_tot[i,s] 
        


    }
    for(i in smolt_year[s]:(m+2)){
        SmoltWobs[i,s]~dlnorm(smoltPred[i,s],tau_SmoltW[i,s])
        R0[i,s]<-(K[s]*(EPR_M74[(i-e_delay[s]),s]-alphaSR[s]))/EPR_M74[(i-e_delay[s]),s] 
    }
}

for(s in 3:stocks){
    for (i in 6:(m+proj_years)){

        Eggstot[i,s] <- NspW[(i-1),2,s]* prop_fem[(i-1),1,s] * fec[1] + NspW[(i-2),3,s]* prop_fem[(i-2),2,s] * fec[2] + NspW[(i-3),4,s]* prop_fem[(i-3),3,s] * fec[3] + NspW[(i-4),5,s] * prop_fem[(i-4),4,s] * fec[4] + NspW[(i-5),6,s]* prop_fem[(i-5),5,s] * fec[5]
        			
        Eggstot_M74[i,s]<- Eggstot[i,s] * (1-M74[i,s])	  #Eggs surviving after M74
 
        # Beverton Holt SR model for predicted smolt abundance
        #In AU 1-3 it takes 4 years from egg to smolt (1st BH smolts in yr 10)		
        # In AU 4 it takes 3 years from egg to smolt (1st BH smolts in yr 9)
        
        smoltPred[(i+e_delay[s]),s] <-log(Eggstot_M74[i,s]/(alphaSR[s]+betaSR[s]*Eggstot_M74[i,s]))+error_SR[i,s]   
        M74[i,s]~dbeta(M74_alpha[i,s], M74_beta[i,s])   #M74 alpha and beta from M74 data file
        error_SR[i,s] ~dnorm(mu_SR,tau_SR)
        
        NspWtot[i,s] <- NspW[(i-1), 2,s] +NspW[(i-2), 3,s] +NspW[(i-3), 4,s] +NspW[(i-4), 5,s] + NspW[(i-5),6,s] 
       	
        NrW_msw[i,s] <- NrW[(i-2),3,s] +NrW[(i-3),4,s] +NrW[(i-4),5,s] +NrW[(i-5),6,s]
        NrW_tot[i,s] <- NrW[(i-1),2,s]+NrW_msw[i,s]  #NrWtot1 winbugs (1000s)
        NladderW_tot[i,s]<- (NladderW[(i-1),2,s]+NladderW[(i-2),3,s]+NladderW[(i-3),4,s]+NladderW[(i-4),5,s]+NladderW[(i-5),6,s])*1000 
        
        
        NrRsp_msw[i,s]<-0  
        NrRsp_tot[i,s]<-0
        
        NrAll_msw[i,s] <- NrW_msw[i,s]+NrRsp_msw[i,s] 
        NrAll_tot[i,s] <- NrW_tot[i,s]+NrRsp_tot[i,s]  
   
        NrWtot[i,s] <- round((NrW_tot[i,s]+NrRsp_tot[i,s])*1000)+1    
        #doesn't make a diff for Ume here if NrW or NladderW used to calc below ratio as p.ladder same all ages 
 	      probMSW[i,s] <- NrAll_msw[i,s]/NrAll_tot[i,s] 
     
        PropWgrilse[i,s]<-0
        PropWMSW[i,s]<-0

 
    }
    for(i in smolt_year[s]:(m+2)){
        SmoltWobs[i,s]~dlnorm(smoltPred[i,s],tau_SmoltW[i,s])
        R0[i,s]<-(K[s]*(EPR_M74[(i-e_delay[s]),s]-alphaSR[s]))/EPR_M74[(i-e_delay[s]),s] 
    }
}

#tau_SR ~ dlnorm(0,10)
cv_SR~dlnorm(-1.5,2)
tau_SR<-1/log(cv_SR*cv_SR+1)
mu_SR<- -0.5/tau_SR



#index is number of sea winters
fec[1]~dlnorm(8.266503956,	54.78192169)
fec[2]~dlnorm(9.139093315,	168.451323)
fec[3]~dlnorm(9.504315997,	109.3565359)
fec[4]~dlnorm(9.505447712,	61.70147645)
fec[5]~dlnorm(9.68393554,	38.95507182)

# Stocked as parr/smolts
#mu_Parr, tau_Parr, Smolt_Rsp from ReleaseSimoTorne data file
for(s in 1:stocks){ 
    for (i in 1:m){ 
        Parr[i,s] ~dlnorm(mu_Parr[i,s], tau_Parr[i,s])  
    }
    for (i in (m+1):(m+proj_years+2)){ 
        Parr[i,s]<-0
    }

    for (i in 1:(m-1)){ 
        SmoltRsp[i,s] <-  Smolt_Rsp[i,s]      #was mu_SmoltT and mu_SmoltS
    }
    
    # Number of smolt releases in assessment year are assumed the same as year before
    SmoltRsp[m,s] <- SmoltRsp[(m-1),s]
}

# Smolt abundance	
 
# Fit the latter part of the time series to the prior for the mean amount of observed
# smolts. => smoltPred gets updated and contains afterwards information from both
# model predicted amount of smolts (SR-part) and the mean amount of smolts based 
# on the smolt mark-recapture model.
# Data_SmoltsW.odc

for (s in 1:stocks){

    # Early part of the time series, before the whole life history has gone through:
       for (i in 1:(smolt_year[s]-1)){
      
        R0[i,s]<-exp(M_R0[s])
        SmoltW[i,s] ~ dlnorm(mu_SmoltW[i,s], tau_SmoltW[i,s])T(0.0001,3000000)  # mu_SmoltW and tau_SmoltW from Data_SmoltsW.odc

    }
       
    # From year 10-> we use predicted smolt abundance:
      for (i in smolt_year[s]:(m+proj_years+2)){
		    
		    SmoltW[i,s] <- exp(smoltPred[i,s])
  
	  }

	#     for (i in 1:(m+proj_years)){
	# 	    IBSFC[i,s] <- SmoltW[i,s]/ R0[i,s]  
	# 	    prob.IBSFC50[i,s] <- step(IBSFC[i,s] - 0.5)	
	# 	    prob.IBSFC75[i,s] <- step(IBSFC[i,s] - 0.75)	
	# 	    prob.IBSFC100[i,s] <- step(IBSFC[i,s] - 1)										
	#   }

####################################Population dynamics########################################

# Ncc: number of salmon on May 1st
# Ndo: number of salmon on Jan 1st (imm)
# Nl: number of salmon on Feb 1st (imm)
# Nt: number of salmon on April 1st (imm)										 

# Nc: number of salmon in coastal areas on June 1st (mat)
# Nr: number of salmon in the river on Aug 1st (mat)
# Nsp: number of spawners on Oct 1st (mat)


#surv[,,1,] #natural mortality: 1 month for j= 1 (post-smolts), 0 months for j=2-6
#surv[,,2,] #natural mortality + longline fishery: 2 months for j= 1 (post-smolts) and j=2-6 (immature)
#surv[,,3,] #natural mortality at sea: 0 months for j=1, 8 months for j=2-6 (immature)
#surv[,,4,] #natural mortality + offshore driftnet fishery: 1 month for j= 1 (post-smolts) and j=2-6 (immature)
#surv[,,5,] #natural mortality + river fishery: 1 month for j= 1 (post-smolts), 2 months for j=2-6 (mature)
#surv[,,6,] #natural mortality + offshore trolling fishery: 1 month for j= 1 (post-smolts) and j=2-6 (immature)

#survc[,,]  #natural mortality + coastal trap net and gillnet fisheries: 6 months for j= 1 (post-smolts), 2 months for j=2-6 (mature)
#survdc[,,] #natural mortality + coastal driftnet  fishery: 0 months for j=1, 1 month for j=2-6 (mature)


for (i in 1:(m+proj_years)){              

#Post-smolts

    # Fish in the river in  the beginning of June in year 1 
    NrW[i,1,s] <- SmoltW[i,s]*surv[i,1,1,1]#*kE[i,1,1]    #SmoltW beginning of May (surv[i,1,1,1] 1 month)

    # Coastal fish in the beginning of July in year 1      
    NcW[i,1,s] <- NrW[i,1,s]*surv.riv[i,1,s]#*kE[i,1,5]
    
   # Offshore salmon in the beginning of January in year 1 (calendar year 2)
    NdoW[i,1,s] <- NcW[i,1,s]*survc[i,1,1,AU[s]]#*kEc[i,1] 
   	
    # Offshore salmon in the beginning of February in year 1  (calendar year 2)
    NlW[i,1,s] <- NdoW[i,1,s]*surv[i,1,4,1]#*kE[i,1,4]
   																													
	# Offshore salmon in the beginning of April in year 1  (calendar year 2)
    NtW[i,1,s] <- NlW[i,1,s]*surv[i,1,2,1]#*kE[i,1,2]		
	
    NccW[i,1,s]<-0
    NdcW[i,1,s]<-0  
    NdcWI[i,1,s]<-0
    NladderW[i,1,s]<-0  
    NspW[i,1,s]<-0
   
    for (j in 2:6){ 
           
        # Offshore salmon in the beginning of May in year 2-6  
        #NccW[i,j,s]<-NlW[i,(j-1),s]*surv[i,(j-1),2,1]#*kE[i,(j-1),2]
		    NccW[i,j,s]<-NtW[i,(j-1),s]*surv[i,(j-1),6,1]#*kE[i,(j-1),6]	 
	 
        #Immature salmon in coastal areas in the beginning of May    
        NdcWI[i,j,s] <- NccW[i,j,s]* (1-LW[(i+j-1),(j-1)])  #index 1 of LW is current year, 2 of LW is sea winters  
          
        # Offshore salmon in the beginning of January 
        NdoW[i,j,s] <- NdcWI[i,j,s]*surv[i,j,3,1]#*kE[i,j,3] # Immature 
       
        #Offshore salmon in the beginning of February
        NlW[i,j,s] <- NdoW[i,j,s]*surv[i,j,4,1]#*kE[i,j,4]   #Immature 
  
		    # Offshore salmon in the beginning of April        
        NtW[i,j,s] <- NlW[i,j,s]*surv[i,j,2,1]#*kE[i,j,2]												   

        #Salmon in coastal areas in the beginning of May.     #Mature
        NdcW[i,j,s] <- NccW[i,j,s]*LW[(i+j-1),(j-1)]   #Maturation
        
        # Salmon in the coastal area in the beginning of June 
        NcW[i,j,s] <- NdcW[i,j,s]*survdc[i,j,1,AU[s]]#*kEdc[i,j] 
        
        # Salmon in the river in the beginning of August                  #Mature
        NrW[i,j,s] <- NcW[i,j,s]*survc[i,j,1,AU[s]]#*kEc[i,j] 
        
        # Number of salmon available to river fishery and spawning        #Mature
        #added to deal with different situation in Ume, p.ladder=1 for all rivers except Ume
        NladderW[i,j,s]<-NrW[i,j,s]*p.ladder[(i+j-1),s]	   
        
        # Number of spawners in the river in the beginning of October    #Mature
        NspW[i,j,s]<-NladderW[i,j,s]*surv.riv[i,j,s]*surv_migr[(i+j-1),s]#*kE[i,j,5]	   	   

        
    }
}  

} #s

for (i in 1:(m+proj_years)){             
    for(s in 1:2){    #Torne, Simo

    #Post-smolts

    # Fish in the river in  the beginning of June in year 1 
    NrRsp[i,1,s] <- (SmoltRsp[i,s] + Parr[i,s])*surv[i,1,1,2]#*kE[i,1,1]
   	
    # Coastal fish in the beginning of July in year 1      
    #NcRsp[i,1,s] <- NrRsp[i,1,s]*(1-HrW[i,1])*exp(-MpsR[i]*sealMort[i,1,1]/Tstep)#*kE[i,1,5]
    NcRsp[i,1,s] <- NrRsp[i,1,s]*(1-HrW[i,1]*rivHR[(i+1-1),s])*exp(-MpsR[i]*sealMort[i,1,1]/Tstep)#*kE[i,1,5]
    
    # Offshore salmon in the beginning of January in year 1 (calendar year 2)
    NdoRsp[i,1,s] <- NcRsp[i,1,s]*survc[i,1,2,AU[s]]#*kEc[i,1]		     
   	
    # Offshore salmon in the beginning of February in year 1  (calendar year 2)
    NlRsp[i,1,s] <- NdoRsp[i,1,s]*surv[i,1,4,2]#*kE[i,1,4]	  

														   
	# Offshore salmon in the beginning of April in year 1  (calendar year 2)
    NtRsp[i,1,s] <- NlRsp[i,1,s]*surv[i,1,2,2]#*kE[i,1,2]	  
				
    NccRsp[i,1,s]<-0
    NdcRsp[i,1,s]<-0  
    NdcRspI[i,1,s]<-0  
    NladderRsp[i,1,s]<-0
    NspRsp[i,1,s]<-0
   
    for (j in 2:6){ 
           
        # Offshore salmon in the beginning of May in year 2-6  
        #NccRsp[i,j,s] <- NlRsp[i,(j-1),s]*surv[i,(j-1),2,2]#*kE[i,(j-1),2]  
        NccRsp[i,j,s] <- NtRsp[i,(j-1),s]*surv[i,(j-1),6,2]#*kE[i,(j-1),6]  
        
		
        #Immature salmon in coastal areas in the beginning of May    
        NdcRspI[i,j,s] <-NccRsp[i,j,s]*(1-LR[(i+j-1),(j-1)])
          
        # Offshore salmon in the beginning of January 
        NdoRsp[i,j,s]<- NdcRspI[i,j,s]*surv[i,j,3,2]#*kE[i,j,3]    
        
        #Offshore salmon in the beginning of February
        NlRsp[i,j,s] <- NdoRsp[i,j,s]*surv[i,j,4,2]#*kE[i,j,4]	 
  												  															  
		    #Offshore salmon in the beginning of April
        NtRsp[i,j,s] <- NlRsp[i,j,s]*surv[i,j,2,2]#*kE[i,j,2]															 
        
		    #Salmon in coastal areas in the beginning of May.     #Mature
        NdcRsp[i,j,s] <- NccRsp[i,j,s]*LR[(i+j-1),(j-1)]	            
        
        # Salmon in the coastal area in the beginning of June 
        NcRsp[i,j,s] <- NdcRsp[i,j,s]*survdc[i,j,2,AU[s]]#*kEdc[i,j]	          
        
        # Salmon in the river in the beginning of August              #Mature
        NrRsp[i,j,s] <- NcRsp[i,j,s]*survc[i,j,2,AU[s]]#*kEc[i,j]  
        
        # Number of salmon available to river fishery and spawning         #Mature
        #added to deal with different situation in Ume. p.ladder=1 for all rivers except Ume
        NladderRsp[i,j,s] <- NrRsp[i,j,s]*p.ladder[(i+j-1),s]		 	    
        
        # Number of spawners in the river in the beginning of October    #Mature
        #NspRsp[i,j,s] <- NladderRsp[i,j,s]*(1-HrW[i,j])* exp(-2*MR/Tstep)*surv_migr[(i+j-1),s]#*kE[i,j,5]	
        NspRsp[i,j,s] <- NladderRsp[i,j,s]*(1-HrW[i,j]*rivHR[(i+j-1),s])* exp(-2*MR/Tstep)*surv_migr[(i+j-1),s]#*kE[i,j,5]	
        }
    }
}



for(s in 1:AUS){              #s index denotes AU here 
    for (i in 1:m){ 

#Post-smolts

		# Fish in the river in  the beginning of June in year 1 
                    
        SmoltR[i,s] <- SmoltRdata[i,s] * Usmolt               #SmoltRdata from SmoltR data file Usmolt from
        NrR[i,1,s] <- SmoltR[i,s]*surv[i,1,1,2]#*kE[i,1,1]	                                                      

        TrW[i,1,s] <- rel_W[i,s]*surv[i,1,1,1]#*kE[i,1,1]       #tagged wild salmon releases AU1  Beginning of July
        TrR[i,1,s] <- rel_R[i,s]*surv[i,1,1,2]#*kE[i,1,1]	     #tagged reared salmon NB s index stands for AU 
		    TrRsp[i,1,s] <- rel_Rsp[i,s]*surv[i,1,1,2]#*kE[i,1,1]	 #tagged reared-spawned salmon NB s index stands for AU 

		# Coastal fish in the beginning of July in year 1
      
        NcR[i,1,s] <- NrR[i,1,s]*surv[i,1,5,2]#*kE[i,1,5]
            
        TcW[i,1,s] <- TrW[i,1,s]*surv[i,1,5,1]#*kE[i,1,5]	#rivHR change needed? surv.riv[i,1,AU_stock[s]]?? or leave as is....see process errors	     
        TcR[i,1,s] <- TrR[i,1,s]*surv[i,1,5,2]#*kE[i,1,5]	      
        TcRsp[i,1,s] <- TrRsp[i,1,s]*(1-HrW[i,1])*exp(-MpsR[i]*sealMort[i,1,1]/Tstep)#*kE[i,1,5]	  
	
		# Offshore salmon in the beginning of January in year 1
      
 	 	    NdoR[i,1,s] <- NcR[i,1,s]*survc[i,1,2,s]#*kEc[i,1]	
       
        TdoW[i,1,s] <- TcW[i,1,s]*survc[i,1,1,s]#*kEc[i,1]
        TdoR[i,1,s] <- TcR[i,1,s]*survc[i,1,2,s]#*kEc[i,1] 
		    TdoRsp[i,1,s] <- TcRsp[i,1,s]*survc[i,1,2,s]#*kEc[i,1] 

		# Offshore salmon in the beginning of February in year 1
      
  	  	NlR[i,1,s] <- NdoR[i,1,s]*surv[i,1,4,2]#*kE[i,1,4]	

		# Offshore salmon in the beginning of April in year 1
  	  	NtR[i,1,s] <- NlR[i,1,s]*surv[i,1,2,2]#*kE[i,1,2]												   
													   
        TlW[i,1,s] <- TdoW[i,1,s]*surv[i,1,4,1]#*kE[i,1,4]	
        TlR[i,1,s] <- TdoR[i,1,s]*surv[i,1,4,2]#*kE[i,1,4]	
		    TlRsp[i,1,s] <- TdoRsp[i,1,s]*surv[i,1,4,2]#*kE[i,1,4]		
    
        NccR[i,1,s]<-0
        TccW[i,1,s]<-0
        TccR[i,1,s]<-0
        TccRsp[i,1,s]<-0
         
        NdcR[i,1,s]<-0
        TdcW[i,1,s]<-0 
        TdcR[i,1,s]<-0 
        TdcRsp[i,1,s]<-0
        
        NdcRI[i,1,s]<-0
        TdcWI[i,1,s]<-0 
        TdcRI[i,1,s]<-0 
        TdcRspI[i,1,s]<-0
        
        NspR[i,1,s]<-0
        TspW[i,1,s]<-0
        TspR[i,1,s]<-0
        TspRsp[i,1,s]<-0
     
        for (j in 2:6){ 
        
        # Offshore salmon in the beginning of May in year 2
      		 
        #NccR[i,j,s] <- NlR[i,(j-1),s]*surv[i,(j-1),2,2]*kE[i,(j-1),2]	
			  NccR[i,j,s] <- NtR[i,(j-1),s]*surv[i,(j-1),6,2]#*kE[i,(j-1),6]	

        TccW[i,j,s]<-TlW[i,(j-1),s]*surv[i,(j-1),2,1]#*kE[i,(j-1),2]	
        TccR[i,j,s] <-TlR[i,(j-1),s]*surv[i,(j-1),2,2]#*kE[i,(j-1),2]	
		    TccRsp[i,j,s] <-TlRsp[i,(j-1),s]*surv[i,(j-1),2,2]#*kE[i,(j-1),2]	
            		 
        ##Immature salmon in coastal areas in the beginning of May       
            
        NdcRI[i,j,s] <- NccR[i,j,s]*(1-LR[(i+j-1),(j-1)])
          
        TdcWI[i,j,s] <- TccW[i,j,s]*(1-LW[(i+j-1),(j-1)])
        TdcRI[i,j,s] <- TccR[i,j,s]*(1-LR[(i+j-1),(j-1)])	
        TdcRspI[i,j,s] <- TccRsp[i,j,s]*(1-LR[(i+j-1),(j-1)])
          
        # Offshore salmon in the beginning of January 
       	    
        NdoR[i,j,s] <-  NdcRI[i,j,s]*surv[i,j,3,2]#*kE[i,j,3]

        TdoW[i,j,s] <- TdcWI[i,j,s]*surv[i,j,3,1]#*kE[i,j,3]
        TdoR[i,j,s] <- TdcRI[i,j,s]*surv[i,j,3,2]#*kE[i,j,3]
        TdoRsp[i,j,s]<- TdcRspI[i,j,s]*surv[i,j,3,2]#*kE[i,j,3]
            
        # Offshore salmon in the beginning of February
            
        NlR[i,j,s] <- NdoR[i,j,s]*surv[i,j,4,2]#*kE[i,j,4]	
	   
        TlW[i,j,s] <- TdoW[i,j,s]*surv[i,j,4,1]#*kE[i,j,4]	 
        TlR[i,j,s] <- TdoR[i,j,s]*surv[i,j,4,2]#*kE[i,j,4]	
			  TlRsp[i,j,s] <- TdoRsp[i,j,s]*surv[i,j,4,2]#*kE[i,j,4]	
                 												 
		# Offshore salmon in the beginning of April
        
        NtR[i,j,s] <- NlR[i,j,s]*surv[i,j,2,2]#*kE[i,j,2]															 
        
		#Salmon in coastal areas in the beginning of May.     #Mature
            
        NdcR[i,j,s] <- NccR[i,j,s]*LR[(i+j-1),(j-1)]
       
        TdcW[i,j,s] <- TccW[i,j,s]*LW[(i+j-1),(j-1)]                 
        TdcR[i,j,s] <- TccR[i,j,s]*LR[(i+j-1),(j-1)]	
        TdcRsp[i,j,s] <- TccRsp[i,j,s]*LR[(i+j-1),(j-1)]  
                          
       	# Salmon in the coastal area in the beginning of June 
      		  
        NcR[i,j,s] <- NdcR[i,j,s]*survdc[i,j,2,s]#*kEdc[i,j]	
             
        TcW[i,j,s] <- TdcW[i,j,s]*survdc[i,j,1,s]#*kEdc[i,j]	         
        TcR[i,j,s] <- TdcR[i,j,s]*survdc[i,j,2,s]#*kEdc[i,j]	
        TcRsp[i,j,s] <- TdcRsp[i,j,s]*survdc[i,j,2,s]#*kEdc[i,j]	
             
        # Salmon in the river in the beginning of August              #Mature
		        
        NrR[i,j,s]<- NcR[i,j,s]*survc[i,j,2,s]#*kEc[i,j]
 
        TrW[i,j,s] <- TcW[i,j,s]*survc[i,j,1,s]#*kEc[i,j]	
        TrR[i,j,s]<- TcR[i,j,s]*survc[i,j,2,s]#*kEc[i,j]	             #s stands for AU here
        TrRsp[i,j,s] <- TcRsp[i,j,s]*survc[i,j,2,s]#*kEc[i,j]	 

        # Number of spawners in the river in the beginning of October    #Mature
            
        NspR[i,j,s]  <- NrR[i,j,s]*surv[i,j,5,2]#*kE[i,j,5]	
		        
			  TspW[i,j,s]<-TrW[i,j,s]*surv[i,j,5,1]#*kE[i,j,5]		   #rivHR change needed? see TcW above
        TspR[i,j,s]  <- TrR[i,j,s]*surv[i,j,5,2]#*kE[i,j,5]		
        TspRsp[i,j,s] <- TrRsp[i,j,s]*(1-HrW[i,j])* exp(-2*MR/Tstep) #*kE[i,j,5]		
        }			
    }
}
                                      
for (i in 1:m){                    ### Tagging data likelihoods ### 

# for(j in 1:1){
#   HlW[i,j]<-1-exp(-qlW1[1]*El[i,j]) 
#   HlR[i,j]<-1-exp(-qlR1[1]*El[i,j]) 
# }
#  for(j in 2:5){
#   HlW[i,j]<-1-exp(-qlW[i,j]*El[i,j]) 
#   HlR[i,j]<-1-exp(-qlR[i,j]*El[i,j]) 
# }   
    for(j in 1:5){     #check - seems ok because all immature fish
    
    	# Offshore driftnet fishery
   	 	  
        TdoWtot[i,j]<- sum(TdoW[i,j, ])  
		    TdoRtot[i,j] <- sum(TdoR[i,j, ]) + sum(TdoRsp[i,j, ])

        HdoW[i,j] <- 1-exp(-qdW[i,j]*Edo[i,j]) 
        HdoR[i,j] <- 1-exp(-qdR[i,j]*Edo[i,j]) 

        cdo_predW[i,j] <- TdoWtot[i,j]*HdoW[i,j]*reportd*Tretain        
        psi_doW[i,j]<-rdW[j]/(rdW[j]+cdo_predW[i,j])
        cdo_ObsW[i,j] ~ dnegbin(psi_doW[i,j], rdW[j])	
            
        cdo_predR[i,j] <- TdoRtot[i,j]*HdoR[i,j]*reportd*Tretain      
        psi_doR[i,j]<-rdR[j]/(rdR[j]+cdo_predR[i,j])
        cdo_ObsR[i,j] ~ dnegbin(psi_doR[i,j], rdR[j])	

		  # Offshore longline fishery 
   
        TlWtot[i,j]<- sum(TlW[i,j, ]) 
        TlRtot[i,j] <- sum(TlR[i,j, ]) + sum(TlRsp[i,j, ])	

        HlW[i,j]<-1-exp(-qlW[i,j]*El[i,j]) 
        HlR[i,j]<-1-exp(-qlR[i,j]*El[i,j]) 
        
        cl_predW[i,j] <- TlWtot[i,j]*HlW[i,j] * reportl             
        psi_lW[i,j]<-rlW[j]/(rlW[j]+cl_predW[i,j])               
        cl_ObsW[i,j] ~ dnegbin(psi_lW[i,j], rlW[j])	
         
        cl_predR[i,j] <- TlRtot[i,j]*HlR[i,j] * reportl	  #changed to HlR from HlW for j=2:6 in the WinBUGS code       
        psi_lR[i,j]<-rlR[j]/(rlR[j]+cl_predR[i,j])
        cl_ObsR[i,j] ~ dnegbin(psi_lR[i,j], rlR[j])	
        
        ################## Total catch calculations ######################
        
        NdoW_all[i,j]<-sum(NdoW[i,j,1:stocks])
        NlW_all[i,j]<- sum(NlW[i,j,1:stocks])
		    NtW_all[i,j]<- sum(NtW[i,j,1:stocks])									 
        
        NdoR_all[i,j]<- sum(NdoR[i,j,])+sum(NdoRsp[i,j,1:2])
        NlR_all[i,j]<- sum(NlR[i,j,])+sum(NlRsp[i,j,1:2])
        NtR_all[i,j]<- sum(NtR[i,j,])+sum(NtRsp[i,j,1:2])
 
        # Estimated catches of non-returning salmon in the offshore fishery
        nco_W[i,j] <- PropCW[i] * (HdoW[i,j]*NdoW_all[i,j]+HlW[i,j]*NlW_all[i,j])   
        nco_R[i,j] <- PropCR[i] * (HdoR[i,j]*NdoR_all[i,j]+HlR[i,j]*NlR_all[i,j])   
        nco[i,j] <- nco_W[i,j] + 	nco_R[i,j] 	
		
        # Estimated catches separately for trolling, longline and driftnet 
        nc_otr[i,j] <- PropCW[i] * (HtW[i,j]*NtW_all[i,j]) + PropCR[i] *(HtR[i,j]*NtR_all[i,j])	    #PropCW and PropCR from PropAU16 data file
        nc_otrW[i,j] <- PropCW[i] * (HtW[i,j]*NtW_all[i,j]) #wild only 2024																																			  
        nc_oll[i,j] <- PropCW[i] * (HlW[i,j]*NlW_all[i,j]) + PropCR[i] *(HlR[i,j]*NlR_all[i,j])	    #PropCW and PropCR from PropAU16 data file
        nc_odn[i,j] <- PropCW[i] * (HdoW[i,j]*NdoW_all[i,j]) + PropCR[i] * (HdoR[i,j]*NdoR_all[i,j]) 
        
  	}    
   
    HdcW[i,1]<-0
    HdcR[i,1]<-0
   
   	for(j in 2:6){ 
       HdcW[i,j] <- 1-exp(-qdW[i,j-1]*Edc[i,j])   #WinBUGS j 2:6  HdcW[i,j-1] <- 1-exp(-qdW[j-1]*Edc[i,j]) 
   	   HdcR[i,j] <- 1-exp(-qdR[i,j-1]*Edc[i,j]) 
          
       cdc_ObsW[i,j] ~ dnegbin(psi_dcW[i,j], rdW[j])
       cdc_ObsR[i,j] ~ dnegbin(psi_dcR[i,j], rdR[j])	 	
    }
    
    for (j in 1:6){
     
     # Coastal driftnet  fishery         
   
		    TdcWtot[i,j]<- sum(TdcW[i,j, ])    
        TdcRtot[i,j] <- sum(TdcR[i,j, ]) + sum(TdcRsp[i,j, ])	     
            
        cdc_predW[i,j] <- TdcWtot[i,j]*HdcW[i,j]*reportd*Tretain           
        psi_dcW[i,j]<-rdW[j]/(rdW[j]+cdc_predW[i,j])
   
        cdc_predR[i,j] <- TdcRtot[i,j]*HdcR[i,j]*reportd*Tretain     
        psi_dcR[i,j]<-rdR[j]/(rdR[j]+cdc_predR[i,j])
 

    # River fishery
    	
        TrWtot[i,j]<- sum(TrW[i,j, ])  # Sum of tagged salmon over k areas
		    TrRsptot[i,j]<- sum(TrRsp[i,j, ])  # Sum of tagged salmon over k areas			
		    TrRtot[i,j] <- sum(TrR[i,j, ]) # Sum of tagged salmon over k areas	
        
        cr_predW[i,j] <- TrWtot[i,j]* HrW[i,j]* reportrW      #rivHR change needed?   
        psi_rW[i,j]<-rrW[j]/(rrW[j]+cr_predW[i,j])
        cr_ObsW[i,j] ~ dnegbin(psi_rW[i,j], rrW[j])
     
        cr_predRsp[i,j] <- TrRsptot[i,j]*HrW[i,j]* reportrW   #rivHR change needed?             # note uses reared surv but wild HR        
        psi_rRsp[i,j]<-rrRsp[j]/(rrRsp[j]+cr_predRsp[i,j])
        cr_ObsRsp[i,j] ~ dnegbin(psi_rRsp[i,j], rrRsp[j])
                
        cr_predR[i,j] <- TrRtot[i,j]*HrR[i,j]*reportrR        
        psi_rR[i,j]<-rrR[j]/(rrR[j]+cr_predR[i,j])
        cr_ObsR[i,j] ~ dnegbin(psi_rR[i,j], rrR[j])
    
    # Coastal fishery

      	cc_predW[i,j,1] <- HcW[i,j,1]*reportc*reportcAdj[i,j]* TcW[i,j,1]	   #reportcAdj from the Rest data file        
        psi_cW[i,j,1]<-rcW[j]/(rcW[j]+cc_predW[i,j,1])
        cc_ObsW[i,j,1] ~ dnegbin(psi_cW[i,j,1], rcW[j])	              # MSW AU 1
		 
 	      for(au in 1:AUS){   
            
            TcRtot[i,j,au]<- TcR[i,j,au]+TcRsp[i,j,au]
            
            HcW[i,j,au] <- (1-exp(-(F_sea[i,j,1,au])))	
            HcR[i,j,au] <- (1-exp(-(F_sea[i,j,2,au])))
        
            cc_predR[i,j,au] <- HcR[i,j,au]*reportc*reportcAdj[i,j] * TcRtot[i,j,au]        
            psi_cR[i,j,au]<-rcR[j]/(rcR[j]+cc_predR[i,j,au])

        }
        for(au in 1:3){ 
            cc_ObsR[i,j,au] ~ dnegbin(psi_cR[i,j,au], rcR[j])
        }
        ################## Total catch calculations ######################
        
        # Morrum and Eman not included why not included in river?

        NrW_all[i,j]<- sum(NladderW[i,j,avail_r]*(HrW[i,j]*rivHR[(i+j-1),avail_r]))+sum(NladderRsp[i,j,1:2]*(HrW[i,j]*rivHR[(i+j-1),1:2]))	  #avail_r 1:13, 16 
        NdcW_all[i,j]<-sum(NdcW[i,j,avail_dc])		                  #1:13 ,16
        
        NrR_all[i,j]<- sum(NrR[i,j,])
        NdcR_all[i,j]<- sum(NdcR[i,j,1:(AUS-1)])+sum(NdcRsp[i,j,1:2]) #not AU4     
       
        NcR_all[i,j,1]<- NcR[i,j,1]+sum(NcRsp[i,j,1:2])		
        NcR_all[i,j,2]<- NcR[i,j,2]  					
        NcR_all[i,j,3]<- NcR[i,j,3]  		
         
        # Estimated catches of wild and hatchery-reared salmon in the river
        ncr[i,j]<- NrW_all[i,j]+HrR[i,j]*NrR_all[i,j]
        
        # Estimated catches of salmon in the coastal areas
        for(s in 1:stocks){
            nccs[i,j,s]<-HcW[i,j,AU[s]]*NcW[i,j,s] 
        }
           
    ncc[i,j] <- sum(nccs[i,j,1:stocks]) + inprod(HcR[i,j,1:(AUS-1)],NcR_all[i,j,1:(AUS-1)])+ HdcW[i,j]*NdcW_all[i,j]+HdcR[i,j]*NdcR_all[i,j]  #not AU4	   
        
    }	#j       
}  #i


for(i in 1:5){             #move above later
    ncr_Tot[i]<-0
    ncc_Tot[i]<-0
    nco_Tot[i]<-0
    nct_Tot[i]<-0
    nctW_Tot[i]<-0
    nctW_rel[i]<-0

	nct_ObsTotX[i]<-0				 
    nco_ObsTotX[i]<-0
    ncc_ObsTotX[i]<-0
    ncr_ObsTotX[i]<-0
}
for (i in 6:(m-1)){ 

	nct_Tot[i] <- nc_otr[(i-1),2]+nc_otr[(i-2),3]+nc_otr[(i-3),4]+nc_otr[(i-4),5]   #Estimated total trolling catches
    nctW_Tot[i] <- nc_otrW[(i-1),2]+nc_otrW[(i-2),3]+nc_otrW[(i-3),4]+nc_otrW[(i-4),5] 
    nctW_rel[i] <-  nctW_Tot[i]*p.rel[i] 
    muCT[i] <-log(nct_Tot[i] /ureport_o[i]) - 0.5/tauCT	
    nct_ObsTot[i]~dlnorm(muCT[i], tauCT)
    nct_ObsTotX[i]~dlnorm(muCT[i], tauCT)																											 						 

    nco_Tot[i] <- nco[(i-1),2]+nco[(i-2),3]+nco[(i-3),4]+nco[(i-4),5]                 #Estimated total offshore catches Nl and Ndo
   	muCO[i] <-log(nco_Tot[i] /ureport_o[i]) - 0.5/tauCO	
	nco_ObsTot[i] ~dlnorm(muCO[i], tauCO)
    nco_ObsTotX[i] ~dlnorm(muCO[i], tauCO)
    
    nc_oll_Tot[i] <- nc_oll[(i-1),2]+nc_oll[(i-2),3]+nc_oll[(i-3),4]+nc_oll[(i-4),5]         #Estimated total offshore longline catches Nl only 
	nc_odn_Tot[i] <- nc_odn[(i-1),2]+nc_odn[(i-2),3]+nc_odn[(i-3),4]+nc_odn[(i-4),5]         #Estimated total offshore driftnet catches Ndo only
	    
    ncr_Tot[i] <- ncr[(i-1),2]+ncr[(i-2),3]+ncr[(i-3),4]+ncr[(i-4),5]+ ncr[(i-5),6]     # Estimated total river catches   Nr
	muCR[i] <- log(ncr_Tot[i] /ureport_r[i]) - 0.5/tauCR
    ncr_ObsTot[i] ~dlnorm(muCR[i], tauCR)	
    ncr_ObsTotX[i] ~dlnorm(muCR[i], tauCR)
  
    ncc_Tot[i] <- ncc[(i-1),2]+ncc[(i-2),3]+ncc[(i-3),4]+ncc[(i-4),5]+ ncc[(i-5),6]     # Estimated total coastal catches Nc Ndc
    muCC[i] <- log(ncc_Tot[i] /ureport_c[i]) - 0.5/tauCC	
    ncc_ObsTot[i] ~dlnorm(muCC[i], tauCC)
    ncc_ObsTotX[i] ~dlnorm(muCC[i], tauCC)
	  
     
    ######### Spawner counting observation models (Didson and fishladders) #########
     
    sp_count[i,1]~dbin(p.detect[i,1],NrWtot[i,1]) # Torne Didson count 
    sp_countX[i,1]~dbin(p.detect[i,1],NrWtot[i,1]) 
        
    #ladder_count[i,1]~dbin(p.ladder[i,1],NrWtot[i,1])   #Ume only (propn that finds the ladder)
        
    eta_star[i,1]<-N_sp_count[i,1]*(eta_msw[1]+1)/(eta_msw[1]+N_sp_count[i,1])-1    #N_sp_count=sp_count (observation)
    alpha_msw[i,1]<-probMSW[i,1]*eta_star[i,1]
    beta_msw[i,1]<-(1-probMSW[i,1])*eta_star[i,1]
    MSWprop[i,1]~dbeta(alpha_msw[i,1],beta_msw[i,1])
    
    sp_count[i,2]~dlnorm(muDS[i], tauDS) # Simojoki Didson count
    sp_countX[i,2]~dlnorm(muDS[i], tauDS) 
  
    #muDS[i]<-log(NrWtot[i,2]/coefDS)-0.5*(1/tauDS)
	  NrW_msw_Simo[i]<-round((NrW_msw[i,2]+NrRsp_msw[i,2])*1000)+1   
	 muDS[i]<-log(NrW_msw_Simo[i]/coefDS)-0.5*(1/tauDS) # Without hierarchy
   
   
    for(s in 3:stocks){
    
        sp_count[i,s]~dbin(p.detect[i,s],NrWtot[i,s]) # Didson count Torne, Simo, ladder count Kalix
        sp_countX[i,s]~dbin(p.detect[i,s],NrWtot[i,s]) 
        
        #ladder_count[i,s]~dbin(p.ladder[i,s],NrWtot[i,s])   #Ume only (propn that finds the ladder)
        mu_ladder[i,s]<-log(NladderW_tot[i,s])-0.5/tau_ladder
        ladder_count[i,s]~dlnorm(mu_ladder[i,s],tau_ladder)
        
        eta_star[i,s]<-N_sp_count[i,s]*(eta_msw[s]+1)/(eta_msw[s]+N_sp_count[i,s])-1    #N_sp_count=sp_count (observation)
		alpha_msw[i,s]<-probMSW[i,s]*eta_star[i,s]
        beta_msw[i,s]<-(1-probMSW[i,s])*eta_star[i,s]
        MSWprop[i,s]~dbeta(alpha_msw[i,s],beta_msw[i,s])
      
    }
   	
 
        WGrilse[i,1]~dbin(PropWgrilse[i,1], Grilse_all[i,1]) #proportion of wild grilse, Torne only
        WMSW[i,1]~dbin(PropWMSW[i,1], MSW_all[i,1])
    
    

    Wprop[i,1]<-nco_W[i,2]/(nco_W[i,2]+nco_R[i,2])  
    Wprop[i,2]<-nco_W[i,3]/(nco_W[i,3]+nco_R[i,3])

    for (j in 1:2){     #1SW (j=2 elsewhere) and 2SW (j=3 elsewhere) salmon
       eta_wr[i,j]<-(Wprop[i,j]*(1-Wprop[i,j]))/(sd_wr[i,j]*sd_wr[i,j])#-1  
  
       log_WpropObs[i,j]~dnorm(log(Wprop[i,j])-0.5*log(1/(Wprop[i,j]*eta_wr[i,j])+1),
              1/log(1/(Wprop[i,j]*eta_wr[i,j])+1))
       log_RpropObs[i,j]~dnorm(log(1-Wprop[i,j])-0.5*log(1/((1-Wprop[i,j])*eta_wr[i,j])+1),
              1/log(1/((1-Wprop[i,j])*eta_wr[i,j])+1))
    } 
    
    for(rs in 1:rstocks){  #1=Lule?lven, 2=Dal?lven
   
      #  NrRtot[i,rs]<-round((NrR[(i-1),2,AUR[rs]]*RProp[(i-1),rs]+NrR[(i-2),3,AUR[rs]]*RProp[(i-2),rs]+
#		NrR[(i-3),4,AUR[rs]]*RProp[(i-3),rs]+NrR[(i-4),5,AUR[rs]]*RProp[(i-4),rs]+
#		NrR[(i-5),6,AUR[rs]]*RProp[(i-5),rs])*1000-CatchR[i,rs])
   
           NrRtot.vul[i,rs]<-(NrR[(i-1),2,AUR[rs]]*RProp[(i-1),rs]+NrR[(i-2),3,AUR[rs]]*RProp[(i-2),rs]+
		NrR[(i-3),4,AUR[rs]]*RProp[(i-3),rs]+NrR[(i-4),5,AUR[rs]]*RProp[(i-4),rs]+
		NrR[(i-5),6,AUR[rs]]*RProp[(i-5),rs])*1000
        
        HRNrR[i,rs]<-min(CatchR[i,rs]/NrRtot.vul[i,rs],0.9999)
        NrRtot[i,rs]<-round(NrRtot.vul[i,rs]*(1-HRNrR[i,rs]))
        
        TrapTot[i,rs]~dbin(pTrap[i,rs],NrRtot[i,rs])
        pTrap[i,rs]~dbeta(aTrap[rs],bTrap[rs])
    }
}

for(i in 1:5){
 	 NLuleRec[i]~dbin(pTrap[yLule[i],1],NLuleRel[i])
}	

 
###################MORTALITY AND SURVIVAL RATES#####################

## Instantaneous adult natural mortality rate

MW ~dlnorm(-2.3, 4.3)T(0.025,0.35)
MR ~dlnorm(-2.3, 4.3)T(0.025,0.35)

early_MpsW~dlnorm(0.23,19)T(0.5,5)

for(i in 1:4){
    mu_MpsW[i] <- log(early_MpsW)
}
for(i in 5:(m+proj_years)){
    mu_MpsW[i] <- log(mean(MpsW[(i-4):(i-1)])) - 0.5 / tau_MpsW
}

Reff_mu~dbeta(0.9,1.8)
Reff_eta~dunif(0.001,0.5) 
Ra<-Reff_mu/Reff_eta
Rb<-(1-Reff_mu)/Reff_eta   
  
tau_MpsW <- 1/log(((CV_MpsW)* (CV_MpsW)) + 1)
CV_MpsW~dunif(0.001,1) 

for(i in 1:(m+proj_years)){

    MpsW[i]~dlnorm(mu_MpsW[i],tau_MpsW)     ## Post-smolt M for wild salmon < post-smolt M for reared salmon
    survMpsW[i]<-exp(-MpsW[i])
    
    RMps[i]~dbeta(Ra,Rb)T(0.01,0.99)
    ReffectMps[i] <- (RMps[i] * 1.5)+1                 # hatchery-reared effect between 1 and 2.5

    MpsR[i] <- MpsW[i] * ReffectMps[i]
    survMpsR[i]<-exp(-MpsR[i])

    surv[i,1,1,1]<-exp(-MpsW[i]/Tstep) 		          #survmortW
    surv[i,1,1,2]<-exp(-MpsR[i]/Tstep) 	            #survmortR
                                           
	 # surv[i,1,2,1] <- exp(-qlW[1]*El[i,1]) * exp(-3*MpsW[i]/Tstep) 	       #survlW  
    #surv[i,1,2,2] <- exp(-qlR[1]*El[i,1]) * exp(-3*MpsR[i]/Tstep)          #survlR
    surv[i,1,2,1] <- exp(-qlW[i,1]*El[i,1]) * exp(-2*MpsW[i]/Tstep) 	       #survlW  
    surv[i,1,2,2] <- exp(-qlR[i,1]*El[i,1]) * exp(-2*MpsR[i]/Tstep)          #survlR
    
    surv[i,1,3,1] <- 0.99	      #survnohW (not used anywhere, 1 causes error message in process errors)
    surv[i,1,3,2] <- 0.99
    	
    surv[i,1,4,1] <- exp(-qdW[i,1]*Edo[i,1]) * exp(-MpsW[i]/Tstep)         #survdoW
	  surv[i,1,4,2] <- exp(-qdR[i,1]*Edo[i,1]) * exp(-MpsR[i]/Tstep) 	 
     
    surv[i,1,5,1]<-(1-HrW[i,1])*exp(-MpsW[i]*sealMort[i,1,1]/Tstep)    #survrW apply AU 1 seal M 
    surv[i,1,5,2]<-(1-HrR[i,1])*exp(-MpsR[i]*sealMort[i,1,1]/Tstep)    #survrR
    
    for(s in 1:stocks){
       surv.riv[i,1,s] <- (1-HrW[i,1]*rivHR[(i+1-1),s])*exp(-MpsW[i]*sealMort[i,1,1]/Tstep) 
   }		                

    surv[i,1,6,1]<-(1-HtW[i,1]*(p.rel[(i+1-1)]*p.mort+(1-p.rel[(i+1-1)])))* exp(-MpsW[i]/Tstep) 
	   	         
    surv[i,1,6,2]<-(1-HtR[i,1]) * exp(-MpsR[i]/Tstep)	
    		
																										   
    
    for (au in 1:3){	
    
        F_sea[i,1,1,au]<-qctnW[1,au]*Ectn[i,1,au]+qcgnW[1,au]*Ecgn[i,1,au]														
        survc[i,1,1,au] <- exp(-F_sea[i,1,1,au])* exp(-(6*MpsW[i]/Tstep))   
        
        F_sea[i,1,2,au]<-qctnR[1,au]*Ectn[i,1,au]+qcgnR[1,au]*Ecgn[i,1,au]		
        survc[i,1,2,au] <-exp(-F_sea[i,1,2,au]) * exp(-(6*MpsR[i]/Tstep))   
        
        survdc[i,1,1,au] <- 0.99    #not used anywhere
        survdc[i,1,2,au] <- 0.99
    }
    
    F_sea[i,1,1,4]<-0
    F_sea[i,1,2,4]<-0	
    
    survc[i,1,1,4] <-exp(-(6*MpsW[i]/Tstep))      #wild    
    survc[i,1,2,4] <-exp(-(6*MpsR[i]/Tstep))      #reared 
    
    survdc[i,1,1,4] <- 0.99   
    survdc[i,1,2,4] <- 0.99
    
    for(j in 2:6){
    
        surv[i,j,1,1]<-0.99 		          #survmortW  not used anywhere
        surv[i,j,1,2]<-0.99 	            #survmortR
        				  
        #surv[i,j,2,1] <- exp(-qlW[j]*El[i,j]) * exp(-3*MW/Tstep)                        #survlW  
		    #surv[i,j,2,2] <- exp(-qlR[j]*El[i,j]) * exp(-3*MR/Tstep)		 
        surv[i,j,2,1] <- exp(-qlW[i,j]*El[i,j]) * exp(-2*MW/Tstep)                        #survlW  
		    surv[i,j,2,2] <- exp(-qlR[i,j]*El[i,j]) * exp(-2*MR/Tstep)
			
        surv[i,j,3,1] <- exp(-8*MW/Tstep)	#QUERY 2 time steps in process error calc!	      #survnohW
		    surv[i,j,3,2] <- exp(-8*MR/Tstep)
   	
        surv[i,j,4,1] <- exp(-qdW[i,j]*Edo[i,j]) * exp(-MW/Tstep)       #survdoW
	      surv[i,j,4,2] <- exp(-qdR[i,j]*Edo[i,j]) * exp(-MR/Tstep) 
             
        surv[i,j,5,1] <- (1-HrW[i,j])*exp(-2*MW/Tstep)     #survrW                  
        surv[i,j,5,2] <- (1-HrR[i,j])*exp(-2*MR/Tstep)  	
        
        for(s in 1:stocks){
           surv.riv[i,j,s] <- (1-HrW[i,j]*rivHR[(i+j-1),s])*exp(-2*MW/Tstep)     #survrW
        }		                

        surv[i,j,6,1]<-(1-HtW[i,j]*(p.rel[(i+j-1)]*p.mort+(1-p.rel[(i+j-1)]))) * exp(-MW/Tstep) 	          	         
        surv[i,j,6,2]<-(1-HtR[i,j]) * exp(-MR/Tstep)											   
													  
        for(au in 1:3){
      		  
            # Survival from natural and fishing mortality during June and July			
            F_sea[i,j,1,au]<-qctnW[j,au]*Ectn[i,j,au]+qcgnW[j,au]*Ecgn[i,j,au]		
  	        survc[i,j,1,au] <-exp(-F_sea[i,j,1,au]) * exp(-(2*MW*sealMort[i,j,au]/Tstep)) 
      
            # Survival from natural and fishing mortality during June and July	
            F_sea[i,j,2,au]<-qctnR[j,au]*Ectn[i,j,au]+qcgnR[j,au]*Ecgn[i,j,au]				
	          survc[i,j,2,au] <-exp(-F_sea[i,j,2,au])  * exp(-(2*MR*sealMort[i,j,au]/Tstep)) 
                     
            survdc[i,j,1,au] <- exp(-qdW[i,j-1]*Edc[i,j]) * exp(-MW/Tstep)      
		        survdc[i,j,2,au] <- exp(-qdR[i,j-1]*Edc[i,j]) * exp(-MR/Tstep)
        }
        
        F_sea[i,j,1,4]<-0
        F_sea[i,j,2,4]<-0
         
        survc[i,j,1,4] <- exp(-(2*MW*sealMort[i,j,4]/Tstep))   #wild   #seal M AU4 = 1   
        survc[i,j,2,4] <- exp(-(2*MR*sealMort[i,j,4]/Tstep))   #reared 
        
        survdc[i,j,1,4] <- exp(-MW/Tstep)    
        survdc[i,j,2,4] <- exp(-MR/Tstep)  	      					
    }
}

##################################### Process errors ###########################

# for(i in 1:(m+proj_years)){
#     for(j in 1:6){           
#         for(ii in 1:4){
# 
#             #1 survmort        
#             #2 survl          
#             #3 survnoh       
#             #4 survdo 
#             
#             zz[i,j,ii]~ dgamma(2, 50)I( , maxvar)
#         
#             for(ij in 1:2){  #reared or wild
#         
#                 kk[i,j,ii,ij] <- step(surv[i,j,ii,ij] - 0.5) # if surv >=0.5 then kk=1 else kk=0 
#                 maxE[i,j,ii,ij] <- ((1 / surv[i,j,ii,ij]) * kk[i,j,ii,ij]) + (2 * (1 - kk[i,j,ii,ij]))	
#                 minE[i,j,ii,ij] <- (1 - (maxE[i,j,ii,ij] - 1))  
#                 vvE[i,j,ii,ij] <- pow((maxE[i,j,ii,ij] - minE[i,j,ii,ij]), 2) / 12   # variance depending on survival 
#             }
#    	    
#             minv[i,j,ii]<-min(vvE[i,j,ii,1],vvE[i,j,ii,2])      #compare W with R
#             vE[i,j,ii]<-mon_E[j,ii]*zz[i,j,ii]/12
#             maxError[i,j,ii] <- 1 + pow((3 * min(vE[i,j,ii], minv[i,j,ii])), 0.5)   #variance depending on time step
#             minError[i,j,ii] <- 1 - pow((3 * min(vE[i,j,ii], minv[i,j,ii])), 0.5) 
#                 
#             kE[i,j,ii] ~ dunif(minError[i,j,ii], maxError[i,j,ii])
#         }
#         
#         #5 survr  
#         zzr[i,j]~ dgamma(2, 50)I( , maxvar)         
#         
#         for(s in 1:stocks){  
#         
#             kkrw[i,j,s] <- step(surv.riv[i,j,s]*surv_migr[(i+j-1),s] - 0.5) # if surv >=0.5 then kk=1 else kk=0 
#             maxErw[i,j,s] <- ((1 / (surv.riv[i,j,s]*surv_migr[(i+j-1),s])) * kkrw[i,j,s]) + (2 * (1 - kkrw[i,j,s]))
#             minErw[i,j,s] <- (1 - (maxErw[i,j,s] - 1))
#             vvErw[i,j,s] <- pow((maxErw[i,j,s] - minErw[i,j,s]), 2) / 12   # variance depending on survival 
# 
#         }
# 
#         kkrr[i,j] <- step(surv[i,j,5,2] - 0.5) # if surv >=0.5 then kk=1 else kk=0 
#         maxErr[i,j] <- ((1 / surv[i,j,5,2]) * kkrr[i,j]) + (2 * (1 - kkrr[i,j]))
#         minErr[i,j] <- (1 - (maxErr[i,j] - 1))
#         vvErr[i,j] <- pow((maxErr[i,j] - minErr[i,j]), 2) / 12   # variance depending on survival 
# 
#         minvr[i,j]<-min(vvErw[i,j,1:stocks],vvErr[i,j])      #compare W with R
#         vEr[i,j]<-mon_E[j,5]*zzr[i,j]/12
#         maxErrorr[i,j] <- 1 + pow((3 * min(vEr[i,j], minvr[i,j])), 0.5)   #variance depending on time step
#         minErrorr[i,j] <- 1 - pow((3 * min(vEr[i,j], minvr[i,j])), 0.5) 
#                 
#         kE[i,j,5] ~ dunif(minErrorr[i,j], maxErrorr[i,j])
#         
# 		# 6 survt
#         for(ii in 6:6){
#           zz[i,j,ii]~ dgamma(2, 50)I( , maxvar)
#           
#           for(ij in 1:2){  #reared or wild
#           
#           kk[i,j,ii,ij] <- step(surv[i,j,ii,ij] - 0.5) # if surv >=0.5 then kk=1 else kk=0 
#           maxE[i,j,ii,ij] <- ((1 / surv[i,j,ii,ij]) * kk[i,j,ii,ij]) + (2 * (1 - kk[i,j,ii,ij]))	
#           minE[i,j,ii,ij] <- (1 - (maxE[i,j,ii,ij] - 1))  
#           vvE[i,j,ii,ij] <- pow((maxE[i,j,ii,ij] - minE[i,j,ii,ij]), 2) / 12   # variance depending on survival 
#           }
#           
#           minv[i,j,ii]<-min(vvE[i,j,ii,1],vvE[i,j,ii,2])      #compare W with R
#           vE[i,j,ii]<-mon_E[j,ii]*zz[i,j,ii]/12
#           maxError[i,j,ii] <- 1 + pow((3 * min(vE[i,j,ii], minv[i,j,ii])), 0.5)   #variance depending on time step
#           minError[i,j,ii] <- 1 - pow((3 * min(vE[i,j,ii], minv[i,j,ii])), 0.5) 
#           
#           kE[i,j,ii] ~ dunif(minError[i,j,ii], maxError[i,j,ii])
#         }		 

#         #########################################################################
#         #survc      
#         #survdc (was surv6)      
#         
#         zzc[i,j]~ dgamma(2, 50)I( , maxvar)         #survc
#         zzdc[i,j]~ dgamma(2, 50)I( , maxvar)         #survdc
# 
#         
#         for(ij in 1:2){  #reared or wild
#            for(au in 1:AUS){  
#               kkc[i,j,ij,au] <- step(survc[i,j,ij,au] - 0.5) # if surv >=0.5 then kk=1 else kk=0 
#               kkdc[i,j,ij,au] <- step(survdc[i,j,ij,au] - 0.5)
#               maxEc[i,j,ij,au] <- ((1 / survc[i,j,ij,au]) * kkc[i,j,ij,au]) + (2 * (1 - kkc[i,j,ij,au]))
#               maxEdc[i,j,ij,au] <- ((1 / survdc[i,j,ij,au]) * kkdc[i,j,ij,au]) + (2 * (1 - kkdc[i,j,ij,au]))	
#               minEc[i,j,ij,au] <- (1 - (maxEc[i,j,ij,au] - 1))
#               minEdc[i,j,ij,au] <- (1 - (maxEdc[i,j,ij,au] - 1))  
#               vvEc[i,j,ij,au] <- pow((maxEc[i,j,ij,au] - minEc[i,j,ij,au]), 2) / 12   # variance depending on survival 
#               vvEdc[i,j,ij,au] <- pow((maxEdc[i,j,ij,au] - minEdc[i,j,ij,au]), 2) / 12   # variance depending on survival 
#            }
#         }
#                                                                          
#         minvc[i,j]<-min(vvEc[i,j,1:2,1:AUS])      #compare W with R
#         minvdc[i,j]<-min(vvEdc[i,j,1:2,1:AUS])      #compare W with R
#             
#         vEc[i,j]<-mon_Ec[j]*zzc[i,j]/12               #variance depending on time step
#         vEdc[i,j]<-mon_Edc[j]*zzdc[i,j]/12            #variance depending on time step
# 
#         maxErrorc[i,j] <- 1 + pow((3 * min(vEc[i,j], minvc[i,j])), 0.5)   
#         maxErrordc[i,j] <- 1 + pow((3 * min(vEdc[i,j], minvdc[i,j])), 0.5)    
#         
#         minErrorc[i,j] <- 1 - pow((3 * min(vEc[i,j], minvc[i,j])), 0.5)
#         minErrordc[i,j] <- 1 - pow((3 * min(vEdc[i,j], minvdc[i,j])), 0.5)  
#                 
#         kEc[i,j] ~ dunif(minErrorc[i,j], maxErrorc[i,j])
#         kEdc[i,j] ~ dunif(minErrordc[i,j], maxErrordc[i,j])
# 
#     }
# }
# 
# 
# maxvar <- 0.5 / (12)  # max variance if sur=0.5			
# 
###################################### Catchabilities & harvest rates ####################################

# Mean reverting AR(1)-model for 1SW (==MSW) harvest rate in offshore trolling fishery
# =======================================================================
# Note that autocorrelation takes place between years but in HtW[i,j] index i is smolt cohort 

# Recreational offshore trolling 
for(i in 1:(m+4)){ # i: smolt cohort
  # Post-smolts are assumed released without mortality
  HtW[i,1]<-0 
  HtR[i,1]<-0
  
  # i+1=i+2-1: cohort i age 2 transformed to year i+1
  logit(HtW[i,2])<-logitHtW2[i+1] 
  logit(HtR[i,2])<-logitHtW2[i+1]#<-logitHtR2[i+1] 
}

 for(i in 1:m){ # i: smolt cohort 
  for(j in 3:6){ 
  # Harvest rate of MSW salmon of different age must be the same within years:
  #
  # The cohort i salmon are of age 3 in year y=i+3-1=i+2 and 
  # in the same year, the age 2 salmon originate from cohort y-1=i+2-1= i+1 (=i+3-2)
  #
  # The cohort i salmon are of age 4 in year y=i+4-1=i+3 and
  # in the same year the age 2 salmon originate from cohort y-1=i+3-1=i+2 (=i+4-2)
  # and so on...
    HtW[i,j]<-HtW[i+j-2,2] 
    HtR[i,j]<-HtW[i,j]#HtR[i+j-2,2]
  }
}

for(i in 1:(m+4)){ # i: calendar year
 logitHtW2[i+1]~dnorm(mu_trW[i+1],tau_tr)
  mu_trW[i+1]<-phi_tr*logitHtW2[i]+(1-phi_tr)*mean_trW # mean reverting AR(1)

 #logitHtR2[i+1]~dnorm(mu_trR[i+1],tau_tr)
 # mu_trR[i+1]<-phi_tr*logitHtR2[i]+(1-phi_tr)*mean_trR

}

tau_tr<-1/((1-pow(phi_tr,2))*(sd_tr*sd_tr))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd_tr~dunif(0.01,1.6)
phi_tr~dunif(0,1) #positive autocorrelation
mean_trW~dnorm(0,0.39) # implies uniform[0,1] prior for mean harvest rate
#mean_trR~dnorm(0,0.39) 
logitHtW2[1]~dnorm(0,0.39) 
#logitHtR2[1]~dnorm(0,0.39)


for(i in 1:m){		  
    # Harvest rate of smolts in the river/river mouth		
	
    	HrW[i,1] <- 1-exp(-(qrW[1]*Er[i,1]))
    	HrR[i,1] <- 1-exp(-(qrR[1]*Er[i,1])) 	
    
    for (j in 2:3){ 
 	      HrR[i,j]<-1-exp(-(qrR[j]*Er[i,j]))	    # Harvest rate river fishery reared salmon
    }
    for (j in 4:6){ 
        HrW[i,j] <- HrW[i+j-3,3] #!!!! equal by year not by smolt cohort
        HrR[i,j]<-1-exp(-(qrR[j]*Er[i,j]))	 
    }
}
for(i in 1:(m+3)){ # HrW age 3 must go +3 further to match dimensions above		  
    for (j in 2:3){ 
 	      HrW[i,j]~dbeta(1.6,6.4)	# Harvest rate river fishery in rivers with natural reproduction
}}


for(i in (m+1):(m+proj_years)){
    for(j in 1:6){
        HrW[i,j]<-0 
        HrR[i,j]<-0
    }
}

  #post-smolts, j=1
  # Harvest rates in 1987
  # ==================
  HRR[1]  ~dbeta (1,20)	# Harvest rate smolts in river fishery
 	qrW[1] ~ dlnorm(mqr[1], tauqr)	# Catchability of wild smolts in the river			
	qrR[1] ~ dlnorm(mqr[1], tauqr)	# Catchability of reared smolts in the river			
	
	mqr[1] <- log(-log(1-HRR[1])/Er[1,1])	# Mean catchability river fishery		
  	
  HRL[1] ~ dbeta(1,20)   # Harvest rate of smolts longline fishery	
	HRD[1] ~ dbeta(1,20)   # Harvest rate of smolts in driftnet fishery
 	HRCTN[1,1]  ~dbeta (1,20)	# Harvest rate smolts in coastal trapnet fishery
	HRCGN[1,1] ~dbeta (1,20)	# Harvest rate smolts in coastal gillnet fishery

  qlW1~ dlnorm(mql[1], tauqd)	# Catchability of reproductive salmon in the longline fishery
  qlR1~ dlnorm(mql[1], tauqd)	# Catchability of non-reproductive salmon in the longline 
  for(i in 1:m){
     qlW[i,1]<-qlW1
 	   qlR[i,1]<-qlR1
 	}
	mql[1] <- log(-log(1-HRL[1])/El[1,1])	# Mean catchability coefficient longline fishery	
 
  qdW1 ~ dlnorm(mqd[1], tauqd)	# Catchability of reproductive salmon in the driftnet 	
  qdR1 ~ dlnorm(mqd[1], tauqd)	# Catchability of non-reproductive salmon in the driftnet 
  for(i in 1:m){
     qdW[i,1]<-qdW1
 	   qdR[i,1]<-qdR1
 	}
	# qdW[1] ~ dlnorm(mqd[1], tauqd)	# Catchability of reproductive salmon in the driftnet 	
  # qdR[1] ~ dlnorm(mqd[1], tauqd)	# Catchability of non-reproductive salmon in the driftnet 
  mqd[1] <- log(-log(1-HRD[1])/Edo[1,1])	# Mean catchability coefficient driftnet fishery	
 
	# Catchability coefficient of salmon by coastal fisheries		
	qctnW[1,1] ~ dlnorm(mqctn[1,1], tauqctn)	# Catchability wild salmon coastal trapnet
	qctnR[1,1] ~ dlnorm(mqctn[1,1], tauqctn)	# Catchability reared salmon coastal trapnet
	mqctn[1,1] <- log(-log(1-HRCTN[1,1])/Ectn[1,1,1])# Mean catchability coastal trapnet fishery

	qcgnW[1,1] ~ dlnorm(mqcgn[1,1], tauqcgn)	# Catchability wild salmon coastal gillnet	
	qcgnR[1,1] ~ dlnorm(mqcgn[1,1], tauqcgn)	# Catchability reared salmon coastal gillnet 
	mqcgn[1,1] <- log(-log(1-HRCGN[1,1])/Ecgn[1,1,1])# Mean catchability coastal gillnet fishery
	
  for (au in 2:3){
     
    HRCTN[1,au]  ~dbeta (1,20)	# Harvest rate smolts in coastal trapnet fishery
   	HRCGN[1,au] ~dbeta (1,20)	# Harvest rate smolts in coastal gillnet fishery
		
    qctnR[1,au] <- qctnR[1,1]
		mqctn[1,au] <- mqctn[1,1]		
		qctnW[1,au] <- (qctnW[1,1] / qctnR[1,1] ) * qctnR[1,au]

		qcgnR[1,au] <- qcgnR[1,1]
		mqcgn[1,au] <- mqcgn[1,1]			
		qcgnW[1,au] <- (qcgnW[1,1] / qcgnR[1,1] ) * qcgnR[1,au]
  }

# Grilse (j=2) and 2SW salmon	(j=3)


#qlW[2] ~ dlnorm(mql[2], tauql)	# Catchability of reproductive salmon in the longline fishery
#qlR[2] ~ dlnorm(mql[2], tauql)	# Catchability of non-reproductive salmon in the longline 
#mql[2] <- log(-log(1-HRL[2])/El[1,1])	# Mean catchability coefficient longline fishery	

# Autocorrelation model for 2SW (==MSW) catchability in longline fishery
# =======================================================================
# Note that autocorrelation takes place between years but in qlW[i,j] index i is smolt cohort 
for(i in 1:(m+proj_years+3)){

  # i+1=i+2-1: cohort i age 2 transformed to year i+1
  logit(qlW[i,2])<-logit_qlW[i+1]
  logit(qlR[i,2])<-logit_qlW[i+1]#logit_qlR[i+1]

  # mean reverting AR(1) (index i is year)
  logit_qlW[i+1]~dnorm(mu_qlW[i+1], tau_ql)
  mu_qlW[i+1]<-phi_ql*logit_qlW[i]+(1-phi_ql)*mean_qlW # mean reverting AR(1)

  #logit_qlR[i+1]~dnorm(mu_qlR[i+1], tau_ql)
  #mu_qlR[i+1]<-phi_ql*logit_qlR[i]+(1-phi_ql)*mean_qlR # mean reverting AR(1)
  
}
logit_qlW[1]~dnorm(0,0.39) # implies uniform[0,1] prior for initial catchability
#logit_qlR[1]~dnorm(0,0.39) # implies uniform[0,1] prior for initial catchability

for(i in 1:(m+proj_years)){
  for(j in 3:5){
    # Same catchability of MSW salmon in the longline fishery
    qlW[i,j] <- qlW[i+j-2,2]	
    qlR[i,j] <-qlW[i,j]# qlR[i+j-2,2]
  }
}

mean_qlW~dnorm(0,0.39) # implies uniform[0,1] prior for mean catchability
#mean_qlR~dnorm(0,0.39) # implies uniform[0,1] prior for mean catchability
tau_ql<-1/((1-pow(phi_ql,2))*(sd_ql*sd_ql))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd_ql~dunif(0.01,1.6)
phi_ql~dunif(0,1) #positive autocorrelation


# Mean reverting AR(1)-model for catchability in driftnet fisheries (both offshore and coastal)
# =======================================================================

for(j in 2:3){
  for(i in 1:(m+proj_years+3)){
    # i+j-1: cohort i age j transformed to year
    logit(qdW[i,j])<-logit_qdW[i+j-1,j]
    logit(qdR[i,j])<-logit_qdW[i+j-1,j]#logit_qdR[i+j-1,j]
  }

  for(i in 1:(m+proj_years+4)){
    # mean reverting AR(1) (index i is year)
    logit_qdW[i+1,j]~dnorm(mu_qdW[i+1,j], tau_qd)
    mu_qdW[i+1,j]<-phi_qd*logit_qdW[i,j]+(1-phi_qd)*mean_qdW[j]
    
    #logit_qdR[i+1,j]~dnorm(mu_qdR[i+1,j], tau_qd)
    #mu_qdR[i+1,j]<-phi_qd*logit_qdR[i,j]+(1-phi_qd)*mean_qdR[j]
  }
  
  # implies uniform[0,1] prior for initial catchability
  logit_qdW[1,j]~dnorm(0,0.39) 
  #logit_qdR[1,j]~dnorm(0,0.39)
}
mean_qdW[2]~dnorm(0,0.39) # 2SW
#mean_qdW[1]<-mean_qdW[2]*eff_qd[1] # 1SW # this is dealt with in old way!
mean_qdW[3]<-mean_qdW[2]*eff_qd # MSW
##mean_qdR[3]~dnorm(0,0.39) # 2SW
##mean_qdR[2]<-mean_qdR[3]*eff_qd[1] # 1SW
##mean_qdR[4]<-mean_qdR[3]*eff_qd[2] # MSW

eff_qd~dbeta(10,5)
# for(i in 1:2){
#   eff_qd[i]~dbeta(10,5)
# }

tau_qd<-1/((1-pow(phi_qd,2))*(sd_qd*sd_qd))  
# Marginal variance chosen to give uniform when mean=0, otherwise unimodal
sd_qd~dunif(0.01,1.6)
phi_qd~dunif(0,1) #positive autocorrelation

for(i in 1:(m+proj_years)){
  for(j in 4:6){
    # Same catchability of MSW salmon in the longline fishery
    qdW[i,j] <- qdW[i+j-3,3]	
    qdR[i,j] <-qdW[i,j]# qdR[i+j-4,4]	
  
  }
}

# River fishery
# ================

qrR[2] <- -log(1-HRR[2])/Er[1,1]	# Catchability of non-reproductive salmon in the river	
qrR[3] <-qrR[2]

for (j in 2:3){ 

     HRR[j] ~dbeta (5,1)	# Harvest rate in terminal river fishery	  
    
#    	qdW[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of reproductive salmon in the driftnet 	
# 		qdR[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of non-reproductive salmon in the driftnet 
# 		mqd[j] <- log(-log(1-HRD[j])/Edo[1,1])	# Mean catchability coefficient driftnet fishery	
#     
   	# Catchability coefficient of salmon from area 1 by coastal fishery 
    # This data is available only for AU1
    qctnW[j,1] ~ dlnorm(mqctn[j,1], tauqctn)	# Catchability wild salmon coastal trapnet
    qctnR[j,1] ~ dlnorm(mqctn[j,1], tauqctn)	# Catchability reared salmon coastal trapnet
		mqctn[j,1] <- log(-log(1-HRCTN[j,1])/Ectn[1,1,1])	# Mean catchability coastal trapnet 
		
    qcgnW[j,1] ~ dlnorm(mqcgn[j,1], tauqcgn)	# Catchability wild salmon coastal gillnet
    qcgnR[j,1] ~ dlnorm(mqcgn[j,1], tauqcgn)	# Catchability reared salmon coastal gillnet 
		mqcgn[j,1] <- log(-log(1-HRCGN[j,1])/Ecgn[1,1,1])# Mean catchability coastal gillnet 
   

    # For AU2 and AU3 we assume the ratio between wild and reared to be the same as in AU1
	  for (au in 2:3){
 
        qctnR[j,au] ~ dlnorm(mqctn[j,au], tauqctn)	# Catchability reared salmon coastal trapnet
		    mqctn[j,au] <- log(-log(1-HRCTN[j,au])/Ectn[1,1,1])	# Mean catchability coastal trapnet 
		    qctnW[j,au] <- (qctnW[j,1] / qctnR[j,1] ) * qctnR[j,au]	
    
        qcgnR[j,au] ~ dlnorm(mqcgn[j,au], tauqcgn)	# Catchability reared salmon coastal gillnet 
		    mqcgn[j,au] <- log(-log(1-HRCGN[j,au])/Ecgn[1,1,1])# Mean catchability coastal gillnet 
        qcgnW[j,au] <- (qcgnW[j,1] / qcgnR[j,1] ) * qcgnR[j,au]
      
    }

  #  HRD[j] ~dbeta (2,5)	# Harvest rate driftnet fishery	
  #  HRL[j] ~dbeta (2,5)	# Harvest rate longline fishery	
}
for (au in 1:3){
  HRCGN[2,au]  ~dbeta (2,2.5) 	# Harvest rate coastal gillnet fishery of fish from area k
  HRCTN[2,au]  ~dbeta (2,2.5) 	# Harvest rate coastal trapnet fishery of fish from area k	

  HRCGN[3,au]  ~dbeta (2,5) 	# Harvest rate coastal gillnet fishery of fish from area k
  HRCTN[3,au]  ~dbeta (2,5) 	# Harvest rate coastal trapnet fishery of fish from area k	
}
for (j in 4:5){ 

  HRR[j] ~dbeta (5,1)	# Harvest rate in terminal river fishery	
  qrR[j] <- qrR[3]	# Same catchability of MSW salmon in the river
  	
  #qlW[j] <- qlW[2]	# Same catchability of MSW salmon in the longline fishery
	#qlR[j] <- qlR[2]	# Same catchability of MSW salmon in the longline fishery
 
#  	qdW[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of reproductive salmon in the driftnet 	
# 	qdR[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of non-reproductive salmon in the driftnet 
# 	mqd[j] <- log(-log(1-HRD[j])/Edo[1,1])	# Mean catchability coefficient driftnet fishery	
	
	qctnW[j,1] <- qctnW[3,1]# Same catchability of MSW salmon in the coastal trapnet fishery
	qcgnW[j,1] <- qcgnW[3,1]# Same catchability of MSW salmon in the coastal gillnet fishery
  qctnR[j,1] <- qctnR[3,1]# Same catchability of MSW salmon in the coastal trapnet fishery
	qcgnR[j,1] <- qcgnR[3,1]# Same catchability of MSW salmon in the coastal gillnet fishery
 
 	HRCGN[j,1]  ~dbeta (2,5) 	# Harvest rate coastal gillnet fishery of fish from area k
	HRCTN[j,1]  ~dbeta (2,5) 	# Harvest rate coastal trapnet fishery of fish from area k	
 
	for (au in 2:3){
		qctnW[j,au] <- (qctnW[j,1] / qctnR[j,1] ) * qctnR[j,au]	
		qcgnW[j,au] <- (qcgnW[j,1] / qcgnR[j,1] ) * qcgnR[j,au]	
   	qctnR[j,au] <- qctnR[3,au]# Same catchability of MSW salmon in the coastal trapnet fishery
		qcgnR[j,au] <- qcgnR[3,au]# Same catchability of MSW salmon in the coastal gillnet fishery
   
   	HRCGN[j,au]  ~dbeta (2,5) 	# Harvest rate coastal gillnet fishery of fish from area k
		HRCTN[j,au]  ~dbeta (2,5) 	# Harvest rate coastal trapnet fishery of fish from area k	
    			
   }
#  HRD[j] ~dbeta (2,5)	# Harvest rate driftnet fishery	
#	HRL[j] ~dbeta (2,5)	# Harvest rate longline fishery	
}

for (j in 6:6){ 

  HRR[j] ~dbeta (5,1)	# Harvest rate in terminal river fishery
 	qrR[j] <- qrR[3]	# Catchability of non-reproductive salmon in the river	
		
	for(i in 1:(m+proj_years+3)){
    qlW[i,j] <- 0 #not defined in bugs model
	  qlR[i,j] <- 0 
	}
	
#	qdW[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of reproductive salmon in the driftnet 	
#	qdR[j] ~ dlnorm(mqd[j], tauqd)	# Catchability of non-reproductive salmon in the driftnet 
#	mqd[j] <- log(-log(1-HRD[j])/Edo[1,1])	# Mean catchability coefficient driftnet fishery	

	# Catchability coefficient of salmon from area l by coastal fishery		
	qctnW[j,1] ~ dlnorm(mqctn[j,1], tauqctn)	# Catchability wild salmon coastal trapnet
  qctnR[j,1] ~ dlnorm(mqctn[j,1], tauqctn)	# Catchability reared salmon coastal trapnet
	mqctn[j,1] <- log(-log(1-HRCTN[j,1])/Ectn[1,1,1])# Mean catchability coastal trapnet fishery	

	qcgnW[j,1] ~ dlnorm(mqcgn[j,1], tauqcgn)	# Catchability wild salmon coastal gillnet		
  qcgnR[j,1] ~ dlnorm(mqcgn[j,1], tauqcgn)	# Catchability reared salmon coastal gillnet 
  mqcgn[j,1] <- log(-log(1-HRCGN[j,1])/Ecgn[1,1,1])	# Mean catchability coastal gillnet fishery	
  
 	HRCGN[j,1]  ~dbeta (2,5) 	# Harvest rate coastal gillnet fishery of fish from area k
	HRCTN[j,1]  ~dbeta (2,5) 	# Harvest rate coastal trapnet fishery of fish from area k	
 
	for (au in 2:3){
		qctnW[j,au] <- (qctnW[j,1] / qctnR[j,1] ) * qctnR[j,au]	
		qcgnW[j,au] <- (qcgnW[j,1] / qcgnR[j,1] ) * qcgnR[j,au]
   	qctnR[j,au] ~ dlnorm(mqctn[j,au], tauqctn)	# Catchability reared salmon coastal trapnet
		mqctn[j,au] <- log(-log(1-HRCTN[j,au])/Ectn[1,1,1])# Mean catchability coastal trapnet fishery	
		qcgnR[j,au] ~ dlnorm(mqcgn[j,au], tauqcgn)	# Catchability reared salmon coastal gillnet 
		mqcgn[j,au] <- log(-log(1-HRCGN[j,au])/Ecgn[1,1,1])	# Mean catchability coastal gillnet fishery	
   
   	HRCGN[j,au]  ~dbeta (2,5) 	# Harvest rate coastal gillnet fishery of fish from area k
		HRCTN[j,au]  ~dbeta (2,5) 	# Harvest rate coastal trapnet fishery of fish from area k	
   }

#	HRD[j] ~dbeta (2,5)	# Harvest rate driftnet fishery	
#	HRL[j] ~dbeta (2,5)	# Harvest rate longline fishery	
}

tauqd ~ dgamma(10,1)T(1,)
tauql ~ dgamma(50,1)T(1,)
tauqr ~ dgamma(10,1)T(1,)
tauqcgn ~ dgamma(10,1)T(1,)			
tauqctn ~ dgamma(10,1)T(1,)


#Negbin ~1/overdispersion parameters
for (j in 1:6){
	
	  rrW[j] ~ dunif(1,200)
		rrRsp[j] ~  dunif(1,200)			
		rrR[j] ~  dunif(1,200)   
		rcW[j] ~  dunif(1,200)
	  rcR[j] ~  dunif(1,200)		
		rdW[j]~ dunif(1,200)
		rdR[j] ~ dunif(1,200)		
    rlW[j] ~ dunif(1,200)									
		rlR[j] ~ dunif(1,200)								

}	

# Tag retention rate
Tretain ~ dbeta(20,8)T(0.5,1)
	
# Reporting rates in the different fisheries
reportc ~ dbeta(11,9)T(0.2,0.8)	# coastal fishery
reportrR ~ dbeta(16,6)T(0.3,0.95)	# river fishery
reportrW ~ dbeta(16,6)T(0.3,0.95)	# river fishery	
reportd ~ dbeta(8,4)T(0.2,0.95)	# driftnet fishery
reportl ~ dbeta(10,4)T(0.3,0.95)	# longline fishery

#Catch likelihoods	
tauCR<-1/(SCR*SCR)
tauCC<-1/(SCC*SCC)
tauCO<-1/(SCO*SCO)
tauCT<-1/(SCT*SCT)				  

SCR~dbeta(1.45,8.19)
SCC~dbeta(1.45,8.19) 
SCO~dbeta(1.45,8.19) 
SCT~dbeta(1.45,8.19)					 

# These CVs should be at the original scale, likely to be close to the values of SCR-SCO
cvCR<-sqrt(exp(SCR*SCR)-1)
cvCC<-sqrt(exp(SCC*SCC)-1)
cvCO<-sqrt(exp(SCO*SCO)-1)
cvCT<-sqrt(exp(SCT*SCT)-1)						  

#Maturation

for (i in 1:(m+proj_years+5)){ # calendar years 1987-present+5
  p.rel[i]~dbeta(alpha_rel[i],beta_rel[i])   #i is calendar year
	for (j in 1:4){ # sea ages (1=1SW, 2=2SW etc.)

		muLW[i,j]<-cL[i]+bL[j]+delta[j]*Temp[i] # temperature effect differs on age groups
		#lw[i,j]~dnorm(muLW[i,j],tauL[j])
    lw[i,j]<-muLW[i,j]+(1/sqrt(tauL[j]))*eLW[i]
    logit(LW[i,j])<-lw[i,j]

		muLR[i,j]<-cL[i]+bL[j]+LReffect[j]+delta[j]*Temp[i]
		#lr[i,j]~dnorm(muLR[i,j],tauL[j])
		lr[i,j]<-muLR[i,j]+(1/sqrt(tauL[j]))*eLR[i]
    logit(LR[i,j])<-lr[i,j]

	}	
	eLW[i]~dnorm(0,1)
	eLR[i]~dnorm(0,1)
	Temp[i]~dnorm(muTemp[i],tauTemp[i])     #muTemp and tauTemp from Temperature data file
	cL[i]~dnorm(mucL,taucL)

	LW[i,5]<-1
	LR[i,5]<-1
 
	LW[i,6]<-1   #j=6 not  used as last year of spawners (j=6) use mat[,5] 
	LR[i,6]<-1
}
for (j in 1:3){ # sea ages (1=1SW, 2=2SW etc.)
	delta[j]~dunif(0.001,1)
	LReffect[j]~dlnorm(-1,2)
}
# MSW effects
delta[4]<-delta[3]
LReffect[4]<-LReffect[3]

mucL~dnorm(0.4,5.4)
taucL~dlnorm(4.6,2.8)

bL[1]~dnorm(-2.9,5.4)
bL[2]~dnorm(-0.84,5.4)
bL[3]~dnorm(0.047,5.4)
bL[4]~dnorm(1.40,5.4)

tauL[1]~dlnorm(0.42,49)
tauL[2]~dlnorm(1.7,44)
tauL[3]~dlnorm(2.3,41)
tauL[4]~dlnorm(1.4,46)

#priors for spawner counting
for(s in 1:stocks){
    a_spawn[s]<-mu_spawn[s]*eta_spawn[s]+1
    b_spawn[s]<-(1-mu_spawn[s])*eta_spawn[s]+1
    
    mu_spawn[s]~dbeta(mu_sp_alpha[s],mu_sp_beta[s])      
    CV_spawn[s]~dbeta(CV_sp_alpha[s],CV_sp_beta[s])
    eta_spawn[s]<-(1-mu_spawn[s])/(mu_spawn[s]*CV_spawn[s]*CV_spawn[s])         #-1
    
    for(i in 1:(m+proj_years+5)){
        p.detect[i,s]~dbeta(a_spawn[s],b_spawn[s])         #extra 5 years (m+5) not used here...
        p.ladder[i,s]~dbeta(alpha_ladder[i,s],beta_ladder[i,s])
        surv_migr[i,s]~dbeta(alpha_migr[i,s],beta_migr[i,s])T(0.001,0.999)   
    }
    
    eta_msw[s]<-(1/corr_msw[s])-1
    corr_msw[s]~dunif(0.0001,0.5)
      

}

CV_ladder~dlnorm(-3,4)  #5% CV
sigma_ladder<-sqrt(log(CV_ladder^2 + 1))
tau_ladder<-1/(sigma_ladder*sigma_ladder)

tauDS<-1/(log(cvDS*cvDS+1))
cvDS~dlnorm(-2.37,8)

coefDS<-coefDS_tmp+1   # To ensure Simo count is always overestimation  
coefDS_tmp~dlnorm(-2.029014,1.1211)

#coefDS<-1.05 # assume that Simojoki Didson count is underestimate 
 
for(rs in 1:rstocks){
    aTrap[rs]<-muTrap[rs]*etaTrap[rs]+1
    bTrap[rs]<-(1-muTrap[rs])*etaTrap[rs]+1
}

muTrap[1]~dbeta(2,2)        #Lule?lven AU2
muTrap[2]~dbeta(72.7,197)   #Dal?lven AU3

#etaTrap[1]~dlnorm(10,0.1)
etaTrap[1]~dlnorm(10,1)
etaTrap[2]~dlnorm(3.7,15.7)

Usmolt~dlnorm(0.01, 85)T(0.8,1.5) 		# Uncertainty factor
p.mort~dbeta(4,12)

}"
