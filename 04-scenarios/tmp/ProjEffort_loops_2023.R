

# Function used in the optimisation. Suggests a new value based on two previous values

nextpoint=function(x1,x2,y1,y2,target){
  b=(y2-y1)/(x2-x1)                                               
  a=y1-b*x1
  return((target-a)/b)
}



# while loop is for finding the effort that creates target removal






apu<-0
iter<-0
while(apu==0){
  iter<-iter+1
  
  CoefRiverF<-ifelse(EffScen==1,0,1)
  CoefLL<-ifelse(EffScen>4,0,Coef2)   #no offshore commercial fisheries scens 7-10
  CoefTN<-ifelse(EffScen>20,0,Coef2)
  CoefTRW<-ifelse(EffScen %in% c(5:9,22),0,0.6127289)#0.3335) 
  CoefTRR<-ifelse(EffScen %in% c(5:9,21),0,2.951814)#0.7347)   
   
  
  # First value is for interim year (assessment year) and next for future years
  ScenE_OLL_SWE<-c(0,0)
  ScenE_OLL_FIN<-c(0,0)
  
# NOTE! Coef must affect the interim year for LL and TR
  # because the difference in the model year
  # For coastal fisheries the Coef must influence onwards from the advice year 
 ScenE_OLL_DEN<-c(E_OLL_DEN[1]*CoefLL,E_OLL_DEN[2]*CoefLL) # hundred thousand hookdays      
 ScenE_OLL_PL<-c(E_OLL_PL[1]*CoefLL,E_OLL_PL[2]*CoefLL) # hundred thousand hookdays      
 #ScenE_OLL_TROLLING<-c(E_OLL_TROLLING[1],E_OLL_TROLLING[2]*CoefTrollingF) 
   
 ScenE_CTN_FIN_30<-c(E_CTN_FIN_30[1],E_CTN_FIN_30[2]*CoefTN) # thousand trapdays
 ScenE_CTN_SWE_30<-c(E_CTN_SWE_30[1],E_CTN_SWE_30[2]*CoefTN)                 
 ScenE_CTN_FIN_31<-c(E_CTN_FIN_31[1],E_CTN_FIN_31[2]*CoefTN)       
 ScenE_CTN_SWE_31<-c(E_CTN_SWE_31[1],E_CTN_SWE_31[2]*CoefTN)
  
  
  # =============================================================

  
  ###############################
  ## Load Inputs-files
  ###############################
  
  # save the data for each of the pieces of the MCMC chain
  for(loop in 1:(nsim/100)){
#    for(loop in 1:5){
 #      loop<-1    
    BH_dataFile<-paste0(PathOut_Scen, "ScenHist_JAGSmodel", Model,"_",loop,".RData") 
    load(BH_dataFile)
  
    for(y in 1:Nyears){
      May1stR[,y,,sims[1]:sims[2]]<- RsalmStock[,y,,1,]      
      May1stW[,y,,sims[1]:sims[2]]<- WsalmStock[,y,,1,]
      
      # Spawning stock / number after terminal fishery for reared
      MatW_3[,y,,sims[1]:sims[2]]<- WsalmStock[,y,,2,] 
      MatR_3[,y,,sims[1]:sims[2]]<- RsalmStock[,y,,2,] 
      #May1stW[,,1,1]
      
      ImmW_3[,y,,sims[1]:sims[2]]<-ImmW_3tmp[,y,,1,]
      ImmR_3[,y,,sims[1]:sims[2]]<-ImmR_3tmp[,y,,1,]
      
    }
    
    for(y in 1:yBreak){
      #y<-1
      Effort[y,,,,sims[1]:sims[2]]<-EffortICES[,y,,,,]

      WRF_HR[,y,,sims[1]:sims[2]]<-WRF_HRtmp[,y,,2,]
      RRF_HR[,y,,sims[1]:sims[2]]<-RRF_HRtmp[,y,,2,]
      WCTN_HR[,y,,sims[1]:sims[2]]<-WCTN_HRtmp[,y,,2,]
      RCTN_HR[,y,,sims[1]:sims[2]]<-RCTN_HRtmp[,y,,2,]
      WODN_HR[,y,,sims[1]:sims[2]]<-WODN_HRtmp[,y,,1,]
      RODN_HR[,y,,sims[1]:sims[2]]<-RODN_HRtmp[,y,,1,]
      WOLL_HR[,y,,sims[1]:sims[2]]<-WOLL_HRtmp[,y,,1,]
      ROLL_HR[,y,,sims[1]:sims[2]]<-ROLL_HRtmp[,y,,1,]
      WTR_HR[,y,,sims[1]:sims[2]]<-WTR_HRtmp[,y,,1,]
      RTR_HR[,y,,sims[1]:sims[2]]<-RTR_HRtmp[,y,,1,]
      
      WRF_C[,y,,sims[1]:sims[2]]<-WRF_Ctmp[,y,,2,]
      RRF_C[,y,,sims[1]:sims[2]]<-RRF_Ctmp[,y,,2,]
      WCTN_C[,y,,sims[1]:sims[2]]<-WCTN_Ctmp[,y,,2,]
      RCTN_C[,y,,sims[1]:sims[2]]<-RCTN_Ctmp[,y,,2,]
      WOLL_C[,y,,sims[1]:sims[2]]<-WOLL_Ctmp[,y,,1,]
      ROLL_C[,y,,sims[1]:sims[2]]<-ROLL_Ctmp[,y,,1,]
      WODN_C[,y,,sims[1]:sims[2]]<-WODN_Ctmp[,y,,1,]
      RODN_C[,y,,sims[1]:sims[2]]<-RODN_Ctmp[,y,,1,]
      WTR_C[,y,,sims[1]:sims[2]]<-WTR_Ctmp[,y,,1,]
      RTR_C[,y,,sims[1]:sims[2]]<-RTR_Ctmp[,y,,1,]
      
      PFAW[,y,,sims[1]:sims[2]]<-PFAtmpW[,y,,1,]  
      PFAR[,y,,sims[1]:sims[2]]<-PFAtmpR[,y,,1,]
      
      #PFAW2[,y,,sims[1]:sims[2]]<-PFAtmpW2[,y,,1,]  
      
      ql_W[,y,sims[1]:sims[2]]<-qlW[,y,]
      ql_R[,y,sims[1]:sims[2]]<-qlR[,y,]
      
      Etot[y,,sims[1]:sims[2]]<-Etot_tmp[y,,]
      
      
     #May1stWyBreak[,y,,sims[1]:sims[2]]<-May1stW_yBreak[,y,,1,]
     #May1stRyBreak[,y,,sims[1]:sims[2]]<-May1stR_yBreak[,y,,1,]
      
    }
    # CTN q's do not vary over time series
    qctn_W[,,sims[1]:sims[2]]<-qctnW[,,]
    qctn_R[,,sims[1]:sims[2]]<-qctnR[,,]
    
    for(y in 1:Nyears){
      MW[,y,,,sims[1]:sims[2]]<-WsalmNatMort[,y,,,]
      MR[,y,,,sims[1]:sims[2]]<-RsalmNatMort[,y,,,]
      MatRateW[,y,,sims[1]:sims[2]]<-WsalmMatRate[,y,,1,]
      MatRateR[,y,,sims[1]:sims[2]]<-RsalmMatRate[,y,,1,]
      FecW[,y,sims[1]:sims[2]]<-WsalmMatRate[,y,1,2,]
      FecR[,y,sims[1]:sims[2]]<-RsalmMatRate[,y,1,2,]
    
    }
    
    BHalpha[sims[1]:sims[2],]<-BH_alpha[,]
    BHbeta[sims[1]:sims[2],]<-BH_beta[,]
    tauBH[sims[1]:sims[2]]<-precisionBH
    
    for(y in 1:(Nyears-2)){
      M_74[sims[1]:sims[2],y]<-M74[,y]
    }
    
    # EPR 
    # =======================
    for(i in (yBreak+1):Nyears){
      pmat[i,1,]<-0
      pimm[i,1,]<-1
      
      for(a in 2:6){
        pmat[i,a,]<-WsalmMatRate[a,i,1,1,]*pimm[(i-1),(a-1),]     
        pimm[i,a,]<-(1-WsalmMatRate[a,i,1,1,])*pimm[(i-1),(a-1),]
      }
      
      for(r in 1:Nstocks){
        #here i index denotes smolt year
        simm[i,1,r,]<-exp(-(11*WsalmNatMort[1,i,r,2,]/12))*exp(-(WsalmNatMort[1,i,r,2,]*F_seal[i,1,AU[r]]/12))    
        smat[i,1,r,]<-1    #not used anywhere
        
        for(a in 2:6){
          simm[i,a,r,]<-exp(-(12*WsalmNatMort[a,i,r,2,]/12))
          smat[i,a,r,]<-exp(-(3*WsalmNatMort[a,i,r,2,]/12))*exp(-(2*WsalmNatMort[a,i,r,2,]*F_seal[i,a,AU[r]]/12))*p.ladder[a,i,r,]*surv_migr[a,i,r,]
        }   
        #EPR calculation (check year index for fec if this is changed to vary by year)
        EPR[i,r,]<-pmat[i,2,]*prop_fem[(i-1),2,r]*WsalmMatRate[2,(i-1),r,2,]*simm[(i-1),1,r,]*smat[(i-1),2,r,]+pmat[i,3,]*prop_fem[(i-2),3,r]*WsalmMatRate[3,(i-2),r,2,]*simm[(i-2),1,r,]*simm[(i-1),2,r,]*smat[(i-2),3,r,]+pmat[i,4,]*prop_fem[(i-3),4,r]*WsalmMatRate[4,(i-3),r,2,]*simm[(i-3),1,r,]*simm[(i-2),2,r,]*simm[(i-1),3,r,]*smat[(i-3),4,r,]+pmat[i,5, ]*prop_fem[(i-4),5,r]*WsalmMatRate[5,(i-4),r,2,]*simm[(i-4),1,r,]*simm[(i-3),2,r,]*simm[(i-2),3,r,]*simm[(i-1),4,r,]*smat[(i-4),5,r,]+pmat[i,6,]*prop_fem[(i-5),6,r]*WsalmMatRate[6,(i-5),r,2,]*simm[(i-5),1,r,]*simm[(i-4),2,r,]*simm[(i-3),3,r,]*simm[(i-2),4,r,]*simm[(i-1),5,r,]*smat[(i-5),6,r,]     #5SW
        EPR_M74[i,r,]<-EPR[i,r,]*(1-M74[,(i)])
      }
    }
    
    for(y in 1:Nyears){
      EPRW[y,,sims[1]:sims[2]]<-EPR[y,,]
      EPRW_M74[y,,sims[1]:sims[2]]<-EPR_M74[y,,]
    }
    
    # R0 
    # =======================
    for(y in 1:(yBreak+3)){
      #for(y in 1:(yBreak)){
      for(r in 1:Nstocks){
        R0[y,r,sims[1]:sims[2]]<-R_zero[,y,r]
      }
    }
    for(r in 1:Nstocks){
      for(y in (yBreak+4):(yBreak+NumFutYears)){
        R0[y,r,sims[1]:sims[2]]<-(K[,r]*(EPR_M74[(y-e_delay[r]),r,]-BH_alpha[,r]))/EPR_M74[(y-e_delay[r]),r,] 
      }
    }
    
    
  } # end loop of pieces with 100 iterations 
  
  
  
  # History only, include variables which haven't been calculated before
  for(y in 2:yBreak){
    for(a in 1:6){
      
      MatW_1[,y,,]<-May1stW[,y,,]*MatRateW[,y,,] # Mature on May 1
      MatR_1[,y,,]<-May1stR[,y,,]*MatRateR[,y,,]
      
      ImmW_1[,y,,]<-May1stW[,y,,]*(1-MatRateW[,y,,])  # immature on May 1
      ImmR_1[,y,,]<-May1stR[,y,,]*(1-MatRateR[,y,,])
      
    }
  } # y End of adds to history
  
  dim(MatW_1)
  MatW_1[,,1,1]
  May1stW[,,1,1]
  ImmW_3[,,1,1]
  
  # # Aging from yBreak to yBreak+1
  # for(y in yBreak:yBreak) {
  #   May1stW[2:6,y+1,,]<-ImmW_3[1:5,y,,]*exp(-(MW[1:5,y,,1,]*(2/12)))
  #   May1stR[2:6,y+1,,]<-ImmR_3[1:5,y,,]*exp(-(MR[1:5,y,,1,]*(2/12)))
  # }
  
  
# ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
# Predictions:
# ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    
    #yBreak corresponds to the last historical year 
  for(y in (yBreak+1):(yBreak+NumFutYears)){
  # for(y in (yBreak+1):(yBreak+3)){
      #y<-yBreak+1;y
    
    #y<-yBreak+NumFutYears
    
    #River harvest assume the same as last historic year
    # CoefRiverF will be 0 in no fishing scenario, otherwise 1
    WRF_HR[,y,,]<- WRF_HR[,yBreak,,]*CoefRiverF          #wild  river fishing
    RRF_HR[,y,,]<- RRF_HR[,yBreak,,]*CoefRiverF          #reared
    
    # Recreational trolling HR   
    # Use the HR from the last observed year for the proxy of future 
    WTR_HR_orig[,y,,]<-WTR_HR[,yBreak,,]
    WTR_F[,y,,]<- -log(1-WTR_HR_orig[,y,,])*CoefTRW # Transform into instantanoeus mortality, use the same coef as for LL 
    WTR_HR[,y,,]<- 1-exp(-WTR_F[,y,,]) # And back to HR
    
    RTR_HR_orig[,y,,]<-RTR_HR[,yBreak,,]
    RTR_F[,y,,]<- -log(1-RTR_HR_orig[,y,,])*CoefTRR
    RTR_HR[,y,,]<- 1-exp(-RTR_F[,y,,])
    
    # Use the catchability of last observed year for LL or change 
    ql_W[,y,]<-ql_W[,yBreak,]
    ql_R[,y,]<-ql_R[,yBreak,]
    
    #################################
    # Effort scenarios by country  ##
    #################################
    
    # Sweden will quit longline fishery in 2012/2013 winter.
    Effort[y,"ICES 22-29","OLL","Sweden",]<-0

    # Effort scenario for the following year after the current (yBreak)
    # ================================================================
    if(y==(yBreak+1)){
      
      # OLL
      Effort[y,"ICES 22-29","OLL","Finland",]<-ScenE_OLL_FIN[1] 
      Effort[y,"ICES 22-29","OLL","Sweden",]<-ScenE_OLL_SWE[1]
      Effort[y,"ICES 22-29","OLL","Denmark",]<-ScenE_OLL_DEN[1]
      Effort[y,"ICES 22-29","OLL","Poland",]<-ScenE_OLL_PL[1]
      #Effort[y,"ICES 22-29","OLL","Trolling",]<-ScenE_OLL_TROLLING[1]
      
      # CTN
      Effort[y,"ICES 30","CTN","Finland",]<-ScenE_CTN_FIN_30[1]  
      Effort[y,"ICES 31","CTN","Finland",]<-ScenE_CTN_FIN_31[1]       
      Effort[y,"ICES 30","CTN","Sweden",]<-ScenE_CTN_SWE_30[1]                 
      Effort[y,"ICES 31","CTN","Sweden",]<-ScenE_CTN_SWE_31[1]
      
    }
    
    # Effort scenarios from yBreak+1 until the end of future scenarios
    # ================================================================
    if(y>(yBreak+1)){
      
      # OLL
      Effort[y,"ICES 22-29","OLL","Finland",]<-ScenE_OLL_FIN[2]
      Effort[y,"ICES 22-29","OLL","Sweden",]<-ScenE_OLL_SWE[2]
      Effort[y,"ICES 22-29","OLL","Denmark",]<-ScenE_OLL_DEN[2]
      Effort[y,"ICES 22-29","OLL","Poland",]<-ScenE_OLL_PL[2]
      #Effort[y,"ICES 22-29","OLL","Trolling",]<-ScenE_OLL_TROLLING[2]
      
      # CTN
      Effort[y,"ICES 30","CTN","Finland",]<-ScenE_CTN_FIN_30[2]  
      Effort[y,"ICES 31","CTN","Finland",]<-ScenE_CTN_FIN_31[2]       
      Effort[y,"ICES 30","CTN","Sweden",]<-ScenE_CTN_SWE_30[2]                 
      Effort[y,"ICES 31","CTN","Sweden",]<-ScenE_CTN_SWE_31[2]
      
    }
    
    ############################################################################
    
    #Now to link with the biological model, translate into effort by AU

    #The offshore longline effort consists of the effort from all
    #countries in ICES units 22-29
    # Salmon from all of the units are exposed to longlining at the Main Basin
    EffortAU[y,,"OLL",]<-
      Effort[y,1,"OLL","Finland",]+
      Effort[y,1,"OLL","Sweden",]+
      Effort[y,1,"OLL","Denmark",]+
      Effort[y,1,"OLL","Poland",]#+
      #Effort[y,1,"OLL","Trolling",]
    
    # Salmon from unit 1 are affected by the Finnish coastal trapnet fishery
    # in ICES units 30 and 31 (gulf of Bothnia)
    EffortAU[y,1,"CTN",]<-
      Effort[y,"ICES 30","CTN","Finland",]+  #this is a bit different from historical model because mixed fishery
      Effort[y,"ICES 31","CTN","Finland",] +                             #45 of Swedish effort targets unit 1 and 55% unit 2
      0.45*Effort[y,"ICES 31","CTN","Sweden",]
    
    # Salmon from unit 2 are affected by the Finnish coastal trapnet fishery
    # in ICES units 30 (Aland) and by the Swedish coastal trapnet fishery in
    # ICES unit 31 (back of the gulf of Bothnia)
    EffortAU[y,2,"CTN",]<-
      Effort[y,"ICES 30","CTN","Finland",]+
      0.55*Effort[y,"ICES 31","CTN","Sweden",]
    
    # Salmon from unit 3 are affected by the Finnish coastal trapnet fishery in
    # ICES units 30 and by the Swedish coastal trapnet fishery in ICES unit 30
    EffortAU[y,3,"CTN",]<-
      Effort[y,"ICES 30","CTN","Finland",]+
      Effort[y,"ICES 30","CTN","Sweden",]

    # No coastal fishery at unit 4    
    EffortAU[y,4,"CTN",]<-rep(0, nsim)
    
    #calculate harvest rates
    #!==========================================================================
    #! q*E= instanteneous fishing mortality
    #! exp(-F) = proportion which survives from the fishing
    #! exp(-F-M) = survives both fishing and natural mortality
    #! 1-exp(-F) = the proportion which gets harvested
    #!==========================================================================
    # Smolts/ Post-smolts
    WOLL_HR[1,y,1:Nstocks,]<-0          
    ROLL_HR[1,y,1:4,]<-0
    
    WCDN_HR[1,y,1:Nstocks,]<-0
    WCTN_HR[1,y,1:Nstocks,]<-0
    RCDN_HR[1,y,1:4,]<-0
    RCTN_HR[1,y,1:4,]<-0
    
    for(a in 2:6){
     for(r in 1:Nstocks){
      WOLL_HR[a,y,r,]<- 1-exp(-ql_W[2,y,]* EffortAU[y,1,"OLL",])           
    }
    for(u in 1:4){
       ROLL_HR[a,y,u,]<- 1-exp(-ql_R[2,y,]* EffortAU[y,1,"OLL",])
    }
    }
#    WOLL_HR[3:6,y,,]<- 1-exp(-ql_W[2,y,]* EffortAU[y,1,"OLL",])
#    ROLL_HR[2,y,,]<- 1-exp(-ql_R[1,y,]* EffortAU[y,1,"OLL",])
#    ROLL_HR[3:6,y,,]<- 1-exp(-ql_R[2,y,]* EffortAU[y,1,"OLL",])
    
    # No future DN fishery
    WODN_HR[,y,1:Nstocks,]<-0
    RODN_HR[,y,1:4,]<-0
    WODN_C[,y,1:Nstocks,]<-0
    RODN_C[,y,1:4,]<-0
    
    #AU4 effort=0
    for(u in 1:4){
      RCTN_HR[2,y,u,]<- 1-exp(-qctn_R[1,u,]* EffortAU[y,u,"CTN",])
      for(a in 3:6){
        RCTN_HR[a,y,u,]<- 1-exp(-qctn_R[2,u,]* EffortAU[y,u,"CTN",])
      }
    }
    
    for(r in 1:Nstocks){
      WCTN_HR[2,y,r,]<- 1-exp(-qctn_W[1,AU[r],]* EffortAU[y,AU[r],"CTN",])
      for(a in 3:6){
        WCTN_HR[a,y,r,]<- 1-exp(-qctn_W[2,AU[r],]* EffortAU[y,AU[r],"CTN",])
      }
    }
    #Population dynamics########################################################
    
    # On May 1st, split the number of salmon at sea into the number of salmon at
    # sea that have matured and will spawn this year and the number of salmon at
    # sea that have not yet matured and will spend at least another year at sea.
    # Because smolts spend 1 year at sea before migrating, the values for
    # WsalmMatRate and RsalmMatRate with age=1 is equal to 0.
    
    # On May 1st, the number of mature salmon that will migrate and spawn
    # this year is determined by
    MatW_1[,y,,]<-May1stW[,y,,]*MatRateW[,y,,] # previously temp2W / MigrW
    MatR_1[,y,,]<-May1stR[,y,,]*MatRateR[,y,,]
    
  #  print(y)
  #  print("May1stW[,,1,1]")
  #  print(May1stW[,,1,1])
    
    
  #  print("MatW_1[,,1,1]")
  #  print(MatW_1[,,1,1])
    
    # On May 1st, the number of salmon that have not yet matured this year and
    # that will stay at least another year at sea is determined by
    ImmW_1[,y,,]<-May1stW[,y,,]*(1-MatRateW[,y,,])       #immature on May 1
    ImmR_1[,y,,]<-May1stR[,y,,]*(1-MatRateR[,y,,])

 #   print("ImmW_1[,,1,1]")
 #   print(ImmW_1[,,1,1])
    
    
    ############################################################################
    # Fish on the spawning migration 
    ############################################################################
    # On July 1st, the number of migrating fish caught by the coastal
    # trapnet fishery is determined by
    #!==========================================================================
    #! F_seal/6, so this means that seal induced mortality takes place for
    #! 2 months, that is May and June.
    #! survival from natural mortality alone in two months up to July 1st is
    #! exp(-0.1/6) approx. =  98% and with seal mortality (2010) roughly 87%
    #! assuming F_seal 8.5
    #!==========================================================================
    
    # The number of migrating fish by age on 1 July after the coastal trapnet
    # fishery is given by (all ages have 1 month seal M during June and July)
    #F_seal for post-smolt (a=1) is 1
    for(a in 1:6){
      for(r in 1:Nstocks){
        #a<-2;r<-1

        # 2/12: May, June
        WCTN_C[a,y,r,]<- MatW_1[a,y,r,]*exp(-(MW[a,y,r,2,]*(1/12)))*
          exp(-(MW[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))*WCTN_HR[a,y,r,]
        
        # MatW_2: Number ascending to river
        # 3/12: May, June, July
        MatW_2[a,y,r,]<-(MatW_1[a,y,r,]*exp(-(MW[a,y,r,2,]*(1/12)))*
                           exp(-(MW[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))-WCTN_C[a,y,r,])*
          exp(-(MW[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))
      }
      for(u in 1:4){
        RCTN_C[a,y,u,]<-MatR_1[a,y,u,]*exp(-(MR[a,y,u,2,]*(1/12)))*
          exp(-(MR[a,y,u,2,]*F_seal[y,a,u]*(1/12)))*RCTN_HR[a,y,u,]
        
        MatR_2[a,y,u,]<-(MatR_1[a,y,u,]*exp(-(MR[a,y,u,2,]*(1/12)))*
                           exp(-(MR[a,y,u,2,]*F_seal[y,a,u]*(1/12)))-RCTN_C[a,y,u,])*
          exp(-(MR[a,y,u,2,]*F_seal[y,a,u]*(1/12)))
      }  
    }
    
 #   print("MatW_2[,,1,1]")
 #   print(MatW_2[,,1,1])
    

    # On August 1st the number of migrating fish caught by the river fishery is given by 
    # MatW_3: Number of spawners (NspW in JAGS model) 
    for(a in 1:6){
      for(r in 1:Nstocks){
        # Set river catch zero for Ljungan, Emån, Testeboån & Kåge
        if((RCzero==T) &  (r %in% zero_st) & (y<(yBreak+14))){ #2023-2035 
            WRF_C[a,y,r,]<-MatW_2[a,y,r,]*p.ladder[a,y,r,]*WRF_HR[a,y,r,]*0.10 #2023 
        }else{
           WRF_C[a,y,r,]<-MatW_2[a,y,r,]*p.ladder[a,y,r,]*WRF_HR[a,y,r,]
      }
      # 2/12: August, September
      MatW_3[a,y,r,]<-(MatW_2[a,y,r,]*p.ladder[a,y,r,]-WRF_C[a,y,r,])*
        exp(-(MW[a,y,r,2,]*(2/12)))*surv_migr[a,y,r,]
      
      }
    
      for(u in 1:4){
        RRF_C[a,y,u,]<-MatR_2[a,y,u,]*RRF_HR[a,y,u,]
        MatR_3[a,y,u,]<-(MatR_2[a,y,u,]-RRF_C[a,y,u,])*exp(-(MR[a,y,u,2,]*(2/12)))
      }
    }
    
  #  print("MatW_3[,,1,1]")
  #  print(MatW_3[,,1,1])
    
  
    # 1/12: July
    # 2/12: August, September
    
    ############################################################################
    # Fish on the feeding migration
    ############################################################################
    
    for(a in 1:1){
      for(r in 1:Nstocks){
        #PFA on 1 Sept
        # (7+5)/12: May-April
        # 4 months of adult M -> half point of the rest of the months that are left of that
        # calendar year
        
        PFAW[a,y,r,]<-PropCW[y-a+6]*ImmW_1[a,y,r,]*
          exp(-((MW[a,y,r,1,]*(11/12))+(MW[a,y,r,1,]*F_seal[y,a,AU[r]]/12)+(MW[2,y,r,1,]*(4/12))))

        # PFAW2: stock specific PFA's without addition from AU5-6
        PFAW2[a,y,r,]<-ImmW_1[a,y,r,]*
          exp(-((MW[a,y,r,1,]*(11/12))+(MW[a,y,r,1,]*F_seal[y,a,AU[r]]/12)+(MW[2,y,r,1,]*(4/12))))
        
        WOLL_C[a,y,r,]<-PropCW[y-a+6]*(ImmW_1[a,y,r,]*exp(-(MW[a,y,r,1,]*(8/12)))*
          exp(-(MW[a,y,r,1,]*F_seal[y,a,AU[r]]/12))*WOLL_HR[a,y,r,])
         
        ## The number of fish at sea by age in 1st February after the offshore LL
        # fishery. Post-smolt are assumed not to be affected by the offshore
        # longline fishery with catches being 0 but they are affected by a high
        # post-smolt mortality rate (same as NlW in FLHM)          
        ImmW_2[a,y,r,]<-ImmW_1[a,y,r,]*exp(-(MW[a,y,r,1,]*(8/12)))*
          exp(-(MW[a,y,r,1,]*F_seal[y,a,AU[r]]/12))-WOLL_C[a,y,r,]
        
        # Trolling fishery 1st March
        WTR_C[a,y,r,]<-PropCW[y-a+6]*ImmW_2[a,y,r,]*exp(-(MW[a,y,r,1,]*(1/12)))*WTR_HR[a,y,r,]
        
        ImmW_3[a,y,r,]<-ImmW_2[a,y,r,]*exp(-(MW[a,y,r,1,]*(1/12)))-WTR_C[a,y,r,]
      }                  
      for(u in 1:4){
        PFAR[a,y,u,]<-PropCR[y-a+6]*ImmR_1[a,y,u,]*exp(-((MR[a,y,u,1,]*(11/12))+
                  (MR[a,y,u,1,]*F_seal[y,a,u]/12)+(MR[2,y,u,1,]*(4/12))))
        
        ROLL_C[a,y,u,]<-PropCR[y-a+6]*(ImmR_1[a,y,u,]*
                            exp(-(MR[a,y,u,1,]*(8/12)))*
                            exp(-(MR[a,y,u,1,]*F_seal[y,a,u]/12))*ROLL_HR[a,y,u,])
        
        # 9/12: May 1-Feb 1
        ImmR_2[a,y,u,]<-ImmR_1[a,y,u,]*exp(-(MR[a,y,u,1,]*(8/12)))*
          exp(-(MR[a,y,u,1,]*F_seal[y,a,u]/12))-ROLL_C[a,y,u,]
      
        # Trolling fishery 1st March
        RTR_C[a,y,u,]<-PropCR[y-a+6]*ImmR_2[a,y,u,]*exp(-(MR[a,y,u,1,]*(1/12)))*RTR_HR[a,y,u,]
        
        ImmR_3[a,y,u,]<-ImmR_2[a,y,u,]*exp(-(MR[a,y,u,1,]*(1/12)))-RTR_C[a,y,u,]
      }      
    }
    # 9/12: May-Feb
    for(a in 2:6){
      #a<-2
      # PFAs on 1 July              
      # the year of pfa is the same as the year of winter fishing
      PFAW[a,y,,]<-PropCW[y-a+6]*ImmW_1[a,y,,]*exp(-(MW[a,y,,1,]*((8+6)/12)))
      PFAR[a,y,,]<-PropCR[y-a+6]*ImmR_1[a,y,,]*exp(-(MR[a,y,,1,]*((8+6)/12)))
      
      # PFAW2: stock specific PFA's without addition from AU5-6
      PFAW2[a,y,,]<-ImmW_1[a,y,,]*exp(-(MW[a,y,,1,]*((8+6)/12)))
       
      #this used to be in an a in 2:6 loop but seems to be an error as a indexing was missing
      # WOLL_HR is 0 for smolts
      # On 1st February, the number of fish at sea caught by the offshore longline
      # fishery is determined by
      WOLL_C[a,y,,]<-PropCW[y-a+6]*(ImmW_1[a,y,,]*exp(-(MW[a,y,,1,]*(9/12)))*WOLL_HR[a,y,,])
      ROLL_C[a,y,,]<-PropCR[y-a+6]*(ImmR_1[a,y,,]*exp(-(MR[a,y,,1,]*(9/12)))*ROLL_HR[a,y,,])
      # 9/12: May 1-Feb 1
    
      ImmW_2[a,y,,]<-ImmW_1[a,y,,]*exp(-(MW[a,y,,1,]*(9/12)))-WOLL_C[a,y,,]
      ImmR_2[a,y,,]<-ImmR_1[a,y,,]*exp(-(MR[a,y,,1,]*(9/12)))-ROLL_C[a,y,,]
    
      # Trolling fishery 1st March
      WTR_C[a,y,,]<-PropCW[y-a+6]*ImmW_2[a,y,,]*exp(-(MW[a,y,,1,]*(1/12)))*WTR_HR[a,y,,]
      RTR_C[a,y,,]<-PropCR[y-a+6]*ImmR_2[a,y,,]*exp(-(MR[a,y,,1,]*(1/12)))*RTR_HR[a,y,,]
      
      ImmW_3[a,y,,]<-ImmW_2[a,y,,]*exp(-(MW[a,y,,1,]*(1/12)))-WTR_C[a,y,,]
      ImmR_3[a,y,,]<-ImmR_2[a,y,,]*exp(-(MR[a,y,,1,]*(1/12)))-RTR_C[a,y,,]
    
    }
    
    # The number of fish at sea by age on May 1st in the next year. It is
    # assumed that the post-smolt mortality affects post-smolts for 1
    # full year, starting from May 1st till May 1st the next year.
    
    if(y<(yBreak+NumFutYears)) {
      May1stW[2:6,y+1,,]<-ImmW_3[1:5,y,,]*exp(-(MW[1:5,y,,1,]*(2/12)))
      May1stR[2:6,y+1,,]<-ImmR_3[1:5,y,,]*exp(-(MR[1:5,y,,1,]*(2/12)))
    }
    # 3/12: Feb-April
    
  #  print(y)
  #  print("May1stW[,,1,1]")
  #  print(May1stW[,,1,1])
    
    
    
    # ==========================================================================
    # Stock recruit relationship
    # ==========================================================================
    #in the yBreak+1 -> loop
    #first recruits in yBreak + 2
    if(y<(yBreak+NumFutYears)){
      for(s in 1:nsim){
        for(r in 1:Nstocks){   #STOCK
          #r<-1;s<-1
          #X<-WsalmStock[,(y-2),r,2,s]
          #W<-WsalmMatRate[,(y-2),r,2,s]       #fecundity
          X<-MatW_3[,(y-2),r,s]
          W<-FecW[,(y-2),s]       #fecundity
       
          E<-as.vector(X*W)
          #SexRatio<-c(0,0.06,0.73,0.73,0.89,0.89)     #updated Dec 2016
          #E<-E*SexRatio
          E<-E*prop_fem[(y-2),,r]
          E<-sum(E,na.rm=T)
          E<-E*(1-M_74[s,(y-2)])
          Etot[(y-2),r,s]<-E
          
          error<-errorSR[s,y,r]#<-exp(rnorm(mean= -0.5/precisionBH[s], sd = sqrt(1/precisionBH[s])))        #mean is 0
          
          #In unit 4 the smolts recruit a year earlier
          if(r>13 & r<16){
            May1stW[1,(y+1),r,s]<-as.numeric(E*error/(BHalpha[s,r]+BHbeta[s,r]*E))
          }else{ #stocks other than 14 & 15
            
            if(y<(yBreak+NumFutYears-1)){          # for(y in (yBreak+1):(yBreak+NumFutYears))
              May1stW[1,(y+2),r,s]<-as.numeric(E*error/(BHalpha[s,r]+BHbeta[s,r]*E))
            }
          }
        }
      }
    }

  }#end future loop
    
    
  
  ###########################################
  # Combine parameters for the whole time series
  ###########################################

  # Combined harvest rates
  # Because fishing with different gears takes place sequentially, we need
  # to transform harvest rates first back to instantaneous mortality rates
  # (F = -log(1-HR)) and after that calculate the combined harvest rates
  
  for(a in 1:2){
    # Combined offshore harvest rates, 1SW & MSW
    OffsW_HR[a,,]<- 1-exp(-(-log(1-WODN_HR[a+1,,1,])-log(1-WOLL_HR[a+1,,1,])-log(1-WTR_HR[a+1,,1,])))
    OffsR_HR[a,,]<- 1-exp(-(-log(1-RODN_HR[a+1,,1,])-log(1-ROLL_HR[a+1,,1,])-log(1-RTR_HR[a+1,,1,])))
    
    # Combined coastal harvest rate
    for(u in 1:3){
      CoastW_HR[a,,u,]<-1-exp(-(-log(1-WCTN_HR[a+1,,u,])-log(1-WCGN_HR[a+1,,u,])-log(1-WCDN_HR[a+1,,u,])))
      CoastR_HR[a,,u,]<-1-exp(-(-log(1-RCTN_HR[a+1,,u,])-log(1-RCGN_HR[a+1,,u,])-log(1-RCDN_HR[a+1,,u,])))
    }
  }
    
  CatchSeaW<-WOLL_C+WODN_C+WTR_C
  CatchSeaR<-ROLL_C+RODN_C+RTR_C
  RiverCatchW<-WRF_C
  RiverCatchR<-RRF_C
    
    
  for(s in 1:nsim){
    for(y in 1:Nyears){
        # Sums over age groups
        TornioRiverCatch[y,s]<-sum(WRF_C[,y,1,s])
        TornioSeaCatch[y,s]<-sum(CatchSeaW[,y,1,s])
        UmeRiverCatch[y,s]<-sum(WRF_C[,y,10,s])
        UmeSeaCatch[y,s]<-sum(CatchSeaW[,y,10,s])
        MorrumRiverCatch[y,s]<-sum(WRF_C[,y,14,s])
        MorrumSeaCatch[y,s]<-sum(CatchSeaW[,y,14,s])
     
        Migr_Tornio[y,s]<-sum(MatW_1[2:6,y,1,s])
        Migr_Simo[y,s]<-  sum(MatW_1[2:6,y,2,s])
        Migr_AU1W[y,s]<-  sum(MatW_1[2:6,y,1:4,s])
        Migr_AU13W[y,s]<- sum(MatW_1[2:6,y,1:13,s])+sum(MatW_1[2:6,y,16:17,s])
        Migr_AU1R[y,s]<-  sum(MatR_1[2:6,y,1,s])
        Migr_AU13R[y,s]<- sum(MatR_1[2:6,y,1:3,s])
        Migr_AU13tot[y,s]<-Migr_AU13W[y,s]+Migr_AU13R[y,s]
        
        CalC_OLL[y,s]<-sum(WOLL_C[2:6,y,,s])+sum(ROLL_C[2:6,y,,s])
        CalC_CTN[y,s]<-sum(WCTN_C[2:6,y,,s])+sum(RCTN_C[2:6,y,,s])
        CalC_TR[y,s]<-sum(WTR_C[2:6,y,,s])+sum(RTR_C[2:6,y,,s])
        
        CatchRiver[y,s]<-sum(WRF_C[,y,,s])+sum(RRF_C[,y,,s])
        
        # number of smolts which survive the post smolt mortality phase
        postsmoltsR[y,s]<-sum(May1stR[1,y,,s])*exp(-(MR[1,y,1,1,s]))
        postsmoltsW[y,s]<-sum(May1stW[1,y,,s])*exp(-(MW[1,y,1,1,s]))
        postsmolts[y,s]<-postsmoltsW[y,s]+postsmoltsR[y,s]
        
        # Wild salmon by river
        for(r in 1:Nstocks){
          SmoltW[r,y,s]<-May1stW[1,y,r,s] # smolts
          PSW[r,y,i]<-May1stW[1,y,r,s]*exp(-(MW[1,y,1,1,s])) # post-smolts
          SpawnerW[r,y,s]<-sum(MatW_3[,y,r,s]) # spawners
          
          for(a in 1:6){
            spW_age[r,y,a,s]<-MatW_3[a,y,r,s]
            W_age[r,y,a,s]<-May1stW[a,y,r,s] 
          }
          
        }
        
        # Reared salmon by assessment units
        for(u in 1:4){
          SmoltR[u,y,s]<-May1stR[1,y,u,s] # smolts
          PSR[u,y,s]<-May1stR[1,y,u,s]*exp(-(MR[1,y,1,1,s])) # post-smolts
          SpawnerR[u,y,s]<-sum(MatR_3[,y,u,s]) # Reared "spawners" (reared fish that return to river, but can't spawn.)
          
          for(a in 1:6){
            spR_age[u,y,a,s]<-MatR_3[a,y,u,s]
          }

        }
        # Proportions of spawners by age
        Prop1SWsp[y,i]<-sum(MatW_3[2,y,,s])/sum(MatW_3[,y,,s])
        Prop2SWsp[y,i]<-sum(MatW_3[3,y,,s])/sum(MatW_3[,y,,s])
        Prop3SWsp[y,i]<-sum(MatW_3[4,y,,s])/sum(MatW_3[,y,,s])
        Prop4SWsp[y,i]<-sum(MatW_3[5,y,,s])/sum(MatW_3[,y,,s])
        
    }
  }
    


  
  # =============================================================
  # Check if total removal is what we wanted:
  
  year<-c(1992:LastPredYear)
  
  stats<-function(dat,v){
    sumDat<-summary(as.mcmc(dat[v,]))
    
    med<-sumDat$quantiles[3]
    high<-sumDat$quantiles[5]
    low<-sumDat$quantiles[1]
    
    return(rbind(med, low, high))
  }
  
  
  C_OLL<-array(NA, c(Nyears,nsim))
  C_CTN<-array(NA, c(Nyears,nsim))
  C_TR<-array(NA, c(Nyears,nsim))
  C_TRW<-array(NA, c(Nyears,nsim))
  C_TRR<-array(NA, c(Nyears,nsim))
  CalC_tot<-array(NA, c(Nyears,nsim))
  for(i in 1:nsim){
    for(y in 1:Nyears){
      C_OLL[y,i]<-sum(WOLL_C[2:6,y,1:Nstocks,i])+sum(ROLL_C[2:6,y,1:4,i])
      C_TR[y,i]<-sum(WTR_C[2:6,y,1:Nstocks,i])+sum(RTR_C[2:6,y,1:4,i])
       C_TRW[y,i]<-sum(WTR_C[2:6,y,1:Nstocks,i])  #trolling catch wild only
        C_TRR[y,i]<-sum(RTR_C[2:6,y,1:4,i])       #trolling catch reared only
      C_CTN[y,i]<-sum(WCTN_C[2:6,y,1:Nstocks,i])+sum(RCTN_C[2:6,y,1:4,i])
      if(y>1){
        #COASTAL FISHING IS IN CALENDAR /ADVICE YEAR BUT LONGLINE IS IN THE PREVIOUS ONE
        CalC_tot[y,i]<-C_OLL[y-1,i]+C_CTN[y,i]+C_TRW[y-1,i]+C_TRR[y-1,i]
      }
    }
  }
  
  
  ########################################################################
#  if(EffScen<6 || EffScen>6){
    print("Coef2")
    print(Coef2)
#  }
  # if(EffScen==2 || EffScen==6){
  #   print("CoefTrollingF")
  #   print(CoefTrollingF)
  # }
  print("Total sea catch")
  print(stats(CalC_tot, yCTN)[1]) 
  print(target) #Total target
  print("***")
  # if(EffScen<6 || EffScen>6){
  #   print("Trolling catch")
  #  # print(stats(C_OLL, yOLL)[1]*propTrolling)
  #   print(targetTr) # Trolling target
  # }
  print("--------")
  if(EffScen>0){
    ifelse(abs(round((stats(CalC_tot, yCTN)[1])-target,1))<0.2,
           apu<-1,apu<-0)
    if(iter==1 && Optim==T)
    {
      E1=Coef2
      C1=(stats(CalC_tot, yCTN)[1])
      Coef2=Coef2+0.1
      print(paste0("Optimizer step 1: E1=",E1," C1=",C1))
    }
    if(iter==2 && Optim==T)
    {
      E2=Coef2
      C2=(stats(CalC_tot, yCTN)[1])
      Coef2=nextpoint(E1,E2,C1,C2,target)
      print(paste0("Optimizer step ",iter,": E2=",E2," C2=",C2))
      print(paste0("Suggesting Coef2=",Coef2))
    }
    if(iter>2 && Optim==T && apu==0)
    {
      C1=C2
      C2=(stats(CalC_tot, yCTN)[1])
      E1=E2
      E2=Coef2
      Coef2=nextpoint(E1,E2,C1,C2,target)
      print(paste0("Optimizer step ",iter,": E2=",E2," C2=",C2))
      print(paste0("Suggesting Coef2=",Coef2))
    }
    if(iter>2 && Optim==T && apu==0 && Coef2>MaxCoef)
    {
      apu<-1
      print(paste0("Maximum Coef2 reached! : ",Coef2))
    }
  }
  # if(EffScen==1){
  #   ifelse(abs(round((stats(CalC_tot, yCTN)[1])-target,1))<0.2,
  #          apu<-1,CoefTrollingF<-CoefTrollingF+0.003)
  # }
  # 
  
  
  
} # end while loop !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
