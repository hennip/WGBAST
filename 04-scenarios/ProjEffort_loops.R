# This file correspond to ProjEffort_2019_LR_EPR.r


# while loop is for finding the effort that creates target removal
apu<-0
while(apu==0){
  
  CoefRiverF<-1
  
  Coef<-Coef1*Coef2
  if(EffScen==5 || EffScen==7){
    CoefRiverF<-0
    CoefTrollingF<-0
  }
  
  # First value is for interim year (assessment year) and next for future years
  ScenE_OLL_SWE<-c(0,0)
  ScenE_OLL_FIN<-c(0,0)
  ScenE_OLL_DEN<-E_OLL_DEN*Coef # hundred thousand hookdays      
  ScenE_OLL_PL<-E_OLL_PL*Coef
  ScenE_OLL_TROLLING<-E_OLL_TROLLING*CoefTrollingF 
  propTrolling<-(E_OLL_TROLLING[1]*CoefTrollingF)/((E_OLL_DEN[1]+E_OLL_PL[1])*Coef+E_OLL_TROLLING[1]*CoefTrollingF) # only used for checking
  
  ScenE_CTN_FIN_30<-E_CTN_FIN_30*Coef # thousand trapdays
  ScenE_CTN_SWE_30<-E_CTN_SWE_30*Coef                 
  ScenE_CTN_FIN_31<-E_CTN_FIN_31*Coef       
  ScenE_CTN_SWE_31<-E_CTN_SWE_31*Coef      
  
  # =============================================================

  
  ###############################
  ## Load Inputs-files
  ###############################
  
  # save the data for each of the pieces of the MCMC chain
  for(loop in 1:10){
# loop<-1    
    BH_dataFile<-paste0(PathScen, "ScenHist_JAGSmodel", Model,"_",loop,".RData") 
    load(BH_dataFile)
    
    # Target for history
    #from 2019 assessment use yBreak+3
    for(y in 1:(yBreak+3)){
      #for(y in 1:(yBreak)){
      for(r in 1:Nstocks){
        R0[y,r,sims[1]:sims[2]]<-R_zero[,y,r]
      }
    }
    
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    # Future loop
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    
    #yBreak corresponds to the last historical year 
    for(y in (yBreak+1):(yBreak+NumFutYears)){
    # for(y in (yBreak+1):(yBreak+1)){
        #y<-yBreak+1
      
      #y<-yBreak+NumFutYears
      #River harvest assume the same as last historic year
      # CoefRiverF will be 0 in no fishing scenario, otherwise 1
      WRF_HRtmp[,y,,2,]<- WRF_HRtmp[,yBreak,,2,]*CoefRiverF          #wild  river fishing
      RRF_HRtmp[,y,,2,]<- RRF_HRtmp[,yBreak,,2,]*CoefRiverF          #reared
      
      
      #################################
      # Effort scenarios by country  ##
      #################################
      
      # Simeffort_minmax returns sample of the size of 100 (sims[3]) from
      # lognormal-dist with given min, max and mode based on expert opinion
      
      # Sweden will quit longline fishery in 2012/2013 winter.
      EffortICES[,y,"ICES 22-29","OLL","Sweden",]<-0
      
      # Effort scenario for the following year after the current (yBreak)
      # ================================================================
      if(y==(yBreak+1)){
        
        # OLL
        EffortICES[,y,"ICES 22-29","OLL","Finland",]<-ScenE_OLL_FIN[1]   #FIRST index dim is 1/all ages
        EffortICES[,y,"ICES 22-29","OLL","Sweden",]<-ScenE_OLL_SWE[1]
        EffortICES[,y,"ICES 22-29","OLL","Denmark",]<-ScenE_OLL_DEN[1]
        EffortICES[,y,"ICES 22-29","OLL","Poland",]<-ScenE_OLL_PL[1]
        EffortICES[,y,"ICES 22-29","OLL","Trolling",]<-ScenE_OLL_TROLLING[1]
        
        # CTN
        EffortICES[,y,"ICES 30","CTN","Finland",]<-ScenE_CTN_FIN_30[1]  
        EffortICES[,y,"ICES 31","CTN","Finland",]<-ScenE_CTN_FIN_31[1]       
        EffortICES[,y,"ICES 30","CTN","Sweden",]<-ScenE_CTN_SWE_30[1]                 
        EffortICES[,y,"ICES 31","CTN","Sweden",]<-ScenE_CTN_SWE_31[1]
        
      }
      
      # Effort scenarios from yBreak+1 until the end of future scenarios
      # ================================================================
      if(y>(yBreak+1)){
        
        # OLL
        EffortICES[,y,"ICES 22-29","OLL","Finland",]<-ScenE_OLL_FIN[2]
        EffortICES[,y,"ICES 22-29","OLL","Sweden",]<-ScenE_OLL_SWE[2]
        EffortICES[,y,"ICES 22-29","OLL","Denmark",]<-ScenE_OLL_DEN[2]
        EffortICES[,y,"ICES 22-29","OLL","Poland",]<-ScenE_OLL_PL[2]
        EffortICES[,y,"ICES 22-29","OLL","Trolling",]<-ScenE_OLL_TROLLING[2]
        
        # CTN
        EffortICES[,y,"ICES 30","CTN","Finland",]<-ScenE_CTN_FIN_30[2]  
        EffortICES[,y,"ICES 31","CTN","Finland",]<-ScenE_CTN_FIN_31[2]       
        EffortICES[,y,"ICES 30","CTN","Sweden",]<-ScenE_CTN_SWE_30[2]                 
        EffortICES[,y,"ICES 31","CTN","Sweden",]<-ScenE_CTN_SWE_31[2]
        
      }
      
      ############################################################################
      
      #Now to link with the biological model, translate into effort
      #by assessment unit
      
      #The offshore longline effort consists of the effort from all
      #countries in ICES units 22-29
      # Salmon from all of the units are exposed to longlining at the Main Basin
      EffortAssesUnit[,y,,"OLL",]<-EffortICES[,y,1,"OLL","Finland",]+
        EffortICES[,y,1,"OLL","Sweden",]+
        EffortICES[,y,1,"OLL","Denmark",]+
        EffortICES[,y,1,"OLL","Poland",]+
        EffortICES[,y,1,"OLL","Trolling",]
      
      # Salmon from unit 1 are affected by the Finnish coastal trapnet fishery
      # in ICES units 30 and 31 (gulf of Bothnia)
      EffortAssesUnit[,y,1,"CTN",]<-EffortICES[,y,"ICES 30","CTN","Finland",]+  #this is a bit different from historical model because mixed fishery
        EffortICES[,y,"ICES 31","CTN","Finland",] +                             #45 of Swedish effort targets unit 1 and 55% unit 2
        0.45*EffortICES[,y,"ICES 31","CTN","Sweden",]
      
      # Salmon from unit 2 are affected by the Finnish coastal trapnet fishery
      # in ICES units 30 (Aland) and by the Swedish coastal trapnet fishery in
      # ICES unit 31 (back of the gulf of Bothnia)
      EffortAssesUnit[,y,2,"CTN",]<-EffortICES[,y,"ICES 30","CTN","Finland",]+
        0.55*EffortICES[,y,"ICES 31","CTN","Sweden",]
      
      # Salmon from unit 3 are affected by the Finnish coastal trapnet fishery in
      # ICES units 30 and by the Swedish coastal trapnet fishery in ICES unit 30
      EffortAssesUnit[,y,3,"CTN",]<-EffortICES[,y,"ICES 30","CTN","Finland",]+
        EffortICES[,y,"ICES 30","CTN","Sweden",]
      
      #calculate harvest rates
      #!==========================================================================
      #! q*E= instanteneous fishing mortality
      #! exp(-F) = proportion which survives from the fishing
      #! exp(-F-M) = survives both fishing and natural mortality
      #! 1-exp(-F) = the proportion which gets harvested
      #!==========================================================================
      # Smolts/ Post-smolts
      WOLL_HRtmp[1,y,1:Nstocks,1,]<-0           #these have 100 iterations at a time which is why the're called tmp...without tmp should contain 1000 itns
      ROLL_HRtmp[1,y,1:4,1,]<-0
      WODN_HRtmp[1,y,1:Nstocks,1,]<-0
      RODN_HRtmp[1,y,1:4,1,]<-0
      
      WCDN_HRtmp[1,y,1:Nstocks,2,]<-0
      WCTN_HRtmp[1,y,1:Nstocks,2,]<-0
      RCDN_HRtmp[1,y,1:4,2,]<-0
      RCTN_HRtmp[1,y,1:4,2,]<-0
      
      WOLL_HRtmp[2,y,,1,]<- 1-exp(-qlW[1,]* EffortAssesUnit[,y,1,"OLL",])           #grilse
      WOLL_HRtmp[3:6,y,,1,]<- 1-exp(-qlW[2,]* EffortAssesUnit[,y,1,"OLL",])        #2 msw
      ROLL_HRtmp[2,y,,1,]<- 1-exp(-qlR[1,]* EffortAssesUnit[,y,1,"OLL",])
      ROLL_HRtmp[3:6,y,,1,]<- 1-exp(-qlR[2,]* EffortAssesUnit[,y,1,"OLL",])
      
      #AU4 effort=0
      
      for(u in 1:4){
        RCTN_HRtmp[2,y,u,2,]<- 1-exp(-qctnR[1,u,]* EffortAssesUnit[,y,u,"CTN",])
        RCTN_HRtmp[3:6,y,u,2,]<- 1-exp(-qctnR[2,u,]* EffortAssesUnit[,y,u,"CTN",])
      }
      
      for(r in 1:Nstocks){
        WCTN_HRtmp[2,y,r,2,]<- 1-exp(-qctnW[1,AU[r],]* EffortAssesUnit[,y,AU[r],"CTN",])
        WCTN_HRtmp[3:6,y,r,2,]<- 1-exp(-qctnW[2,AU[r],]* EffortAssesUnit[,y,AU[r],"CTN",])
      }
      
      #Population dynamics########################################################
      
      #           1    2     3     4                5
      #indices  age year stock mature(2)/immature(1)  sim
      
      # Number of salmon WsalmStock and RsalmStock will be continuously updated
      # when running this code. In order to be able to recall the original numbers
      # at age on May 1st, these data are protected by storing them in temporary
      # matrices. Number of fish at sea, available for pre-fishery before the
      # migrants are split.
      tempW<- WsalmStock       #from input file (May 1 abundances)
      tempR<- RsalmStock
      
      tempW[,y,,1,]<- WsalmStock[,y,,1,]
      tempR[,y,,1,]<- RsalmStock[,y,,1,]
      
      # On May 1st, split the number of salmon at sea into the number of salmon at
      # sea that have matured and will spawn this year and the number of salmon at
      # sea that have not yet matured and will spend at least another year at sea.
      # Because smolts spend 1 year at sea before migrating, the values for
      # WsalmMatRate and RsalmMatRate with age=1 is equal to 0.
      
      
      # On May 1st, the number of mature salmon that will migrate and spawn
      # this year is determined by
      WsalmStock[,y,,2,]<-WsalmStock[,y,,1,]*WsalmMatRate[,y,,1,]
      RsalmStock[,y,,2,]<-RsalmStock[,y,,1,]*RsalmMatRate[,y,,1,]
      
      # Use these to save the number of salmon that migrate to the river
      # for spawning (no coastal fishery taken place yet)
      temp2W[,y,,2,]<-WsalmStock[,y,,2,]    #mature on May 1
      temp2R[,y,,2,]<-RsalmStock[,y,,2,]
      
      # On May 1st, the number of salmon that have not yet matured this year and
      # that will stay at least another year at sea is determined by
      WsalmStock[,y,,1,]<-WsalmStock[,y,,1,]*(1-WsalmMatRate[,y,,1,])       #immature on May 1
      RsalmStock[,y,,1,]<-RsalmStock[,y,,1,]*(1-RsalmMatRate[,y,,1,])
      
      
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
      # fishery is given by (all ages have 1 month seal M during June)
      for(a in 1:6){
        for(r in 1:Nstocks){
          WCTN_Ctmp[a,y,r,2,]<- WsalmStock[a,y,r,2,]*exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))*
            exp(-(WsalmNatMort[a,y,r,2,]*(1/12)))*WCTN_HRtmp[a,y,r,2,]
          WsalmStock[a,y,r,2,]<-WsalmStock[a,y,r,2,]*exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))*
            exp(-(WsalmNatMort[a,y,r,2,]*(1/12)))-WCTN_Ctmp[a,y,r,2,]
        }
        # 2/12: May, June
        for(u in 1:4){
          RCTN_Ctmp[a,y,u,2,]<-RsalmStock[a,y,u,2,]*exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]*(1/12)))*
            exp(-(RsalmNatMort[a,y,u,2,]*(1/12)))*RCTN_HRtmp[a,y,u,2,]
          RsalmStock[a,y,u,2,]<-RsalmStock[a,y,u,2,]*exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]*(1/12)))*
            exp(-(RsalmNatMort[a,y,u,2,]*(1/12)))-RCTN_Ctmp[a,y,u,2,]
        }  
        
        # On October 1st the number of migrating fish caught by the river fishery is given by 
        # (WsalmStock below same as NspW in JAGS model) 
        #F_seal for post-smolt (a=1) is 1
        for(r in 1:Nstocks){
          WRF_Ctmp[a,y,r,2,]<-WsalmStock[a,y,r,2,]*exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12)))*p.ladder[a,y,r,]*WRF_HRtmp[a,y,r,2,]
          
          WsalmStock[a,y,r,2,]<-((WsalmStock[a,y,r,2,]*exp(-(WsalmNatMort[a,y,r,2,]*F_seal[y,a,AU[r]]*(1/12))))*p.ladder[a,y,r,]-WRF_Ctmp[a,y,r,2,])*exp(-(WsalmNatMort[a,y,r,2,]*(2/12)))*surv_migr[a,y,r,]
          
        }
        
        for(u in 1:4){
          RRF_Ctmp[a,y,u,2,]<-RsalmStock[a,y,u,2,]*exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]*(1/12)))*
            RRF_HRtmp[a,y,u,2,]
          RsalmStock[a,y,u,2,]<-((RsalmStock[a,y,u,2,]*exp(-(RsalmNatMort[a,y,u,2,]*F_seal[y,a,u]*(1/12))))-
                                   RRF_Ctmp[a,y,u,2,])*exp(-(RsalmNatMort[a,y,u,2,]*(2/12)))
        }
      }
      
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
          
          PFAtmpW[a,y,r,1,]<-PropCW[y-a+6]*WsalmStock[a,y,r,1,]*
            exp(-((WsalmNatMort[a,y,r,1,]*(11/12))+(WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12)+
                    (WsalmNatMort[2,y,r,1,]*(4/12))))
          
          WOLL_Ctmp[a,y,r,1,]<-PropCW[y-a+6]*(WsalmStock[a,y,r,1,]*exp(-(WsalmNatMort[a,y,r,1,]*(8/12)))*
                                                exp(-(WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12))*WOLL_HRtmp[a,y,r,1,])
          
          ## The number of fish at sea by age in 1st February after the offshore LL
          # fishery. Post-smolt are assumed not to be affected by the offshore
          # longline fishery with catches being 0 but they are affected by a high
          # post-smolt mortality rate (same as NlW in FLHM)          
          WsalmStock[a,y,r,1,]<-WsalmStock[a,y,r,1,]*exp(-(WsalmNatMort[a,y,r,1,]*(8/12)))*
            exp(-(WsalmNatMort[a,y,r,1,]*F_seal[y,a,AU[r]]/12))-WOLL_Ctmp[a,y,r,1,]
        }                  
        for(u in 1:4){
          PFAtmpR[a,y,u,1,]<-PropCR[y-a+6]*RsalmStock[a,y,u,1,]*
            exp(-((RsalmNatMort[a,y,u,1,]*(11/12))+
                    (RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12)+(RsalmNatMort[2,y,u,1,]*(4/12))))
          
          ROLL_Ctmp[a,y,u,1,]<-PropCR[y-a+6]*(RsalmStock[a,y,u,1,]*
                                                exp(-(RsalmNatMort[a,y,u,1,]*(8/12)))*
                                                exp(-(RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12))*ROLL_HRtmp[a,y,u,1,])
          # 9/12: May 1-Feb 1
          RsalmStock[a,y,u,1,]<-RsalmStock[a,y,u,1,]*
            exp(-(RsalmNatMort[a,y,u,1,]*(8/12)))*
            exp(-(RsalmNatMort[a,y,u,1,]*F_seal[y,a,u]/12))-ROLL_Ctmp[a,y,u,1,]
        }      
      }
      # 9/12: May-Feb
      for(a in 2:6){
        #a<-2
        # PFAs on 1 July              
        # the year of pfa is the same as the year of winter fishing
        PFAtmpW[a,y,,1,]<-PropCW[y-a+6]*WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*((8+6)/12)))
        PFAtmpR[a,y,,1,]<-PropCR[y-a+6]*RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*((8+6)/12)))
        
        May1stW[a,y,,1,]<-tempW[a,y,,1,]
        May1stR[a,y,,1,]<-tempR[a,y,,1,]
        MigrW[a,y,,1,]<-temp2W[a,y,,2,]
        MigrR[a,y,,1,]<-temp2R[a,y,,2,] 
        
        #this used to be in an a in 2:6 loop but seems to be an error as a indexing was missing
        #WOLL_HR is 0 for smolts
        WOLL_Ctmp[a,y,,1,]<-PropCW[y-a+6]*(WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*(9/12)))*WOLL_HRtmp[a,y,,1,])
        ROLL_Ctmp[a,y,,1,]<-PropCR[y-a+6]*(RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*(9/12)))*ROLL_HRtmp[a,y,,1,])
        # 9/12: May 1-Feb 1
        
        # On 1st February, the number of fish at sea caught by the offshore longline
        # fishery is determined by
        
        WsalmStock[a,y,,1,]<-WsalmStock[a,y,,1,]*exp(-(WsalmNatMort[a,y,,1,]*(9/12)))-WOLL_Ctmp[a,y,,1,]
        RsalmStock[a,y,,1,]<-RsalmStock[a,y,,1,]*exp(-(RsalmNatMort[a,y,,1,]*(9/12)))-ROLL_Ctmp[a,y,,1,]
      }
      
      # The number of fish at sea by age on May 1st in the next year. It is
      # assumed that the post-smolt mortality affects post-smolts for 1
      # full year, starting from May 1st till May 1st the next year.
      
      if(y<(yBreak+NumFutYears)) {
        WsalmStock[2:6,y+1,,1,]<-WsalmStock[1:5,y,,1,]*exp(-(WsalmNatMort[1:5,y,,1,]*(3/12)))
        RsalmStock[2:6,y+1,,1,]<-RsalmStock[1:5,y,,1,]*exp(-(RsalmNatMort[1:5,y,,1,]*(3/12)))
      }
      # 3/12: Feb-April
      
      # ==========================================================================
      # Stock recruit relationship
      # ==========================================================================
      #in the yBreak+1 -> loop
      #first recruits in yBreak + 2
      if(y<(yBreak+NumFutYears)){
        for(s in 1:sims[3]){
          for(r in 1:Nstocks){   #STOCK
            X<-WsalmStock[,(y-2),r,2,s]
            W<-WsalmMatRate[,(y-2),r,2,s]       #fecundity
            E<-as.vector(X*W)
            #SexRatio<-c(0,0.06,0.73,0.73,0.89,0.89)     #updated Dec 2016
            #E<-E*SexRatio
            E<-E*prop_fem[(y-2),,r]
            E<-sum(E,na.rm=T)
            E<-E*(1-M74[s,(y-2)])
            Etot_tmp[(y-2),r,s]<-E
            
            error<-exp(rnorm(1, sd = sqrt(1/precisionBH[s])))        #mean is 0
            
            #In unit 4 the smolts recruit a year earlier
            if(r>13 & r<16){
              WsalmStock[1,(y+1),r,1,s]<-as.numeric(E*error/(BH_alpha[s,r]+BH_beta[s,r]*E))
            }
            
            else{ #stocks other than 14 & 15
              
              if(y<(yBreak+NumFutYears-1)){          # for(y in (yBreak+1):(yBreak+NumFutYears))
                WsalmStock[1,(y+2),r,1,s]<-as.numeric(E*error/(BH_alpha[s,r]+BH_beta[s,r]*E))
              }
            }
          }
        }
      }
      #===========================================
      #===========================================
      
      #Restore the values at sea to what they were on May 1st to store in FLStock
      #for later analysis
      WsalmStock[,y,,1,]<-tempW[,y,,1,]
      RsalmStock[,y,,1,]<-tempR[,y,,1,]
    }#end future loop
    
    
    ##EPR calculation    
    for(i in (yBreak+1):years[3]){
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
    
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    # Historical part, include such variables which haven't been calculated before
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    for(y in 2:yBreak){
      for(a in 1:6){
        
        temp2W[a,y,,2,]<-tempW[a,y,,1,]*WsalmMatRate[a,y,,1,]
        temp2R[a,y,,2,]<-tempR[a,y,,1,]*RsalmMatRate[a,y,,1,]
        
        May1stW[a,y,,1,]<-tempW[a,y,,1,]
        May1stR[a,y,,1,]<-tempR[a,y,,1,]
        
        MigrW[a,y,,1,]<-temp2W[a,y,,2,]
        MigrR[a,y,,1,]<-temp2R[a,y,,2,]
      }
    } # y End of adds to history
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    
    ###########################################
    # Combine parameters for the whole time series
    ###########################################
    
    CatchSeaW<-WOLL_Ctmp+WODN_Ctmp
    CatchSeaR<-ROLL_Ctmp+RODN_Ctmp
    
    
    for(s in 1:sims[3]){
      #s<-1 
      i<-sims[1]-1+s
      
      for(y in 1:(yBreak+NumFutYears)){
        #y<-1
        
        # Save 1000 sims of harvest rates & efforts
        # Still missing: "EffortAssesUnit"
        for(a in 1:6){
          MaturationW[a,y,i]<-WsalmMatRate[a,y,1,1,s]
          MaturationR[a,y,i]<-RsalmMatRate[a,y,1,1,s]
          FecW[a,y,i]<-WsalmMatRate[a,y,1,2,s]
          FecR[a,y,i]<-RsalmMatRate[a,y,1,2,s]
        }
        for(a in 1:2){ # Grilse & MSW
          # add one year to the age index to correspond grilse/MSW
          # (index 1= post-smolts)
          WOLL_HR[a,y,i]<-WOLL_HRtmp[a+1,y,1,1,s]
          ROLL_HR[a,y,i]<-ROLL_HRtmp[a+1,y,1,1,s]
          WODN_HR[a,y,i]<-WODN_HRtmp[a+1,y,1,1,s]
          RODN_HR[a,y,i]<-RODN_HRtmp[a+1,y,1,1,s]
          
          WCTN_HR[a,y,1,i]<-WCTN_HRtmp[a+1,y,1,2,s] # AU1
          WCTN_HR[a,y,2,i]<-WCTN_HRtmp[a+1,y,5,2,s] # AU2
          WCTN_HR[a,y,3,i]<-WCTN_HRtmp[a+1,y,13,2,s] # AU3
          WCGN_HR[a,y,1,i]<-WCGN_HRtmp[a+1,y,1,2,s] # AU1
          WCGN_HR[a,y,2,i]<-WCGN_HRtmp[a+1,y,5,2,s] # AU2
          WCGN_HR[a,y,3,i]<-WCGN_HRtmp[a+1,y,13,2,s] # AU3
          WCDN_HR[a,y,1,i]<-WCDN_HRtmp[a+1,y,1,2,s] # AU1
          WCDN_HR[a,y,2,i]<-WCDN_HRtmp[a+1,y,5,2,s] # AU2
          WCDN_HR[a,y,3,i]<-WCDN_HRtmp[a+1,y,13,2,s] # AU3
          
          for(u in 1:3){
            RCTN_HR[a,y,u,i]<-RCTN_HRtmp[a+1,y,u,2,s]
            RCGN_HR[a,y,u,i]<-RCGN_HRtmp[a+1,y,u,2,s]
            RCDN_HR[a,y,u,i]<-RCDN_HRtmp[a+1,y,u,2,s]
          }
          
          # Combined harvest rates
          # Because fishing with different gears takes place sequentially, we need
          # to transform harvest rates first back to instantaneous mortality rates
          # (F = -log(1-HR)) and after that calculate the combined harvest rates
          
          # Combined offshore harvest rates
          OffsW_HR[a,y,i]<- 1-exp(-(-log(1-WODN_HR[a,y,i])-log(1-WOLL_HR[a,y,i])))
          OffsR_HR[a,y,i]<- 1-exp(-(-log(1-RODN_HR[a,y,i])-log(1-ROLL_HR[a,y,i])))
          
          # Combined coastal harvest rate
          for(u in 1:3){
            CoastW_HR[a,y,u,i]<-1-exp(-(-log(1-WCTN_HR[a,y,u,i])-
                                          log(1-WCGN_HR[a,y,u,i])-log(1-WCDN_HR[a,y,u,i])))
            CoastR_HR[a,y,u,i]<-1-exp(-(-log(1-RCTN_HR[a,y,u,i])-
                                          log(1-RCGN_HR[a,y,u,i])-log(1-RCDN_HR[a,y,u,i])))
          }
        }
        for(u in 1:4){
          EffortAU[y,u,,i]<-EffortAssesUnit[,y,u,,s]
        }
        
        #!========================================================================
        #! Sums the catches over the age groups
        #!========================================================================
        TornioRiverCatch[y,i]<-sum(WRF_Ctmp[,y,1,2,s], na.rm = T)
        TornioSeaCatch[y,i]<-sum(CatchSeaW[,y,1,1,s], na.rm = T)
        UmeRiverCatch[y,i]<-sum(WRF_Ctmp[,y,10,2,s], na.rm = T)
        UmeSeaCatch[y,i]<-sum(CatchSeaW[,y,10,1,s], na.rm = T)
        MorrumRiverCatch[y,i]<-sum(WRF_Ctmp[,y,14,2,s], na.rm = T)
        MorrumSeaCatch[y,i]<-sum(CatchSeaW[,y,14,1,s], na.rm = T)
        
        # This calculates the number of smolts which survive from the post
        # smolt mortality phase...
        postsmolts[y,i]<-sum(WsalmStock[1,y,,1,s],na.rm = T)*exp(-(WsalmNatMort[1,y,1,1,s]))+
          sum(RsalmStock[1,y,,1,s],na.rm = T)*exp(-(RsalmNatMort[1,y,1,1,s]))
        
        postsmoltsR[y,i]<-sum(RsalmStock[1,y,,1,s],na.rm = T)*exp(-(RsalmNatMort[1,y,1,1,s]))
        postsmoltsW[y,i]<-sum(WsalmStock[1,y,,1,s],na.rm = T)*exp(-(WsalmNatMort[1,y,1,1,s]))
        
        # Wild salmon by river
        for(r in 1:Nstocks){
          # Smolts
          SmoltW[r,y,i]<-WsalmStock[1,y,r,1,s]
          # Post-smolts
          PSW[r,y,i]<-WsalmStock[1,y,r,1,s]*exp(-(WsalmNatMort[1,y,1,1,s]))
          # Spawners
          SpawnerW[r,y,i]<-sum(WsalmStock[,y,r,2,s], na.rm = T)
          
          # Eggs per recruit     
          EPRW[y,r,i]<-EPR_M74[y,r,s]
          
          for(a in 1:6){
            spW_age[r,y,a,i]<-WsalmStock[a,y,r,2,s]
            W_age[r,y,a,i]<-WsalmStock[a,y,r,1,s]
          }
          
          Etot[y,r,i]<-Etot_tmp[y,r,s]
        }
        
        # Reared salmon by assessment units
        for(u in 1:4){
          # Smolts
          SmoltR[u,y,i]<-RsalmStock[1,y,u,1,s]
          # Post-smolts
          PSR[u,y,i]<-RsalmStock[1,y,u,1,s]*exp(-(RsalmNatMort[1,y,1,1,s]))
          # Reared "spawners" (reared fish that return to river, but can't spawn.)
          SpawnerR[u,y,i]<-sum(RsalmStock[,y,u,2,s], na.rm = T)
          
          for(a in 1:6){
            spR_age[u,y,a,i]<-RsalmStock[a,y,u,2,s]
          }
        }
        
        # Fourth index (1) are the migrating
        Migr_Tornio[y,i]<-sum(MigrW[2:6,y,1,1,s], na.rm=T)
        Migr_Simo[y,i]<-  sum(MigrW[2:6,y,2,1,s], na.rm=T)
        
        Migr_AU1W[y,i]<-  sum(MigrW[2:6,y,1:4,1,s], na.rm=T)
        Migr_AU13W[y,i]<- sum(MigrW[2:6,y,1:13,1,s], na.rm=T)+sum(MigrW[2:6,y,16:17,1,s], na.rm=T) # kage & testeboån
        Migr_AU1R[y,i]<-  sum(MigrR[2:6,y,1,1,s], na.rm=T)
        Migr_AU13R[y,i]<- sum(MigrR[2:6,y,1:3,1,s], na.rm=T)
        Migr_AU13tot[y,i]<-Migr_AU13W[y,i]+Migr_AU13R[y,i]
        
        CalC_OLL[y,i]<-sum(WOLL_Ctmp[2:6,y,,1,s], na.rm = T)+
          sum(ROLL_Ctmp[2:6,y,,1,s], na.rm = T)
        CalC_CTN[y,i]<-sum(WCTN_Ctmp[2:6,y,,1,s], na.rm = T)+
          sum(RCTN_Ctmp[2:6,y,,1,s], na.rm = T)
        
        CatchRiver[y,i]<-sum(WRF_Ctmp[2:6,y,,2,s], na.rm=T)+
          sum(RRF_Ctmp[2:6,y,,2,s], na.rm=T)
        
        # Proportions of spawners by ages
        # = numb1SWspawners/all spawners
        #iniAgeQuantW  <- array(NA, dim = c(ages[3], years[3], 15,2,1,sims[3]))
        
        Prop1SWsp[y,i]<-sum(WsalmStock[2,y,,2,s], na.rm = T)/
          sum(WsalmStock[,y,,2,s], na.rm = T)
        Prop2SWsp[y,i]<-sum(WsalmStock[3,y,,2,s], na.rm = T)/
          sum(WsalmStock[,y,,2,s], na.rm = T)
        Prop3SWsp[y,i]<-sum(WsalmStock[4,y,,2,s], na.rm = T)/
          sum(WsalmStock[,y,,2,s], na.rm = T)
        Prop4SWsp[y,i]<-sum(WsalmStock[5,y,,2,s], na.rm = T)/
          sum(WsalmStock[,y,,2,s], na.rm = T)
        
        WOLLCtot[,y,,i]<-WOLL_Ctmp[,y,,1,s]
        ROLLCtot[,y,,i]<-ROLL_Ctmp[,y,,1,s]
        WCTNCtot[,y,,i]<-WCTN_Ctmp[,y,,2,s]
        RCTNCtot[,y,,i]<-RCTN_Ctmp[,y,,2,s]
        
        PFAW[,y,,,i]<-PFAtmpW[,y,,,s]
        PFAR[,y,,,i]<-PFAtmpR[,y,,,s]
      }
      
      #fill in R0S
      for(r in 1:Nstocks){
        for(y in (yBreak+4):(yBreak+NumFutYears)){
          R0[y,r,i]<-(K[s,r]*(EPR_M74[(y-e_delay[r]),r,s]-BH_alpha[s,r]))/EPR_M74[(y-e_delay[r]),r,s] 
        }
        
      }
    }
    
  } # end loop of 10 pieces !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  # =============================================================
  # Check if total removal is what we wanted:
  
  year<-c(1992:LastPredYear)
  length(year)
  Nyears<-length(year)
  
  stats<-function(dat,v){
    sumDat<-summary(as.mcmc(dat[v,]))
    
    med<-sumDat$quantiles[3]
    high<-sumDat$quantiles[5]
    low<-sumDat$quantiles[1]
    
    return(rbind(med, low, high))
  }
  
  
  C_OLL<-array(NA, c(Nyears,1000))
  C_CTN<-array(NA, c(Nyears,1000))
  CalC_tot<-array(NA, c(Nyears,1000))
  for(i in 1:1000){
    for(y in 1:Nyears){
      C_OLL[y,i]<-sum(WOLLCtot[2:6,y,1:Nstocks,i])+sum(ROLLCtot[2:6,y,1:4,i])
      C_CTN[y,i]<-sum(WCTNCtot[2:6,y,1:Nstocks,i])+sum(RCTNCtot[2:6,y,1:4,i])
      if(y>1){
        #COASTAL FISHING IS IN CALENDAR /ADVICE YEAR BUT LONGLINE IS IN THE PREVIOUS ONE
        CalC_tot[y,i]<-C_OLL[y-1,i]+C_CTN[y,i]
      }
    }
  }
  
  # calendar year 2020 is year 29 for trapnetting and 
  # year 28 for offshore fisheries
  if(EffScen<6 || EffScen>6){
    print("Coef2")
    print(Coef2)
  }
  if(EffScen==6){
    print("CoefTrollingF")
    print(CoefTrollingF)
  }
  print("Total sea catch")
  print(stats(CalC_tot, yCTN)[1]) 
  print(target) #Total target
  print("***")
  if(EffScen<6 || EffScen>6){
    print("Trolling catch")
    print(stats(C_OLL, yOLL)[1]*propTrolling)
    print(targetTr) # Trolling target
  }
  print("--------")
  if(EffScen<6 || EffScen>6){
    ifelse(abs(round((stats(CalC_tot, yCTN)[1])-target,1))<0.2,
           apu<-1,Coef2<-Coef2+0.003)
  }
  if(EffScen==6){
    ifelse(abs(round((stats(CalC_tot, yCTN)[1])-target,1))<0.2,
           apu<-1,CoefTrollingF<-CoefTrollingF+0.003)
  }
  
  
} # end while loop !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
