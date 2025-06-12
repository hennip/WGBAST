get_R0<-function(Model,stock,histyr,ymax,evec,nsim){

#istock<-stocks[ij]     #debugging
#evec<-errorSR[,,stocks[ij]]

Coef<-0
istock<-stock
nsim<-nsim
ymax<-ymax 

LastHistYear<-histyr
LastPredYear<-1992+ymax
#FUTURE PROJECTIONS BASED ON EFFORT SCENARIOS
NumFutYears<-LastPredYear-LastHistYear


AUS<-4
Nstocks<-17
HistYears<-c(1992:LastHistYear)
#Define a year that separates historic part from future part
yBreak<-length(HistYears)

    
years<-c(1992,LastPredYear)
years<-c(years[],yBreak+NumFutYears)
yCTN<-years[3]
  
z.tmp<-array(NA,dim=c(Nstocks,years[3],100))
z.tmp1<-array(NA,dim=c(Nstocks,years[3],100))
z.all<-array(NA,dim=c(Nstocks,years[3],nsim))
EPR.all<-array(NA,dim=c(Nstocks,years[3],nsim))
Etot.all<-array(NA,dim=c(Nstocks,years[3],nsim))
zprop<-array(NA,dim=c(Nstocks,years[3]))  
sprop<-array(NA,dim=c(Nstocks,years[3]))

eggs1SW.tmp<-array(NA,dim=c(Nstocks,years[3],100))
eggs2SW.tmp<-array(NA,dim=c(Nstocks,years[3],100))
eggs3SW.tmp<-array(NA,dim=c(Nstocks,years[3],100))
eggs4SW.tmp<-array(NA,dim=c(Nstocks,years[3],100))
eggs5SW.tmp<-array(NA,dim=c(Nstocks,years[3],100))

eggs1SW.all<-array(NA,dim=c(Nstocks,years[3],nsim))
eggs2SW.all<-array(NA,dim=c(Nstocks,years[3],nsim))
eggs3SW.all<-array(NA,dim=c(Nstocks,years[3],nsim))
eggs4SW.all<-array(NA,dim=c(Nstocks,years[3],nsim))
eggs5SW.all<-array(NA,dim=c(Nstocks,years[3],nsim))

eggs1SW<-array(NA,dim=c(Nstocks,years[3]))
eggs2SW<-array(NA,dim=c(Nstocks,years[3]))
eggs3SW<-array(NA,dim=c(Nstocks,years[3]))
eggs4SW<-array(NA,dim=c(Nstocks,years[3]))
eggs5SW<-array(NA,dim=c(Nstocks,years[3]))


source("04-scenarios/refpts/InitArrays_R0.R")

CoefRiverF<-0                      #ICES COEFS
CoefTrollingF<-0  #value from 2019 ProjEffort code  
     
        ScenE_OLL_SWE<-c(0,0)*Coef          #Effort offshore longlines
        ScenE_OLL_FIN<-c(0,0)*Coef
        ScenE_OLL_DEN<-c(0.42,0.42)*Coef # hundred thousand hookdays      
        ScenE_OLL_PL<-c(13.5,13.5)*Coef
        ScenE_OLL_TROLLING<-c(6.56,6.56)*CoefTrollingF  ### NEW!!! Give scenarios to trolling (included in LL)
        
        ScenE_CTN_FIN_30<-c(3.9,3.9)*Coef # thousand trapdays coastal trapnets by ICES subdiv.
        ScenE_CTN_SWE_30<-c(4.8,4.8)*Coef   
        ScenE_CTN_FIN_31<-c(5.6,5.6)*Coef       
        ScenE_CTN_SWE_31<-c(7.7,6.1)*Coef    


 
      
        # ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
        #############################
        ##    Load input files     ##
        #############################
        
        # save the data for each of the pieces of the MCMC chain
 for(loop in 1:(nsim/100)){
 #       loop<-1

    BH_dataFile<-paste0(PathOut_Scen, "ScenHist_", Model,"_",loop,".RData")

  load(BH_dataFile)

    #WsalmNatMort[1,,,1,tmpind]<-WsalmNatMort[1,yBreak,,1,tmpind]
    #           1    2     3     4                5
    #indices  age year stock river(2) or sea(1)  sim    
    
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    # Future loop
    # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
    
    #yBreak corresponds to the last historical year 
    for(y in (yBreak+1):(yBreak+NumFutYears)){
      #y<-yBreak+1
      
      #y<-yBreak+NumFutYears
      #River harvest assume the same as last historic year
      # CoefRiverF will be 0 in no fishing scenario, otherwise 1
      WRF_HRtmp[,y,,2,]<- WRF_HRtmp[,yBreak,,2,]*CoefRiverF          #wild  river fishing
      RRF_HRtmp[,y,,2,]<- RRF_HRtmp[,yBreak,,2,]*CoefRiverF          #reared
      
         # Use the catchability of last observed year for LL or change 
      qlW[,y,]<-qlW[,yBreak,]
      qlR[,y,]<-qlR[,yBreak,]
      
      #################################
      # Effort scenarios by country  ##
      #################################
      
      # Simeffort_minmax returns sample of the size of 100 (sims[3]) from
      # lognormal-dist with given min, max and mode based on expert opinion
      
      # Sweden will quit longline fishery in 2012/2013 winter.
      EffortICES[,y,"ICES 22-29","OLL","Sweden",]<-0
      
      # Effort scenario for the following year after the current (yBreak)
      # ================================================================
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
        EffortICES[,y,1,"OLL","Poland",]#+
        #EffortICES[,y,1,"OLL","Trolling",]
      
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
      WOLL_HRtmp[1,y,1:Nstocks,1,]<-0           #these have 100 iterations at a time which is why the're called tmp...without tmp should contain nsim itns
      ROLL_HRtmp[1,y,1:4,1,]<-0
      WODN_HRtmp[1,y,1:Nstocks,1,]<-0
      RODN_HRtmp[1,y,1:4,1,]<-0
      
      WCDN_HRtmp[1,y,1:AUS,2,]<-0
      WCTN_HRtmp[1,y,1:AUS,2,]<-0
      RCDN_HRtmp[1,y,1:AUS,2,]<-0
      RCTN_HRtmp[1,y,1:AUS,2,]<-0
      
      WOLL_HRtmp[2,y,,1,]<- 1-exp(-qlW[1,y,]* EffortAssesUnit[,y,1,"OLL",])           #grilse
      WOLL_HRtmp[3:6,y,,1,]<- 1-exp(-qlW[2,y,]* EffortAssesUnit[,y,1,"OLL",])        #2 msw
      ROLL_HRtmp[2,y,,1,]<- 1-exp(-qlR[1,y,]* EffortAssesUnit[,y,1,"OLL",])
      ROLL_HRtmp[3:6,y,,1,]<- 1-exp(-qlR[2,y,]* EffortAssesUnit[,y,1,"OLL",])


      #AU4 effort=0
      
      for(u in 1:4){
        RCTN_HRtmp[2,y,u,2,]<- 1-exp(-qctnR[1,u,]* EffortAssesUnit[,y,u,"CTN",])
        RCTN_HRtmp[3:6,y,u,2,]<- 1-exp(-qctnR[2,u,]* EffortAssesUnit[,y,u,"CTN",])
      }
      
      #for(r in 1:Nstocks){
        WCTN_HRtmp[2,y,AU[istock],2,]<- 1-exp(-qctnW[1,AU[istock],]* EffortAssesUnit[,y,AU[istock],"CTN",])
        WCTN_HRtmp[3:6,y,AU[istock],2,]<- 1-exp(-qctnW[2,AU[istock],]* EffortAssesUnit[,y,AU[istock],"CTN",])
      #}
      
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
       # for(r in 1:Nstocks){
          WCTN_Ctmp[a,y,istock,2,]<- WsalmStock[a,y,istock,2,]*exp(-(WsalmNatMort[a,y,istock,2,]*F_seal[y,a,AU[istock]]*(1/12)))*
            exp(-(WsalmNatMort[a,y,istock,2,]*(1/12)))*WCTN_HRtmp[a,y,AU[istock],2,]
          WsalmStock[a,y,istock,2,]<-WsalmStock[a,y,istock,2,]*exp(-(WsalmNatMort[a,y,istock,2,]*F_seal[y,a,AU[istock]]*(1/12)))*
            exp(-(WsalmNatMort[a,y,istock,2,]*(1/12)))-WCTN_Ctmp[a,y,istock,2,]
        #}
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
        #for(r in 1:Nstocks){
          WRF_Ctmp[a,y,istock,2,]<-WsalmStock[a,y,istock,2,]*exp(-(WsalmNatMort[a,y,istock,2,]*F_seal[y,a,AU[istock]]*(1/12)))*p.ladder[a,y,istock,]*WRF_HRtmp[a,y,istock,2,]
 
          WsalmStock[a,y,istock,2,]<-((WsalmStock[a,y,istock,2,]*exp(-(WsalmNatMort[a,y,istock,2,]*F_seal[y,a,AU[istock]]*(1/12))))*p.ladder[a,y,istock,]-WRF_Ctmp[a,y,istock,2,])*exp(-(WsalmNatMort[a,y,istock,2,]*(2/12)))*surv_migr[a,y,istock,]

        #}
        
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
      #  for(r in 1:Nstocks){
          #PFA on 1 Sept
          # (7+5)/12: May-April
          # 4 months of adult M -> half point of the rest of the months that are left of that
          # calendar year
          
          PFAtmpW[a,y,istock,1,]<-PropCW[y-a+6]*WsalmStock[a,y,istock,1,]*
            exp(-((WsalmNatMort[a,y,istock,1,]*(11/12))+(WsalmNatMort[a,y,istock,1,]*F_seal[y,a,AU[istock]]/12)+
                    (WsalmNatMort[2,y,istock,1,]*(4/12))))
          
          WOLL_Ctmp[a,y,istock,1,]<-PropCW[y-a+6]*(WsalmStock[a,y,istock,1,]*exp(-(WsalmNatMort[a,y,istock,1,]*(8/12)))*
                                                exp(-(WsalmNatMort[a,y,istock,1,]*F_seal[y,a,AU[istock]]/12))*WOLL_HRtmp[a,y,istock,1,])
          
          ## The number of fish at sea by age in 1st February after the offshore LL
          # fishery. Post-smolt are assumed not to be affected by the offshore
          # longline fishery with catches being 0 but they are affected by a high
          # post-smolt mortality rate (same as NlW in FLHM)          
          WsalmStock[a,y,istock,1,]<-WsalmStock[a,y,istock,1,]*exp(-(WsalmNatMort[a,y,istock,1,]*(8/12)))*
            exp(-(WsalmNatMort[a,y,istock,1,]*F_seal[y,a,AU[istock]]/12))-WOLL_Ctmp[a,y,istock,1,]
       # }                  
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
          #for(r in 1:Nstocks){   #STOCK
            
            X<-WsalmStock[,(y-2),istock,2,]
            W<-WsalmMatRate[,(y-2),istock,2,]       #fecundity
            E<-as.matrix(X*W)
            E<-E*prop_fem[(y-2),,istock]
            E<-apply(E,2,sum,na.rm=T)
            E<-E*(1-M74[,(y-2)])
            Etot_tmp[(y-2),istock,]<-E
            #SexRatio<-c(0,0.06,0.73,0.73,0.89,0.89)     #updated Dec 2016
            #E<-E*SexRatio
            
            jj<-(loop*100)-99
            jk<-loop*100
            error<-evec[jj:jk,y]      
            #error<-exp(rnorm(100, sd = sqrt(1/precisionBH)))    #mean is 0
            #error<-rep(1,times=100)
            
            #In unit 4 the smolts recruit a year earlier
            if(istock>13 & istock<16){
              WsalmStock[1,(y+1),istock,1,]<-as.numeric(E*error/(BH_alpha[,istock]+BH_beta[,istock]*E))
            }
            
            else{ #stocks other than 14 & 15

              if(y<(yBreak+NumFutYears-1)){          # for(y in (yBreak+1):(yBreak+NumFutYears))
                WsalmStock[1,(y+2),istock,1,]<-as.numeric(E*error/(BH_alpha[,istock]+BH_beta[,istock]*E))
              }
            }
         # }
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


#here i index denotes smolt year
      
    simm[i,1,istock,]<-exp(-(11*WsalmNatMort[1,i,istock,2,]/12))*exp(-(WsalmNatMort[1,i,istock,2,]*F_seal[i,1,AU[istock]]/12))    
   smat[i,1,istock,]<-1    #not used anywhere
      
   for(a in 2:6){
        simm[i,a,istock,]<-exp(-(12*WsalmNatMort[a,i,istock,2,]/12))
        smat[i,a,istock,]<-exp(-(3*WsalmNatMort[a,i,istock,2,]/12))*exp(-(2*WsalmNatMort[a,i,istock,2,]*F_seal[i,a,AU[istock]]/12))*p.ladder[a,i,istock,]*surv_migr[a,i,istock,]
    }   

#EPR calculation (check year index for fec if this is changed to vary by year)

eggs1SW.tmp[istock,i,]<-pmat[i,2,]*prop_fem[(i-1),2,istock]*WsalmMatRate[2,(i-1),istock,2,]*simm[(i-1),1,istock,]*smat[(i-1),2,istock,]

eggs2SW.tmp[istock,i,]<-pmat[i,3,]*prop_fem[(i-2),3,istock]*WsalmMatRate[3,(i-2),istock,2,]*simm[(i-2),1,istock,]*simm[(i-1),2,istock,]*smat[(i-2),3,istock,]
 
eggs3SW.tmp[istock,i,]<-pmat[i,4,]*prop_fem[(i-3),4,istock]*WsalmMatRate[4,(i-3),istock,2,]*simm[(i-3),1,istock,]*simm[(i-2),2,istock,]*simm[(i-1),3,istock,]*smat[(i-3),4,istock,]
 
eggs4SW.tmp[istock,i,]<-pmat[i,5, ]*prop_fem[(i-4),5,istock]*WsalmMatRate[5,(i-4),istock,2,]*simm[(i-4),1,istock,]*simm[(i-3),2,istock,]*simm[(i-2),3,istock,]*simm[(i-1),4,istock,]*smat[(i-4),5,istock,]
 
eggs5SW.tmp[istock,i,]<-pmat[i,6,]*prop_fem[(i-5),6,istock]*WsalmMatRate[6,(i-5),istock,2,]*simm[(i-5),1,istock,]*simm[(i-4),2,istock,]*simm[(i-3),3,istock,]*simm[(i-2),4,istock,]*simm[(i-1),5,istock,]*smat[(i-5),6,istock,]     #5SW

EPR[i,istock,]<-(eggs1SW.tmp[istock,i,]+eggs2SW.tmp[istock,i,]+eggs3SW.tmp[istock,i,]+eggs4SW.tmp[istock,i,]+eggs5SW.tmp[istock,i,])    
EPR_M74[i,istock,]<-EPR[i,istock,]*(1-M74[,(i)])


z.tmp[istock,i,] <-((1/BH_alpha[,istock])*EPR_M74[i,istock,])/(4+(1/BH_alpha[,istock])*EPR_M74[i,istock,])  
z.tmp1[istock,i,]<-sum(which(z.tmp[istock,i,]<0.20))  
}



 # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
 # Historical part, include such variables which haven't been calculated before
 # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*
  for(y in 2:yBreak){
    for(a in 2:6){ # MSW salmon
      temp2W[a,y,,2,]<-tempW[a,y,stock_indices,1,]*WsalmMatRate[a,y,stock_indices,1,]
      temp2R[a,y,,2,]<-tempR[a,y,,1,]*RsalmMatRate[a,y,,1,]

      May1stW[a,y,,1,]<-tempW[a,y,stock_indices,1,]
      May1stR[a,y,,1,]<-tempR[a,y,,1,]

      MigrW[a,y,,1,]<-temp2W[a,y,,2,]
      MigrR[a,y,,1,]<-temp2R[a,y,,2,]
    }
  } # End of adds to history
  # ~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*~*~*~*~*~~*~*~*~*

          ###########################################
          # Combine parameters for the whole time series      #THIS COMBINES THE BLOCKS OF 100 TOGETHER
          ###########################################
        
          CatchSeaW<-WOLL_Ctmp+WODN_Ctmp
          CatchSeaR<-ROLL_Ctmp+RODN_Ctmp
        
          for( y in 1:(yBreak+NumFutYears)){
            #y<-1
            #for(s in 1:sims[3]){
#            #for(s in 1:2){
#            #s<-1 
#              i<-sims[1]-1+s
               jj<-(loop*100)-99
               jk<-loop*100
              # Save nsim sims of harvest rates & efforts
              # Still missing: "EffortAssesUnit"
              
         
              for(a in 1:6){
                  MaturationW[a,y,jj:jk]<-WsalmMatRate[a,y,1,1,]
                  MaturationR[a,y,jj:jk]<-RsalmMatRate[a,y,1,1,]
              }
                
              for(a in 1:2){ # Grilse & MSW
                # add one year to the age index to correspond grilse/MSW
                # (index 1= post-smolts)
                WOLL_HR[a,y,jj:jk]<-WOLL_HRtmp[a+1,y,1,1,]
                ROLL_HR[a,y,jj:jk]<-ROLL_HRtmp[a+1,y,1,1,]
                WODN_HR[a,y,jj:jk]<-WODN_HRtmp[a+1,y,1,1,]
                RODN_HR[a,y,jj:jk]<-RODN_HRtmp[a+1,y,1,1,]
                WRF_HR[a,y,jj:jk]<-WRF_HRtmp[a,y,1,2,]      #not stock specific
                RRF_HR[a,y,jj:jk]<-RRF_HRtmp[a,y,1,2,]
        
                WCTN_HR[a,y,1,jj:jk]<-WCTN_HRtmp[a+1,y,1,2,] # AU1
                WCTN_HR[a,y,2,jj:jk]<-WCTN_HRtmp[a+1,y,2,2,] # AU2
                WCTN_HR[a,y,3,jj:jk]<-WCTN_HRtmp[a+1,y,3,2,] # AU3
                WCTN_HR[a,y,4,jj:jk]<-WCTN_HRtmp[a+1,y,4,2,] # AU4
                
                WCGN_HR[a,y,1,jj:jk]<-WCGN_HRtmp[a+1,y,1,2,] # AU1
                WCGN_HR[a,y,2,jj:jk]<-WCGN_HRtmp[a+1,y,5,2,] # AU2
                WCGN_HR[a,y,3,jj:jk]<-WCGN_HRtmp[a+1,y,13,2,] # AU3
                WCGN_HR[a,y,4,jj:jk]<-WCGN_HRtmp[a+1,y,14,2,] # AU4
                
                WCDN_HR[a,y,1,jj:jk]<-WCDN_HRtmp[a+1,y,1,2,] # AU1
                WCDN_HR[a,y,2,jj:jk]<-WCDN_HRtmp[a+1,y,2,2,] # AU2
                WCDN_HR[a,y,3,jj:jk]<-WCDN_HRtmp[a+1,y,3,2,] # AU3
                WCDN_HR[a,y,4,jj:jk]<-WCDN_HRtmp[a+1,y,4,2,] # AU4
        
                for(u in 1:3){
                  RCTN_HR[a,y,u,jj:jk]<-RCTN_HRtmp[a+1,y,u,2,]
                  RCGN_HR[a,y,u,jj:jk]<-RCGN_HRtmp[a+1,y,u,2,]
                  RCDN_HR[a,y,u,jj:jk]<-RCDN_HRtmp[a+1,y,u,2,]
                }
        
                # Combined harvest rates
                # Because fishing with different gears takes place sequentially, we need
                # to transform harvest rates first back to instantaneous mortality rates
                # (F = -log(1-HR)) and after that calculate the combined harvest rates
        
                # Combined offshore harvest rates
                OffsW_HR[a,y,jj:jk]<- 1-exp(-(-log(1-WODN_HR[a,y,jj:jk])-log(1-WOLL_HR[a,y,jj:jk])))
                OffsR_HR[a,y,jj:jk]<- 1-exp(-(-log(1-RODN_HR[a,y,jj:jk])-log(1-ROLL_HR[a,y,jj:jk])))
        
                # Combined coastal harvest rate
                for(u in 1:3){
                  CoastW_HR[a,y,u,jj:jk]<-1-exp(-(-log(1-WCTN_HR[a,y,u,jj:jk])-
                            log(1-WCGN_HR[a,y,u,jj:jk])-log(1-WCDN_HR[a,y,u,jj:jk])))
                  CoastR_HR[a,y,u,jj:jk]<-1-exp(-(-log(1-RCTN_HR[a,y,u,jj:jk])-
                            log(1-RCGN_HR[a,y,u,jj:jk])-log(1-RCDN_HR[a,y,u,jj:jk])))
                }
              }
              
              WRF_HR[3,y,jj:jk]<-WRF_HRtmp[3,y,1,2,]      #not stock specific
              RRF_HR[3,y,jj:jk]<-RRF_HRtmp[3,y,1,2,]
        
              for(u in 1:4){
                EffortAU[y,u,,jj:jk]<-EffortAssesUnit[,y,u,,]
              }
        
              #!========================================================================
              #! Sums the catches over the age groups
              #!========================================================================
             
              TornioRiverCatch[y,jj:jk]<-apply(WRF_Ctmp[,y,1,2,],2,sum, na.rm = TRUE)
              TornioSeaCatch[y,jj:jk]<-apply(CatchSeaW[,y,1,1,],2,sum, na.rm = TRUE) 
              UmeRiverCatch[y,jj:jk]<-apply(WRF_Ctmp[,y,10,2,],2,sum, na.rm = TRUE)
              UmeSeaCatch[y,jj:jk]<-apply(CatchSeaW[,y,10,1,],2,sum, na.rm = TRUE)
        
                 # This calculates the number of smolts which survive from the post
              # smolt mortality phase...
           postsmolts[y,jj:jk]<-
            apply(WsalmStock[1,y,,1,],2,sum,na.rm = TRUE)*exp(-(WsalmNatMort[1,y,1,1,]))+
            apply(RsalmStock[1,y,,1,],2,sum,na.rm = TRUE)*exp(-(RsalmNatMort[1,y,1,1,]))
        
           postsmoltsR[y,jj:jk]<-apply(RsalmStock[1,y,,1,],2,sum,na.rm = TRUE)*
                             exp(-(RsalmNatMort[1,y,1,1,]))
        
           postsmoltsW[y,jj:jk]<-apply(WsalmStock[1,y,,1,],2,sum,na.rm = TRUE)*
                             exp(-(WsalmNatMort[1,y,1,1,]))
                             
                             
               # Wild salmon by river
              for(r in 1:Nstocks){
                # Smolts
                SmoltW[r,y,jj:jk]<-WsalmStock[1,y,r,1,]
                # Post-smolts
                PSW[r,y,jj:jk]<-WsalmStock[1,y,r,1,]*exp(-(WsalmNatMort[1,y,1,1,]))
                # Spawners
                SpawnerW[r,y,jj:jk]<-apply(WsalmStock[,y,r,2,],2,sum, na.rm = TRUE)               #ESCAPEMENT
        
                for(a in 1:6){
                  spW_age[r,y,a,jj:jk]<-WsalmStock[a,y,r,2,]
                }
                
                   
                CatchRiverS[r,y,jj:jk]<-apply(WRF_Ctmp[2:6,y,r,2,],2,sum, na.rm=T)
              
                
                z.all[r,y,jj:jk]<-z.tmp[r,y,]
                EPR.all[r,y,jj:jk]<-EPR_M74[y,r,]
                Etot.all[r,y,jj:jk]<-Etot_tmp[y,r,]
                
                   eggs1SW.all[r,y,jj:jk]<-eggs1SW.tmp[r,y,]
                      eggs2SW.all[r,y,jj:jk]<-eggs2SW.tmp[r,y,]
                         eggs3SW.all[r,y,jj:jk]<-eggs3SW.tmp[r,y,]
                            eggs4SW.all[r,y,jj:jk]<-eggs4SW.tmp[r,y,]
                               eggs5SW.all[r,y,jj:jk]<-eggs5SW.tmp[r,y,]
        
             }
             # Reared salmon by assessment units
              for(u in 1:4){
                # Smolts
                SmoltR[u,y,jj:jk]<-RsalmStock[1,y,u,1,]
                # Post-smolts
                PSR[u,y,jj:jk]<-RsalmStock[1,y,u,1,]*exp(-(RsalmNatMort[1,y,1,1,]))
                # Reared "spawners" (reared fish that return to river, but can't spawn.)
                SpawnerR[u,y,jj:jk]<-apply(RsalmStock[,y,u,2,],2,sum, na.rm = TRUE)                     #ESCAPEMENT
        
                for(a in 1:6){
                  spR_age[u,y,a,jj:jk]<-RsalmStock[a,y,u,2,]
                }
              }

              Migr_Tornio[y,]<-apply(MigrW[2:6,y,1,1,],2,sum, na.rm=T)
              Migr_Simo[y,]<-  apply(MigrW[2:6,y,2,1,],2,sum, na.rm=T)
        
              Migr_AU1W[y,]<-  apply(MigrW[2:6,y,1:4,1,],3,sum, na.rm=T)
              Migr_AU13W[y,]<- apply(MigrW[2:6,y,1:13,1,],3,sum, na.rm=T)
              Migr_AU1R[y,]<-  apply(MigrR[2:6,y,1,1,],2,sum, na.rm=T)
              Migr_AU13R[y,]<- apply(MigrR[2:6,y,1:3,1,],3,sum, na.rm=T)
              Migr_AU13tot[y,]<-Migr_AU13W[y,]+Migr_AU13R[y,]
        
             CalC_OLL[y,jj:jk]<-apply(WOLL_Ctmp[2:6,y,,1,],3,sum, na.rm = TRUE)+
                             apply(ROLL_Ctmp[2:6,y,,1,],3,sum, na.rm = TRUE)
              CalC_CTN[y,jj:jk]<-apply(WCTN_Ctmp[2:6,y,,1,],3,sum, na.rm = TRUE)+
                             apply(RCTN_Ctmp[2:6,y,,1,],3,sum, na.rm = TRUE)
        
              CatchRiver[y,jj:jk]<-apply(WRF_Ctmp[2:6,y,,2,],3,sum, na.rm=T)+
                               apply(RRF_Ctmp[2:6,y,,2,],3,sum, na.rm=T)
        
              # Proportions of spawners by ages
              # = numb1SWspawners/all spawners
              #iniAgeQuantW  <- array(NA, dim = c(ages[3], years[3], 15,2,1,sims[3]))
        
              Prop1SWsp[y,jj:jk]<-apply(WsalmStock[2,y,,2,],2,sum, na.rm = TRUE)/
                              apply(WsalmStock[,y,,2,],3,sum, na.rm = TRUE)
              Prop2SWsp[y,jj:jk]<-apply(WsalmStock[3,y,,2,],2,sum, na.rm = TRUE)/
                              apply(WsalmStock[,y,,2,],3,sum, na.rm = TRUE)
              Prop3SWsp[y,jj:jk]<-apply(WsalmStock[4,y,,2,],2,sum, na.rm = TRUE)/
                              apply(WsalmStock[,y,,2,],3,sum, na.rm = TRUE)
              Prop4SWsp[y,jj:jk]<-apply(WsalmStock[5,y,,2,],2,sum, na.rm = TRUE)/
                              apply(WsalmStock[,y,,2,],3,sum, na.rm = TRUE)
        
            WOLLCtot[,y,,jj:jk]<-WOLL_Ctmp[,y,,1,]
            ROLLCtot[,y,,jj:jk]<-ROLL_Ctmp[,y,,1,]
            WCTNCtot[,y,,jj:jk]<-WCTN_Ctmp[,y,,2,]
            RCTNCtot[,y,,jj:jk]<-RCTN_Ctmp[,y,,2,]
            
            WStockAll[,y,,,jj:jk]<-WsalmStock[,y,,,]
            
    
        
            PFAW[,y,,,jj:jk]<-PFAtmpW[,y,,,]
            PFAR[,y,,,jj:jk]<-PFAtmpR[,y,,,]
           # }   #s
          } #y
      } # end loop of 10 pieces
        
        
        #Combine relevant information
        Perform_Stats <- c(
        "MaturationW","MaturationR",
        "postsmolts","postsmoltsR","postsmoltsW", "Mps_All", "M74",
        "SmoltW", "SmoltR", "SpawnerW","SpawnerR", "PSW", "PSR",
        "spW_age", "spR_age",
        "EffortAU", "EffortAssesUnit",
        "CatchRiver",
        "WOLL_HR","ROLL_HR",
        "WODN_HR","RODN_HR",
        "WCTN_HR","RCTN_HR",
        "WCGN_HR","RCGN_HR",
        "WCDN_HR","RCDN_HR",
        "CoastW_HR","CoastR_HR","OffsW_HR","OffsR_HR",
        "May1stW","May1stR",
        "MigrW","MigrR",
        "Migr_Tornio","Migr_Simo","Migr_AU1W","Migr_AU13W",
        "Migr_AU1R","Migr_AU13R","Migr_AU13tot",
        "PFAW", "PFAR",
        "WOLLCtot", "ROLLCtot",
        "WCTNCtot", "RCTNCtot"
        )
        
      
    
        # ??????????????????????????????????????
        # Check if total removal is what we wanted:
    
        
        C_OLL<-array(NA, c(yCTN,Nstocks,nsim))
        C_CTN<-array(NA, c(yCTN,Nstocks,nsim))
        WStock<-array(NA, c(yCTN,Nstocks,nsim))
        CalC_tot<-array(NA, c(yCTN,Nstocks,nsim))
        HR_tot<-array(NA, c(yCTN,Nstocks,2, nsim))    #2 is age 2 and ages 3:6 blocks for q
        HR_totR<-array(NA, c(yCTN,Nstocks,2, nsim))
        HR_smolt<-array(NA, c(yCTN,Nstocks,nsim))

        for(iind in 1:nsim){
          for(y in 1:yCTN){
            #for(s in 1:Nstocks){
                  C_OLL[y,istock,iind]<-sum(WOLLCtot[2:6,y,istock,iind])  
                  C_CTN[y,istock,iind]<-sum(WCTNCtot[2:6,y,istock,iind])
                  WStock[y,istock,iind]<-sum(WStockAll[1:6,y,istock,2,iind])      #mature fish       WStockAll[,y,,,iind]<-WsalmStock[,y,,,istock]    0 mature age 1
                  if(y>1){
                             CalC_tot[y,istock,iind]<-C_OLL[y-1,istock,iind]+C_CTN[y,istock,iind] #+CatchRiverS[s,y,iind]    #ICES COEFS COASTAL FISHING IS IN CALENDAR /ADVICE YEAR BUT LONGLINE IS IN THE PREVIOUS ONE
                              #CalC_tot[y,s,iind]<-C_OLL[y-1,s,iind]+C_CTN[y,s,iind]+CatchRiverS[s,y,iind]    #REC VARYING COASTAL FISHING IS IN CALENDAR /ADVICE YEAR BUT LONGLINE IS IN THE PREVIOUS ONE
                         }
            #} #s  
          }
        }
        #use apply to get distributions over samples!
     
        smolt0<- apply(WStockAll[1,((yCTN-200):yCTN),istock,1,],2,mean) 
        spawner0<-apply(WStock[((yCTN-200):yCTN),istock,],2,mean)
        EPR0<-apply(EPR.all[istock,((yCTN-200):yCTN),],2,mean)
        E0<-apply(Etot.all[istock,((yCTN-200):yCTN),],2,mean)
        #z0<-apply(z.all[istock,((yCTN-200):yCTN),],2,mean)
        z0<-numeric(nsim)
        for(i in 1:nsim){
          z0[i]<-length(which(z.all[istock,((yCTN-200):yCTN),i]<0.20))/length(z.all[istock,((yCTN-200):yCTN),i])   #propn of years with z<-0.2 per sample
        }
       
        #smolt_MSY<- apply(WStockAll[1,((yCTN-20):yCTN),istock,1,],2,mean) 
        #spawner_MSY<-apply(WStock[((yCTN-20):yCTN),istock,],2,mean)
        
        #EPR0<-apply(EPR.all[istock,((yCTN-20):yCTN),],2,mean)
        for(y in 1:yCTN){
        zprop[istock,y]<-sum(which(z.all[istock,y,]<0.20))
        eggs1SW[istock,y]<-mean(eggs1SW.all[istock,y, ])
          eggs2SW[istock,y]<-mean(eggs2SW.all[istock,y, ])
            eggs3SW[istock,y]<-mean(eggs3SW.all[istock,y, ])
              eggs4SW[istock,y]<-mean(eggs4SW.all[istock,y, ])
                eggs5SW[istock,y]<-mean(eggs5SW.all[istock,y, ])
        }
        
        RiverNames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
            "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")
        
        eggsSW<-data.frame((1:yCTN)+1991,eggs1SW[istock,],eggs2SW[istock,],eggs3SW[istock,],eggs4SW[istock,],eggs5SW[istock,])
        #write.table(zprop[istock,],file=paste0("zprop_long_",RiverNames[istock],".csv"),row.names=F,col.names=F,sep=",")
        write.table(z.all[istock,((yCTN-200):yCTN),],file=paste0("zprop_long_",RiverNames[istock],".csv"),row.names=F,col.names=F,sep=",")
        #write.table(eggsSW[(yBreak+1):yCTN,],file=paste0("eggsSW_long_",RiverNames[istock],".csv"),row.names=F,col.names=F,sep=",")
        #return(list(-mean(CalC_tot[(yCTN-20):yCTN,istock,iind]),mean(WStockAll[1,((yCTN-20):yCTN),istock,1,iind])))
        #without wrapper
        #return(MSY)
        #with wrapper

        return(list(smolt0,spawner0, E0,z0))
}


#apply(WStockAll[1,,1,1,],1,mean) 
