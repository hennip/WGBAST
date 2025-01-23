
  
M1<-"model{
  #epsilon~dnorm(1,10000)I(0,)	# a super narrow pd just to deal with discrete attributes
  
  # Mortalities of discarded undersized salmon
  # These are considered to be same for all coutries and years in the whole Baltic Sea
  # and therefore not in the input file where country and year specific parameter values are taken
  # notice:	1) driftnet fishery stopped at the end of 2007 i.e. zero catches in drifnets from 2008
  #			   2) discarding ban for longline fishery valid from 2015 but not implemented so far
  
  # longline
  MDisLL~dlnorm(MLLM,MLLtau)I(0.5,1.1)
  
  #  driftnet
  MDisDN~dlnorm(MDNM,MDNtau)I(0.4,1.1)
  
  # trapnet
  MDisC~dlnorm(MTNM,MTNtau)I(0.1,1.1)
  
  # Country and year specific Mean and SD values of lognormal disributions (from input file xxxx)
  # same values are used for both management units
  
  #	Omu[,,]	Osd[,,]	unreporting all fisheries off-shore
  #	Cmu[,,]	Csd[,,]	unreporting all fisheries coast
  #	Rmu[,,]	Rsd[,,]	unreporting all fisheries river
  #	LLmu[,,]	LLsd[,,]		share of discarded undersized longline
  #	DNmu[,,]	DNvar[,,]	share if discarded undersized driftnet
  #	TNmu[,,]	TNsd[,,]	share if discarded undersized trapnet and other coastal gears
  #	SLLDmu[,,]	SLLDsd[,,]	share if seal damages longline
  #	SGNDmu[,,]	SGNDsd[,,]	share of seal damages driftnet
  #	STNmu[,,]	STNsd[,,]	share of seal damages trapnet  and other coastal gears
  
  for (j in 1:9){          # countries 1=FI, 2=SE, 3=DK, 4=PL, 5=LV, 6=LT, 7=DE, 8=EE, 9=RU
    for (i in 1:23){       # years 2001-2023				
      
      Oconv[i,j]~dlnorm(OM[i,j],Otau[i,j])I(0,0.6)	# unreporting all fisheries off-shore
      Cconv[i,j]~dlnorm(CM[i,j],Ctau[i,j])I(0,0.7)	# unreporting all fisheries coast
      Rconv[i,j]~dlnorm(RM[i,j],Rtau[i,j])I(0,0.7)	# unreporting all fisheries river
      
      DisLL[i,j]~dlnorm(LLM[i,j],LLtau[i,j])I(0,0.3)	# share of discarded undersized longline
      DisDN[i,j]~dlnorm(DNM[i,j],DNtau[i,j])I(0,0.2)	# share if discarded undersized driftnet
      DisC[i,j]~dlnorm(TNM[i,j],TNtau[i,j])I(0,0.2)	# share if discarded undersized trapnet and other coastal gears
      
      SealLL[i,j]~dlnorm(SLLDM[i,j],SLLDtau[i,j])I(0,0.2)	# share if seal damages longline
      SealDN[i,j]~dlnorm(SGNDM[i,j],SGNDtau[i,j])I(0,0.2)	# share of seal damages driftne
      SealC[i,j]~dlnorm(STNM[i,j],STNtau[i,j])I(0,0.35)	# share of seal damages trapnet  and other coastal gears
      
      
    }
  }
}"


