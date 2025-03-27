#BUGS:
  # LLcv[i,j]<-LLsd[i,j]/LLmu[i,j]		#Dis_LLD, discarded undersized longline
  # LLM[i,j]<-log(LLmu[i,j])-0.5/LLtau[i,j]
  # LLtau[i,j]<-1/log(LLcv[i,j]*LLcv[i,j]+1)

# LLmu[,1]	LLsd[,1]
# 0.03007	0.008249
# LLmu[,2]	LLsd[,2]
# 0.02003	0.004091

LLcv11<-0.008249/0.03007
LLtau11<-1/log(LLcv11*LLcv11+1)
LLM11<-log(0.03007)-0.5/LLtau11
LLM11 # -3.540506 OK!
LLtau11 #Ok

LLcv12<-0.004091/0.02003
LLtau12<-1/log(LLcv12*LLcv12+1)
LLM12<-log(0.02003)-0.5/LLtau12
LLM12 # -3.930959 OK!
LLtau12 #Ok





MX<-"model{
  DisC12~dlnorm(-2.432467,23.60443)I(0,0.2)	# share if discarded undersized trapnet and other coastal gears
      
  
  #    DisLL11~dlnorm(-3.540506,13.7821)I(0,0.3)	# share of discarded undersized longline
  #    DisLL12~dlnorm(-3.930959,24.46852)I(0,0.3)	# share of discarded undersized longline
}"



runX <- run.jags(MX, monitor= c("DisC12"),
  #"DisLL11", "DisLL12"),
                  #data=datalist,#inits = initsall,
                  n.chains = 2, method = 'parallel', thin=100,
                  burnin =10000, modules = "mix",
                  sample =1000, adapt = 10000,
                  keep.jags.files=F,
                  progress.bar=TRUE, jags.refresh=100)

summary(runX)
#           Lower95    Median   Upper95       Mean          SD
# DisLL11 0.0161603 0.0290034 0.0469481 0.02996572 0.008254468
# DisLL12 0.0127865 0.0195772 0.0278109 0.02002473 0.004135777
# Mode        MCerr MC%ofSD SSeff      AC.1000      psrf
# DisLL11   NA 1.845755e-04     2.2  2000 -0.001523924 0.9997526
# DisLL12   NA 8.811334e-05     2.1  2203  0.016058427 1.0042110
 # Vrt excel, nämä mediaanit on samat mitä fi ja se pitäisi olla koko aikasarja
# fi menee vikaan v 17 alkaen (0.68), se on pielessä koko aikasarjalta (0.088)

# Kuitenkin nyt näyttää siltä että run00 tuottaa ihan oikeita arvoja DisLL-
# parametreille. Miksi yhteenveto on pielessä? Meneekö indeksit väärin
# kun lasketaan statseja???

