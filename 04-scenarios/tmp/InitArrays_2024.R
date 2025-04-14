# Initialise arrays for ProjEffort-file.

WRF_HR<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
RRF_HR<-array(NA, dim=c(6,Nyears,4,nsim))

WOLL_HR<-array(NA, dim=c(6,Nyears,Nstocks, nsim))
WODN_HR<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
WTR_HR<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
WTR_HR_orig<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
ROLL_HR<-array(NA, dim=c(6,Nyears,4,nsim))
RODN_HR<-array(NA, dim=c(6,Nyears,4,nsim))
RTR_HR<-array(NA, dim=c(6,Nyears,4,nsim))
RTR_HR_orig<-array(NA, dim=c(6,Nyears,4,nsim))

WTR_F<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
RTR_F<-array(NA, dim=c(6,Nyears,4,nsim))


WCTN_HR<-array(NA, dim=c(6,Nyears,Nstocks, nsim))
WCGN_HR<-array(NA, dim=c(6,Nyears,Nstocks, nsim))
WCDN_HR<-array(NA, dim=c(6,Nyears,Nstocks, nsim))
RCTN_HR<-array(NA, dim=c(6,Nyears,4, nsim))
RCGN_HR<-array(NA, dim=c(6,Nyears,4, nsim))
RCDN_HR<-array(NA, dim=c(6,Nyears,4, nsim))

OffsW_HR<-array(NA, dim=c(2,Nyears, nsim))
OffsR_HR<-array(NA, dim=c(2,Nyears, nsim))
CoastW_HR<-array(NA, dim=c(2,Nyears,3, nsim))
CoastR_HR<-array(NA, dim=c(2,Nyears,3, nsim))
TotW_HR<-array(NA, dim=c(2,Nyears, nsim))
TotR_HR<-array(NA, dim=c(2,Nyears, nsim))

WCTN_C<-array(NA, dim=c(6,Nyears,Nstocks, nsim))
RCTN_C<-array(NA, dim=c(6,Nyears,4, nsim))
WRF_C<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
RRF_C<-array(NA, dim=c(6,Nyears,4,nsim))
WOLL_C<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
ROLL_C<-array(NA, dim=c(6,Nyears,4,nsim))
WODN_C<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
RODN_C<-array(NA, dim=c(6,Nyears,4,nsim))
WTR_C<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
RTR_C<-array(NA, dim=c(6,Nyears,4,nsim))


Effort<-array(NA, dim=c(Nyears,3,5,5,nsim))

dimnames(Effort) <- list(year=years[1]:years[2],
                             unit=c("ICES 22-29","ICES 30","ICES 31"),
                             season=c("OLL","ODN","CDN","CTN","CGN"), 
                             area = c("Finland","Sweden","Denmark","Poland","Trolling"), # "Other" changed to "Poland", "Trolling" added
                             iter=1:nsim) 

EffortAU<-array(NA, dim=c(Nyears,4,5,nsim))
dimnames(EffortAU) <- list(year=years[1]:years[2],unit=1:4,
                           season=c("OLL","ODN","CDN","CTN","CGN"), iter=1:nsim)


# Abundances on May1st for wild and reared before migrants are split
May1stW<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
May1stR<-array(NA, dim = c(6,Nyears,4,nsim))
# May1stWyBreak<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
# May1stRyBreak<-array(NA, dim = c(6,Nyears,4,nsim))

# Mature at May 1st
MatW_1<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
MatR_1<-array(NA, dim = c(6,Nyears,4,nsim))

# Mature after coastal fisheries
MatW_2<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
MatR_2<-array(NA, dim = c(6,Nyears,4,nsim))

# Mature after river fisheries
MatW_3<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
MatR_3<-array(NA, dim = c(6,Nyears,4,nsim))

# Immature at May 1st
ImmW_1<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
ImmR_1<-array(NA, dim = c(6,Nyears,4,nsim))

# Immature after offshore longline
ImmW_2<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
ImmR_2<-array(NA, dim = c(6,Nyears,4,nsim))

# Immature after recreational trolling
ImmW_3<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
ImmR_3<-array(NA, dim = c(6,Nyears,4,nsim))


ql_W<-array(NA, dim=c(2,Nyears,nsim))
ql_R<-array(NA, dim=c(2,Nyears,nsim))
qctn_W<-array(NA, dim=c(2,4,nsim))
qctn_R<-array(NA, dim=c(2,4,nsim))

MW<-array(NA, dim = c(6,Nyears,Nstocks,2,nsim))
MR<-array(NA, dim = c(6,Nyears,4,2,nsim))

MatRateW<-array(NA, dim = c(6,Nyears,Nstocks,nsim))
MatRateR<-array(NA, dim = c(6,Nyears,4,nsim))
FecW<-array(NA, dim = c(6,Nyears,nsim))
FecR<-array(NA, dim = c(6,Nyears,nsim))

# Pre fishery abundances for wild and reared
PFAW<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
PFAR<-array(NA, dim=c(6,Nyears,4,nsim))
PFAW2<-array(NA, dim=c(6,Nyears,Nstocks,nsim))

BHalpha<-array(NA, dim=c(nsim,Nstocks))
BHbeta<-array(NA, dim=c(nsim,Nstocks))
tauBH<-c()

M_74<-array(NA, dim=c(nsim,Nyears-2))

EPRW<-array(0,dim=c(Nyears,Nstocks,nsim))   #first year is yBreak+1 (year of assessment) 
EPRW_M74<-array(NA, dim = c(Nyears,Nstocks,nsim))

R0<-array(0,dim=c(Nyears,Nstocks,nsim))
FecW<-array(0,dim=c(6,Nyears,nsim))
FecR<-array(0,dim=c(6,Nyears,nsim))
Etot<-array(0,dim=c(Nyears,Nstocks,nsim))
W_age<-array(0,dim=c(Nstocks,Nyears,6,nsim))

#WOLLCtot<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
#ROLLCtot<-array(NA, dim=c(6,Nyears,4,nsim))
#WCTNCtot<-array(NA, dim=c(6,Nyears,Nstocks,nsim))
#RCTNCtot<-array(NA, dim=c(6,Nyears,4,nsim))

#MaturationW<-array(NA, dim = c(6,Nyears,nsim))
#MaturationR<-array(NA, dim = c(6,Nyears,nsim))
#coast_MW<-array(NA, dim = c(6,Nyears,Nstocks,nsim))

RiverCatchR<-array(NA, dim=c(6,4,Nyears, nsim))
RiverCatchW<-array(NA, dim=c(6,Nstocks,Nyears, nsim))
UmeRiverCatch<-array(NA, dim=c(Nyears, nsim))
UmeCoastCatch<-array(NA, dim=c(Nyears, nsim))
UmeSeaCatch<-array(NA, dim=c(Nyears, nsim))
MorrumSeaCatch<-array(NA, dim=c(Nyears, nsim))
MorrumRiverCatch<-array(NA, dim=c(Nyears, nsim))
#RepCatchTotal<-array(NA, dim=c(Nyears, nsim))
#RepRiverTotal<-array(NA, dim=c(Nyears, nsim))
TornioRiverCatch<-array(NA, dim=c(Nyears, nsim))
TornioCoastCatch<-array(NA, dim=c(Nyears, nsim))
TornioSeaCatch<-array(NA, dim=c(Nyears, nsim))
#CatchCoastTotal<-array(NA, dim=c(Nyears, nsim))
#CatchSeaTotal<-array(NA, dim=c(Nyears, nsim))
#CatchRiverTotal<-array(NA, dim=c(Nyears, nsim))
CatchRiver<-array(NA, dim=c(Nyears, nsim))

SmoltW<-array(NA, dim=c(Nstocks,Nyears, nsim))
SmoltR<-array(NA, dim=c(4,Nyears, nsim))
SpawnerW<-array(NA, dim=c(Nstocks,Nyears, nsim))
SpawnerR<-array(NA, dim=c(4,Nyears, nsim))
PSW<-array(NA, dim=c(Nstocks,Nyears, nsim))
PSR<-array(NA, dim=c(4,Nyears, nsim))
spW_age<-array(NA, dim=c(Nstocks,Nyears,6,nsim))
spR_age<-array(NA, dim=c(4,Nyears,6,nsim))

Prop1SWsp<-array(NA, dim=c(Nyears, nsim))
Prop2SWsp<-array(NA, dim=c(Nyears, nsim))
Prop3SWsp<-array(NA, dim=c(Nyears, nsim))
Prop4SWsp<-array(NA, dim=c(Nyears, nsim))

postsmolts<-array(NA, dim=c(Nyears, nsim))
postsmoltsR<-array(NA, dim=c(Nyears, nsim))
postsmoltsW<-array(NA, dim=c(Nyears, nsim))

Migr_Tornio<-array(NA, dim=c(Nyears, nsim))
Migr_Simo<-array(NA, dim=c(Nyears, nsim))
Migr_Kalix<-array(NA, dim=c(Nyears, nsim))

Migr_AU1W<-array(NA, dim=c(Nyears, nsim))
Migr_AU1R<-array(NA, dim=c(Nyears, nsim))
Migr_AU13W<-array(NA, dim=c(Nyears, nsim))
Migr_AU13R<-array(NA, dim=c(Nyears, nsim))
Migr_AU13tot<-array(NA, dim=c(Nyears, nsim))

CalC_OLL<-array(NA, dim=c(Nyears, nsim))
CalC_CTN<-array(NA, dim=c(Nyears, nsim))
CalC_TR<-array(NA, dim=c(Nyears, nsim))

PFA<-array(NA, dim=c(Nyears, nsim))
PFA2plus<-array(NA, dim=c(Nyears, nsim))
PFA2plusW<-array(NA, dim=c(Nyears, nsim))
PFA2plusR<-array(NA, dim=c(Nyears, nsim))
PFAgrilse<-array(NA, dim=c(Nyears, nsim))
PFAgrilseW<-array(NA, dim=c(Nyears, nsim))
PFAgrilseR<-array(NA, dim=c(Nyears, nsim))

