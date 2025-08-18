# Initialise arrays for ProjEffort-file.
nsim<-1000
iniAgeQuantW<-array(NA, dim = c(6,years[3],Nstocks,2,nsim))
iniAgeQuantR<-array(NA, dim = c(6,years[3],4,2,nsim))

# Pre fishery abundances for wild and reared
PFAW<-array(NA, dim=c(6,years[3],Nstocks,2,nsim))
PFAR<-array(NA, dim=c(6,years[3],4,2,nsim))

WOLLCtot<-array(NA, dim=c(6,years[3],Nstocks,nsim))
ROLLCtot<-array(NA, dim=c(6,years[3],4,nsim))
WCTNCtot<-array(NA, dim=c(6,years[3],Nstocks,nsim))
RCTNCtot<-array(NA, dim=c(6,years[3],4,nsim))

WStockAll<-array(NA, dim=c(6,years[3],Nstocks,2,nsim))

# Abundances on May1st for wild and reared before migrants are split
May1stW<-iniAgeQuantW
May1stR<-iniAgeQuantR
# Abundances on May1st of those that migrate
MigrW<-iniAgeQuantW
MigrR<-iniAgeQuantR

MaturationW<-array(NA, dim = c(6,years[3],nsim))
MaturationR<-array(NA, dim = c(6,years[3],nsim))

temp2W<-iniAgeQuantW
temp2R<-iniAgeQuantR

UmeRiverCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
UmeCoastCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
UmeSeaCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
#RepCatchTotal<-array(NA, dim=c((yBreak+NumFutYears), nsim))
#RepRiverTotal<-array(NA, dim=c((yBreak+NumFutYears), nsim))
TornioRiverCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
TornioCoastCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
TornioSeaCatch<-array(NA, dim=c((yBreak+NumFutYears), nsim))
#CatchCoastTotal<-array(NA, dim=c((yBreak+NumFutYears), nsim))
#CatchSeaTotal<-array(NA, dim=c((yBreak+NumFutYears), nsim))
#CatchRiverTotal<-array(NA, dim=c((yBreak+NumFutYears), nsim))
CatchRiver<-array(NA, dim=c((yBreak+NumFutYears), nsim))
CatchRiverS<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), nsim))

SmoltW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), nsim))
SmoltR<-array(NA, dim=c(4,(yBreak+NumFutYears), nsim))
SpawnerW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), nsim))
SpawnerR<-array(NA, dim=c(4,(yBreak+NumFutYears), nsim))
PSW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), nsim))
PSR<-array(NA, dim=c(4,(yBreak+NumFutYears), nsim))
spW_age<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears),6,nsim))
spR_age<-array(NA, dim=c(4,(yBreak+NumFutYears),6,nsim))

Prop1SWsp<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Prop2SWsp<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Prop3SWsp<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Prop4SWsp<-array(NA, dim=c((yBreak+NumFutYears), nsim))

postsmolts<-array(NA, dim=c((yBreak+NumFutYears), nsim))
postsmoltsR<-array(NA, dim=c((yBreak+NumFutYears), nsim))
postsmoltsW<-array(NA, dim=c((yBreak+NumFutYears), nsim))

Migr_Tornio<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_Simo<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_Kalix<-array(NA, dim=c((yBreak+NumFutYears), nsim))

Migr_AU1W<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_AU1R<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_AU13W<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_AU13R<-array(NA, dim=c((yBreak+NumFutYears), nsim))
Migr_AU13tot<-array(NA, dim=c((yBreak+NumFutYears), nsim))

CalC_OLL<-array(NA, dim=c((yBreak+NumFutYears), nsim))
CalC_CTN<-array(NA, dim=c((yBreak+NumFutYears), nsim))

WOLL_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
ROLL_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
WODN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
RODN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
WRF_HR<-array(NA, dim=c(3,(yBreak+NumFutYears), nsim))
RRF_HR<-array(NA, dim=c(3,(yBreak+NumFutYears), nsim))
OffsW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
OffsR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))

TotW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))
TotR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), nsim))


WCTN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
RCTN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
WCGN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
RCGN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
WCDN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
RCDN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
CoastW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))
CoastR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),4, nsim))


PFA<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFA2plus<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFA2plusW<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFA2plusR<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFAgrilse<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFAgrilseW<-array(NA, dim=c((yBreak+NumFutYears), nsim))
PFAgrilseR<-array(NA, dim=c((yBreak+NumFutYears), nsim))

EffortAU<-array(NA, dim=c(yBreak+NumFutYears,4,5,nsim))
dimnames(EffortAU) <- list(year=1992 :(1992+yBreak+NumFutYears-1),unit=1:4,
    season=c("OLL","ODN","CDN","CTN","CGN"), iter=1:nsim)
