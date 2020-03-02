# Initialise arrays for ProjEffort-file.

iniAgeQuantW<-array(NA, dim = c(6,years[3],Nstocks,2,1000))
iniAgeQuantR<-array(NA, dim = c(6,years[3],4,2,1000))

R0<-array(0,dim=c(years[3],Nstocks,1000))
FecW<-array(0,dim=c(6,years[3],1000))
FecR<-array(0,dim=c(6,years[3],1000))
Etot<-array(0,dim=c(years[3],Nstocks,1000))
W_age<-array(0,dim=c(Nstocks,years[3],6,1000))
EPRW<-array(0,dim=c(years[3],Nstocks,1000))   #first year is yBreak+1 (year of assessment) 


# Pre fishery abundances for wild and reared
PFAW<-array(NA, dim=c(6,years[3],Nstocks,2,1000))
PFAR<-array(NA, dim=c(6,years[3],4,2,1000))

WOLLCtot<-array(NA, dim=c(6,years[3],Nstocks,1000))
ROLLCtot<-array(NA, dim=c(6,years[3],4,1000))
WCTNCtot<-array(NA, dim=c(6,years[3],Nstocks,1000))
RCTNCtot<-array(NA, dim=c(6,years[3],4,1000))

# Abundances on May1st for wild and reared before migrants are split
May1stW<-iniAgeQuantW
May1stR<-iniAgeQuantR
# Abundances on May1st of those that migrate
MigrW<-iniAgeQuantW
MigrR<-iniAgeQuantR

MaturationW<-array(NA, dim = c(6,years[3],1000))
MaturationR<-array(NA, dim = c(6,years[3],1000))

temp2W<-iniAgeQuantW
temp2R<-iniAgeQuantR

UmeRiverCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
UmeCoastCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
UmeSeaCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
MorrumSeaCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
#RepCatchTotal<-array(NA, dim=c((yBreak+NumFutYears), 1000))
#RepRiverTotal<-array(NA, dim=c((yBreak+NumFutYears), 1000))
TornioRiverCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
TornioCoastCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
TornioSeaCatch<-array(NA, dim=c((yBreak+NumFutYears), 1000))
#CatchCoastTotal<-array(NA, dim=c((yBreak+NumFutYears), 1000))
#CatchSeaTotal<-array(NA, dim=c((yBreak+NumFutYears), 1000))
#CatchRiverTotal<-array(NA, dim=c((yBreak+NumFutYears), 1000))
CatchRiver<-array(NA, dim=c((yBreak+NumFutYears), 1000))

SmoltW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), 1000))
SmoltR<-array(NA, dim=c(4,(yBreak+NumFutYears), 1000))
SpawnerW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), 1000))
SpawnerR<-array(NA, dim=c(4,(yBreak+NumFutYears), 1000))
PSW<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears), 1000))
PSR<-array(NA, dim=c(4,(yBreak+NumFutYears), 1000))
spW_age<-array(NA, dim=c(Nstocks,(yBreak+NumFutYears),6,1000))
spR_age<-array(NA, dim=c(4,(yBreak+NumFutYears),6,1000))

Prop1SWsp<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Prop2SWsp<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Prop3SWsp<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Prop4SWsp<-array(NA, dim=c((yBreak+NumFutYears), 1000))

postsmolts<-array(NA, dim=c((yBreak+NumFutYears), 1000))
postsmoltsR<-array(NA, dim=c((yBreak+NumFutYears), 1000))
postsmoltsW<-array(NA, dim=c((yBreak+NumFutYears), 1000))

Migr_Tornio<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_Simo<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_Kalix<-array(NA, dim=c((yBreak+NumFutYears), 1000))

Migr_AU1W<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_AU1R<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_AU13W<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_AU13R<-array(NA, dim=c((yBreak+NumFutYears), 1000))
Migr_AU13tot<-array(NA, dim=c((yBreak+NumFutYears), 1000))

CalC_OLL<-array(NA, dim=c((yBreak+NumFutYears), 1000))
CalC_CTN<-array(NA, dim=c((yBreak+NumFutYears), 1000))

WOLL_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
ROLL_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
WODN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
RODN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
OffsW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
OffsR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))

TotW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))
TotR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears), 1000))


WCTN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
RCTN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
WCGN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
RCGN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
WCDN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
RCDN_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
CoastW_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))
CoastR_HR<-array(NA, dim=c(2,(yBreak+NumFutYears),3, 1000))


PFA<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFA2plus<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFA2plusW<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFA2plusR<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFAgrilse<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFAgrilseW<-array(NA, dim=c((yBreak+NumFutYears), 1000))
PFAgrilseR<-array(NA, dim=c((yBreak+NumFutYears), 1000))

EffortAU<-array(NA, dim=c(yBreak+NumFutYears,4,5,1000))
dimnames(EffortAU) <- list(year=years[1]:years[2],unit=1:4,
    season=c("OLL","ODN","CDN","CTN","CGN"), iter=1:1000)
