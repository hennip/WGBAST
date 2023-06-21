#######################
## Prepare SST data on 9 stations for JAGS model
##
#######################

## ---- load-data-sst


# Workflow:

# 1: Run data-SST-17.R
# 2: Input data to BUGS (data-SST-Aug17.odc) and run model-SST.odc 
# 3: Run graphs-SST.R for figures
# 4: Produce input data for the full life history model (SSTinputToJAGS.xlsx)

#install.packages("tidyverse")


# Path for input data
source("00-basics/run-this-first.R")
source("00-basics/packages.R")
pathIn<-pathDataSST

#############
# Data from 8 stations

(dat1<-read_xlsx(str_c(pathIn,"SMHI tempdata 8 stations jan1980-feb2023.xlsx"),
                sheet="Data", na=c("","NaN")))%>%
  select(Station, Year, Month,Day,Depth,Temperature)


station<-c()
for(i in 1:(dim(dat1)[1])){
  if(dat1$Station[i]=="BY1"){station[i]<-1}
  if(dat1$Station[i]=="BY2"){station[i]<-2}
  if(dat1$Station[i]=="BY38"){station[i]<-3}
  if(dat1$Station[i]=="BY4"){station[i]<-4}
  if(dat1$Station[i]=="BY5"){station[i]<-5}
  if(dat1$Station[i]=="BY10"){station[i]<-6}
  if(dat1$Station[i]=="BCS III-10"){station[i]<-7}
  if(dat1$Station[i]=="Hanöbukten"){station[i]<-8}
  if(dat1$Station[i]=="HANÖBUKTEN"){station[i]<-8}
}
dat1$station<-station

#filter(dat1, is.na(station)==T)

dat1<-filter(dat1, Month<5 & Depth<=10 & Year>1991 & is.na(Temperature)==F)%>%
  mutate(year=Year-1991)%>%
  select(Temperature, Year, Month, Day, year, station)
dat1

#############
# Knolls Grund -data (station nr. 9)

dat2<-read_xlsx(str_c(pathIn,"SST Knolls grund 2023-03-21.xlsx"),
               sheet=1,
  col_names = T, range="A7:B96246", guess_max = 100000)%>%
  setNames(c("Date", "SST"))#%>%
  #filter(Date<"2023-03-01")
#setNames(c("Date", "Time", "Temperature", "Quality", "Depth"))
#setNames(c("Date", "Temperature", "Quality", "Depth"))
dat2

# Never mind the warnings about column 'Station' etc.just some bug in the package
dat2<-dat2%>%
  mutate(Year=year(Date))%>%
  mutate(Month=month(Date))%>%
  mutate(Day=day(Date))%>%
  filter(Month<5)%>%
  mutate(year=Year-1991)%>%
  mutate(station=9)%>%
  #mutate(Temperature=parse_double(SST))%>%
  mutate(Temperature=SST)%>%
  select(Temperature, Year, Month, Day, year, station)

#filter(dat2,Year==2023 & Month==3)

###################
# Join datasets, full set covers 9 stations

(dat<-full_join(dat1,dat2, by=NULL))
#View(dat)


#############
# Data to be inputted to BUGS
df.bugs<-dat%>%
    group_by(station,year, Month) %>%
    summarise(sst.station=mean(Temperature)) # mean SST per station

# This makes an empty set of data for the assessment year +1 for scenarios 
extra.year<-tibble(
  year=rep(max(select(df.bugs,year))+1,9*4),
  Month=rep(1:4, 9)
  )
extra.year

# Full join adds NA's for station and sst for the extra.year
df.bugs<-full_join(df.bugs,extra.year, by=NULL)
#View(df.bugs)

write_xlsx(df.bugs, path=paste0(pathIn,"data.bugs.xlsx"))
# copy paste data (year, month & sst) from excel to OpenBUGS using paste special -> unicode text
# change column names as year[]	month[]	SST[]
# Add text END at the bottom of the data file and press ENTER (empty line is needed at the end of the file)






