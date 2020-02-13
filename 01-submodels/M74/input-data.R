# Note!!! Not sure why this is here. Maybe the idea was to modify data for JAGS?
# Anyway seems to do the same stuff as data-M74.r, so please use that as long 
# as BUGS model is used.

library(rjags)
library(runjags)
library(tidyverse)
library(readxl)
library(forcats)
library(lubridate)
library(stringr)
library(gridExtra)
library(coda)

pathM74<-"H:/Biom/SubE_M74/2019/"

# FI data

df<-read_xlsx(path=str_c(pathM74,"dat/orig/Finnish_M74_data-2018_paivitetty.xlsx"), 
              col_names = T, guess_max = 10000, sheet=1, na="")%>%   
  mutate(river=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  mutate(YEAR=FEMALE_YEAR)%>%
  mutate(year=FEMALE_YEAR-1984)%>%
  mutate(Eggs=ifelse(is.na(eggs)==F,eggs,ifelse(year<10,100,115)))%>%
  mutate(eggs=Eggs)%>%
  mutate(surv_eggs=round(eggs*(1-(YSFM/100)),0) )%>%
  mutate(M74_mort=fct_recode(M74,
                             "1"="Ei",
                             "2"="M74"))%>%
  # mort100 is 2 if 100% mortality, 1 if <100% and NA if M74 unknown (XX should not appear anywhere)
  mutate(mortality100=ifelse(is.na(M74)==F, ifelse(is.na(M74_100)==F, ifelse(M74_100==100,2,"XX"),1),NA ))

# check that these match                      
#View(select(df, M74,`M74 mortality`,  M74_mort,M74_100,`100% mortality`, mortality100))
#View(select(df, `surviving eggs`, surv_eggs))
            

dfFI<-df%>% 
  mutate(M74=as.numeric(M74_mort))%>%
  select(YEAR, year, RIVER, river, eggs, surv_eggs, M74, mortality100, YSFM)
#%>%filter(river!=4) # to remove iijoki, avoiding confusion with Luleälven.
  
#View(filter(df, river==4))


# SE data
df1<-read_tsv(str_c(pathM74,"data/der/datR_M74_SE16.txt"), 
              col_types = cols(yy="i",ss="c",ff="i", xx="i"))%>%
  mutate(s=parse_double(ss))


df2<-read_xlsx(str_c(pathM74,"data/der/Swedish_M74_data_17.xlsx"))%>%
  mutate(ss=fct_recode(river,
                       "11"="Dal", "10"="Ljusnan", "8"="Indals", "7"="Angerman",
                       "6"="Ume", "5"="Skellefte", "4"="Lule"
                       ))%>%
  mutate(yy=32)%>%
  mutate(ff=Kramade)%>%
  mutate(xx=`Antal M74`)%>%
  select(ss, yy, ff, xx)%>%
  mutate(s=parse_double(ss))

# add missing data for 
df3<-tibble(
  yy=32,
  s=c(9,12,13),
  ff=100,
  xx=parse_double(NA)
)
  
dfSE<-full_join(df1,df2)
dfSE<-full_join(dfSE,df3)

dfSE<-dfSE%>%
  mutate(stock=s+1)%>% # +1 is needed to start SE stocks from index 5 (Iijoki included as stock 4)
  select(yy,stock,ff,xx)

#View(dfFI)
#View(dfSE)


# input to BUGS:

length(dfFI$eggs)
# 1437
length(dfSE$xx)
# 320

dfFI.bugs<-dfFI%>%
  select(eggs, year, river, surv_eggs, M74, mortality100)

dfSE.bugs<-dfSE
  
write_tsv(dfFI.bugs, str_c(pathM74,"prg/input/M74dataFI17.txt"))
write_tsv(dfSE.bugs, str_c(pathM74,"prg/input/M74dataSE17.txt"))


# wrangle for figures

by_ry<-dfFI%>%
  group_by(RIVER, YEAR)
  
ysfm<-by_ry%>%
  summarise(ysfm=round(mean(YSFM/100),2),
            N_fem=n())

n_M74notNA<-dfFI%>%
  filter(is.na(M74)==F)%>%
  group_by(RIVER, YEAR)%>%
  summarise(N_M74=n())

cases_M74<-by_ry%>% 
  count(M74)%>%
  filter(M74==2)%>%
  mutate(N_M74fem=n)

cases_mort100<-by_ry%>% 
  count(mortality100)%>%
  filter(mortality100==2)%>%
  mutate(N_mort100=n)

tmp<-full_join(ysfm, n_M74notNA, by=c("RIVER", "YEAR"))
tmp<-full_join(tmp, cases_M74, by=c("RIVER", "YEAR"))
dfFI.2<-full_join(tmp, cases_mort100, by=c("RIVER", "YEAR"))%>%
  select(RIVER, YEAR, ysfm, N_fem, N_M74, N_M74fem, N_mort100)%>%
  mutate(N_M74fem=ifelse(is.na(N_M74fem)==T, ifelse(is.na(N_M74)==T, NA, 0),N_M74fem))%>%
  mutate(N_mort100=ifelse(is.na(N_mort100)==T, ifelse(is.na(N_M74)==T, NA, 0),N_mort100))%>%
  mutate(propM74=round(N_M74fem/N_fem,2))%>%
  mutate(prop_mort100=round(N_mort100/N_M74,2))

           
View(df)

dfSE.2<-dfSE%>%
  mutate(propM74=round(xx/ff,2))%>%
  mutate(RIVER=fct_recode(parse_character(stock),
                          "Lule"="5","Skellefte"="6",
                          "Ume"="7","Angerman"="8",
                          "Indals"="9","Ljungan"="10",
                          "Ljusnan"="11","Dal"="12",
                          "Morrum"="13","Unknown"="14"))%>%
  mutate(YEAR=yy+1984)%>%
  mutate(N_fem=ff)%>%
  mutate(N_M74fem=xx)%>%
  select(YEAR, RIVER, N_fem, N_M74fem, propM74)

View(tmp)

tmp<-full_join(dfFI.2,dfSE.2)%>%
  select(-N_M74)

