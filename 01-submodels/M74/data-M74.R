
# Modify input data for M74 model (OpenBUGS)

# Stocks:
# Simojoki="1", Tornionjoki="2", Kemijoki="3", Iijoki="4",
# Luleälven="5",Skellefteälven="6",Umeälven="7",Ångermanälven="8",
# Indalsälven="9",Ljungan="10",Ljusnan="11",Dalälven="12",
# Morrumsån="13",`Unsampled stock`="14"))


source("run-this-first.R")
source("00-basics/packages.r")
source("00-basics/boxplot-functions.r")
pathM74old<-pathM74_2021
pathM74<-pathM74_current
  
# FI data
# dat<-read_xlsx(path=str_c(pathM74,"dat/orig/Finnish_M74_data-2022_paivitetty_TPa_21_01_2023.xlsx"), 
#               col_names = T, guess_max = 10000, sheet="Data", na=c("", "NA"))   

dat<-read_xlsx(path="C:/Users/03195892/OneDrive - Valtion/01-PROJECTS/M74/2024/data/Finnish_M74_data-2023_päivitetty_TPa_08_02_2024.xlsx", 
               col_names = T, guess_max = 10000, sheet="Data", na=c("", "NA"))  
#View(dat)

df<-dat%>%
  mutate(river=fct_recode(RIVER,
                                    "1"="Simo",
                                    "2"="Tornio",
                                    "3"="Kemi",
                                    "4"="Iijoki"))%>%
  # this keeps stock numbers in a correct order!
  mutate(stock=parse_double(as.character(river)))%>%
  mutate(rivername=RIVER)%>%
  mutate(YEAR=FEMALE_YEAR)%>%
  mutate(year=FEMALE_YEAR-1984)%>%
  mutate(Eggs=ifelse(is.na(eggs)==F,eggs,ifelse(year<10,100,115)))%>%
  mutate(eggs=Eggs)%>%
  mutate(surv_eggs=round(eggs*(1-(YSFM/100)),0))%>%
  mutate(M74_mort=fct_recode(M74,
                             "1"="Ei",
                             "1"="EI",
                             "2"="M74"))%>%
  mutate(M74_mort=as.numeric(M74_mort))%>%
  #mutate(M74_100_2=ifelse(M74_100=="M74100",100,M74_100))%>%
  # mort100 is 2 if 100% mortality, 1 if <100% and NA if M74 unknown (XX should not appear anywhere)
  #mutate(mortality100=ifelse(is.na(M74)==F, ifelse(is.na(M74_100_2)==F, ifelse(M74_100_2==100,2,"XX"),1),NA ))
  mutate(mortality100=ifelse(YSFM==100, 2,1))

# Check that this is empty
#filter(df, mortality100=="XX")


dfFI<-df%>% 
  mutate(M74=as.numeric(M74_mort))%>%
  select(YEAR, year, rivername,stock, eggs, surv_eggs, M74, mortality100, YSFM)

filter(dfFI, surv_eggs>0 &  mortality100==2 & is.na(mortality100)==F)

# REMOVE THE LAST YEAR'S DATA (NOT HATCHED YET)
#View(filter(dfFI, YEAR==2022 ))
dfFI<-filter(dfFI, YEAR<2022)

#View(dfFI)

# Swedish M74 data until 2016 (in final format)
df1<-read_tsv(str_c(pathM74old,"dat/der/M74dataSE16.txt"), col_names = T)
              
# New Swedish M74 data
df2<-read_xlsx(str_c(pathM74,"dat/der/Swedish_M74_data_17-22.xlsx"), na="NA")

df2<-df2%>%
  #mutate(river=as_factor(river))%>%
  mutate(stock=ifelse(river=="Dal",11, 
                      ifelse(river=="Ljusnan", 10,
                             ifelse(river=="Indals", 8,
                                    ifelse(river=="Angerman",7,
                                           ifelse(river=="Ume",6,
                                                  ifelse(river=="Skellefte",5,
                                                         ifelse(river=="Lule",4,"X"))))))))%>%
  mutate(yy=year-1985)%>%
  mutate(Females=Kramade)%>%
  mutate(xx=`Antal M74`)%>%
  select(stock,river, yy, Females, xx)
#View(df2)

# add missing data for swedish stocks with no recent M74 data, plus for unsampled stock 
df3<-tibble(
  yy=35, #2020, add +1 each year (year of spawning)
  stock=as_factor(c(9,12,13)), # Ljungan, Morrum, unsampled stock
  Females=100,
  xx=as.numeric(NA)
)
df4<-df3%>%mutate(yy=yy+1) # assessment year-1 (year of hatching)
  
dfSE<-df2%>%
  full_join(df3)%>%
  full_join(df4)%>%
  mutate(stock=as.numeric(stock))%>%
  mutate(stock=stock+1)%>% # +1 is needed to start SE stocks from index 5 (Iijoki included as stock 4)
  select(yy,stock,Females,xx)%>%
  full_join(df1) # df1 has correct stock numbers

# Check that stock >= 5 for all swedish stocks
dfSE%>%filter(stock<5)
#View(dfSE)



# input to BUGS:

length(dfFI$eggs)
# 1865
length(dfSE$xx)
# 358

dfFI.bugs<-dfFI%>%
  select(eggs, year, stock, surv_eggs, M74, mortality100)

dfSE.bugs<-dfSE
  
#write_csv(dfFI.bugs, str_c(pathM74,"prg/input/M74dataFI22.csv"))
#write_csv(dfSE.bugs, str_c(pathM74,"prg/input/M74dataSE22.csv"))



# wrangle for figures

ysfm<-dfFI%>%
  group_by(stock, YEAR)%>%
  summarise(ysfm=round(mean(YSFM/100),2),
            N_fem=n())

n_M74notNA<-dfFI%>%
  filter(is.na(M74)==F)%>%
  group_by(stock, YEAR)%>%
  summarise(N_M74=n())

cases_M74<-dfFI%>%
  group_by(stock, YEAR)%>%
  count(M74)%>%
  filter(M74==2)%>%
  mutate(N_M74fem=n)

cases_mort100<-dfFI%>%
  group_by(stock, YEAR)%>%
  count(mortality100)%>%
  filter(mortality100==2)%>%
  mutate(N_mort100=n)

dfFI.2<-full_join(ysfm, n_M74notNA)%>%
  full_join(cases_M74)%>%
  full_join(cases_mort100, by=c("stock", "YEAR"))%>%
  select(stock, YEAR, ysfm, N_fem, N_M74, N_M74fem, N_mort100)%>%
  mutate(N_M74fem=ifelse(is.na(N_M74fem)==T, ifelse(is.na(N_M74)==T, NA, 0),N_M74fem))%>%
  mutate(N_mort100=ifelse(is.na(N_mort100)==T, ifelse(is.na(N_M74)==T, NA, 0),N_mort100))%>%
  mutate(propM74=round(N_M74fem/N_fem,2))%>%
  mutate(prop_mort100=round(N_mort100/N_M74,2))#%>%

#View(dfFI.2)

dfSE.2<-dfSE%>%
  mutate(propM74=round(xx/Females,2))%>%
  mutate(river=as.factor(stock))%>%
  mutate(YEAR=yy+1984)%>%
  mutate(N_fem=Females)%>%
  mutate(N_M74fem=xx)%>%
  select(YEAR, stock, N_fem, N_M74fem, propM74)#%>%

#View(dfSE.2)

# Joining must be made by stock (numeric). With river (factor/char) the numbers will mess up 
dfM74<-full_join(dfFI.2,dfSE.2)%>% 
  select(-N_M74)%>%
  ungroup()

#View(dfM74%>%filter(YEAR>2016)%>%arrange(stock))

