



source("../run-this-first-wgbast.r")

################################################################################
# Sorry about a bit bad logic, but releases are under submodels together with 
# catch related datasets
################################################################################

# Releases - from Azure data lake
##################################
(releases<-read_xlsx("../../WGBAST_shared/submodels/releases/Releases_FI_SAL_1959-2024_TRS_1981-2024.xlsx",
                    range="A1:Y23695", guess_max = 100000))
#  View(releases)  

# River names
##################################
# Tapsalta 29/8/25
# Inkoonjoki ja Ladjankoskenoja ovat pienvesistöalueita, joille kyllä löytyy oma 
# numeronsa SYKEn vesistöalueluettelosta. Kannattaisi harkita, että raakadatassa 
# alettaisiin käytettää SYKEn numerointia myös pienvesistöjen kohdalla. Silloin 
# ne saadaan rimmaamaan ICES Vocabylary  RiverAndCatchment  notaation kanssa.  
# ICES Vocabylaryn ko. listaus Suomen jokien osalta on ohessa (siitä puuttuu 
# AU -numerointi, mutta lisää se mukaan). Katso myös ICES Reference Codes - RECO 
# ja siellä RiversAndCatchments (https://vocab.ices.dk/)

(df_names<-read_xlsx("../../WGBAST_shared/submodels/releases/WGBAST_Rivers_FI_revised_Aug2025.xlsx"))
#View(df_names)

df <-releases |> 
  mutate(Meri=recode(Meri, Meri="MERI", Joki="JOKI")) |> 
  mutate(year=Istvuosi, country="FI", numb=Kpl/1000, sub_div=`osa-alue`) |>
  mutate(min=NA, max=NA, n_type=NA, orig="R") |> 
  mutate(species= recode(Laji, Lohi="SAL", Merilohi="SAL", Meritaimen="TRS" )) |> 
  mutate(age=recode(Ika, aik="adult",vk="alevin", ek="fry", mspa="eyed egg")) |> 
  mutate(age=gsub("k", "s parr", age)) |> # 1 and 2 summer parr 
  mutate(age=gsub("vjp", "yr parr", age)) |> # parr, all ages
  mutate(age=gsub("1v", "1yr", age)) |> # 1yr smolts
  mutate(age=ifelse(age=="2v" | age=="3k", "2yr", age))|> # 2yr smolts
  mutate(age=ifelse(age=="3v" | age=="4k"| age=="4v", "3yr", age)) |>  # 3yr smolts
  
  mutate(Tunnus= as.numeric(Tunnus)) |> 
  mutate(Tunnus=ifelse(is.na(Tunnus)==T, 1000, Tunnus)) |> 
  mutate(Tunnus= ifelse(Istutuspaikka=="Pohjoisselkä Otusteininvuorenranta, Virolahti", 81, Tunnus)) |> # Lisätään puuttuva, korjattava myös alkup. dataan
  mutate(river=Tunnus) |> 
  mutate(river=ifelse(is.na(river)==T, 1000, river)) |> 

  mutate(river=ifelse(Tunnus==82 & `osa-alue`==33, 99, NA)) |> # Åland
  mutate(sub_div=ifelse(Tunnus==82 & `osa-alue`==33, 29, sub_div)) |>  # Åland
  mutate(river=ifelse(Tunnus<=80, Tunnus, river)) |> 
  
  # Pienvesistöt 81-84, jokin pieni joki, ei pysty kiinnittämään selkeästi tiettyyn virtaveteen
  # 81: 
  # 82: Saaristomeri
  # 83:
  # 84: Perämeri
  mutate(river=ifelse(Tunnus %in% (81:84) & Meri=="MERI" & (is.na(river)==T | river!=99), 101, river)) |> # at sea 

  mutate(ass_unit=ifelse(river<=23,6, NA)) |> 
  mutate(ass_unit=ifelse(Tunnus==81,6, ass_unit)) |> # Tähän asti toimii
  mutate(ass_unit=ifelse(river %in% (24:48),3, ass_unit)) |> # ja tähän
  
  
  mutate(ass_unit=ifelse(Tunnus %in% (82:83),3, ass_unit)) |> 
  mutate(ass_unit=ifelse(Tunnus %in% (49:67),1, ass_unit)) |> 
  mutate(ass_unit=ifelse(Tunnus==84,1, ass_unit)) |> 
  
  mutate(sub_div2=ifelse(sub_div %in% (29:31) , "22-31", 
                  ifelse(sub_div==32, 32, NA))) |> 
  mutate(sub_div3=ifelse(sub_div==29 , "200", 
                  ifelse(sub_div %in% (30:31), 300, 
                  ifelse(sub_div==32, 32, NA)))) |>   # Huom! Muutetaan REL_ARS myöhemmin niin että 1: eväleikkaus, 2: ARS, 3:Molemmat, 0:Ei
mutate(REL_ARS = ifelse(REL==1 & (ARS =="Ei"|is.na(ARS)==T), "adipose fin clipped",
                        ifelse(REL==0 & ARS =="Kyllä", "alizarin marked", 
                        ifelse(REL==1 & ARS =="Kyllä", "fin clipped and alizarin marked",
                               NA
                        ))))  

df2<-df |> select(Tunnus, river, ass_unit,everything())|> 
  filter(year>=2023) |> 
  mutate(Tunnus=as_factor(Tunnus))
#View(df2)

df_names2<-df_names |> 
  select(Subdivision, `River name`, National_River_ID ) |> 
  mutate(Tunnus=as_factor(National_River_ID), river_name=`River name`)# |> 
#  select(-National_River_ID)
#View(df_names2)

df3<-left_join(df2, df_names2) |> 
  select(Tunnus, river, river_name, Vesisto, Istutuspaikka, Vesistotunnus, Subdivision, everything()) |> 
  mutate(river_name=ifelse(is.na(river_name)==T & river==101, "at sea", river_name)) |> 
  mutate(river_name=ifelse(is.na(river_name)==T & river==99 & Tunnus==82, "Saaristomeren rannikkoalue, Ahvenanmaa", river_name)) |> 
  mutate(river_name=ifelse(is.na(river_name)==T & river==99 & Tunnus==81, "Suomenlahden rannikkoalue", river_name)) |> 
# Pienvesistöjä
  mutate(river_name=ifelse(is.na(river_name)==T & Vesisto == "Inkoonjoki", Vesisto, river_name)) |> 
  mutate(river_name=ifelse(is.na(river_name)==T & Vesisto == "Ladjankoskenoja", Vesisto, river_name))

# Tarkista ettei ole puuttuvia
df3 |> filter(is.na(river_name==T))
df3 |> filter(year>=2023) |> filter(is.na(ass_unit)==T)#filter(Tunnus==81)
df3 |> filter(year>=2023) |> filter(river==99, is.na(ass_unit)==T)#filter(Tunnus==81)

df4<-df3|> 
  filter(year>=2023) |> group_by(species, country, year, ass_unit, 
                                 sub_div, sub_div2, sub_div3, river_name, age, REL_ARS) |> 
  filter(age!="adult") |> 
  summarise(numb=sum(numb))
View(df4)
write_xlsx(df4, path="../../WGBAST_shared/submodels/releases/releases_FI23-24_ICESformat.xlsx")

df4 |> filter(river_name=="at sea", sub_div==30)

tmp<-df3 |> filter(river_name=="at sea", sub_div==30)
View(tmp)


df4 |> ungroup() |> summarise(N=sum(numb))


