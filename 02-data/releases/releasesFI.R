



source("../run-this-first-wgbast.r")


releases<-read_xlsx("../../WGBAST_shared/submodels/releases/Tapsa_08_25/Releases_FI_SAL_1959-2024_TRS_1981-2024.xlsx",
                    range="A1:Y23695", guess_max = 100000)

  View(releases)  

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
  # Pienvesistöt
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

# HUOM! Tarkasta että ei jää rivejä joilta AU puuttuu.
# Jos jää, jokikoodilla 99 olevat pitäisi laittaa ass_unit 3:een. Jostain
# syystä tämä ei nyt 8/25 toiminut täysin, yhdeltä riviltä putosi AU pois
# Puuttuvia ei kuitenkaan nyt jää joten toistaiseksi tämä ok.
#mutate(ass_unit=ifelse(river==99,3, ass_unit)) |> # Tässä vika! Tämä sotkee tunnuksen 81 
df |> filter(year>=2023) |> filter(is.na(ass_unit)==T)#filter(Tunnus==81)
df |> filter(year>=2023) |> filter(river==99, is.na(ass_unit)==T)#filter(Tunnus==81)


df2<-df |> select(Tunnus, river, ass_unit,everything())|> 
  filter(year>=2023) 
View(df2)
write_xlsx(df2, path="../../WGBAST_shared/submodels/releases/releases23-24_allparams_wip.xlsx")

# Vielä jää kaksi riviä joilla river on puuttuva, tunnus 81-82 mutta nämä ovatkin joki-istutuksia eli siltä osin ok
# se outous jää, että tiivistetysttä datassa (alla) on 156 riviä, mutta Tapsan SAS-ajossa vain 148 (muistaakseni...)
# Eli ihan yksi yhteen nämä taulut eivät vielä käy. Tapsan mukaan dataan ei ole tullut tässä välissä lisäyksiä.


df3<-df|> 
  filter(year>=2023) |> group_by(species, country, year, ass_unit, sub_div, sub_div2, sub_div3, river, age, REL_ARS) |> 
  summarise(numb=sum(numb))
View(df3)
write_xlsx(df3, path="../../WGBAST_shared/submodels/releases/WGBAST_releases2324_wip.xlsx")





tmp2<-df |> select(Tunnus, river, ass_unit,everything()) |> filter(Tunnus %in% (81:82))
View(tmp2)



df<-df |> filter(year>=2023) |> filter(river==99)#filter(Tunnus==81)
#View(df)  

tmp2<-df |> select(Tunnus, river, ass_unit,everything())
View(tmp2)

df |> filter(Tunnus==81, year>2022, is.na(ass_unit)==T) 
View(df |> filter(Tunnus==81, year>2022, is.na(ass_unit)==T) )
# A tibble: 0 × 37

# Nimistön ja tunnusten selkeytys
df2<-df
# df2 <-df |>mutate(rivername=Vesisto) |> 
#   mutate(rivername= recode(rivername, Kuivajokisuu="Kuivajoki")) |> 
#   mutate(rivername= recode(rivername, Isojoki ="Lapväärtinjoki", `Lapväärtinjoki (Isojoki)`= "Lapväärtinjoki",
#                            Summanjoki="Summajoki", Viantiejoki="Viantienjoki"))|> 
#   mutate(river= ifelse(rivername=="Lapväärtinjoki", 37, river)) 

tmp<-df2 |> group_by (river, rivername)|> summarise(N=n())|> na.omit()
View(tmp)


View(df2 |> group_by(rivername, river)   |> summarise(N=n())) 
# Ks 36, 37
View(df2 |> group_by(river, rivername)   |> summarise(N=n()))

df2<-df |> select(species, country, year, ass_unit, sub_div, sub_div2, sub_div3, river, age, orig, min, numb, max, n_type, REL_ARS)



df2 |> group_by(river) |> summarise(N=n())
df2 |> group_by(sub_div) |> summarise(N=n())
df2 |> group_by(sub_div2) |> summarise(N=n())
df2 |> group_by(sub_div3) |> summarise(N=n())

#View(df)

# For checkup
df |> group_by(Meri) |> summarise(N=n())
df |> group_by(species) |> summarise(N=n())
View(df |> group_by(river) |> summarise(N=n()))
df |> group_by(age) |> summarise(N=n())
df |> group_by(species) |> summarise(N=n())
df |> group_by(Laji) |> summarise(N=n())
