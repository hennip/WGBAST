# Aja ensin input-osa Snapshots_new.r-tiedostosta!

# Petri ja Timo, Pohjanlahden merkintätutkimus:
# Itämeren malli: Laske millainen osuus Pohjanlahdelle nousevista AU1 kaloista jää rannikko- tai 
# jokikalastuksen saaliiksi?
# Miten saatu tulos vertautuu merkintätutkimuksen tuloksiin?
35/250 #14% Selkämeri
9/63 #14% Luoto
26/246 # 11%

stats<-function(var){
  q5<-q25<-q50<-q75<-q95<-c()
  
  for(i in 1:dim(var)[1]){
    tmp<-summary(as.mcmc(var[i,]), quantiles=c(0.05,0.25,0.5,0.75,0.95))
    q5[i]<-tmp$quantiles[1]
    q25[i]<-tmp$quantiles[2]
    q50[i]<-tmp$quantiles[3]
    q75[i]<-tmp$quantiles[4]
    q95[i]<-tmp$quantiles[5]
  }
  #res<-cbind(year[1]:year[length(year)],q50,q5,q95)
  res<-cbind(q5,q25,q50,q75,q95)
  return(res)
}



# Selkämerellä on AU2 ja AU3 kaloja
# Perämeren kalat voi olla myös Kalixista
# Hyljekuolleisuus vaikuttaa Selkämerellä ja Luodossa pyydettyihin, ei niinkään Perämerisiin
# Onko geneettisiä näytteitä otettu, tiedetäänkö kalojen alkuperää?
# Miten pyydyksiin jäämisen tn on muuttunut 2000-2020?

# Katso 1: kaikkien pohjanlahdelle palaavien tn jäädä saaliiksi kaikessa rannikko- ja jokikalastuksessa
# 2: kaikkien AU1 kalojen tn jäädä saaliiksi rannikolla/joessa
# Voiko näistä erottaa Suomen puolen kalastusta? Tornionjoki & Simojoki
# Rannikkopyynnin osalta erottelua on vaikea tehdä...


#RiverNames<-c("Torne", "Simo","Kalix","Rane","Pite","Aby","Byske","Rickle","Savaran",
#             "Ume","Ore","Logde","Ljungan","Morrum","Eman","Kage","Test")

# mat1: Pohjanlahdelle palaavat villit AU1-kalat
# cc : Rannikkosaalis, AU1 villit

dim(MatW_1) # Mature at May 1st
MatW_1[1,,1,1]
dim(CoastCatchW) # Coastal catch
CoastCatchW[1,,1,1]
CoastCatchW[1,,1,1]
dim(RiverCatchW) # Coastal catch
RiverCatchW[1,,1,1]

crW_MSW<-ccW_MSW<-matW_MSW<-crW_1SW<-ccW_1SW<-matW_1SW<-array(NA, dim=c(dim(MatW_1)[2],dim(MatW_1)[4]))
crAll_MSW<-ccAll_MSW<-matAll_MSW<-crAll_1SW<-ccAll_1SW<-matAll_1SW<-array(NA, dim=c(dim(MatR_1)[2],dim(MatR_1)[4]))
for(i in 1:dim(MatW_1)[2]){
  for(j in 1:dim(MatW_1)[4]){
    # 1SW-kalat AU1 villit
    matW_1SW[i,j]<-sum(MatW_1[2,i,1:4,j])
    ccW_1SW[i,j]<-sum(CoastCatchW[2,i,1:4,j])
    crW_1SW[i,j]<-sum(RiverCatchW[2,i,1:4,j])
  
    # MSW-kalat, AU1 villit
    matW_MSW[i,j]<-sum(MatW_1[3:6,i,1:4,j])
    ccW_MSW[i,j]<-sum(CoastCatchW[3:6,i,1:4,j])
    crW_MSW[i,j]<-sum(RiverCatchW[3:6,i,1:4,j])

    # 1SW-kalat AU1 villit + viljellyt
    matAll_1SW[i,j]<-sum(MatW_1[2,i,1:4,j])+MatR_1[2,i,1,j]
    ccAll_1SW[i,j]<-sum(CoastCatchW[2,i,1:4,j])+CoastCatchR[2,i,1,j]
    crAll_1SW[i,j]<-sum(RiverCatchW[2,i,1:4,j])+RiverCatchR[2,i,1,j]
    
    # MSW-kalat, AU1 villit+ viljellyt
    matAll_MSW[i,j]<-sum(MatW_1[3:6,i,1:4,j])+sum(MatR_1[3:6,i,1,j])
    ccAll_MSW[i,j]<-sum(CoastCatchW[3:6,i,1:4,j])+sum(CoastCatchR[3:6,i,1,j])
    crAll_MSW[i,j]<-sum(RiverCatchW[3:6,i,1:4,j])+sum(RiverCatchR[3:6,i,1,j])
  }
}
stats(mat_MSW[2:Nyears,])
stats(cc_MSW[2:Nyears,])
stats(cr_MSW[2:Nyears,])

# Pori-Merikarvia
# Merkityt MSW-kaloja. Whitlock et al 2018 -> Selkämeren näytteissä ~80% AU1-kalaa
# Lasketaan rannikon ja jokikalastuksen HR (sekä yhdistetty) AU1 MSW-kalojen mukaan


###############
# Rannikko HR

HRW_c_MSW<-ccW_MSW/matW_MSW
HRW_c_1SW<-ccW_1SW/matW_1SW
HRAll_c_MSW<-ccAll_MSW/matAll_MSW
HRAll_c_1SW<-ccAll_1SW/matAll_1SW
df1<-cbind(year[2:Nyears],stats(HRW_c_1SW[2:Nyears,]))
df2<-cbind(year[2:Nyears],stats(HRW_c_MSW[2:Nyears,]))
df3<-cbind(year[2:Nyears],stats(HRAll_c_1SW[2:Nyears,]))
df4<-cbind(year[2:Nyears],stats(HRAll_c_MSW[2:Nyears,]))
colnames(df1)<-colnames(df2)<-colnames(df3)<-colnames(df4)<-c("year","q5", "q25", "q50", "q75", "q95")
df1<-as_tibble(df1)%>%mutate(age="1SW", origin="Villi")
df2<-as_tibble(df2)%>%mutate(age="MSW", origin="Villi")
df3<-as_tibble(df3)%>%mutate(age="1SW", origin="Villi + viljelty")
df4<-as_tibble(df4)%>%mutate(age="MSW", origin="Villi + viljelty")

df<-full_join(df1,df2)%>%
  #full_join(df3)%>% full_join(df4)%>%
  filter(year > 1999 & year<2020)

ggplot(df, aes(x=year, group=year))+
  geom_boxplot(    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                   stat = "identity")+
  labs(x="Vuosi", y="Kalastuskuolleisuus", title="Kalastuskuolleisuus, Pohjanlahti")+
  scale_y_continuous(limits=c(0,0.6))+
  facet_wrap(~age)

###############
# Joki HR

HRW_r_MSW<-crW_MSW/matW_MSW
HRW_r_1SW<-crW_1SW/matW_1SW
HRAll_r_MSW<-crAll_MSW/matAll_MSW
HRAll_r_1SW<-crAll_1SW/matAll_1SW
df1<-cbind(year[2:Nyears],stats(HRW_r_1SW[2:Nyears,]))
df2<-cbind(year[2:Nyears],stats(HRW_r_MSW[2:Nyears,]))
df3<-cbind(year[2:Nyears],stats(HRAll_r_1SW[2:Nyears,]))
df4<-cbind(year[2:Nyears],stats(HRAll_r_MSW[2:Nyears,]))
colnames(df1)<-colnames(df2)<-colnames(df3)<-colnames(df4)<-c("year","q5", "q25", "q50", "q75", "q95")
df1<-as_tibble(df1)%>%mutate(age="1SW", origin="Villi")
df2<-as_tibble(df2)%>%mutate(age="MSW", origin="Villi")
df3<-as_tibble(df1)%>%mutate(age="1SW", origin="Villi + viljelty") #Oleta sama kuolleisuus viljellyille!
df4<-as_tibble(df2)%>%mutate(age="MSW", origin="Villi + viljelty")#Oleta sama kuolleisuus viljellyille!
# Edellä ei käytetty viljeltyjen jokikuolleisuuden estimaattia koska se on ajassa vakio (ei luotettava)!

df<-full_join(df1,df2)%>%
 # full_join(df3)%>% full_join(df4)%>%
  filter(year > 1999 & year<2020)

ggplot(df, aes(x=year, group=year))+
  geom_boxplot(    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                   stat = "identity")+
  labs(x="Vuosi", y="Kalastuskuolleisuus", title="Jokikalastuskuolleisuus")+
  scale_y_continuous(limits=c(0,0.5))+
facet_wrap(~age)


  ###############
# Rannikko+joki HR

HRW_tot_MSW<-(ccW_MSW+crW_MSW)/matW_MSW
HRW_tot_1SW<-(ccW_1SW+crW_1SW)/matW_1SW
df1<-cbind(year[2:Nyears],stats(HRW_tot_1SW[2:Nyears,]))
df2<-cbind(year[2:Nyears],stats(HRW_tot_MSW[2:Nyears,]))
colnames(df1)<-colnames(df2)<-c("year","q5", "q25", "q50", "q75", "q95")
df1<-as_tibble(df1)%>%mutate(age="1SW", origin="Villi")
df2<-as_tibble(df2)%>%mutate(age="MSW", origin="Villi")

df<-full_join(df1,df2)%>%
  filter(year > 1999 & year<2020)

ggplot(df, aes(x=year, group=year))+
  geom_boxplot(    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                   stat = "identity")+
  scale_y_continuous(limits=c(0,0.7))+
  labs(x="Vuosi", y="Kalastuskuolleisuus", title="Yhdistetty rannikko- ja jokikalastuskuolleisuus")+
  facet_wrap(~age)
  #geom_hline(yintercept = 0.3, col="red")+
  #geom_hline(yintercept = 0.14, col="blue")

#cbind(year, c(1:Nyears)) # 2015-2019 = 24-28 # 2000-2001 = 9-10

# Pori-Merikarvia 2020, MSW
# ==================================

# ilman katoa
HRW_tot_MSW<-(ccW_MSW+crW_MSW)/matW_MSW
mean_HR1_MSW<-apply(HRW_tot_MSW[26:28,], 2, mean);summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*249), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

0.14/0.259
0.14/0.178
0.14/0.358


# Merkkipalautuskato 30% rannikko, 20% jokipyynti, 15% merkeistä irtoaa
HRW_tot_MSW<-(0.7*ccW_MSW+0.8*crW_MSW)/matW_MSW
mean_HR1_MSW<-apply(HRW_tot_MSW[26:28,], 2, mean);summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*249*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-36/(mean_HR1_MSW*249*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# riskiä välttävä arvio lisäkuolevuudesta on keskimäärin 20%, maksimissaan 42% (95% todennäköisyydellä)


#mean_HR1_1SW<-apply(HR_tot_1SW[26:28,], 2, mean);summary(as.mcmc(mean_HR1_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# length(mean_HR1_MSW)
# z<-c()
# for(i in 1:1000){
#   z<-ifelse((1-35/(x*0.8*mean_HR1_MSW))>=0.42,1,0)
# }
# mean(z)

# Ajos 2020 1SW:
# ====================
mean_HRc_1SW<-apply(HRW_c_1SW[26:28,], 2, mean); summary(as.mcmc(mean_HRc_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
mean_mat_1SW<-apply(matW_1SW[26:28,], 2, mean); summary(as.mcmc(mean_mat_1SW))
mean_cc_1SW<-apply(ccW_1SW[26:28,], 2, mean); summary(as.mcmc(mean_cc_1SW))
mean_cr_1SW<-apply(crW_1SW[26:28,], 2, mean); summary(as.mcmc(mean_cr_1SW))
c1_1SW<-0.57*mean_cc_1SW;summary(as.mcmc(c1_1SW))
c2_1SW<-0.43*mean_cc_1SW; summary(as.mcmc(c2_1SW))
#HRc1_1SW<-(c1)/(mean_mat);summary(as.mcmc(HRc1))
HRc2_1SW<-(c2_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HRc2_1SW))
#HRc_new_1SW<-(c1_1SW+c2_1SW)/(mean_mat_1SW); summary(as.mcmc(HRc_new_1SW))

# Ilman katoa
HR_tot_ajos_1SW<-(c2_1SW+mean_cr_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HR_tot_ajos_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_ajos_1SW*183), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 30% rannikko, 20% jokipyynti, 15% merkeistä irtoaa
HR_tot_ajos_1SW<-(0.7*c2_1SW+0.8*mean_cr_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HR_tot_ajos_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_ajos_1SW*183*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-18/(HR_tot_ajos_1SW*183*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))


# Ajos 2020 MSW
# ====================
mean_HRc_MSW<-apply(HRW_c_MSW[26:28,], 2, mean); summary(as.mcmc(mean_HRc_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
mean_mat_MSW<-apply(matW_MSW[26:28,], 2, mean); summary(as.mcmc(mean_mat_MSW))
mean_cc_MSW<-apply(ccW_MSW[26:28,], 2, mean); summary(as.mcmc(mean_cc_MSW))
mean_cr_MSW<-apply(crW_MSW[26:28,], 2, mean); summary(as.mcmc(mean_cr_MSW))
c1_MSW<-0.57*mean_cc_MSW;summary(as.mcmc(c1_MSW))
c2_MSW<-0.43*mean_cc_MSW; summary(as.mcmc(c2_MSW))
HRc2_MSW<-(c2_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HRc2_MSW))
#tmp<-(c2*mean_HRc)/(c1+c2-mean_HRc*c1);summary(as.mcmc(tmp))

# Ilman katoa
HR_tot_ajos_MSW<-(c2_MSW+mean_cr_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HR_tot_ajos_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_ajos_MSW*58), quantiles=c(0.05,0.25,0.5,0.75,0.95))

# Merkkipalautuskato 30% rannikko, 20% jokipyynti, 15% merkeistä irtoaa
HR_tot_ajos_MSW<-(0.7*c2_MSW+0.8*mean_cr_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HR_tot_ajos_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_ajos_MSW*58*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.95))

summary(as.mcmc(1-8/(HR_tot_ajos_MSW*58*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.95))

# Siira et al

# Selkämeri 1SW 2001
# =============================
summary(as.mcmc(HRW_tot_1SW[10,]), quantiles=c(0.05,0.25,0.5,0.75,0.95))

# ilman katoa
HRW_tot_1SW<-(ccW_1SW+crW_1SW)/matW_1SW
mean_HR1_1SW<-HRW_tot_1SW[10,];summary(as.mcmc(mean_HR1_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_1SW*65), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 17% joki ja rannikko, 15% merkeistä irtoaa
HRW_tot_1SW<-0.83*(ccW_1SW+crW_1SW)/matW_1SW
mean_HR1_1SW<-HRW_tot_1SW[10,];summary(as.mcmc(mean_HR1_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_1SW*65), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-14/(mean_HR1_MSW*65*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))


# Selkämeri MSW 2001
# =============================
summary(as.mcmc(HRW_tot_MSW[10,]), quantiles=c(0.05,0.25,0.5,0.75,0.95))

# ilman katoa
HRW_tot_MSW<-(ccW_MSW+crW_MSW)/matW_MSW
mean_HR1_MSW<-HRW_tot_MSW[10,];summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*131), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 17% joki ja rannikko, 15% merkeistä irtoaa
HRW_tot_MSW<-0.83*(ccW_MSW+crW_MSW)/matW_MSW
mean_HR1_MSW<-HRW_tot_MSW[10,];summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*131*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-40/(mean_HR1_MSW*131*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Selkämeri MSW 2002
# =============================
summary(as.mcmc(HRW_tot_MSW[11,]), quantiles=c(0.05,0.25,0.5,0.75,0.95))

# ilman katoa
HRW_tot_MSW<-(ccW_MSW+crW_MSW)/matW_MSW
mean_HR1_MSW<-HRW_tot_MSW[11,];summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*75), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 17% joki ja rannikko, 15% merkeistä irtoaa
HRW_tot_MSW<-0.83*(ccW_MSW+crW_MSW)/matW_MSW
mean_HR1_MSW<-HRW_tot_MSW[11,];summary(as.mcmc(mean_HR1_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))
summary(as.mcmc(mean_HR1_MSW*75*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-24/(mean_HR1_MSW*75*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))



# Perämeren perukka 2001 1SW:
# ====================
mean_HRc_1SW<-HRW_c_1SW[10,]; summary(as.mcmc(mean_HRc_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
mean_mat_1SW<-matW_1SW[10,]; summary(as.mcmc(mean_mat_1SW))
mean_cc_1SW<-ccW_1SW[10,]; summary(as.mcmc(mean_cc_1SW))
mean_cr_1SW<-crW_1SW[10,]; summary(as.mcmc(mean_cr_1SW))
c1_1SW<-0.59*mean_cc_1SW;summary(as.mcmc(c1_1SW))
c2_1SW<-0.41*mean_cc_1SW; summary(as.mcmc(c2_1SW))
#HRc1_1SW<-(c1)/(mean_mat);summary(as.mcmc(HRc1))
HRc2_1SW<-(c2_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HRc2_1SW))
#HRc_new_1SW<-(c1_1SW+c2_1SW)/(mean_mat_1SW); summary(as.mcmc(HRc_new_1SW))

# Ilman katoa
HR_tot_pp_1SW<-(c2_1SW+mean_cr_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HR_tot_pp_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_pp_1SW*151), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 17% rannikko+joki, 15% merkeistä irtoaa
HR_tot_pp_1SW<-0.83*(c2_1SW+mean_cr_1SW)/(mean_mat_1SW-c1_1SW); summary(as.mcmc(HR_tot_pp_1SW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_pp_1SW*151*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-57/(HR_tot_pp_1SW*151*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Perämeren perukka 2001 MSW:
# ====================
mean_HRc_MSW<-HRW_c_MSW[10,]; summary(as.mcmc(mean_HRc_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
mean_mat_MSW<-matW_MSW[10,]; summary(as.mcmc(mean_mat_MSW))
mean_cc_MSW<-ccW_MSW[10,]; summary(as.mcmc(mean_cc_MSW))
mean_cr_MSW<-crW_MSW[10,]; summary(as.mcmc(mean_cr_MSW))
c1_MSW<-0.59*mean_cc_MSW;summary(as.mcmc(c1_MSW))
c2_MSW<-0.41*mean_cc_MSW; summary(as.mcmc(c2_MSW))
#HRc1_MSW<-(c1)/(mean_mat);summary(as.mcmc(HRc1))
HRc2_MSW<-(c2_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HRc2_MSW))
#HRc_new_MSW<-(c1_MSW+c2_MSW)/(mean_mat_MSW); summary(as.mcmc(HRc_new_MSW))

# Ilman katoa
HR_tot_pp_MSW<-(c2_MSW+mean_cr_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HR_tot_pp_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_pp_MSW*266), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

# Merkkipalautuskato 17% rannikko+joki, 15% merkeistä irtoaa
HR_tot_pp_MSW<-0.83*(c2_MSW+mean_cr_MSW)/(mean_mat_MSW-c1_MSW); summary(as.mcmc(HR_tot_pp_MSW), quantiles=c(0.05,0.25,0.5,0.75,0.95))
summary(as.mcmc(HR_tot_pp_MSW*266*0.85), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))

summary(as.mcmc(1-113/(HR_tot_pp_MSW*266*0.85)), quantiles=c(0.05,0.25,0.5,0.75,0.9,0.95))












# Tornion- ja Simojoen lohien osuus kaikista pohjanlahtisista

dim(MatW_1[])

tmp<-array(NA, dim=c(dim(MatW_1)[2],dim(MatW_1)[4]))
dim(tmp)
for(i in 1:dim(MatW_1)[2]){
  for(j in 1:dim(MatW_1)[4]){
# i<-1
# j<-1
       tmp[i,j]<-sum(MatW_1[2:6,i,1:3,j])/(sum(MatW_1[2:6,i,1:13,j])+sum(MatW_1[2:6,i,16,j]))
}}
df1<-cbind(year[2:Nyears],stats(tmp[2:Nyears,]))
colnames(df1)<-c("year","q5", "q25", "q50", "q75", "q95")

df<-as_tibble(df1)%>%
  filter(year > 1999 & year<2020)

ggplot(df, aes(x=year, group=year))+
  geom_boxplot(    aes(ymin = q5, lower = q25, middle = q50, upper = q75, ymax = q95),
                   stat = "identity")+
  scale_y_continuous(limits=c(0,1))

df

