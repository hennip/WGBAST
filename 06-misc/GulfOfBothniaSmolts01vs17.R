library(tidyverse)
library(forcats)
library(readxl)
library(RColorBrewer)
library(wesanderson)

display.brewer.pal(n=8,"Dark2")
display.brewer.pal(n=8,"Set2")
display.brewer.pal(n=8,"Set1")
display.brewer.pal(n=10,"Paired")
display.brewer.pal(n=8,"RdBu")

my_palette = c(
  brewer.pal(8, "Paired"))
palette(my_palette)



pathIn<-"H:/Biom/doc/"

(dat<-read_xlsx(paste(sep="",pathIn,"GulfOfBothniaSmolts01vs17.xlsx"), skip=1, na="NA"))

#df<-mutate(dat, est=parse_factor(est, levels=c("med01","med17","low17","high17"))) # parse_factor jos haluaa vierheilmot
df<-mutate(dat, est=factor(est, levels=c("med01","med17","low17","high17")))
df2<-mutate(df,est = fct_recode(est,
                               "2001"="med01", 
                           "Q50%, 2017"="med17", 
                           "Q5%, 2017"="low17", 
                           "Q95%, 2017"="high17"
                               ))

df2

(tac<-read_xlsx(paste(sep="",pathIn,"TACfrom2000.xlsx"), na="NA"))
#(tac<-mutate(tac, Year=parse_integer(year)))
(tac<-mutate(tac, removal=Reported+Disc+UnRep))


ggplot(filter(df2,est=="2001"))+
  geom_line(aes( x=year, y=n), col=2, size=1.2)+
  geom_hline(yintercept =1312, col=2, size=1.2)+
  labs(y="Lukumäärä tuhansina", x="Vuosi", title="Arvioitu vaelluspoikastuotanto, Pohjanlahti")+
  coord_cartesian(ylim=c(0,2000), xlim=c(1993:2018))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


ggplot(df2)+
  geom_line(aes( x=year, y=n, color=est),size=1.2)+
  scale_color_manual(values=c(2,4,3,3))+
#  geom_line(data=tac,aes(x=year, y=TAC*10), col=6, size=1.2)+
#  geom_line(data=tac,aes(x=year, y=removal*10), col=8, size=1.2)+
  geom_hline(yintercept =1312, col=2, size=1.2)+
  geom_hline(yintercept =3408, col=4, size=1.2)+
  geom_hline(yintercept =2830, col=3, size=1.2)+
  geom_hline(yintercept =4134, col=3, size=1.2)+
  labs(y="Lukumäärä tuhansina", x="Vuosi", title="Arvioitu vaelluspoikastuotanto, Pohjanlahti", color="Estimaatti")+
  coord_cartesian(ylim=c(0,5000), xlim=c(1993:2018))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))

ggplot(tac)+
  geom_line(aes(x=year, y=TAC), col=6, size=1.2)+
  geom_line(aes(x=year, y=removal), col=8, size=1.2)
