#   jennille

###################################################################
#   creating a table of M74 data and drawing the plot associated   #
###################################################################
source("run-this-first.R")

library(tidyverse)
library(reshape2)

# reading in M74 model inputs (ask from ANtti)
#   Finnish M74 input j = 
M74fi <- read.table(str_c(pathMain,"01-PROJECTS/M74/2024/data/2025/input/M74dataFI25.csv"),
                    header = T, sep = ",")



colnames(M74fi) <- c("Eggs", "year", "ss", "sur_eggs", "j", "k")
#   j == female; 1 no M74, 2 yes M74

#   wrangling finnish data to get ysfm and propM74
Mfi <- M74fi %>% 
  mutate(
    year = year %>% as.numeric,
    Eggs = Eggs %>% as.numeric,
    die_eggs = Eggs - sur_eggs,
    ysfm = die_eggs/Eggs) %>% 
  group_by(year, ss) %>% 
  mutate(
    ysfm = ysfm %>% mean(na.rm=T),
    propM74 = (sum(j-1)/n())
  ) %>% 
  select(Year = year, stock = ss, Eggs, ysfm, propM74) %>% unique

#   readinng swedish data
M74se <- read.table(str_c(pathMain,"01-PROJECTS/M74/2024/data/2025/input/M74dataSE25.csv"),
                    header = T, sep = ",")
colnames(M74se) <- c("year","ss","Females",  "m74fem")

#   wrangling swedish data for propM74
Mse <- M74se %>% 
  mutate(
    year = year %>% as.numeric,
    propM74 = m74fem/Females) %>% 
  group_by(year, ss) %>% 
  mutate(
    propM74 = propM74 %>% mean(na.rm=T)
  ) %>% 
  select(Year = year, stock = ss, Females, propM74) %>% unique



#   adding contry and  grouping over years
Mfi_m <- Mfi %>% 
  group_by(Year) %>% 
  transmute(
    propM74 = mean(propM74, na.rm=T)
  ) %>%
  mutate(Country = "Finland") %>% 
  filter(!is.na(propM74)) %>% 
  unique()

Mse_m <- Mse %>% 
  group_by(Year) %>% 
  transmute(
    propM74 = mean(propM74, na.rm=T)
    
  ) %>%
  mutate(Country = "Sweden") %>% 
  filter(!is.na(propM74)) %>% 
  unique()




M74_data_m <- left_join(Mse_m, Mfi_m, by = "Year") #    data for excel
M74_data <- full_join(Mse_m, Mfi_m)                #    data fro plot

df <- data.frame(
  "Year" = M74_data_m$Year+1984,
  "Finland" = M74_data_m$propM74.y,
  "Sweden" = M74_data_m$propM74.x
) %>% arrange(Year)

library("openxlsx")
# if excel is needed, uncomment this and change the file location where it saves
#write.xlsx(df ,file = "06-misc/M74Jenni.xlsx")


ggplot(data = M74_data)+
  geom_line(mapping = aes(x = Year+1984, y = propM74,
                          group = Country,
                          col = Country),
            stat = "identity",
            lwd = 1.3)+
  theme_bw(base_size = 13)+
  scale_color_manual(values = c(Finland = "blue", Sweden = "gold"))+
  ylab("Proportion of M74 females")+
  xlab("Year")





M74_tbl_r <- full_join(Mfi %>% select(Year, stock, propM74, ysfm), 
                       Mse %>% select(Year, stock, propM74),
) %>% 
  mutate(YEAR = Year+1984+1) %>% 
  ungroup() %>% 
  mutate(propM74_m = ifelse(is.na(propM74), ysfm, propM74))

names <- c("Simojoki","Tornionjoki","Kemijoki","Iijoki","Luleälven",
           "Skellelteälven","Ume/Vindelälven","Ångermanälven","Indalsälven",
           "Ljungan","Ljusnan","Dalälven","Mörrumsån","Unsampled stock")


M74_tbl = dcast(M74_tbl_r %>% select(-Year) %>% unique, formula = stock~YEAR, value = c("propM74")) %>% as.data.frame()
rownames(M74_tbl) <- c(names)

M74_t_r <- M74_tbl[1:13,2:40]


table(m74t$YEAR)
table(M74_tbl_r$YEAR)


#  mean simo tornio
#  mean lule indals dale
#  grand mean 

meanST <- colMeans(
  rbind(
    M74_t_r["Simojoki",],
    M74_t_r["Tornionjoki",]),
  na.rm=T)


meanLID <- colMeans(
  rbind(
    M74_t_r["Luleälven",],
    M74_t_r["Indalsälven",],
    M74_t_r["Daleälven",]),
  na.rm=T)

meanGRAND <- colMeans(M74_t_r, na.rm=T)

M74_t <- rbind(
  M74_t_r,
  "Mean ST" = meanST,
  "Mean LID" = meanLID,
  "Mean Grand" = meanGRAND
)

M74_table = ceiling(M74_t*100)
library(openxlsx)

write.xlsx(M74_table, file = "06-misc/M74Jenni(rounded).xlsx", rowNames=T)
