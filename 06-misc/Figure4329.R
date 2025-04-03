#   figure 4329 (tapsa stacked barplot)

library(tidyverse)
library(ggthemes)
library(readxl)

df_r <- read_xlsx("../../../Downloads/F4_3_2_9.xlsx",
                sheet = "F4.3.2.9 data",
                range = "A4:G41" )

df <- df_r %>% rename(Year = ...1,
                      `Commercial catch sea` = `commercial catch sea`,
                      `Misreported catch sea` = `misreported catch sea`,
                      `Unreported catch sea` = `unreported catch sea`, 
                      `Dead discarded catch sea` = `dead discarded catch sea`, 
                      `Recreational sea` = `recreational sea`,
                      `River catch` = `river catch`)



df_m <- df %>% melt(variable.name = "Style", value.name = "Number of salmon", id.vars = "Year")

ggplot(data = df_m,
       aes(
         x = Year ,
         y = `Number of salmon`,
         fill = Style
       ))+
  geom_bar(stat = "identity",
           position = position_stack(reverse = T))+
  scale_fill_manual(values =c(
    `Commercial catch sea` = "#4572A7",
    `Misreported catch sea` = "#AA4643",
    `Unreported catch sea` = "#89A54E",
    `Dead discarded catch sea` = "#71588F",
    `Recreational sea` = "#4198AF",
    `River catch` = "#DB843D"
  ))+
  guides(fill = guide_legend(reverse = T))+
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(df_m$Year),max(df_m$Year), by =2))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())+
  xlab(NULL)

df_p <- 
  df %>% 
    transmute(
      Year = Year,
      tot = rowSums(select(.,-Year), na.rm=T),
      `Commercial catch sea` = `Commercial catch sea`/tot,
      `Misreported catch sea` = `Misreported catch sea`/tot,
      `Unreported catch sea` = `Unreported catch sea`/tot,
      `Dead discarded catch sea` = `Dead discarded catch sea`/tot,
      `Recreational sea` = `Recreational sea`/tot,
      `River catch` = `River catch`/tot
      ) %>% 
    select(-tot)

df_mp <- df_p %>% melt(variable.name = "Style", value.name = "Number of salmon", id.vars = "Year")


ggplot(data = df_mp,
       aes(
         x = Year ,
         y = `Number of salmon`,
         fill = Style
       ))+
  geom_bar(stat = "identity",
           position = position_stack(reverse = T))+
  scale_fill_manual(values =c(
    `Commercial catch sea` = "#4572A7",
    `Misreported catch sea` = "#AA4643",
    `Unreported catch sea` = "#89A54E",
    `Dead discarded catch sea` = "#71588F",
    `Recreational sea` = "#4198AF",
    `River catch` = "#DB843D"
  ))+
  guides(fill = guide_legend(reverse = T))+
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(df_m$Year),max(df_m$Year), by =2))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,1, by =0.1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())+
  xlab(NULL)+
  ylab(NULL)
  
  
