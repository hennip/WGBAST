
# Read in WGBAST catch and effort data in basic form (no further filtering)
# This object is used for compiling catch&effort data, ureported&discards and trolling catch data

wgbast_catch_data<-read_xlsx(str_c(pathIn, 
                "WGBAST_2025_Catch_20.02.2025.xlsx"), # Update!
          range="A1:Q18586", # Update!
          sheet="Catch data", col_names = T, guess_max = 10000, na=c("",".", "NaN", "NA"))%>%
  # filter(YEAR>2005)%>% # Include results only 2009 onwards, catch DB has only updates from those years 
  # mutate(NUMB=parse_double(NUMB))%>%
  select(SPECIES, COUNTRY, YEAR, TIME_PERIOD, TP_TYPE, sub_div2, FISHERY, F_TYPE, GEAR, NUMB, EFFORT, everything())%>%
  mutate(TP_TYPE=ifelse(TP_TYPE=="QRT", "QTR", TP_TYPE)) |> 
  mutate(GEAR=ifelse(GEAR=="GNS", "MIS", GEAR))


