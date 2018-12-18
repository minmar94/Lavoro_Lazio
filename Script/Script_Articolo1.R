# Load packages
require(tidyverse)
require(magrittr)
require(readxl)
require(lubridate)
source("Script/Funzioni.R")

# Import Data -------------------------------------------------------------
# Medits TA 1994-2014
medits_TA <- read_excel("Dati Medits/Medits_TA_Lazio 1994-2014.xls", sheet = 1, col_names = TRUE)
# Medits TB 1994-2003
medits_TB1 <- read_excel("Dati Medits/Medits_TB_Lazio1994-2003.xls", sheet = 1, col_names = TRUE)
# Medits TB 2004-2014
medits_TB2 <- read_excel("Dati Medits/Medits_TB_Lazio 2004-2014.xls", sheet = 1, col_names = TRUE)
# Medits_2015-2017
medits_1517_TA <- read_excel("Dati Medits/medits2015_2017.xlsx", sheet = 1, col_names = TRUE) %>%
  set_colnames(toupper(colnames(.)))
medits_1517_TB <- read_excel("Dati Medits/medits2015_2017.xlsx", sheet = 2, col_names = TRUE) %>%
  set_colnames(toupper(colnames(.)))


# Data cleaning and preparation -------------------------------------------
# Join of TB tables
medits_TB <- bind_rows(medits_TB1, medits_TB2) %>%
  mutate(SPECIE = paste(GENUS, SPECIES, sep = "-"))
medits_1517_TB %<>% mutate(SPECIE = paste(GENUS, SPECIES, sep = "-"))
rm(medits_TB1); rm(medits_TB2)

# Select variable of interest
medits_TA %<>%
  select(YEAR, MONTH, DAY, HAUL_NUMBER, Y, X, SHOOTING_DEPTH,
         VALIDITY, BOTTOM_TEMPERATURE_BEGINNING, SWEPT) %>%
  mutate(BOTTOM_TEMPERATURE_BEGINNING = ifelse(is.na(BOTTOM_TEMPERATURE_BEGINNING),
                                               median(BOTTOM_TEMPERATURE_BEGINNING,na.rm = T),
                                               BOTTOM_TEMPERATURE_BEGINNING),
         DATA = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
  select(-MONTH, -DAY)

medits_TB %<>%
  select(YEAR, MONTH, DAY, HAUL_NUMBER, SPECIE, TOTAL_WEIGHT_IN_HAUL, TOTAL_NUMBER_IN_HAUL, FAUNISTIC_CATEGORY) %>% 
  mutate(DATA = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
  select(-MONTH, -DAY)

medits_1517_TA %<>% 
  rename(Y = SHOOTING_LATITUDE, X = SHOOTING_LONGITUDE, BOTTOM_TEMPERATURE_BEGINNING = TODEB) %>%
  select(YEAR, MONTH, DAY, HAUL_NUMBER, Y, X, SHOOTING_DEPTH, 
         VALIDITY, BOTTOM_TEMPERATURE_BEGINNING, SWEPT) %>%
  mutate(BOTTOM_TEMPERATURE_BEGINNING = ifelse(is.na(BOTTOM_TEMPERATURE_BEGINNING),
                                               median(BOTTOM_TEMPERATURE_BEGINNING,na.rm = T),
                                               BOTTOM_TEMPERATURE_BEGINNING),
         DATA = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
  select(-MONTH, -DAY)

# From UTM to decimal degree coordinates
medits_1517_TA[, c("X", "Y")] <- medits_1517_TA[, c("X", "Y")] %>% mutate_if(is.numeric, coord_dec_degree)

medits_1517_TB %<>%
  rename(TOTAL_WEIGHT_IN_HAUL = PTOT, TOTAL_NUMBER_IN_HAUL = NBTOT, FAUNISTIC_CATEGORY = CATFAU) %>%
  mutate(DATA = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
  select(YEAR,HAUL_NUMBER, SPECIE, TOTAL_WEIGHT_IN_HAUL,TOTAL_NUMBER_IN_HAUL, FAUNISTIC_CATEGORY, DATA) 


# Same number of species in each haul?
# If a species is not caught set its value in the haul and in that year = 0.
# Hauls 17,22,29,30,37,45 are missing from 2002
# unique(medits_1517_TB$HAUL_NUMBER[medits_1517_TB$HAUL_NUMBER<47]) %>% sort

medits_TB <- complete(medits_TB,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                      fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))
medits_1517_TB <- complete(medits_1517_TB,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                      fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))

# Remove the hauls cited above
fac_no1 <- expand.grid(2002:2014, c(17,22,29,30,37,45))
fac_no1 <- paste(fac_no1$Var1, fac_no1$Var2, sep = "-")
# fac_no2 <- expand.grid(2015:2017, c(17,22,29,30,37,45))
# fac_no2 <- paste(fac_no2$Var1, fac_no2$Var2, sep = "-")

medits_TB <- medits_TB %>% mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>%
  filter(!(fac %in% fac_no1)) %>% select(-fac)
medits_1517_TB <- medits_1517_TB %>% 
  filter(HAUL_NUMBER < 47)


# Quante specie non sono in comune con l'ultimo triennio?
# setdiff(unique(medits_1517_TB$SPECIE), unique(medits_TB$SPECIE)) %>% length()

# Join of all tables
medits_ALL_9114 <- left_join(medits_TB, medits_TA, by = c("YEAR", "HAUL_NUMBER")) %>%
  mutate(HAUL_NUMBER = factor(HAUL_NUMBER, levels = 1:46)) %>% select(-DATA.x) %>%
  rename(DATA = DATA.y) 
medits_ALL_1517 <- left_join(medits_1517_TB, medits_1517_TA, by = c("YEAR", "HAUL_NUMBER")) %>%
  select(-DATA.x) %>% rename(DATA = DATA.y) 

medits_ALL_9114$SHOOTING_DEPTH <- as.numeric(medits_ALL_9114$SHOOTING_DEPTH)
medits_ALL_1517$BOTTOM_TEMPERATURE_BEGINNING <- as.numeric(medits_ALL_1517$BOTTOM_TEMPERATURE_BEGINNING)
medits_ALL_1517$HAUL_NUMBER <- factor(medits_ALL_1517$HAUL_NUMBER, levels = 1:46)
medits_ALL <- bind_rows(medits_ALL_9114,medits_ALL_1517)
# spec_only3years <- setdiff(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]),
#                            unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))

# Effettivamente le cale nel nuovo dataset sono numerate allo stesso modo, da 1 a 46. 
# spec_only3years <- setdiff(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
#                            unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
# spec_intersection_overall <- intersect(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
#                            unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
# spec_union_overall <- union(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
#                                        unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
rm(medits_ALL_1517); rm(medits_ALL_9114); rm(medits_TA); rm(medits_TB); rm(medits_1517_TA); rm(medits_1517_TB)

# Complete again the dataset (?)
medits_ALL <- complete(medits_ALL,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                       fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))
fac_no1 <- expand.grid(1994:2017, c(17,22,29,30,37,45))
fac_no1 <- paste(fac_no1$Var1, fac_no1$Var2, sep = "-")
medits_ALL %<>% mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>%
  filter(!(fac %in% fac_no1)) %>% select(-fac)


# Abundance relative indeces
medits_ALL %<>% 
  mutate(N_KM = TOTAL_NUMBER_IN_HAUL/SWEPT, 
         KG_KM = TOTAL_WEIGHT_IN_HAUL/SWEPT/1000)
medits_ALL$N_KM[is.na(medits_ALL$N_KM)] <- 0
medits_ALL$KG_KM[is.na(medits_ALL$KG_KM)] <- 0
write_delim(medits_ALL, delim = ",", path = "Dati Medits/Medits_Join_1994to2017.csv")
save(medits_ALL, file = "Dati Medits/Medits_Join_1994to2017.RData")

