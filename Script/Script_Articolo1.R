# Load packages
require(tidyverse)
require(magrittr)
require(readxl)
require(ade4)
require(lubridate)
require(factoextra)
source("Funzioni.R")
# source("https://gist.githubusercontent.com/TonyLadson/f37aab3e2ef517188a7f27166307c985/raw/0822970769bc90fcc28052a91b375399d665286e/UTM2deg.R")

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
# Appendo i due Medits TB e li rimuovo
medits_TB <- bind_rows(medits_TB1, medits_TB2) %>%
  mutate(SPECIE = paste(GENUS, SPECIES, sep = "-"))
medits_1517_TB %<>% mutate(SPECIE = paste(GENUS, SPECIES, sep = "-"))
rm(medits_TB1); rm(medits_TB2)

# Seleziono dai Medits solo le variabili di interesse
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

# Trasformare in decimal degree
medits_1517_TA[, c("X", "Y")] <- medits_1517_TA[, c("X", "Y")] %>% mutate_if(is.numeric, coord_dec_degree)

medits_1517_TB %<>%
  rename(TOTAL_WEIGHT_IN_HAUL = PTOT, TOTAL_NUMBER_IN_HAUL = NBTOT, FAUNISTIC_CATEGORY = CATFAU) %>%
  mutate(DATA = lubridate::ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
  select(YEAR,HAUL_NUMBER, SPECIE, TOTAL_WEIGHT_IN_HAUL,TOTAL_NUMBER_IN_HAUL, FAUNISTIC_CATEGORY, DATA) 

# Verifico se per ogni cala ho lo stesso numero di specie.
# Vorrei una situazione in cui per ogni cala ci siano tutte le specie e, se alcune di queste non sono pescate, settare a 0 il valore della variabile risposta
# Le cale 17,22,29,30,37,45 mancano anche per l'ultimo triennio
unique(medits_1517_TB$HAUL_NUMBER[medits_1517_TB$HAUL_NUMBER<47]) %>% sort

medits_TB <- complete(medits_TB,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                      fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))
medits_1517_TB <- complete(medits_1517_TB,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                      fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))

# Tolgo le cale mai presenti negli ultimi 13 anni 
fac_no1 <- expand.grid(2002:2014, c(17,22,29,30,37,45))
fac_no1 <- paste(fac_no1$Var1, fac_no1$Var2, sep = "-")
# fac_no2 <- expand.grid(2015:2017, c(17,22,29,30,37,45))
# fac_no2 <- paste(fac_no2$Var1, fac_no2$Var2, sep = "-")

medits_TB <- medits_TB %>% mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>%
  filter(!(fac %in% fac_no1)) %>% select(-fac)
medits_1517_TB <- medits_1517_TB %>% 
  filter(HAUL_NUMBER < 47) # %>% 
  # mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>%
  # filter(!(fac %in% fac_no2)) %>% select(-fac)

# Quante specie non sono in comune con l'ultimo triennio?
setdiff(unique(medits_1517_TB$SPECIE), unique(medits_TB$SPECIE)) %>% length()

#medits_1517_TA %<>% filter(as.numeric(HAUL_NUMBER) <= 46) 
# spec_only3years <- setdiff(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
#                            unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
# medits_1517_TB %<>% filter(SPECIE %in% unique(medits_TB$SPECIE))
# medits_1517_TB %<>% filter(as.numeric(HAUL_NUMBER) <= 46) 

# Join delle due tabelle 
medits_ALL_9114 <- left_join(medits_TB, medits_TA, by = c("YEAR", "HAUL_NUMBER")) %>%
  mutate(HAUL_NUMBER = factor(HAUL_NUMBER, levels = 1:46)) %>% select(-DATA.x) %>%
  rename(DATA = DATA.y) 
medits_ALL_1517 <- left_join(medits_1517_TB, medits_1517_TA, by = c("YEAR", "HAUL_NUMBER")) %>%
  select(-DATA.x) %>% rename(DATA = DATA.y) 

medits_ALL_9114$SHOOTING_DEPTH <- as.numeric(medits_ALL_9114$SHOOTING_DEPTH)
medits_ALL_1517$BOTTOM_TEMPERATURE_BEGINNING <- as.numeric(medits_ALL_1517$BOTTOM_TEMPERATURE_BEGINNING)
medits_ALL_1517$HAUL_NUMBER <- factor(medits_ALL_1517$HAUL_NUMBER, levels = 1:46)
medits_ALL <- bind_rows(medits_ALL_9114,medits_ALL_1517)

# medits_ALL %>% 
#   mutate(yy = ifelse(YEAR>=2015, 1, 0)) %>% 
#   distinct(X, Y, HAUL_NUMBER, yy) %>% 
#   ggplot(aes(x = X, y = Y, fill = factor(yy))) + geom_label(aes(label = HAUL_NUMBER))
# Effettivamente le cale nel nuovo dataset sono numerate allo stesso modo, da 1 a 46. 
spec_only3years <- setdiff(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
                           unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
spec_intersection_overall <- intersect(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
                           unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
spec_union_overall <- union(unique(medits_ALL$SPECIE[medits_ALL$YEAR>2014]), 
                                       unique(medits_ALL$SPECIE[medits_ALL$YEAR<2015]))
#medits_ALL %<>% filter(SPECIE %in% spec_intersection_overall)

rm(medits_ALL_1517); rm(medits_ALL_9114); rm(medits_TA); rm(medits_TB); rm(medits_1517_TA); rm(medits_1517_TB)


medits_ALL <- complete(medits_ALL,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
                       fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))
fac_no1 <- expand.grid(1994:2017, c(17,22,29,30,37,45))
fac_no1 <- paste(fac_no1$Var1, fac_no1$Var2, sep = "-")
medits_ALL %<>% mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>%
  filter(!(fac %in% fac_no1)) %>% select(-fac)
# Calcolo gli indici di biomassa 
medits_ALL %<>% 
  mutate(N_KM = TOTAL_NUMBER_IN_HAUL/SWEPT, 
         KG_KM = TOTAL_WEIGHT_IN_HAUL/SWEPT/1000)
medits_ALL$N_KM[is.na(medits_ALL$N_KM)] <- 0
medits_ALL$KG_KM[is.na(medits_ALL$KG_KM)] <- 0
write_delim(medits_ALL, delim = ",", path = "Dati Medits/Medits_Join_1994to2017.csv")
save(medits_ALL, file = "Dati Medits/Medits_Join_1994to2017.RData")
# Replico analisi entropart e ade4 senza Benthos ---------------------------
require(entropart)
# Load data dal 1991 al 2014
load("Dati Medits/Medits_Join_1994to2017.RData")

# Rimuovo il Benthos
medits_ALL %<>% filter(FAUNISTIC_CATEGORY %in% c("Ae", "Ao", "B", "C"))
medits_ALL %<>% mutate(DEPTH_CLASS = cut(as.numeric(SHOOTING_DEPTH), 
                                         breaks = c(0, 130, 200, +Inf),
                                         labels = c("Costa", "Bordo", "Scarpata")))
# Creo e calcolo i valori degli indici di biodiversità per le metacomunità annuali
mat_entropart <- create_metacommunity(data = medits_ALL, faunistic = F, fau_label = NULL, 
                                      index = "KG_KM")
Weights_list <- map(mat_entropart, colSums)
MC_allyears <- map2(mat_entropart, Weights_list, MetaCommunity_mine)

# Plot delle diversità per 4 diversi valori di q nel tempo
Tsallis_years(MC = MC_allyears, type = "Diversity", plot = T)

# Diversity Profiling for all years
profile_list <- list()
for(i in names(MC_allyears)){
  profile_list[[i]] <- DivProfile(q.seq = seq(0,2,0.1), MC = MC_allyears[[i]],
                                  Biased = TRUE, Correction = "Best")
}
### Alpha Communities
data_profile <- profile_list %>% map(`[[` ,"CommunityAlphaDiversities") %>%
  map(as_tibble)

len <- data_profile %>% map(ncol)
q_list <- replicate(max(medits_ALL$YEAR) - min(medits_ALL$YEAR) +1,
                    seq(0,2, by = 0.1), simplify = FALSE) %>% map2(., len, rep)

data_profile <- data_profile %>%
  map(gather, "key", "value")  %>%
  map2(., names(len), mutate) %>%
  map2(., q_list, mutate) %>%
  map(set_colnames, c("Haul", "Alphadiv", "Year", "q")) %>%
  Reduce(f = "bind_rows", x = .)
pdf("Immagini/Last_AlphaCommunities_byYears.pdf", height = 8, width = 15)
data_profile %>%
  ggplot + geom_line(aes(x = q, y = Alphadiv, colour = Haul)) + facet_wrap(~Year) +
  labs(x = "q", y = "Alpha diversity", title = "Alpha diversity of communities") +
  theme_bw() + theme(legend.position = "none")
dev.off()

# Total alpha, beta e gamma diversity
Total_div <- bind_rows(Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"TotalAlphaDiversity")) %>% as_tibble() %>% 
                         set_colnames(1994:2017) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Alpha"),
                       Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"TotalBetaDiversity")) %>% as_tibble() %>% 
                         set_colnames(1994:2017) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Beta"),
                       Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"GammaDiversity")) %>% as_tibble() %>% 
                         set_colnames(1994:2017) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Gamma")) %>%
  mutate(q = rep(seq(0,2, by = 0.1), 24*3))

for(j in c("Alpha", "Beta", "Gamma")){
  name <- paste("Immagini/Total", j, "byYear", ".pdf", sep = "")
  pdf(name, height = 8, width = 15)
  p <- Total_div %>% filter(Type == j) %>% 
    ggplot + aes(x = q, y = Diversity) + geom_line() +
    facet_wrap(~Year) + geom_point() +
    labs(x = "q", y = "Diversity", title = paste("Total", j, "varying q")) + 
    theme_bw() 
  print(p)
  dev.off()
}


### Ade4

pca_bymat <- map(mat_entropart, t) %>% map(dudi.pca, scannf = F, nf = 5)

# Plot delle cale
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(diameter = 2, npoints = 1000)

# Plot delle specie

for(yy in 1994:2014 %>% as.character){
  print(yy)
  fac <- factor(medits_ALL$FAUNISTIC_CATEGORY[order(rownames(pca_bymat[[yy]]$co))])
  name <- paste("Immagini/Specie/Specie_pca", yy, ".pdf", sep = "")
  pdf(name, height = 8, width = 12)
  p <- ggplot(dat,aes(x,y)) + geom_path() +
    #scale_x_continuous(limits = c(-5, 5)) + scale_y_continuous(limits = c(-5,5)) +
    geom_label(data = pca_bymat[[yy]]$co[,1:2], aes(x = Comp1, y = Comp2, fill = fac,
                                                    label = rownames(pca_bymat[[yy]]$co)), size = 2) +
    geom_vline(xintercept = 0, linetype = "dashed") + labs(title = yy) +
    geom_hline(yintercept = 0, linetype = "dashed") + theme_bw()
  print(p)
  dev.off()
}

#pca_bymat_traspose <- map(pca_bymat, t)
# Separare primi 8 anni dagli altri
# Ktable_pca <- ktab.list.dudi(list(pca_bymat$`1994`, pca_bymat$`1995`, pca_bymat$`1996`,
#                                   pca_bymat$`1997`, pca_bymat$`1998`, pca_bymat$`1999`,
#                                   pca_bymat$`2000`, pca_bymat$`2001`), tabnames = names(pca_bymat)[1:8])
# Ktable_pca2 <- ktab.list.dudi(list(pca_bymat$`2002`, pca_bymat$`2003`, pca_bymat$`2004`,
#                                   pca_bymat$`2005`, pca_bymat$`2006`, pca_bymat$`2007`,
#                                   pca_bymat$`2008`, pca_bymat$`2009`, pca_bymat$`2010`,
#                                   pca_bymat$`2011`, pca_bymat$`2012`, pca_bymat$`2013`,
#                                   pca_bymat$`2014`, pca_bymat$`2015`, pca_bymat$`2016`,
#                                   pca_bymat$`2017`), 
#                               tabnames = names(pca_bymat)[9:24])
Ktable_pca <- ktab.list.dudi(map(pca_bymat,t))
statis1 <- statis(Ktable_pca, scannf = FALSE)
# ade4::s.corcircle(pca_bymat$`1994`$co[,1:2], clabel = 0.2)
# scatter(pca_bymat$`1994`, clab.col = .4, clab.row = .6)
# bb <- pca_bymat$`1994`$c1[abs(pca_bymat$`1994`$c1[,2])>0.1,2, drop =F]
# barplot(as.matrix(bb) %>% t , las = 2, cex.names = .7)
fac_haul <- factor(medits_ALL %>% distinct(HAUL_NUMBER, DEPTH_CLASS) %>% na.omit() %$% 
                     DEPTH_CLASS[order(HAUL_NUMBER)])
fac_spec <- factor(medits_ALL %>% distinct(SPECIE, FAUNISTIC_CATEGORY) %$% 
                     FAUNISTIC_CATEGORY[order(SPECIE)])
plot(statis1)
names(statis1)

ggplot() +
  geom_label(data = statis1$C.li, aes(x = C1, y = C2, fill = fac_spec,
                                                   label = rownames(statis1$C.li)), size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") + #labs(title = yy) +
  geom_hline(yintercept = 0, linetype = "dashed") + theme_bw()
ggplot(data = data_frame(X = statis1$RV.tabw, Y = statis1$cos2, label = statis1$tab.names)) +
  geom_label(aes(x = X, y = Y, label = label, size = 1)) + theme_bw()
# ggplot(data = statis1$C.T4) + aes(x = C1,y=C2) + 
#   geom_label(label = rownames(statis1$C.T4)) + coord_polar()
