# Load Packages
require(tidyverse)
require(magrittr)
require(readxl)
require(lubridate)

# Import Data
# Medits TA 1994-2014
medits_TA <- read_excel("Dati Medits/Medits_TA_Lazio 1994-2014.xls", sheet = 1, col_names = TRUE)
# Medits TB 1994-2003
medits_TB1 <- read_excel("Dati Medits/Medits_TB_Lazio1994-2003.xls", sheet = 1, col_names = TRUE)
# Medits TB 2004-2014
medits_TB2 <- read_excel("Dati Medits/Medits_TB_Lazio 2004-2014.xls", sheet = 1, col_names = TRUE)

# Appendo i due Medits TB e li rimuovo
medits_TB <- bind_rows(medits_TB1, medits_TB2) %>%
  mutate(SPECIE = paste(GENUS, SPECIES, sep = "-"))

rm(medits_TB1); rm(medits_TB2)

# Seleziono dai due Medits solo le variabili di interesse
medits_TA <- medits_TA %>%
  select(YEAR, MONTH, DAY, HAUL_NUMBER, Y, X, SHOOTING_DEPTH, VALIDITY, BOTTOM_TEMPERATURE_BEGINNING, SWEPT) %>%
  mutate(BOTTOM_TEMPERATURE_BEGINNING = ifelse(is.na(BOTTOM_TEMPERATURE_BEGINNING),median(BOTTOM_TEMPERATURE_BEGINNING,na.rm = T),
                                               BOTTOM_TEMPERATURE_BEGINNING),
         DATA = as.Date(paste(YEAR, MONTH, DAY, sep = "/")))
medits_TB <- medits_TB %>%
  mutate(DATA = as.Date(paste(YEAR, MONTH, DAY, sep = "/"))) %>%
  select(YEAR, MONTH, DAY, HAUL_NUMBER, SPECIE, TOTAL_WEIGHT_IN_HAUL, TOTAL_NUMBER_IN_HAUL,
         FAUNISTIC_CATEGORY)

# Qui si visualizza il numero complessivo di specie diverse pescate negli anni in corrispondenza di una data cala
sort(purrr::map(by(medits_TB$SPECIE, medits_TB$HAUL_NUMBER, unique), length) %>% unlist())

# Calcolo il numero di specie diverso che viene pescato per ogni cala in ogni anno e ne visualizzo l'andamento nel tempo
# In alcune cale 
gg <- medits_TB %>%
  #mutate(HAUL_YEAR = paste(HAUL_NUMBER, YEAR, sep = "-")) %>%
  group_by(HAUL_NUMBER, YEAR) %>%
  summarise(n = length(unique(SPECIE))) %>%
  #filter(HAUL_NUMBER == 22)
  ggplot(aes(x = YEAR, y = n, frame = paste("Cala n.: ",HAUL_NUMBER))) + #geom_bar(stat = "identity") +
  geom_line() + #facet_wrap(~HAUL_NUMBER, ncol = 4) +
  labs(y = "N. of different species") +
  #scale_fill_manual(values = sample(colors(), 46)) +
  theme_minimal() #+ theme(legend.position = "none")
gganimate::gganimate(gg)  

# Verifico se per ogni cala ho lo stesso numero di specie.
# Vorrei una situazione in cui per ogni cala ci siano tutte le specie e, se alcune di queste non sono pescate, settare a 0 il valore della variabile risposta
# Creo una griglia con tutte le combinazioni di Cale x Specie x Anni = 46 x 319 x 21
# NB: La griglia non è completa perchè 46 sono le cale dal 1994 al 2001, ci sono invece solo 40 cale dal 2002 al 2014
# Per un totale di 46 * 319 * 8 + 40 * 319 * 13 righe
medits_TB <- complete(medits_TB,YEAR,HAUL_NUMBER,nesting(SPECIE,FAUNISTIC_CATEGORY),
               fill=list(TOTAL_WEIGHT_IN_HAUL=0,TOTAL_NUMBER_IN_HAUL=0))

fac_no <- expand.grid(2002:2014, c(17,22,29,30,37,45))
fac_no <- paste(fac_no$Var1, fac_no$Var2, sep = "-")

medits_TB <- medits_TB %>% mutate(fac = paste(YEAR, HAUL_NUMBER, sep = "-")) %>% 
  filter(!(fac %in% fac_no)) %>% select(-fac)

# Join delle due tabelle 
medits_ALL <- left_join(medits_TB, medits_TA, by = c("YEAR", "HAUL_NUMBER")) %>%
  mutate(HAUL_NUMBER = factor(HAUL_NUMBER, levels = 1:46)) %>% select(-MONTH.x, -DAY.x) %>%
  rename(MONTH = MONTH.y, DAY = DAY.y) 


# Le seguenti cale hanno dati solo dal 1994 al 2001
medits_ALL %>%
  #mutate(HAUL_YEAR = paste(HAUL_NUMBER, YEAR, sep = "-")) %>%
  distinct(HAUL_NUMBER, YEAR) %>%
  group_by(HAUL_NUMBER) %>% count() %>% filter(n!=21)


# Calcolo gli indici di biomassa 
medits_ALL %<>% 
  mutate(N_KM = TOTAL_NUMBER_IN_HAUL/SWEPT, 
         KG_KM = TOTAL_WEIGHT_IN_HAUL/SWEPT/1000)
  # Se la specie non è pescata -> valore 0

write_delim(medits_ALL, delim = ",", path = "Dati Medits/Medits_Join.csv")
rm(medits_TA); rm(medits_TB)
# Plot di esempio
medits_ALL %>%
  # Scelgo una specie
  filter(SPECIE == "NEPR-NOR") %>% 
  # Calcolo per la specie selezionata, la biomassa media per anno sulle diverse cale
  group_by(YEAR) %>%
  summarise(KG_KM_AVG = mean(KG_KM, na.rm = TRUE), KG_KM_MED = median(KG_KM, na.rm = TRUE)) %>%
  ggplot(aes(x = YEAR, y = KG_KM_AVG)) + geom_point() + geom_line() +
  #geom_smooth(se = FALSE) +
  #geom_line(aes(y = KG_KM_MED), linetype = "dashed") + 
  theme_minimal()

# Quante cale per ogni anno
purrr::map(by(medits_ALL$HAUL_NUMBER, factor(medits_ALL$YEAR), unique), length) %>% unlist()

# Grafici
# Evoluzione della Meta-Community nel tempo
p <- medits_ALL %>% 
  mutate(N_KM = ifelse(N_KM == 0, NA, N_KM),
         KG_KM = ifelse(KG_KM == 0, NA, KG_KM)) %>%
  ggplot(aes(x = droplevels(factor(HAUL_NUMBER)), y = SPECIE, fill = log(KG_KM), frame = YEAR)) + geom_raster() + 
  scale_fill_distiller(palette = "Blues", na.value = "white", direction = 1) +
  labs(x = "Haul", y = "Specie") +
  ggthemes::theme_few() +
  #scale_x_discrete(limits = 1:40, labels = 1:40) +
  theme(axis.text.y = element_text(size = 4), 
        axis.text.x =  element_text(size = 6), 
        legend.position = "none")

gganimate::gganimate(p, filename = "MetaCommunity.gif")
# d3heatmap(prova_entropart, dendrogram = "none", colors = "Blues")

# Plot dei siti di pesca
require(ggmap)
#require(rworldmap)
require(emojifont)
Lazio <- get_map(c(10.5,40.5,14,43), maptype = "watercolor", source = "stamen")
# 885 siti diversi si inizio strascico
svg("Tropical_fish.svg")
Lazio %>% ggmap() + 
  geom_point(data = medits_ALL %>% distinct(HAUL_NUMBER, X,Y), 
             aes(x = X, y = Y, shape = "c", colour = factor(HAUL_NUMBER)),
                               size = 2) +
  # geom_text(data = medits_ALL %>% sample_n(500) %>% distinct(X,Y), 
  #           aes(x = X, y = Y, label = emoji("tropical_fish")),
  #            colour = I("black"), family = "EmojiOne", size = 9) +
  theme(legend.position = "none")
dev.off()
# require(plotly)
# map_data("world", "Italy") %>% plot_geo() %>%
#   add_markers(data = medits_ALL %>% sample_n(500) %>% distinct(X,Y), x = ~X, y = ~Y, 
#                             text = ~emoji("cow"))
#   #filter(between(long, 10.5, 14) & between(lat, 40.5, 43)) %>% 
#   #group_by(group) %>%
#   plot_geo(x = ~long, y = ~lat) %>%
#   add_markers(size = I(1))

# ENTROPART -> Indici di biodiversità
require(entropart)
#require(reshape2)

# Esempio anno 2014
# Creo dataset nella forma di una Meta-Community con righe = SPECIE, colonne = HAUL_NUMBER
prova_entropart <- matrix(medits_ALL$KG_KM[medits_ALL$YEAR == "2014"], nrow = 319, ncol = 40, byrow = FALSE)
rownames(prova_entropart) <- unique(medits_ALL$SPECIE)
colnames(prova_entropart) <- 1:40
MC <- MetaCommunity(prova_entropart, Weights = rep(1, ncol(prova_entropart)))
# Summary base
summary(MC)
# Plot base
set.seed(7777)
colors_species <- sample(colors(distinct = TRUE), 319)
plot(MC, col = colors_species)
# Replica del plot con ggplot --> + bello!
p <-data.frame(SPECIE = rep(rownames(MC$Psi), ncol(MC$Psi)), 
           HAUL_NUMBER = factor(rep(colnames(MC$Psi), each = nrow(MC$Psi)), levels = 1:40),
           KG_KM = c(MC$Psi)) %>%
  ggplot(aes(x = HAUL_NUMBER, y = KG_KM, fill = SPECIE)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_species) +
  scale_y_continuous(expand = c(0,0)) + labs(y = "Species frequencies", x = "") +
  theme_minimal() +  theme(legend.position = "none")

p1 <- data.frame(KG_KM = MC$Ps, SPECIE = names(MC$Ps), HAUL_NUMBER = "Meta-Community") %>%
  ggplot(aes(x = HAUL_NUMBER, y = KG_KM, fill = SPECIE)) + geom_bar(stat = "identity") +
  scale_fill_manual(values = colors_species) +
  scale_y_continuous(expand = c(0,0)) + labs(y = "", x = "") +
  theme_minimal() +  theme(legend.position = "none")
gridExtra::grid.arrange(p, p1, ncol = 2)

# Tsallis - q
curve(lnq(x,3), xlim = c(0,1), ylim = c(-4,0), ylab = expression(ln[q](x)))
curve(lnq(x,2), lty = 2, add = T)
curve(lnq(x,1), lty = 3, add = T)
curve(lnq(x,0), lty = 4, add = T)
#plot(MC$Ps, lnq(x = MC$Ps,3), col = "red")
# Indici di entropia al variare di q
round(cbind(`Richness` = as.numeric(Tsallis(Ps = MC$Ps, q = 0)), # Richness
          `Shannon` = as.numeric(Tsallis(Ps = MC$Ps, q = 1)), # Shannon
          `Simpson` = as.numeric(Tsallis(Ps = MC$Ps, q = 2)), # Simpson
          `Generalized` = as.numeric(Tsallis(Ps = MC$Ps, q = 3))),2) %>%  
  knitr::kable(format = "pandoc", caption = "Entropy")
# Indici di entropia al variare di q con correzione
round(cbind(`Richness` = as.numeric(bcTsallis(Ns = round(MC$Ns), q = 0)), # Richness
            `Shannon` = as.numeric(bcTsallis(Ns = round(MC$Ns), q = 1)), # Shannon
            `Simpson` = as.numeric(bcTsallis(Ns = round(MC$Ns), q = 2)), # Simpson
            `Generalized` = as.numeric(bcTsallis(Ns = round(MC$Ns), q = 3))),2) %>%  
  knitr::kable(format = "pandoc", caption = "Entropy Bias Corrected")

# Converto Entropy a Diversity
round(cbind(`Richness` = as.numeric(Diversity(Ps = MC$Ps, q = 0)),
            `Shannon` = as.numeric(Diversity(Ps = MC$Ps, q = 1)),
            `Simpson` = as.numeric(Diversity(Ps = MC$Ps, q = 2)),
            `Generalized` = as.numeric(Diversity(Ps = MC$Ps, q = 3))),2) %>%  
  knitr::kable(format = "pandoc", caption = "Diversity")

# Diversity Bias Corrected
round(cbind(`Richness` = as.numeric(bcDiversity(Ns = round(MC$Ns), q = 0)),
            `Shannon` = as.numeric(bcDiversity(Ns = round(MC$Ns), q = 1)),
            `Simpson` = as.numeric(bcDiversity(Ns = round(MC$Ns), q = 2)),
            `Generalized` = as.numeric(bcDiversity(Ns = round(MC$Ns), q = 3))),2) %>%  
  knitr::kable(format = "pandoc", caption = "Diversity Bias Corrected")

# Diversity Partitioning
q0 <- DivPart(q = 0, MC = MC, Biased = TRUE, Correction = "Best")
q1 <- DivPart(q = 1, MC = MC, Biased = TRUE, Correction = "Best")
q2 <- DivPart(q = 2, MC = MC, Biased = TRUE, Correction = "Best")
q3 <- DivPart(q = 3, MC = MC, Biased = TRUE, Correction = "Best")
purrr::map(list(q0, q1, q2, q3), summary)

par(mfrow = c(2,2))
# Plot of the Diversity Partitioning of the Meta-Community varying with q
# The long rectangle (of height 1) represents gamma diversity
# The narrower and higher rectangles have the same area and represents alpha diversity and beta diversity respectively
plot(q0, ylim = c(0,8), main = "q = 0")
# add text()
plot(q1, ylim = c(0,8), main = "q = 1")
plot(q2, ylim = c(0,8), main = "q = 2")
plot(q3, ylim = c(0,8), main = "q = 3")

# Diversity Partitioning Estimation
q0_est <- DivEst(q = 0, MC = MC, Biased = TRUE, Correction = "Best")
q1_est <- DivEst(q = 1, MC = MC, Biased = TRUE, Correction = "Best")
q2_est <- DivEst(q = 2, MC = MC, Biased = TRUE, Correction = "Best")
q3_est <- DivEst(q = 3, MC = MC, Biased = TRUE, Correction = "Best")
purrr::map(list(q0_est, q1_est, q2_est, q3_est), summary)

par(mfrow = c(2,2))
# Plot of the Estimated Diversity Partitioning (Densities and CI)
plot(q0_est, ylim = c(0,8), main = "q = 0")
plot(q1_est, ylim = c(0,8), main = "q = 1")
plot(q2_est, ylim = c(0,8), main = "q = 2")
plot(q3_est, ylim = c(0,8), main = "q = 3")

# Diversity Profiling
profile <- DivProfile(q.seq = seq(0,3,0.1), MC = MC, Biased = TRUE, Correction = "Best")
#summary(profile)
plot(profile)

# Entropy of MonteCarlo simulated Communities
SimulatedDiversity <- expq(EntropyCI(FUN = bcTsallis, Simulations = 1000, Ns = round(as.numeric(MC$Ns)), q = 2), q = 2)
bcDiversity(Ns = round(as.numeric(MC$Ns)), q = 2)
quantile(SimulatedDiversity, probs = c(0.025, 0.975))


# require(tmap)
# require(tmaptools)
# require(sf)
# 
# medits_SF <- st_as_sf(medits_ALL %>% filter(!(is.na(X) & is.na(Y))), coords = c("X", "Y"), crs = 4326, na.fail = F)
# colnames(medits_ALL)
# colnames(medits_SF)
# 
# tm_shape(Europe, bbox = "Italy") + tm_fill() + tmap_mode(mode = "plot")
# plot(st_geometry(medits_SF))
