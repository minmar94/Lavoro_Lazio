#' ---
#' title: "Fish fauna - EDA Fase 2"
#' author: "Marco Mingione"
#' output:
#'  html_document
#'
#' always_allow_html: yes
#'
#' ---

#+ setup, echo=FALSE, warning = FALSE, message = FALSE
# Riunione 25 Giugno ------------------------------------------------------
# Load packages
require(tidyverse)
require(magrittr)
require(entropart)
require(summarytools)
require(entropart)
require(emojifont)
knitr::opts_chunk$set(fig.align = "center", warning = FALSE, message = FALSE, results = "hide", cache = TRUE, echo = FALSE)
# Load Data
medits_ALL <- read.csv("Dati Medits/Medits_Join.csv", header = TRUE, sep = ",") %>%
  as_tibble()


#' ## Classificazione della profondità
#' - Costa = [0m, 130m); Bordo = [130m, 200m); Scarpata = [200m, 750m).
#+ costa
medits_ALL %<>% mutate(DEPTH_CLASS = cut(SHOOTING_DEPTH, breaks = c(0, 130, 200, 750),
                                         labels = c("Costa", "Bordo", "Scarpata")))
# summary
medits_ALL %>%
  filter(KG_KM !=0) %>%
  group_by(HAUL_NUMBER, YEAR) %>%
  mutate(n = length(unique(SPECIE))) %>%
  ungroup() %>%
  group_by(DEPTH_CLASS) %>%
  summarise(min = min(n),
            q1 = quantile(n, 0.25),
            median = median(n),
            mean = mean(n),
            q3 = quantile(n, 0.75),
            IQR = q3-q1,
            std.dev = sd(n),
            CV = std.dev/abs(mean),
            max = max(n),
            tot = n()) %>% xtable::xtable() %>%
  print(type="latex", comment=FALSE)
  # knitr::kable(format = "html", align = "c") %>%
  # kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "center")
#print(tab)
# Bubble Plot: size prop to number of caught species, color distinguishing by depth class
# p <- medits_ALL %>%
#   filter(KG_KM !=0) %>%
#   group_by(HAUL_NUMBER, YEAR) %>%
#   mutate(n = length(unique(SPECIE))) %>% ungroup() %>%
#   ggplot(aes(x = X, y = Y, frame = YEAR)) +
#   # geom_text(data = medits_ALL, aes(label = emoji("tropical_fish")),
#   #           colour = DEPTH_CLASS, family = "EmojiOne", size = n) +
#   geom_point(aes(size = n, color = DEPTH_CLASS)) +
#   theme_minimal()
# gganimate::gganimate(p, filename = "Bubble_plot.gif", interval = 5)

#   
# # Entropart per tutti gli anni (con KG_KM)
# # Creo una lista in cui ogni elemento è la matrice della MC in un dato anno
# mat_entropart <- medits_ALL %>%
#   group_by(YEAR) %>% nest() %$% data %>%
#   map(., `[[`, "KG_KM") %>%
#   map(., matrix, nrow = length(unique(medits_ALL$SPECIE)), byrow = FALSE) %>%
#   map(., set_rownames, unique(medits_ALL$SPECIE)) %>%
#   set_names(1994:2014)
# 
# set.seed(7777)
# colors_species <- sample(colors(distinct = TRUE), 319)
# # Metacommunities for all the years
# MC_allyears <- map(mat_entropart, MetaCommunity)
# 
# pdf("MC_plotbyyear.pdf", width = 12, height = 7) 
# for(i in names(MC_allyears)){
#   p <- data.frame(SPECIE = rep(rownames(MC_allyears[[i]]$Psi), ncol(MC_allyears[[i]]$Psi)), 
#                  HAUL_NUMBER = factor(rep(colnames(MC_allyears[[i]]$Psi), 
#                                           each = nrow(MC_allyears[[i]]$Psi)),
#                                       levels = colnames(MC_allyears[[i]]$Psi)),
#                  KG_KM = c(MC_allyears[[i]]$Psi)) %>%
#     ggplot(aes(x = HAUL_NUMBER, y = KG_KM, fill = SPECIE)) + geom_bar(stat = "identity") +
#     scale_fill_manual(values = colors_species) +
#     scale_y_continuous(expand = c(0,0)) + labs(y = "Species frequencies", x = "") +
#     theme_minimal() +  theme(legend.position = "none", 
#                              axis.text.x = element_text(size = 4))
#   
#   p1 <- data.frame(KG_KM = MC_allyears[[i]]$Ps, 
#                    SPECIE = names(MC_allyears[[i]]$Ps), HAUL_NUMBER = "Meta-Community") %>%
#     ggplot(aes(x = HAUL_NUMBER, y = KG_KM, fill = SPECIE)) + geom_bar(stat = "identity") +
#     scale_fill_manual(values = colors_species) +
#     scale_y_continuous(expand = c(0,0)) + labs(y = "", x = "", title = i) +
#     theme_minimal() +  theme(legend.position = "none")
#   gridExtra::grid.arrange(p, p1, ncol = 2)
# }
# dev.off()
# 
# # Tsallis entropy for all years
# Tsallis_mat <- bind_cols(`Richness` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Tsallis, q = 0) %>% 
#                       map(., as.numeric) %>% unlist(),
#                       `Shannon` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Tsallis, q = 1) %>% 
#                         map(., as.numeric) %>% unlist(),
#                       `Simpson` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Tsallis, q = 2) %>% 
#                         map(., as.numeric) %>% unlist(),
#                       `Generalized` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Tsallis, q = 3) %>% 
#                         map(., as.numeric) %>% unlist()) %>% mutate(Year = 1994:2014)
# Tsallis_mat %>%
#   gather(Entropy, Value, Richness:Generalized) %>%
#   ggplot + aes(x = Year, y = Value) + geom_line() + theme_light() + labs(y = "Entropy") + 
#   facet_wrap(~Entropy, scales = "free_y")
# 
# # Tsallis diversity for all years
# TDiver_mat <- bind_cols(`Richness` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Diversity, q = 0) %>% 
#                            map(., as.numeric) %>% unlist(),
#                          `Shannon` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Diversity, q = 1) %>% 
#                            map(., as.numeric) %>% unlist(),
#                          `Simpson` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Diversity, q = 2) %>% 
#                            map(., as.numeric) %>% unlist(),
#                         `Generalized` = map(.x = map(MC_allyears, `[[`, "Ps"), .f = Diversity, q = 3) %>%                            map(., as.numeric) %>% unlist()) %>% mutate(Year = 1994:2014)
# TDiver_mat %>%
#   gather(Entropy, Value, Richness:Generalized) %>%
#   ggplot + aes(x = Year, y = Value) + geom_line() + theme_light() + labs(y = "Entropy") + 
#   facet_wrap(~Entropy, scales = "free_y")
# # Chiaramente i grafici sono equivalenti
# 
# # Diversity Profiling for all years
# profile_list <- list()
# for(i in names(MC_allyears)){
#   profile_list[[i]] <- DivProfile(q.seq = seq(0,3,0.1), MC = MC_allyears[[i]],
#                                   Biased = TRUE, Correction = "Best")
# }
# 
# pdf("Profiling_years.pdf")
# map(profile_list, plot)
# dev.off()