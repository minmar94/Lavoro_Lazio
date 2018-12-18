# Entropart+ Ade4 analysis excluding Benthos ---------------------------
require(tidyverse)
require(magrittr)
require(readxl)
require(lubridate)
source("Script/Funzioni.R")
require(entropart)
require(ade4)
require(factoextra)

# Load data 
load("Dati Medits/Medits_Join_1994to2017.RData")

# Exclude the il Benthos
medits_ALL %<>% filter(FAUNISTIC_CATEGORY %in% c("Ae", "Ao", "B", "C"))
# Depth classes
medits_ALL %<>% mutate(DEPTH_CLASS = cut(as.numeric(SHOOTING_DEPTH), 
                                         breaks = c(0, 130, 200, +Inf),
                                         labels = c("Costa", "Bordo", "Scarpata")))

# Yearly abundance matrices of the metacommunity
mat_entropart <- create_metacommunity(data = medits_ALL, faunistic = F, fau_label = NULL, 
                                      index = "KG_KM")
Weights_list <- map(mat_entropart, colSums)
MC_allyears <- map2(mat_entropart, Weights_list, MetaCommunity_mine)
save(mat_entropart, file = "Dati Medits/mat_entropart.RData")
# Diversity plot for q = 0, 1, 2, 3.
Tsallis_years(MC = MC_allyears, type = "Diversity", plot = T)

# Diversity Profiling for all years
profile_list <- list()
for(i in names(MC_allyears)){
  profile_list[[i]] <- DivProfile(q.seq = seq(0,2,0.1), MC = MC_allyears[[i]],
                                  Biased = TRUE, Correction = "Best")
}

### Alpha Communities
data_profile <- profile_list %>% map(`[[` ,"CommunityAlphaDiversities") %>% map(as_tibble)

q_list <- replicate(max(medits_ALL$YEAR) - min(medits_ALL$YEAR) +1,
                    seq(0,2, by = 0.1), simplify = FALSE) %>% 
  map2(., data_profile %>% map(ncol), rep)

data_profile <- data_profile %>%
  map(gather, "key", "value")  %>%
  map2(., names(data_profile), mutate) %>%
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
coln <- unique(data_profile$Year)
Total_div <- bind_rows(Reduce(f = "cbind", 
                              x = profile_list %>% map(`[[` ,"TotalAlphaDiversity")) %>% as_tibble() %>% 
                         set_colnames(coln) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Alpha"),
                       Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"TotalBetaDiversity")) %>% as_tibble() %>%
                         set_colnames(coln) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Beta"),
                       Reduce(f = "cbind", x = profile_list %>% map(`[[` ,"GammaDiversity")) %>% as_tibble() %>% 
                         set_colnames(coln) %>% gather(., "Year", "Diversity") %>% mutate(Type = "Gamma")) %>%
  mutate(q = rep(seq(0,2, by = 0.1), length(coln)*3))

Total_div %<>% group_by(Type) %>% 
  nest() %>% 
  mutate(Plot_div = modify(.x = data, 
                           .f = function(.x){
                             .x %>% 
                               ggplot + aes(x = q, y = Diversity) + geom_line() +
                               facet_wrap(~Year) + geom_point() +
                               labs(x = "q", y = "Diversity", title = paste("Total", Type, "varying q")) + 
                               theme_bw() 
                           }))


#for(j in c("Alpha", "Beta", "Gamma")){#
  name <- paste("Immagini/Total", j, "byYear", ".pdf", sep = "")
  pdf(name, height = 8, width = 15)
  p <- Total_div %>% filter(Type == j) %>% 
    ggplot + aes(x = q, y = Diversity) + geom_line() +
    facet_wrap(~Year) + geom_point() +
    labs(x = "q", y = "Diversity", title = paste("Total", j, "varying q")) + 
    theme_bw() 
  print(p)
  dev.off()
#}


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
