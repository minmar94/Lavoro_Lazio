# Load packages
require(tidyverse)
require(magrittr)
require(summarytools)
require(ade4)
require(vegan)
require(factoextra)
require(labstatR)
source("Funzioni.R")

# Load abundance matrices
load("mat_entropart.RData")
medits_ALL <- read.csv("Dati Medits/Medits_Join.csv", header = TRUE, sep = ",") %>%
  as_tibble()

data_nest <- medits_ALL %>% group_by(HAUL_NUMBER) %>% nest() %$% data %>% set_names(1:46)

centroid_list <- map(data_nest, find_centroid) %>% 
  Reduce(rbind, .) %>% set_rownames(1:46) %>% 
  as_data_frame() %>% rename(Y = x, X = y) %>% mutate(Stazione = paste("V",1:46, sep = ""))


# data("jv73")
# jv73
# plot(jv73$xy, col = sample(colors(distinct = TRUE), size = 12)[jv73$fac.riv], pch = 20)
# 
# data_dudi <- apply(mat_entropart$`1994`, 2, function(x) x/sum(x))
# 
# pca_prova <- dudi.pca(mat_entropart$`1994`, scannf = FALSE, nf = 3, center = T, scale = T)
# fviz_eig(pca_prova)
# 
# fviz_pca_var(pca_prova,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
#              )


Years_dat_9401 <- split(rep(1994:2001, times = 286), 1:8) %>% map(as_tibble) %>%
  map(set_colnames, "Year")

Years_dat_0214 <- split(rep(2002:2014, times = 286), 1:13) %>% map(as_tibble) %>%
  map(set_colnames, "Year")

Abund_mat_stack_9401 <- map(mat_entropart[1:8], as.data.frame) %>%
  map(rownames_to_column, var = "Specie") %>%
  map2(., Years_dat_9401, bind_cols) %>%
  map(gather, Stazione, Abund, V1:V46) %>% map(as_tibble) %>%
  do.call(what = "bind_rows")

Abund_mat_stack_0214 <- map(mat_entropart[9:21], as.data.frame) %>%
  map(rownames_to_column, var = "Specie") %>%
  map2(., Years_dat_0214, bind_cols) %>%
  map(gather, Stazione, Abund, V1:V40) %>% map(as_tibble) %>%
  do.call(what = "bind_rows")

Abund_mat_stack <- bind_rows(Abund_mat_stack_9401, Abund_mat_stack_0214)
rm(Abund_mat_stack_0214); rm(Abund_mat_stack_9401); rm(Years_dat_0214); rm(Years_dat_9401)

#pdf("Plottone.pdf", height = 12, width = 16)
Abund_mat_stack %>% filter(Stazione == "V10", Specie == "ABRA-VER") %>%
  ggplot(aes(x = Year, y = log1p(Abund), colour = Specie)) + geom_line() + geom_point() +
  #facet_wrap(~Stazione, nrow = 46) + 
  theme_bw() + theme(legend.position = "none")
#dev.off()


Abund_mat_stack %<>% left_join(centroid_list, by = "Stazione")

big_mat_abund <- Abund_mat_stack %>% mutate(ff = paste(Stazione, Year, sep = "-")) %>%
  select(Specie, ff, Abund) %>%
  spread(ff, Abund) %>% column_to_rownames("Specie") %>% as.matrix()

pca1 <- dudi.pca(big_mat_abund, scannf = F, nf = 4)
fviz_eig(pca1)

fviz_pca_ind(pca1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("darkgreen", "darkorange", "grey", "firebrick"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(pca1,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("darkgreen", "darkorange", "grey", "firebrick"),
             repel = TRUE     # Avoid text overlapping
)

#mahal_dist <- StatMatch::mahalanobis.dist(scale(Abund_mat_stack %>% distinct(Year, Abund, Y, X)))
# sil_width <- c(NA)
# for(i in 2:20){
#   pam_fit <- pam(mahal_dist, diss = TRUE, k = i)
#   sil_width[i] <- pam_fit$silinfo$avg.width
# }
# kmedoid <- cluster::pam(scale(Abund_mat_stack %>% distinct(Year, Abund, Y, X)), 10)


pca_bymat <- map(mat_entropart, t) %>% map(dudi.pca, scannf = F, nf = 5)

par(mfrow = c(7, 3))

scatter(pca_bymat$`1994`, clab.col = .4, clab.row = .6)
par(mfrow = c(1,1), pty = "m")
bb <- pca_bymat$`1994`$c1[abs(pca_bymat$`1994`$c1[,2])>0.1,2, drop =F]
barplot(as.matrix(bb) %>% t , las = 2, cex.names = .7)

#ggplot(data = bind_cols(pca_bymat$`1994`$co[,1:2], Specie = rownames(pca_bymat$`1994`$co)))


