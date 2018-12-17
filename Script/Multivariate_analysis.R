# Load packages
require(tidyverse)
require(Rcpp)
require(magrittr)
require(summarytools)
require(entropart)
require(magick)
require(vegan)
require(labstatR)
require(microbenchmark)
source("Funzioni.R")
sourceCpp("J_fun.cpp")
load("mat_entropart.RData")

# Dissimilarity Index
# SPEED TEST!
# t0 = Sys.time()
# D_list <- map(mat_entropart, Dissimilarity_list)
# Sys.time()-t0
# t0 = Sys.time()
# D_prova <- Dissimilarity_list(mat_entropart$`1994`)
# Sys.time()-t0
# 
# t0 = Sys.time()
# D_list <- map(mat_entropart, J_index_cpp)
# Sys.time()-t0
# t0 = Sys.time()
# D_prova <- J_index_cpp(MAT = mat_entropart$`1994`)
# Sys.time()-t0

# speed <- microbenchmark(
#   map(mat_entropart, Dissimilarity_list),
#   map(mat_entropart, J_index_cpp)
# )
D_list <- map(mat_entropart, J_index_cpp) %>% 
  map2(., rep(list(1:46, 1:40), c(8,13)), set_colnames) %>%
  map2(., rep(list(1:46, 1:40), c(8,13)), set_rownames)

img <- image_graph(450, 450)
out <- lapply(D_list, function(data){
  p <- data %>% as_tibble() %>% rownames_to_column("Stazioni") %>% gather(Stazioni2, Jacc, -Stazioni) %>%
    mutate(Stazioni = factor(Stazioni, levels = as.character(1:46)),
           Stazioni2 = factor(Stazioni2, levels = as.character(1:46))) %>%
    ggplot + geom_raster(aes(x = Stazioni, y = Stazioni2, fill = Jacc)) + 
    labs(x = "Stazione", y = "Stazione") + 
    scale_fill_distiller("Jaccard",palette = "Reds", direction = 1) + theme_bw()
  print(p)
})
image_write(image_animate(img, fps = 0.5), path = "GIF/Diss_year.gif")

#D_list$`1994`[upper.tri(D_list$`1994`)]

# ADE4 --------------------------------------------------------------------
#require(ade4)
corrplot::corrplot(1-D_list$`1994`, method = "circle", order = "hclust", tl.col = "black", is.corr = FALSE,
                   type = "upper")

img <- image_graph(450, 450)
out <- lapply(D_list, function(data){
  p <- corrplot::corrplot(1-data, method = "circle", order = "hclust", tl.col = "black", is.corr = FALSE)
  print(p)
})
image_write(image_animate(img, fps = 0.5), path = "GIF/Diss_clust_year.gif")
dev.off()

# Bray-Curtis
BC_list <- map(mat_entropart, t) %>% 
  map(vegdist, method = "bray") %>%
  map(as.matrix)

img <- image_graph(450, 450)
out <- lapply(BC_list, function(data){
  p <- corrplot::corrplot(1-data, method = "circle", order = "hclust", tl.col = "black", is.corr = FALSE)
  print(p)
})
image_write(image_animate(img, fps = 0.5), path = "GIF/BC_clust_year.gif")
dev.off()

corrplot::corrplot(1-BC_list$`1994`, method = "circle", order = "hclust", tl.col = "black", is.corr = FALSE,
                   type = "upper")

######

tab1 <- read.table("http://pbil.univ-lyon1.fr/members/thioulouse/TTGE/poolsPA.txt", header=TRUE)
#
# Read the farm and band names and pool numbers files:
#
farmNames <- scan("http://pbil.univ-lyon1.fr/members/thioulouse/TTGE/farms.txt", what="character")
poolNumbers <- scan("http://pbil.univ-lyon1.fr/members/thioulouse/TTGE/poolNumbers.txt")
codeBandes <- scan("http://pbil.univ-lyon1.fr/members/thioulouse/TTGE/CodeBandes.txt") #
# make the farms factor:
#
farms <- as.factor(rep(farmNames, poolNumbers))

pca1 <- dudi.pca(df = tab1, scale = FALSE, scannf = FALSE, nf = 4)
bga1 <- between(dudi = pca1, fac = farms, scannf = FALSE, nf = 3)
wga1 <- within(dudi = pca1, fac = farms, scannf = FALSE, nf = 3)
