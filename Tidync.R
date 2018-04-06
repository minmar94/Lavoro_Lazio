# Load packages -----------------------------------------------------------
require(tidyverse)
require(RNetCDF)
require(raster)
require(chron)
require(lattice)
require(RColorBrewer)
require(tidync)
require(DataExplorer)
require(gridExtra)
require(gganimate)
#source("Functions.R")
#load("Variables.RData")
# Data Import -------------------------------------------------------------

dirs_all <- system2("ls", args = "*nc", stdout = TRUE)
# read data in a list
Data_MyOcean <- pmap(list(dirs_all), tidync) %>% set_names(dirs_all)
# Data_MyOcean is a list of list: each sublist contains one .nc file (in filenames)

hyper_tbl_BIO <- hyper_tibble(Data_MyOcean$BIO_006_008_BIO.nc, na.rm = FALSE) # 1.3 Giga

# hyper_tbl_BIO <- hyper_tbl_cube(Data_MyOcean$BIO_006_008_BIO.nc) # 428 Mb

# hyper_tbl_BIO %>%
#   gather(Variable, Value, ppn:dox) %>%
#   group_by(Variable) %>%
#   summarise(minimo = min(Value, na.rm = TRUE),
#             q1 = quantile(Value, probs = 0.25, na.rm = TRUE),
#             q2 = quantile(Value, probs = 0.5, na.rm = TRUE),
#             mean = mean(Value, na.rm = TRUE),
#             q3 = quantile(Value, probs = 0.75, na.rm = TRUE), 
#             massimo = max(Value, na.rm = TRUE))

p1<-hyper_tbl_BIO %>%
  filter(depth == hyper_tbl_BIO$depth[1], time == unique(hyper_tbl_BIO$time)[1]) %>%
  #gather(Variable, Value, ppn:dox) %>%
  ggplot + aes(x = longitude, y = latitude) +
  scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "white") + ggthemes::theme_few() +
  labs(title = paste("Depth =", hyper_tbl_BIO$depth[1])) #+ facet_wrap(~Variable, scales = "free")
grid.arrange(p1 + geom_raster(aes(fill = ppn)), p1 + geom_raster(aes(fill = dox)), nrow = 1)

gganimate(hyper_tbl_BIO %>% 
            filter(time %in% unique(hyper_tbl_BIO$time)[1:12]) %>%
            ggplot + aes(x = depth, y = dox, frame = time) + geom_point())

gganimate(hyper_tbl_BIO %>% 
            filter(time %in% unique(hyper_tbl_BIO$time)[1:12]) %>%
            ggplot + aes(x = depth, y = ppn, frame = time) + geom_point())
