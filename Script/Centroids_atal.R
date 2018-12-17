require(tidyverse)
require(magrittr)
require(entropart)
#require(ncdf4)
require(raster)
require(rgeos)
require(tidync)

# Capire come far "parlare" il dataset con le pescate e il dataset con le variabili ambientali

#Bio_var <- nc_open("/home/marco/Dropbox/Surveys Lazio/BIO_TRIMESTRI_out.nc")
Phy_var_tidy <- tidync("/home/marco/Dropbox/Surveys Lazio/TEMPE_TRIMESTRI_out.nc")
medits_ALL <- read.csv("Dati Medits/Medits_Join.csv", header = TRUE, sep = ",") %>%
  as_tibble()
# Trova i centroidi di un set di coordinate
find_centroid <- function(data){
  subset_coord <- data %>% distinct(X,Y) %>% sp::SpatialPoints()
  cc <- rgeos::gCentroid(subset_coord) %>% sp::coordinates()
  return(cc)
}

data_nest <- medits_ALL %>% group_by(HAUL_NUMBER) %>% nest() %$% data %>% set_names(1:46)

centroid_list <- map(data_nest, find_centroid) %>% 
  Reduce(rbind, .) %>% set_rownames(1:46) %>% 
  as_data_frame() %>% rename(Y = x, X = y)
require(sp); require(rgdal)
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}
medits_ALL %>% distinct(X,Y,HAUL_NUMBER) %>%
  ggplot + aes(x = X, y = Y, colour = I("grey")) + geom_point()+
  geom_text(data = centroid_list, aes(x=X,y=Y, colour = I("red"), label = 1:46), size = 5) +
  ggthemes::theme_few()



Temperature <- Phy_var_tidy %>% activate("votemper") %>% hyper_tibble() %>% 
  mutate(time = as.POSIXct(time, origin = "1970-01-01"))
#Dox %>% filter(longitude %in% unique(medits_ALL$X) | latitude %in% unique(medits_ALL$Y))
ggplot(data = Dox %>% filter(depth == min(Dox$depth), time == min(Dox$time))) + 
  geom_raster(aes(x = longitude, y = latitude, fill = dox)) +
  geom_text(data = centroid_list %>% 
              rename(longitude = X, latitude = Y), 
            aes(x=longitude,y=latitude, colour = I("red"), label = 1:46), size = 5) +
  ggthemes::theme_few()

# Dox %>% mutate(XY = paste(longitude, latitude, sep = "-")) %>%
#   filter(XY %in% medits_ALL %>% mutate(XY1 = paste(X,Y, sep = "-")) %>% distinct(XY1) %$% XY1)
# 
# 
# filter_map <- function(coord){
#   medits_ALL %>% group_by(HAUL_NUMBER) %>%
#     filter(between(longitude,coord[1],coord[2]), between(latitude,coord[3], coord[4]))
# }

# data_haul_filter <- pmap()
# # fare merge dati coordinate haul variabili
# Bio_haul_1 <- Bio_tibble_dox %>% 
#   filter(between(longitude,12.27267, 12.37250), between(latitude,41.32750, 41.38967))



