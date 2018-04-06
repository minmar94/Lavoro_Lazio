# Load packages -----------------------------------------------------------
require(tidyverse)
require(ncdf4)
require(raster)
require(chron)
require(lattice)
require(RColorBrewer)
source("Functions.R")
# Data Import -------------------------------------------------------------

dirs_all <- system2("ls", args = "*nc", stdout = TRUE)
# read data in a list
Data_MyOcean <- pmap(list(dirs_all), nc_open) %>% set_names(., dirs_all)
# Data_MyOcean is a list of list: each sublist contains one .nc file (in filenames)

# BIO_BIO <- Data_MyOcean[[1]]

# get longitude and latitude
for(i in c(1,2,3,4,5,6,8)){
  if(i>4){
    print(c(length(ncvar_get(Data_MyOcean[[i]], varid = "lon")), 
            length(ncvar_get(Data_MyOcean[[i]], varid = "lat")),
            length(ncvar_get(Data_MyOcean[[i]], varid = "depth")),
            length(ncvar_get(Data_MyOcean[[i]], varid = "time"))))
  }else{
    print(c(length(ncvar_get(Data_MyOcean[[i]], varid = "longitude")), 
            length(ncvar_get(Data_MyOcean[[i]], varid = "latitude")),
            length(ncvar_get(Data_MyOcean[[i]], varid = "depth")),
            length(ncvar_get(Data_MyOcean[[i]], varid = "time"))))
  }
}
# The file PHY_006_009_SSH.nc has only three dimensions: lat, lon e time -> non ci sono dati per la depth
# names(Data_MyOcean$PHY_006_009_SSH.nc$dim)

BIO_latitudes <- map(Data_MyOcean[1:4], ncvar_get, "latitude") %>% set_names(., nm = filenames[1:4])
PHY_latitudes <- map(Data_MyOcean[5:8], ncvar_get, "lat") %>% set_names(., nm = filenames[5:8])
BIO_longitudes <- map(Data_MyOcean[1:4], ncvar_get, "longitude") %>% set_names(., nm = filenames[1:4])
PHY_longitudes <- map(Data_MyOcean[5:8], ncvar_get, "lon") %>% set_names(., nm = filenames[5:8])
BIO_depth <- map(Data_MyOcean[1:4], ncvar_get, "depth") %>% set_names(., nm = filenames[1:4])
#PHY_depth <- map(Data_MyOcean[5:8], ncvar_get, "depth") %>% set_names(., nm = filenames[5:8])
PHY_depth <- BIO_depth
BIO_time <- map(Data_MyOcean[1:4], ncvar_get, "time") %>% set_names(., nm = filenames[1:4])
PHY_time <- map(Data_MyOcean[5:8], ncvar_get, "time") %>% set_names(., nm = filenames[5:8])
# For all the other datasets, there are 61 longitude, 48 latitude coordinates and 47 depths.
# Time dimension differs between BIO and PHY data. Particularly we have data for 17 years (from Jan 1999 to Dec 2015) for BIO datasets, and data for 61 years (from Jan 1955 to Dec 2015) for PHY data
Reduce(intersect, BIO_time) %>% range() %>% as.POSIXct(., origin = "1970-01-01")
Reduce(intersect, PHY_time) %>% range() %>% as.POSIXct(., origin = "1950-01-01")
setdiff(Reduce(intersect, PHY_latitudes),Reduce(intersect, BIO_latitudes))
setdiff(Reduce(intersect, PHY_longitudes),Reduce(intersect, BIO_longitudes))

# Get code, names and measure unit for of all the variables in each dataset
variable_list <- list()
wholename <- list()
units <- list()
for(i in names(Data_MyOcean)){
  variable_list[[`i`]] <- names(Data_MyOcean[[`i`]]$var)
  for(j in 1:length(variable_list[[`i`]])){
    wholename[[`i`]][[`j`]] <- Data_MyOcean[[`i`]]$var[[`j`]]$longname
    units[[`i`]][[`j`]] <- Data_MyOcean[[`i`]]$var[[`j`]]$units
  }
}
variable_list <- bind_cols(Dataset = rep(names(variable_list),times = c(2,2,2,2,2,1,1,1)), Varcod = unlist(variable_list))
wholename <- bind_cols(Dataset = rep(names(wholename),times = c(2,2,2,2,2,1,1,1)), Varname = unlist(wholename),
                       Varcod = variable_list$Varcod, Unit = unlist(units))
save(wholename, file = "Variables.RData")
# arrays <- list()
# for(j in 1:nrow(wholename)){
#     dat <- wholename$Dataset[j]
#     varname <- wholename$Varname[j] %>% as.character()
#     arrays[[varname]] <- ncvar_get(Data_MyOcean[[dat]],as.character(wholename$Varcod[j]))
# }

dox_array <- extract_var(Data_MyOcean[[1]], "dox")
vomecrty_array <- extract_var(Data_MyOcean[[5]], "vomecrty")

# Fix Depth and Time
# Depth distribution: 1.472   55.610  176.829  284.028  442.023 1011.211 
# DOX distribution: 
# Scelgo le profondità nelle posizioni corrispondenti agli indici della distribuzione: min, q1,q2,q3,max
# par(mfrow = c(3,2), pty = "m")
# image(BIO_longitudes[[1]], BIO_latitudes[[1]],dox_array[,,1,204], col=rev(brewer.pal(10,"RdBu")),
#       xlab = "Longitude", ylab = "Latitude", main = "DOX December 2015", sub = "Min Depth")
# image(BIO_longitudes[[1]], BIO_latitudes[[1]],dox_array[,,12,204], col=rev(brewer.pal(10,"RdBu")),
#       xlab = "Longitude", ylab = "Latitude", main = "DOX December 2015", sub = "Q1 Depth")
# image(BIO_longitudes[[1]], BIO_latitudes[[1]],dox_array[,,24,204], col=rev(brewer.pal(10,"RdBu")),
#       xlab = "Longitude", ylab = "Latitude", main = "DOX December 2015", sub = "Q2 Depth")
# image(BIO_longitudes[[1]], BIO_latitudes[[1]],dox_array[,,36,204], col=rev(brewer.pal(10,"RdBu")),
#       xlab = "Longitude", ylab = "Latitude", main = "DOX December 2015", sub = "Q3 Depth")
# image(BIO_longitudes[[1]], BIO_latitudes[[1]],dox_array[,,47,204], col=rev(brewer.pal(10,"RdBu")),
#       xlab = "Longitude", ylab = "Latitude", main = "DOX December 2015", sub = "Max Depth")

# coords <- expand.grid(BIO_longitudes[[1]], BIO_latitudes[[1]])
# depth <- rep(BIO_depth[[1]], 61*48*204)
# value <- as.vector(dox_array[,,,])
# dox_array_long <- bind_cols(coords = do.call("bind_rows", replicate(47*204, coords, simplify = FALSE)),
#                             Time = rep(BIO_time[[1]], 61*48*47),
#                             Depth = depth,DOX = value)
# colnames(dox_array_long)[1:2] <- c("Lon", "Lat")
# 
# dox_array_long <- dox_array_long %>% mutate(Time = as.POSIXct(Time, origin = "1970-01-01")) #%>% as.tbl_cube(dim_names = 1:4)
# # 
# dox_array_long %>%
#   mutate(Depth = round(Depth, 3)) %>%
#   #filter(Time %in% unique(dox_array_long$Time)[204]) %>%
#   ggplot + aes(x = Lon, y = Lat, fill = DOX) + geom_raster() +
#   scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "white") + facet_wrap(~Depth) + 
#   labs(title = "DOX ~ Depth", caption = "December 2015") + ggthemes::theme_few()
# 
# #gganimate::gganimate(p)
# 
# dox_array_long <- as.tbl_cube(dox_array_long, dim_names = 1:4)
# 
# dox_array_long %>% group_by(Depth) %>% summarise(mean(DOX, na.rm = T)) %>% as.data.frame()
# dox_array_long %>% as.data.frame() %>%
#   mutate(Depth = round(Depth, 3)) %>%
#   ggplot + aes(x = factor(Depth), y = DOX) + geom_boxplot()
# devtools::install_github("hypertidy/tidync", dependencies = TRUE) -> configure: error: "unable to use udunits2 or udunits"