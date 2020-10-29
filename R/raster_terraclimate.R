library(ncdf4)
library(raster)

# water_evaporation_amount
# https://climate.northwestknowledge.net/TERRACLIMATE/index_directDownloads.php

a = raster("TerraClimate_aet_1958.nc", band = 10)
x11();plot(a)
valores <- values(a)
length(valores)

library(sf)

library(tidyverse)

col <- readRDS("D:/DocumentosEdimer/Github/Spatial-Data-Science/SIG_R/gadm36_COL_0_sp.rds")


class(col)


data_col <- st_as_sf(col)
class(data_col)

x11()
data_col %>% 
  ggplot() + 
  geom_sf()

plot(a, colNA = "black")

solo_colombia <- a %>% 
  crop(data_col) %>% 
  mask(data_col)

plot(solo_colombia, colNA = "black")


