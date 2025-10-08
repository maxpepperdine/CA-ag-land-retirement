library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(fasterize)

source("Scripts/0_startup/0_2_functions.R")


tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf("Data/6_foxExtraction/kernExtractions.shp") %>% 
  st_make_valid() %>% 
  # There is one outlier due to processing issues. 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM)
  )


raster_template = rast(ext = ext(habitatExtract), 
                       resolution = 15,
                       crs = st_crs(habitatExtract)$wkt)


fieldRaster <- habitatExtract %>% 
  rasterize(raster_template, field = "geoGrop")


reVector <- fieldRaster %>% 
  as.polygons() %>% 


tm_shape(reVector) + tm_fill(col = "green", alpha = 0.3) + 
  tm_shape(habitatExtract) + tm_borders(col = "red")



summary(fieldRaster)

tm_shape(fieldRaster) + tm_raster() + tm_shape(habitatExtract) + tm_borders(col = "red")


