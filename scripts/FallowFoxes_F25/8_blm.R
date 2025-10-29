# clear environment
rm(list = ls())

library(tidyverse)
library(prioritizr)
#library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(fasterize)

source("Scripts/0_startup/0_2_functions.R")


# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)



# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf(here("data/intermediate/7_foxExtraction/kernExtractions/kernExtractions.shp")) %>% 
  st_make_valid()


rast_template <- rast(ext = ext(habitatExtract), 
                      resolution = 30, 
                      crs = st_crs(habitatExtract)$wkt) # wkt -> well-known text of the projection definition




# Rasterize Plots ---------------------------------------------------------


# Rasterize

fieldRast <- habitatExtract %>% 
  rasterize(rast_template, field = "id", touches = TRUE, fun = min)




# Re-Vectorize ------------------------------------------------------------


reVector <- fieldRast %>% 
  as.polygons() %>% 
  st_as_sf()



# Remove fields dropped from rasterization --------------------------------


# ID of missing fields
missing <- habitatExtract %>% dplyr::pull(id) %>% 
  setdiff(reVector %>% dplyr::pull(id))


missingPolys <- habitatExtract %>%
  filter(id %in% missing)



badList <- missingPolys %>% 
  pull(id)

habitatExtract2 <- habitatExtract %>% 
  filter(!(id %in% badList))



# Export ------------------------------------------------------------------



write_sf(habitatExtract2, here("data/intermediate/8_blm/cleanShapes/cleanShapes.shp"))

write_sf(reVector, "data/intermediate/8_blm/blmShapes/blmShapes.shp")

