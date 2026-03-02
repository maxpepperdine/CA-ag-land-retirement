# clear environment
rm(list = ls())

library(tidyverse)
library(prioritizr)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(fasterize)


# Read In Data ------------------------------------------------------------

# Land IQ fields with habitat area extracted to each field
habitatExtract <- read_sf(here("data/intermediate/7_foxExtraction/kernExtractions/kernExtractions.shp")) %>% 
  st_make_valid()

# create raster template based on output from 7_habitatExtraction
rast_template <- rast(ext = ext(habitatExtract), 
                      resolution = 30, 
                      crs = st_crs(habitatExtract)$wkt) # wkt -> well-known text of the projection definition




# Rasterize & re-vectorize plots ---------------------------------------------------------

# Rasterize 
fieldRast <- habitatExtract %>% 
  rasterize(rast_template, 
            field = "id", 
            touches = TRUE,  # ensure all pixels that touch the field are included in the rasterization
            fun = min)       # assign the minimum id value to each pixel (in case of overlapping fields)


# Re-Vectorize
reVector <- fieldRast %>% 
  as.polygons() %>% 
  st_as_sf()



# Remove fields dropped from rasterization --------------------------------


# some fields might be lost in rasterization due to narrowness of shape
# crop these from the collection

# ID of missing fields
missing <- habitatExtract %>% dplyr::pull(id) %>% 
  setdiff(reVector %>% dplyr::pull(id))

# subset habitatExtract to just the missing fields
missingPolys <- habitatExtract %>%
  filter(id %in% missing)

# list of missing field IDs
badList <- missingPolys %>% 
  pull(id)

# filter habitatExtract to remove missing fields
habitatExtract2 <- habitatExtract %>% 
  filter(!(id %in% badList))



# Export ------------------------------------------------------------------



write_sf(habitatExtract2, here("data/intermediate/8_blm/cleanShapes/cleanShapes.shp"))

write_sf(reVector, here("data/intermediate/8_blm/blmShapes/blmShapes.shp"))

