library(tidyverse)
library(prioritizr)
#library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(fasterize)

source("Scripts/0_startup/0_2_functions.R")


tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf("Data/7_foxExtraction/kernExtractions.shp") %>% 
  st_make_valid() %>% 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM),
    jobsB = hrs / 20.8,
    jobsQ = jobs,
    .keep = "unused"
  )



rast_template  <-  rast(ext = ext(habitatExtract), 
                        resolution = 30,
                        crs = st_crs(habitatExtract)$wkt)




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



write_sf(habitatExtract2, "./Data/8_blm/cleanShapes.shp")

write_sf(reVector, "./Data/8_blm/blmShapes.shp")

