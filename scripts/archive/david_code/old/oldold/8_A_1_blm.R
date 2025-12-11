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


habitatExtract %>% 
  slice_head(prop = 0.05) %>% 
  tm_shape() + 
  tm_borders(col = "red")


# Using map shaper doesn't really make a difference
tryMapShaper <- habitatExtract %>% 
  ms_simplify(keep = .5, keep_shapes = T, snap_interval = 50)


# # Snap to grid 
# tryMapShaper <- habitatExtract %>% 
#   st_geometry() %>% 
#   st_snap_to_grid(10) %>% 
#   st_as_sf()

tryMapShaper %>% 
  st_make_valid() %>% 
  #slice_head(prop = 0.05) %>% 
  tm_shape() + 
  tm_polygons(col = "red", alpha = 0.5, border.col = "black")

habitatExtract %>% 
  filter(is.na(COMM))

habitatExtract %>% 
  arrange(COMM) %>% 
  pull(COMM) %>% 
  unique()


