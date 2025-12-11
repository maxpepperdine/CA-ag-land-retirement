# clear environment 
rm(list = ls())


# Load Packages -----------------------------------------------------------


library(sf)
library(tidyverse)
library(terra)
library(tigris)
library(tmap)
library(smoothr)
library(exactextractr)
library(prioritizr)

options(tigris_use_cache = TRUE)


# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)


# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_F25/0_startup/functions.R"))


# Read In Revenue and Water data ------------------------------------------

#Read in Extract layer
fieldData <- read_sf(here("data/intermediate/6_estimateFallowingE/kernAddFallow/kernAddFallow.shp"))



#fieldData %>% str()

#Read in fox habitat raster
foxHabitatRaw <- rast(here("data/raw/SJKF_pred_habitat/ds2599.tif"))
# check CRS
crs(foxHabitatRaw)



# Crop Kit Fox to Kern County ---------------------------------------------

#Load in Kern Polygon
CA <- counties(state = "CA", year = 2021, refresh = TRUE)

kern <- CA %>% 
  filter(NAMELSAD == "Kern County") %>% 
  st_transform(crs(foxHabitatRaw))

#Convert from sf object to SpatVector
kernVect <- vect(kern)



#Crop kit fox raster
foxHabitat <- crop(foxHabitatRaw, kernVect)
plot(foxHabitat)



# Mask Poor and NA habitat ------------------------------------------------



#Isolate 128 values in raster for mask
foxNull <- foxHabitat > 100
plot(foxNull)


#Isolate poor quality habitat for mask
foxPoor <- foxHabitat < 34


#Mask no value values
foxNoValMask <- mask(foxHabitat, foxNull, maskvalues = 1)



#Mask poor quality habitat
foxPoorMask <- mask(foxNoValMask, foxPoor, maskvalues = 1)



#Turn logical
foxMask <- foxPoorMask > 1



# Convert Fox habitat to area ---------------------------------------------

#Probably don't need to do this

foxAreaRaw <- cellSize(foxMask)

foxArea <- foxAreaRaw * foxMask


# Run Habitat Extractions -------------------------------------------------


# Get extraction as single column tibble
habitat <- exact_extract(x = foxMask, 
                         y = fieldData, 
                         fun = 'weighted_sum', 
                         weights = cellSize(foxArea)
)

# Cbind to fieldData table and convert m2 to acres
habitatExtract <- fieldData %>% 
  cbind(habitat) %>% 
  mutate(
    habitat = habitat / (4047)
  )


# Export ------------------------------------------------------------------

write_sf(habitatExtract, here("data/intermediate/7_foxExtraction/kernExtractions/kernExtractions.shp"))

