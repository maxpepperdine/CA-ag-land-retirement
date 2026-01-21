# =============================================================================
# Extracting habitat values for fields in SJV
# =============================================================================
# Purpose: Find the amount of high quality habitat for our species of interest 
#          (BNLL, SJKF, GKR, SJWT, TKR) in each SJV field.
# =============================================================================


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



# =============================================================================
# STEP 1: Load data
# =============================================================================

# Read in Extract layer (SJV fields with revenue and water data, fully estimated)
fieldData <- read_sf(here("data/intermediate/6_estimateFallowing_median/sjvAddFallowMedian/sjvAddFallowMedian.shp"))
crs(fieldData) # make sure EPSG:3310

# inspect data
fieldData %>% str()


# Read in blunt-nosed leopard lizard habitat raster (replace file paths later)
bnllHabitatRaw <- rast(here("data/intermediate/"))
# check CRS
crs(bnllHabitatRaw)

# Read in giant kangaroo rat habitat raster
gkrHabitatRaw <- rast(here("data/intermediate/"))
crs(gkrHabitatRaw)

# Read in San Joaquin kit fox habitat raster
sjkfHabitatRaw <- rast(here("data/intermediate/"))
crs(sjkfHabitatRaw)

# Read in San Joaquin woolly threads habitat raster
sjwtHabitatRaw <- rast(here("data/intermediate/"))
crs(sjwtHabitatRaw)

# Read in Tipton kangaroo rat habitat raster
tkrHabitatRaw <- rast(here("data/intermediate/"))
crs(tkrHabitatRaw)



# =============================================================================
# STEP 2: Crop habitat rasters to SJV counties
# =============================================================================


# Create SJV polygon -----------------------------------------------

# get California counties from Census TIGER/Line shapefiles
ca_counties <- counties(state = "CA")

# define San Joaquin Valley counties
sjv_counties <- c("San Joaquin", "Stanislaus", "Merced", "Madera",
                  "Fresno", "Kings", "Tulare", "Kern")

# filter and combine into one polygon
sjv_vect <- ca_counties %>%
  filter(NAME %in% sjv_counties) %>%
  st_union() %>%
  st_transform(crs = crs(fieldData)) %>% # transform to EPSG:3310
  vect() # convert to terra vector format



# Crop habitat rasters to SJV --------------------------------------------

# blunt-nosed leopard lizard
bnllHabitat <- crop(foxHabitatRaw, sjv_vect)
plot(bnllHabitat)

# giant kangaroo rat
gkrHabitat <- crop(foxHabitatRaw, sjv_vect)
plot(gkrHabitat)

# San Joaquin kit fox
sjkfHabitat <- crop(foxHabitatRaw, sjv_vect)
plot(sjkfHabitat)

# San Joaquin woolly threads
sjwtHabitat <- crop(foxHabitatRaw, sjv_vect)
plot(sjwtHabitat)

# Tipton kangaroo rat
tkrHabitat <- crop(foxHabitatRaw, sjv_vect)
plot(tkrHabitat)


# =============================================================================
# STEP 3: Mask poor and NA habitat (define our 'high quality' habitat)
# =============================================================================

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

















