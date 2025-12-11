

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


tmap_mode("view")
tmap_options(check.and.fix = TRUE)


# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")


# Read In Revenue and Water data ------------------------------------------

#Read in Extract layer
fieldData <- read_sf("Data/6_estimateFallowing/kernAddFallow.shp")



#fieldData %>% str()

#Read in fox habitat raster
foxHabitatRaw <- rast("Data/0_input/ds2599.tif")



# Crop Kit Fox to Kern County ---------------------------------------------

#Load in Kern Polygon
CA <- counties(state = "CA", year = 2015, refresh = TRUE)

kern <- CA %>% 
  filter(NAMELSAD == "Kern County") %>% 
  st_transform(crs(foxHabitatRaw))

#Convert from sf object to SpatVector
kernVect <- vect(kern)

plot(kernVect)

#Crop kit fox raster
foxHabitat <- crop(foxHabitatRaw, kernVect)


plot(foxHabitat)

#Get summary statistics
summary(foxHabitat)


#There are values of 128 for some reason, I believe these are no data values
#But need to mask from the raster
hist(foxHabitat < 100)




# Mask Poor and NA habitat ------------------------------------------------



#Isolate 128 values in raster for mask
foxNull <- foxHabitat > 100

plot(foxNull)

#Isolate poor quality habitat for mask
foxPoor <- foxHabitat < 34

plot(foxPoor)


#Mask no value values
foxNoValMask <- mask(foxHabitat, foxNull, maskvalues = 1)
plot(foxNoValMask)

#Mask poor quality habitat
foxPoorMask <- mask(foxNoValMask, foxPoor, maskvalues = 1)
plot(foxPoorMask)

#Turn logical
foxMask <- foxPoorMask > 1
plot(foxMask)






# Convert Fox habitat to area ---------------------------------------------

#Probably don't need to do this

foxAreaRaw <- cellSize(foxMask)

foxArea <- foxAreaRaw * foxMask

hist(foxArea)

plot(foxArea)

summary(foxArea)



# Run Habitat Extractions -------------------------------------------------


#Get extraction as single column tibble
habitat <- exact_extract(x = foxMask, 
                         y = fieldData, 
                         fun = 'weighted_sum', 
                         weights = cellSize(foxArea)
)

#Cbind to fieldData table and convert m2 to acres
habitatExtract <- fieldData %>% 
  cbind(habitat) %>% 
  mutate(
    habitat = habitat / (4047)
  )


# habitatExtract %>%
#   st_drop_geometry() %>% 
#   distinct(COMM) %>% 
#   view()

# Export ------------------------------------------------------------------

st_write(habitatExtract, "Data/7_foxExtraction/kernExtractions.shp", delete_layer = TRUE)


# Appendix ----------------------------------------------------------------


# 
# # Get GSA and Water District ----------------------------------------------
# 
# ppic <- read_sf("Data/0_input/pp1766_cvhm_texture_regions/cvhm_texture_regions.shp") %>% 
#   st_transform(st_crs(habitatExtract))
# 
# gsa <- read_sf("./ExclusiveGsaMasterSet/ExclusiveGSA_Master.shp") %>% 
#   st_transform(st_crs(habitatExtract))
# 
# 
# 
# test <- habitatExtract %>% st_join(ppic) %>% st_join(gsa)
# 
# 
# test %>% 
#   # filter(duplicated(geoGroup) == TRUE) %>% 
#   group_by(geoGroup) %>% 
#   mutate(
#     count = n()
#   ) %>% 
#   filter(count > 1) %>% 
#   view()
# 
# 
# 
# # Human Footprint ---------------------------------------------------------
# 
# hf <- rast("./Human_footprint_maps/hfp2013_merisINT.tif")
# 
# minmax(hf)
# 
# kernHF <- habitatExtract %>% 
#   st_transform(crs(hf))
# 
# hfTest <- exact_extract(x = hf, 
#                         y = kernHF, 
#                         'mean', 
#                         weights = cellSize(hf)
# )
# 
# metrics(hfTest)
# 
# hfClean <- kernHF %>% 
#   cbind(hfTest)
# 
# 
# # Environment dissimilarity -----------------------------------------------
# 
# dissim <- rast("Dissimilarity_01_05_1km_uint32.tif")
# 
# minmax(dissim)
# 
# kernDissim <- habitatExtract %>% 
#   st_transform(crs(dissim))
# 
# dissimTest <- exact_extract(x = dissim,
#                             y = kernDissim,
#                             "mean", 
#                             weights = "area")
# 
# metrics(dissimTest)
