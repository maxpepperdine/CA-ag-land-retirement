
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tigris)
library(readxl)


#Visualization
tmap_mode("view")
tmap_options(check.and.fix = TRUE)





# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")


# Load Kern ---------------------------------------------------------------

kern <- read_sf("Data/3_cleanPlots/kernID.shp") %>%
  select(geoGroup) %>% 
  filter(duplicated(geoGroup) == FALSE)



# Get FID data ------------------------------------------------------------




# Plots Iris Created with geometry
irisShp <- read_sf("Data/0_input/Iris_w_Fid/testProjn.shp") %>% 
  st_transform(crs(kern))


# Ashley Processed Iris Plots with last crop cultivated Data
splitFieldsRaw <- read_csv("Data/0_input/KernFieldsThruTime_David.csv")

# Subset to 2015
splitFields <- splitFieldsRaw %>%
  filter(year == 2015)

#SJV Shapefile
sjv <- read_sf("Data/0_input/pp1766_cvhm_texture_regions/cvhm_texture_regions.shp") %>% 
  st_transform(crs(irisShp))



# Test to see same if FID's match

# Fid's from shapefiles
fid <- irisShp %>% pull(fid) %>% unique() %>% length()
# FID's from ashley
fid2 <- splitFields %>% pull(fidIris) %>% unique() %>% length()
# No difference
setdiff(fid, fid2)


# Keep fid For join
iris <- irisShp %>%
  select(fid) %>% 
  st_filter(y = sjv, .predicate = st_intersects)


# Join shapefiles to Ashley data
irisFields <- iris %>%
  left_join(splitFields, by = c("fid" = "fidIris")) %>% 
  mutate(fidIris = fid, .keep = "unused") %>% 
  # Filter out pasture and rangeland
  filter(commcodeLastActive != 28045, commcodeLastActive != 28035)

  
pastRange <- iris %>%
  left_join(splitFields, by = c("fid" = "fidIris")) %>% 
  mutate(fidIris = fid, .keep = "unused") %>% 
  filter(commcodeLastActive == 28045 | commcodeLastActive == 28035)  %>% 
  mutate(
    aream2 = st_area(.) %>% as.numeric() / 10000
  )


sum(pastRange$aream2)


# Write out shapefile

write_sf(irisFields, "./Data/5_geoGroupFIDKey/irisFieldsYear.shp")
   




# Read in COMM-code crosswalk ---------------------------------------------



revenue <- read_csv("Data/2_masterCrosswalk/masterCrosswalk.csv")



commWalk <- read_csv("Data/0_input/comm_codes.csv")




# Create geoGroup : iris Comm key for fallowed plots  --------------------

# Load in annual rotated plots 

annual <- read_csv("Data/4_cropRotation/annualKey.csv")

# Spatial Join Kern Geogroups to iris plots to get geoGroup:fidIris key
# Then join to revenue to determine most valuable crop on plots
testRaw <- kern %>% 
  st_join(irisFields) %>% 
  left_join(revenue, by = c("commcodeLastActive" = "comm_code")) 

# Join with annual rotated data and update values where applicable
# Organize each geoGroup by most valuable crop and drop the rest of the FID's
test <- testRaw %>% 
  left_join(annual, by = c("COMM")) %>% #view()
  mutate(
    rev = ifelse(is.na(pricePerAcre.y),
                 pricePerAcre.x,
                 pricePerAcre.y),
    water = ifelse(is.na(waterUse.y),
                   waterUse.x,
                   waterUse.y)
    
  ) %>%
  group_by(geoGroup) %>% 
  arrange(geoGroup, desc(rev)) %>% 
  filter(duplicated(geoGroup) == FALSE)

# Select fidIris, geoGroup, and COMM for export
testCSV <- test %>% 
  select(fidIris, geoGroup, COMM) %>% 
  st_drop_geometry()



write_csv(testCSV, "Data/5_geoGroupFIDKey/geoGroupFIDKey.csv")





