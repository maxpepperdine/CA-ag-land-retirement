
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




# Fields Iris Created with geometry
irisShp <- read_sf("Data/0_input/Iris_w_Fid/testProjn.shp") %>% 
  st_transform(crs(kern))


# Ashley Processed Iris Fields with last crop cultivated Data
splitFieldsRaw <- read_csv("Data/0_input/KernFieldsThruTime_David.csv")

#splitFieldsRaw2 <- read_csv("Data/0_input/KernFieldsThruTime_David_121922.csv")




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
  filter(commcodeLastActive != 28045, commcodeLastActive != 28035)

  

# Write out shapefile

write_sf(irisFields, "./Data/5_geoGroupFIDKey/irisFieldsYear.shp")
   





# Find fallowed fields

irisFallow <- irisFields %>%
  filter(commcodeFields == 77000)


irisFallow %>% 
  filter(is.na(commcodeLastActive)) %>% 
  mutate(area = st_area(.)) %>% 
  {{sum(.$area)}}


# 77000 fields that don't have a last comm code even in 2021
missingComm <- irisFallow %>%
  filter(is.na(commcodeLastActive)) %>%
  mutate(area = st_area(.))

missingfid <- missingComm %>% 
  pull(fidIris) %>% 
  unique()


irisFallowClean <- irisFallow %>% 
  filter(!(fidIris %in% missingfid))
  


# Read in COMM-code crosswalk ---------------------------------------------



# Spatial Join ------------------------------------------------------------


# irisMap <- irisFallow %>% 
#   tm_shape() + 
#   tm_borders()
# 
# irisMap



revenue <- read_csv("Data/2_masterCrosswalk/masterCrosswalk.csv")



# Code / Comm not lining up between tables, need to investigate -----------
# 
# apWalk <- read_csv("Data/0_input/Crops0020FamiliesImport.csv") %>% 
#   select(commodity, annual)
# 
commWalk <- read_csv("Data/0_input/comm_codes.csv")
# 
# 
# comparison <- commWalk %>% 
#   left_join(apWalk, by = c(#"comm_code" = "commcodeFill", 
#                            "comm" = "commodity"), 
#             #keep = TRUE
#             ) 
# 
# commWalkTrim <- commWalk %>% 
#   distinct(comm)
# 
# 
# 
# apWalk %>% 
#   left_join(commw)

# Create geoGroup : iris Comm key for fallowed fields  --------------------

# Load in annual rotated fields 

annual <- read_csv("Data/4_cropRotation/annualKey.csv")

#Spatial Join Kern Geogroups to iris fields to get geoGroup:fidIris key
#Then join to revenue to determine most valuable crop on field
testRaw <- kern %>% 
  st_join(irisFields) %>% 
  #select(fidIris, geoGroup, commcodeFields, commcodeLastActive) %>%
  left_join(revenue, by = c("commcodeLastActive" = "comm_code")) 

#Join with annual rotated data and update values where applicable
#Organize each geoGroup by most valuable crop and drop the rest of the FID's
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


testCSV2 <- testCSV 


write_csv(testCSV, "Data/5_geoGroupFIDKey/geoGroupFIDKey.csv")



# Investigate Crops never permitted ---------------------------------------
# 
# 
# 
# noCOMM <- test %>%
#   filter(is.na(COMM)) %>%
#   pull(fidIris) %>%
#   unique()
# 
# noCOMM1 <- test %>%
#   filter(is.na(COMM))
# 
# 
# fallowPrice <- irisFallow %>%
#   left_join(revenue, by = c("commcodeLastActive" = "comm_code"))
# 
# 
# noCOMMF <- fallowPrice %>%
#   filter(is.na(COMM)) %>%
#   pull(fidIris) %>%
#   unique()
# 
# totalNoComm <- c(noCOMMF, noCOMM) %>%
#   unique()
# 
# 
# 
# # Subset to 2021
# splitFieldsEnd <- splitFieldsRaw %>%
#   filter(year == 2021)
# 
# 
# splitFieldsRaw %>%
#   distinct(year)
# 
# splitFieldsEnd %>%
#   filter(fidIris %in% totalNoComm) %>%
#   filter(is.na(commcodeLastActive)) %>%
#   view()
# 
# 
# 
# problemCOMM <- irisFields %>%
#   filter(fidIris %in% noCOMM) %>%
#   pull(commcodeLastActive)
# 
# 
# 
# 
# # 77000 fields that don't have a last comm code even in 2021
# missingComm <- irisFallow %>%
#   filter(is.na(commcodeLastActive)) %>%
#   mutate(area = st_area(.))
# 
# 
# summary(missingComm)
# 
# 
# # Sum of area missing
# weirdArea <- missingComm %>%
#   {{sum(.$area)}}
# 
# # Kern total area
# totalArea <- kern %>%
#   mutate(area = st_area(.)) %>%
#   {{sum(.$area)}}
# 
# weirdPercent <- weirdArea / totalArea
#
# 
# 
# tm_shape(kern) +
#   tm_borders() +
#   tm_shape(missingComm) +
#   tm_fill(col = "red") +
#   tm_legend()
# 
# 
# 
# kern %>%
#   pull(geoGroup) %>%
#   unique() %>%
#   length()
# 
# testCSV %>%
#   pull(geoGroup) %>%
#   unique() %>%
#   length()
# 
# revenue %>%
#   filter(commcodeFill %in% problemCOMM)
# 
# commWalk %>%
#   filter(comm_code %in% problemCOMM)
# 
# irisFields %>%
#   pull(commcodeLastActive) %>%
#   unique()
# 
# # Join price crosswalk to test?
# 
# 
# # Test if FID's are being lost in joins / where
# 
# test %>%
#   group_by(fidIris) %>%
#   mutate(n = n()) %>%
#   filter(n >1)
# 
# irisFallow %>%
#   group_by(fidIris) %>%
#   mutate(n = n()) %>%
#   filter(n >1)
# 
# 
# irisFields %>%
#   filter(fidIris == 2) %>%
#   tm_shape() +
#   tm_polygons(col = "red")
# 
# 
# irisShp %>%
#   pull(fid) %>%
#   unique() %>%
#   length()
# 
# 
# iris %>%
#   pull(fid) %>%
#   unique() %>%
#   length()
# 
# 
# irisFallow %>%
#   pull(fidIris) %>%
#   unique() %>%
#   length()
# 
# 
# test2 %>%
#   pull(fidIris) %>%
#   unique() %>%
#   length()
# 
# 
# allFID <- irisFields %>%
#   pull(fidIris) %>%
#   unique()
# 
# 
# joinFID <- test %>%
#   pull(fidIris) %>%
#   sort() %>%
#   unique()
# 
# 
# nonJoinFID <- setdiff(allFID, joinFID)


# Find most valuable crop for non joined fields






