# clear environment
rm(list = ls())

# purpose: examine comm codes from LandIQ data for SJV
# we'll use these to crosswalk with crop revenue and DWR data later 

# load packages
library(tidyverse)
library(lubridate)
library(sf)
library(here)
library(janitor)

# load the LandIQ data for 2014
SJV2014Raw <- read_sf(here("data/raw/LandIQ/i15_crop_mapping_2014_shp/i15_Crop_Mapping_2014_SHP/i15_Crop_Mapping_2014.shp"))
st_crs(SJV2014Raw) # check CRS

# SJV clean plots for 2014
SJV_clean_2014 <- SJV2014Raw %>% 
  clean_names() %>% 
  # filter to SJV counties
  filter(county %in% c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                       "Fresno", "Kings", "Tulare", "Kern")) %>% 
  select(acres, county, 
         crop2014, dwr_standa) %>% 
  # make sure CRS is correct (CA Albers EPSG:3310)
  st_transform(crs = 3310)



# create another unique ID based on plot geometry
SJV_Geo <- SJV_clean_2014 %>% 
  select(geometry) %>%
  # remove duplicate geometries
  distinct() %>%
  # create group number based on geometry
  mutate(geoGroup = row_number()) %>%
  mutate(
    area = as.vector(st_area(.))
  )

# join the geoGroup back to the main data
SJV2014 <- SJV_clean_2014 %>% 
  st_join(SJV_Geo, join = st_equals)


# load the matching sheet
matching <- read_csv(here("data/intermediate/0_input/matchingSheet.csv"))


# join to get full crop names and clean column names
SJV2014 <- SJV2014 %>%
  left_join(matching, by = c("crop2014" = "crop_type_name"), 
            keep = TRUE) %>%
  clean_names()

# check CRS -- should be EPSG 3310
st_crs(SJV2014)

# drop Z values
SJV2014_2D <- st_zm(SJV2014)


################################################################################
# we're thinking of dropping the following LandIQ crop types:
# "Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", "Greenhouse"
# first, let's see how many acres they represent in the SJV for 2014

# acres we'd be dropping
acres_to_drop <- SJV2014_2D %>%
  filter(crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                               "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                               "Miscellaneous Truck Crops", "Greenhouse")) %>%
  st_drop_geometry() %>%
  summarise(drop_acres = sum(acres, na.rm = TRUE))

# total acres in LandIQ SJV plots for 2014
total_acres <- SJV2014_2D %>%
  st_drop_geometry() %>%
  summarise(total_acres = sum(acres, na.rm = TRUE))

# calculate percentage of acres to drop
percentage_dropped <- (acres_to_drop$drop_acres / total_acres$total_acres) * 100

# we're only losing about 2.3% of total acres, so dropping these crop types seems reasonable

################################################################################

# drop the crop types from the data
SJV2014_final <- SJV2014_2D %>%
  filter(!crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                                "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                                "Miscellaneous Truck Crops", "Greenhouse")) %>% 
  # drop any observation that's less than 1 acre
  filter(acres >= 1) %>% 
  select(geo_group, acres, county, crop2014, crop_type_name, area, 
         code = code_x)


# write to shapefile
write_sf(SJV2014_final, here("data/intermediate/misc/LandIQ_processing/2014/2_cleanPlotsLandIQ_2014/SJVID_2014/SJVID_2014.shp"), 
         append = FALSE)


################################################################################


# examine all distinct crop types in the SJV for 2014 LandIQ data
# this will inform the crosswalk with crop revenue, water use, and annual/perennial data in 3_masterCrosswalk.R
distinct_crops_2014 <- SJV2014_final %>%
  st_drop_geometry() %>%
  distinct(croptyp, crop_type_name) %>%
  arrange(croptyp)
  
  























