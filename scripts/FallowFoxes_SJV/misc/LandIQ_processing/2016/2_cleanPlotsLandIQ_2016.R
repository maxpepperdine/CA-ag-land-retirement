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

# load the LandIQ data for 2016
SJV2016Raw <- read_sf(here("data/raw/LandIQ/i15_crop_mapping_2016_shp/i15_Crop_Mapping_2016_SHP/i15_Crop_Mapping_2016.shp"))
st_crs(SJV2016Raw) # check CRS

# SJV clean plots for 2016
SJV_clean_2016 <- SJV2016Raw %>% 
  clean_names() %>% 
  # filter to SJV counties
  filter(county %in% c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                       "Fresno", "Kings", "Tulare", "Kern")) %>% 
  select(acres, county, region, 
         croptyp1, croptyp2, croptyp3, 
         crop2016) %>% 
  # make sure CRS is correct (CA Albers EPSG:3310)
  st_transform(crs = 3310)


# final cleaning
SJV_clean_final <- SJV_clean_2016 %>%
  # pivot longer to have one crop type per row (i.e., one crop occurrence for each field)
  pivot_longer(
    # select all columns for each crop type and their corresponding dates
    cols = matches("croptyp[1-3]"),
    names_to = c(".value", "season"),
    # split column names into crop type and season number
    names_pattern = "([a-z_]+)([1-3])"
  ) %>%
  # remove placeholder and missing crops
  filter(croptyp != "****" & !is.na(croptyp)) %>%  
  mutate(
    croptype_category = paste0("croptyp", season)
  ) %>%
  select(acres, county, region, croptype_category, crop2016, croptyp)


# create another unique ID based on plot geometry
SJV_Geo <- SJV_clean_final %>% 
  select(geometry) %>%
  # remove duplicate geometries
  distinct() %>%
  # create group number based on geometry
  mutate(geoGroup = row_number()) %>%
  mutate(
    area = as.vector(st_area(.))
  )

# join the geoGroup back to the main data
SJV2016 <- SJV_clean_final %>% 
  st_join(SJV_Geo, join = st_equals)


# load the matching sheet
matching <- read_csv(here("data/intermediate/0_input/matchingSheet.csv"))


# join to get full crop names and clean column names
SJV2016 <- SJV2016 %>%
  left_join(matching, by = c("croptyp" = "Code")) %>%
  clean_names()

# check CRS -- should be EPSG 3310
st_crs(SJV2016)

# drop Z values
SJV2016_2D <- st_zm(SJV2016)


################################################################################
# we're thinking of dropping the following LandIQ crop types:
# "Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", "Greenhouse"
# first, let's see how many acres they represent in the SJV for 2016

# acres we'd be dropping
acres_to_drop <- SJV2016_2D %>%
  filter(crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                               "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                               "Miscellaneous Truck Crops", "Greenhouse")) %>%
  st_drop_geometry() %>%
  summarise(drop_acres = sum(acres, na.rm = TRUE))

# total acres in LandIQ SJV plots for 2016
total_acres <- SJV2016_2D %>%
  st_drop_geometry() %>%
  summarise(total_acres = sum(acres, na.rm = TRUE))

# calculate percentage of acres to drop
percentage_dropped <- (acres_to_drop$drop_acres / total_acres$total_acres) * 100

# we're only losing about 2.3% of total acres, so dropping these crop types seems reasonable

################################################################################

# drop the crop types from the data
SJV2016_final <- SJV2016_2D %>%
  filter(!crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                                "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                                "Miscellaneous Truck Crops", "Greenhouse")) %>% 
  # drop any observation that's less than 1 acre
  filter(acres >= 1) %>% 
  select(-index)


# write to shapefile
write_sf(SJV2016_final, here("data/intermediate/misc/LandIQ_processing/2016/2_cleanPlotsLandIQ_2016/SJVID_2016/SJVID_2016.shp"), 
         append = FALSE)


################################################################################


# examine all distinct crop types in the SJV for 2016 LandIQ data
# this will inform the crosswalk with crop revenue, water use, and annual/perennial data in 3_masterCrosswalk.R
distinct_crops_2016 <- SJV2016_final %>%
  st_drop_geometry() %>%
  distinct(croptyp, crop_type_name) %>%
  arrange(croptyp)
  
  























