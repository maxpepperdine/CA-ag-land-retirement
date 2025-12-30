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

# load the LandIQ data for 2018
SJV2018Raw <- read_sf(here("data/raw/LandIQ/i15_Crop_Mapping_2018_SHP"))
st_crs(SJV2018Raw) # check CRS

# SJV clean plots for 2018
SJV_clean_2018 <- SJV2018Raw %>% 
  clean_names() %>% 
  # filter to SJV counties
  filter(county %in% c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                       "Fresno", "Kings", "Tulare", "Kern")) %>% 
  select(unique_id, acres, county, region, 
         croptyp1, adoy1, 
         croptyp2, adoy2, 
         croptyp3, adoy3, 
         croptyp4, adoy4) %>% 
  # make sure CRS is correct (CA Albers EPSG:3310)
  st_transform(crs = 3310) %>% 
  
  # ensure adoy values are numeric
  mutate(across(starts_with("adoy"), ~ suppressWarnings(as.numeric(.)))) %>%
  
  # set adoy to NA if croptype is '****'
  mutate(
    adoy1 = if_else(croptyp1 == "****", NA_real_, adoy1),
    adoy2 = if_else(croptyp2 == "****", NA_real_, adoy2),
    adoy3 = if_else(croptyp3 == "****", NA_real_, adoy3),
    adoy4 = if_else(croptyp4 == "****", NA_real_, adoy4)
  ) %>%
  
  # calculate midpoints in numeric DOY space
  mutate(
    date_active2_doy = floor((adoy1 + adoy2) / 2),
    date_active3_doy = floor((adoy2 + adoy3) / 2),
    date_active4_doy = floor((adoy3 + adoy4) / 2),
    
    # inactive DOYs (except for the last one, which will be overwritten)
    # set date inactices to 1 day before the next active date
    date_inactive1_doy = date_active2_doy - 1,
    date_inactive2_doy = date_active3_doy - 1,
    date_inactive3_doy = date_active4_doy - 1
  ) %>%
  
  # convert all DOYs to Date format
  mutate(
    adate1 = as.Date(adoy1 - 1, origin = "2018-01-01"),
    adate2 = as.Date(adoy2 - 1, origin = "2018-01-01"),
    adate3 = as.Date(adoy3 - 1, origin = "2018-01-01"),
    adate4 = as.Date(adoy4 - 1, origin = "2018-01-01"),
    
    # fixed start and end of water year
    date_active1 = as.Date("2017-10-01"),
    date_inactive4 = as.Date("2018-09-30"),
    
    # other active/inactive dates based on midpoints
    # subtracting 1 aligns numeric DOY with Date format correctly
    date_active2 = as.Date(date_active2_doy - 1, origin = "2018-01-01"),
    date_active3 = as.Date(date_active3_doy - 1, origin = "2018-01-01"),
    date_active4 = as.Date(date_active4_doy - 1, origin = "2018-01-01"),
    
    date_inactive1 = as.Date(date_inactive1_doy - 1, origin = "2018-01-01"),
    date_inactive2 = as.Date(date_inactive2_doy - 1, origin = "2018-01-01"),
    date_inactive3 = as.Date(date_inactive3_doy - 1, origin = "2018-01-01")
  ) %>%
  
  select(
    unique_id, acres, county, region,
    croptyp1, adoy1, adate1, date_active1, date_inactive1,
    croptyp2, adoy2, adate2, date_active2, date_inactive2,
    croptyp3, adoy3, adate3, date_active3, date_inactive3,
    croptyp4, adoy4, adate4, date_active4, date_inactive4
  )


# final cleaning
SJV_clean_final <- SJV_clean_2018 %>%
  # pivot longer to have one crop type per row (i.e., one crop occurrence for each field)
  pivot_longer(
    # select all columns for each crop type and their corresponding dates
    cols = matches("croptyp[1-4]|date_active[1-4]|date_inactive[1-4]"),
    names_to = c(".value", "season"),
    # split column names into crop type and season number
    names_pattern = "([a-z_]+)([1-4])"
  ) %>%
  # remove placeholder and missing crops
  filter(croptyp != "****" & !is.na(croptyp)) %>%  
  mutate(
    date_active = if_else(is.na(date_active), as.Date("2017-10-01"), date_active),
    date_inactive = if_else(is.na(date_inactive), as.Date("2018-09-30"), date_inactive),
    croptype_category = paste0("croptyp", season)
  ) %>%
  select(unique_id, acres, county, region, croptype_category, croptyp, date_active, 
         date_inactive, geometry)


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
SJV2018 <- SJV_clean_final %>% 
  st_join(SJV_Geo, join = st_equals)


# load the matching sheet
matching <- read_csv(here("data/intermediate/0_input/matchingSheet.csv"))


# join to get full crop names and clean column names
SJV2018 <- SJV2018 %>%
  left_join(matching, by = c("croptyp" = "Code")) %>%
  clean_names()

# check CRS -- should be EPSG 3310
st_crs(SJV2018)

# drop Z values
SJV2018_2D <- st_zm(SJV2018)


################################################################################
# we're thinking of dropping the following LandIQ crop types:
# "Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", "Greenhouse"
# first, let's see how many acres they represent in the SJV for 2018

# acres we'd be dropping
acres_to_drop <- SJV2018_2D %>%
  filter(crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                               "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                               "Miscellaneous Truck Crops", "Greenhouse")) %>%
  st_drop_geometry() %>%
  summarise(drop_acres = sum(acres, na.rm = TRUE))

# total acres in LandIQ SJV plots for 2018
total_acres <- SJV2018_2D %>%
  st_drop_geometry() %>%
  summarise(total_acres = sum(acres, na.rm = TRUE))

# calculate percentage of acres to drop
percentage_dropped <- (acres_to_drop$drop_acres / total_acres$total_acres) * 100

# we're only losing about 2.3% of total acres, so dropping these crop types seems reasonable

################################################################################

# drop the crop types from the data
SJV2018_final <- SJV2018_2D %>%
  filter(!crop_type_name %in% c("Eucalyptus", "Miscellaneous Deciduous", "Mixed Pasture", 
                                "Turf Farms", "Flowers, Nursery and Christmas Tree Farms", 
                                "Miscellaneous Truck Crops", "Greenhouse")) %>% 
  # drop any observation that's less than 1 acre
  filter(acres >= 1) %>% 
  select(-index)


# write to shapefile
write_sf(SJV2018_final, here("data/intermediate/misc/LandIQ_processing/2018/2_cleanPlotsLandIQ_2018/SJVID_2018/SJVID_2018.shp"), 
         append = FALSE)


################################################################################


# examine all distinct crop types in the SJV for 2018 LandIQ data
# this will inform the crosswalk with crop revenue, water use, and annual/perennial data in 3_masterCrosswalk.R
distinct_crops_2018 <- SJV2018_final %>%
  st_drop_geometry() %>%
  distinct(croptyp, crop_type_name) %>%
  arrange(croptyp)
  
  























