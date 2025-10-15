# clear environment
rm(list = ls())

# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(dplyr)
library(tidyr)
library(here)
library(janitor)



# Import Kern -------------------------------------------------------------

# Import Kern Data from shapefiles for 2015. We are filtering out livestock
# land (pasture and range), clipping to a USGS San Joaquin Valley shapefile
# and then deleting a few observations of fields that are inactive before
# 1/1/15. 

# Kern Plots for 2021
#CV2021Raw <- read_sf(here("data", "Redownload_2021"))
CV2021Raw <- read_sf(here("data/raw/LandIQ/i15_Crop_Mapping_2021_SHP")) 
# Per conversation had with ashley 8/11 filter these out **
# filter(COMM != "PASTURELAND" & COMM != "RANGELAND")

kern_clean_2021 <- CV2021Raw |> 
  clean_names() |> 
  filter(county == "Kern") |> 
  select(acres, county, region,
         croptyp1, adoy1,
         croptyp2, adoy2,
         croptyp3, adoy3,
         croptyp4, adoy4) |> 
  st_transform(crs = 3310) |> 
  
  # Ensure adoy values are numeric
  mutate(across(starts_with("adoy"), ~ suppressWarnings(as.numeric(.)))) |>
  
  # Set adoy to NA if croptype is '****'
  mutate(
    adoy1 = if_else(croptyp1 == "****", NA_real_, adoy1),
    adoy2 = if_else(croptyp2 == "****", NA_real_, adoy2),
    adoy3 = if_else(croptyp3 == "****", NA_real_, adoy3),
    adoy4 = if_else(croptyp4 == "****", NA_real_, adoy4)
  ) |>
  
  # Calculate midpoints in numeric DOY space
  mutate(
    date_active2_doy = floor((adoy1 + adoy2) / 2),
    date_active3_doy = floor((adoy2 + adoy3) / 2),
    date_active4_doy = floor((adoy3 + adoy4) / 2),
    
    # Inactive DOYs (except for the last one, which will be overwritten)
    date_inactive1_doy = date_active2_doy - 1,
    date_inactive2_doy = date_active3_doy - 1,
    date_inactive3_doy = date_active4_doy - 1
  ) |>
  
  # Convert all DOYs to Date format
  mutate(
    adate1 = as.Date(adoy1 - 1, origin = "2021-01-01"),
    adate2 = as.Date(adoy2 - 1, origin = "2021-01-01"),
    adate3 = as.Date(adoy3 - 1, origin = "2021-01-01"),
    adate4 = as.Date(adoy4 - 1, origin = "2021-01-01"),
    
    # Fixed start and end of water year
    date_active1 = as.Date("2020-10-01"),
    date_inactive4 = as.Date("2021-09-30"),
    
    # Other active/inactive dates based on midpoints
    date_active2 = as.Date(date_active2_doy - 1, origin = "2021-01-01"),
    date_active3 = as.Date(date_active3_doy - 1, origin = "2021-01-01"),
    date_active4 = as.Date(date_active4_doy - 1, origin = "2021-01-01"),
    
    date_inactive1 = as.Date(date_inactive1_doy - 1, origin = "2021-01-01"),
    date_inactive2 = as.Date(date_inactive2_doy - 1, origin = "2021-01-01"),
    date_inactive3 = as.Date(date_inactive3_doy - 1, origin = "2021-01-01")
  ) |>
  
  select(
    acres, county, region,
    croptyp1, adoy1, adate1, date_active1, date_inactive1,
    croptyp2, adoy2, adate2, date_active2, date_inactive2,
    croptyp3, adoy3, adate3, date_active3, date_inactive3,
    croptyp4, adoy4, adate4, date_active4, date_inactive4
  )



kern_clean_final <- kern_clean_2021 |>
  pivot_longer(
    cols = matches("croptyp[1-4]|date_active[1-4]|date_inactive[1-4]"),
    names_to = c(".value", "season"),
    names_pattern = "([a-z_]+)([1-4])"
  ) |>
  filter(croptyp != "****" & !is.na(croptyp)) |>  # remove placeholder and missing crops
  mutate(
    date_active = if_else(is.na(date_active), as.Date("2020-10-01"), date_active),
    date_inactive = if_else(is.na(date_inactive), as.Date("2021-09-30"), date_inactive),
    croptype_category = paste0("croptyp", season)
  ) |>
  select(acres, county, region, croptype_category, croptyp, date_active, date_inactive, geometry)





# Create unique ID based on plot geometry
kernGeo <- kern_clean_final %>% 
  select(geometry) %>% 
  # This removes duplicate geometries
  distinct() %>% 
  # Create group number based on geometry
  mutate(geoGroup = row_number()) %>% 
  mutate(
    area = as.vector(st_area(.))
  )

kern2021 <- kern_clean_final %>% 
  st_join(kernGeo, join = st_equals) 


matching <- read_csv(here("data/intermediate/0_input/matchingSheet.csv")) 

kern2021 <- kern2021 %>% 
  left_join(matching, by = c("croptyp" = "Code")) |> 
  clean_names()


st_crs(kern2021)

# Drop Z values
kern2021_2D <- st_zm(kern2021)

# Write to shapefile
write_sf(kern2021_2D, here("data/intermediate/3_cleanPlots/kernID/kernID.shp"))


############################# TEST the map files to see what was deleted ##########################################
# Load required libraries
library(sf)
library(ggplot2)

# Read the shapefile
kern_shp <- read_sf(here("data/intermediate/kernID/kernID.shp"))

# Basic plot: geometry only
ggplot(data = kern_clean_final) +
  geom_sf() +
  labs(title = "Kern County Crops - 2021") +
  theme_minimal()

ggplot(data = kern_clean_2021) +
  geom_sf() +
  labs(title = "Kern County Crops - 2021") +
  theme_minimal() +
  geom_sf(data = kern_shp, aes(color = "blue"))


