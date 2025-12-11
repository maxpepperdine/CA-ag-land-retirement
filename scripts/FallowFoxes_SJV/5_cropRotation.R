# clear environment
rm(list=ls())

# Load Packages -----------------------------------------------------------

library(sf)
library(lubridate)
library(tidyverse)
library(terra)
library(spData)
library(tigris)
library(readxl)
library(here)
library(janitor)


# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_SJV/0_startup/functions.R"))


# read in the .shp just created in 4_revenueEstimation 
sjv <- read_sf(here("data/intermediate/4_revenueEstimation/sjv_landIQ_fullCrosswalk/sjv_landIQ_fullCrosswalk.shp")) %>% 
  clean_names()


# rename columns; sf and dplyr don't always like the rename() function
names(sjv)[names(sjv) == "crp_ty"] <- "comm"
names(sjv)[names(sjv) == "water_us"] <- "water_use"
names(sjv)[names(sjv) == "prc_pr"] <- "price_per_acre"


# Filter plots present at the end of the water year, arrange by the most valuable 
endDate <- sjv %>% 
  filter(dt_nctv == "2022-09-30") %>% 
  mutate(
    # Assign fallowed plots annual status for ease of processing
    annual = if_else(is.na(annual), 1, annual)
  ) %>% 
  group_by(geo_grp, annual) %>% 
  arrange(desc(price_per_acre)) %>% 
  # keep only the most valuable crop for each plot (the 1st record after arranging)
  filter(duplicated(geo_grp) == FALSE) 


# Get Most Valuable End of Year Crops -------------------------------------

# Add ACRES Column to each plot
sjv2022Group <- endDate %>% 
  mutate(
    #Fix acres data column
    acres1 = round(as.numeric(area) / 4046.86 , digits = 3), .after=acres
  ) %>% 
  st_drop_geometry()


# Create Annual Table
annual <- sjv2022Group %>% 
  select(geo_grp, everything()) %>% 
  group_by(geo_grp) %>% 
  filter(annual == 1) %>% 
  arrange(geo_grp, desc(dt_nctv)) 


# Add endCrop column
# Create separate table
endCropKey <- sjv2022Group %>% 
  select(geo_grp, everything()) %>% 
  group_by(geo_grp) %>% 
  filter(annual == 1) %>% 
  arrange(geo_grp, desc(dt_nctv)) %>% 
  filter(duplicated(geo_grp) == FALSE) %>% 
  select(geo_grp, comm) %>% 
  mutate(
    endCrop = comm,
    .keep = 'unused'
  )



# Drop Duplicate Annual Crops ---------------------------------------------


# Create double cultivated vector 
# These are crops that can be cultivated twice within one year according 
# to Kern Ag Reports

doubleCult <- sjv2022Group %>% 
  pull(comm) %>% 
  unique() %>% 
  sort() %>% 
  str_subset("Lettuce/Leafy Greens") %>% 
  c("Potatoes", "Carrots", "Broccoli")



# Drop records of more than 3 crops, and then drop any duplicated
# COMMs except for those in the double Cult list. 

dropDuplicates <- annual %>% 
  left_join(endCropKey, by = "geo_grp") %>% 
  group_by(geo_grp, endCrop, comm) %>%
  mutate(
    count = row_number()
  ) %>% 
  filter(count < 3) %>% 
  filter(duplicated(comm) == FALSE | comm %in% doubleCult)


# Summarize Annual Rotations by Plot --------------------------------------


# Find Annual Value for each plot


# Summarize to get key of average rev and water use rates by ending crops
# This summarizes revenue rates for rotated crops to get a fields total annual price/acre
joinAnnualKey <- dropDuplicates %>% 
  group_by(endCrop, geo_grp) %>% 
  # For each plot sum the per acre revenue rates
  summarize(
    sumRev = sum(price_per_acre, na.rm = TRUE),
    sumWater = sum(water_use, na.rm = TRUE),
    area = mean(area)
  ) %>%
  # Leave each plot unique rates
  mutate(
    # names are inaccurate to facilitate ease of code
    # These are not means, they are sums of different rotations
    meanRev = sumRev,
    meanWater = sumWater,
    .keep = "unused"
  )



# Join Crop rotation table and clean --------------------------------------


# Join annual rates with endDate table and update rates of annuals
cropRotationRaw <- endDate %>% 
  left_join(joinAnnualKey, by = c("geo_grp",
                                  "comm" = "endCrop"))  %>% 
  mutate(
    revPart = if_else(is.na(meanRev), price_per_acre * acres, meanRev * acres),
    waterPart = if_else(is.na(meanWater), water_use * acres, meanWater * acres),
    #add fallow marker for estimating fallowed field values
    fallow = if_else(comm %in% c("Unclassified Fallow", "Idle - Long Term", 
                                 "Idle - Short Term"), TRUE, FALSE),
  ) %>% 
  select(-c(price_per_acre, meanRev, water_use, meanWater, area.x, area.y)) %>% 
  select(uniqu_d, comm, acres, geo_grp:fallow) %>% 
  group_by(geo_grp)




# Summarize for each field ------------------------------------------------ 

# SJV Geometry with ID

sjvGeo <- sjv %>% 
  select(geo_grp) %>%
  filter(duplicated(geo_grp) == FALSE)


#Add annual and perennial revenue / water data
cropRotation <- cropRotationRaw %>% 
  st_drop_geometry() %>% 
  ungroup() %>%
  group_by(geo_grp) %>% 
  # Choose either annual or perennial (most valuable)
  arrange(geo_grp, desc(revPart)) %>% 
  filter(duplicated(geo_grp) == FALSE) %>% 
  # Clean columns and update per acre values
  mutate(
    revYear = revPart,
    waterYear = waterPart,
    acres = acres,
    revPerAcre = revYear / acres,
    waterPerAcre = waterYear / acres,
    .keep = "unused"
  ) %>%
  left_join(sjvGeo, by = "geo_grp") %>%
  st_as_sf()


# Check values across all plots
metrics(cropRotation$revPerAcre)

metrics(cropRotation$revYear)

metrics(cropRotation$waterYear)

metrics(cropRotation$waterPerAcre)


# Export
write_sf(cropRotation, 
         here("data/intermediate/5_cropRotation/sjvYearRotation/sjvYearRotation.shp"), 
         append = FALSE)




























