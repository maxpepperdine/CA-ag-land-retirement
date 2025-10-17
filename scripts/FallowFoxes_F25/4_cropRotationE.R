
# clear environment
rm(list=ls())

# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tigris)
library(readxl)
library(here)


#Visualization
# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)

# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_max/0_startup/functions.R"))


# read in the .shp just created in cleanPlots 
kern <- read_sf(here("data/intermediate/3_cleanPlots/kernID/kernID.shp")) |> 
  rename(comm = crp_ty_)


# read in MB masterCrosswalk
masterCrosswalk <- read_csv(here("data/intermediate/2_masterCrosswalkE/masterCrosswalkE.csv"))  


# Join Year and Crosswalk -------------------------------------------------

kernPrice <- kern %>% 
  left_join(masterCrosswalk, by = "comm")

################################################################################

######## quick check to see how much area is lost with misc truck crops ########

# calculate the total area of misc truck crops in kernPrice as a percentage of total area

# first, calc total area of the "acres" column in kernPrice
totalArea <- sum(kernPrice$area, na.rm = TRUE)
# next, filter for misc truck crops and calc total area of those crops
totalAreaTruck <- kernPrice %>% 
  filter(comm == "Miscellaneous Truck Crops") %>% 
  summarize(totalAreaTruck = sum(area, na.rm = TRUE)) %>% 
  pull(totalAreaTruck)

# calc percentage of total area that is misc truck crops
percentTruck <- (totalAreaTruck / totalArea) * 100


################################################################################



# Filter plots present at the end of the water year, arrange by the most valuable 
endDate <- kernPrice %>% 
  filter(dt_nctv == "2021-09-30") %>% 
  mutate(
    # Assign fallowed plots annual status for ease of processing
    annual = if_else(is.na(annual), 1, annual)
  ) %>% 
  group_by(geo_grp, annual) %>% 
  arrange(desc(price_per_acre)) %>% 
  filter(duplicated(geo_grp) == FALSE) 


# Get Most Valuable End of Year Crops -------------------------------------

# Add ACRES Column to each plot
kern2021Group <- endDate %>% 
  mutate(
    #Fix acres data column
    ACRES = round(as.numeric(area) / 4047 , digits = 2)
  ) %>% 
  st_drop_geometry()


# Create Annual Table

annual <- kern2021Group %>% 
  select(geo_grp, everything()) %>% 
  group_by(geo_grp) %>% 
  filter(annual == 1) %>% 
  arrange(geo_grp, desc(dt_nctv)) 


# Add endCrop column
# Create separate table

endCropKey <- kern2021Group %>% 
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

doubleCult <- kern2021Group %>% 
  pull(comm) %>% 
  unique() %>% 
  sort() %>% 
  str_subset("lettuce") %>% 
  c("potato", "carrot", "broccoli")



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
joinAnnualKey <- dropDuplicates %>% 
  group_by(endCrop, geo_grp) %>% 
  # For each plot sum revenue rates
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


# Get average values for all crops to estimate fallowed value


joinAnnualKeyMean <- dropDuplicates %>% 
  group_by(endCrop, geo_grp) %>% 
  # For each plot sum revenue rates
  summarize(
    sumRev = sum(price_per_acre),
    sumWater = sum(water_use),
    area = mean(area)
  ) %>%
  # For each ending crop, find area-weighted average of all plots
  summarize(
    meanRev = weighted.mean(sumRev, area),
    meanWater = weighted.mean(sumWater, area),
  )

# Update master crosswalk with rotated annual rates
revenueRotationKey <- masterCrosswalk %>% 
  left_join(joinAnnualKeyMean, by = c("comm" = "endCrop")) %>% 
  mutate(
    pricePerAcre = ifelse(is.na(meanRev), 
                          price_per_acre,
                          meanRev),
    waterUse = ifelse(is.na(meanWater),
                      water_use,
                      meanWater),
  ) %>% 
  select(-c(meanRev, meanWater))

# Export 
write_csv(revenueRotationKey, here("data/intermediate/annualKey_e.csv"))


# Join Crop rotation table and clean --------------------------------------



# Join annual rates with endDate table and update rates of annuals
cropRotationRaw <- endDate %>% 
  left_join(joinAnnualKey, by = c("geo_grp",
                                  "comm" = "endCrop"))  %>% 
  mutate(
    revPart = if_else(is.na(meanRev), price_per_acre * acres, meanRev * acres),
    waterPart = if_else(is.na(meanWater), water_use * acres, meanWater * acres),
    #add fallow marker for estimating fallowed field values
    fallow = if_else(comm == "Unclassified Fallow", TRUE, FALSE),
  ) %>% 
  select(-c(price_per_acre, meanRev, water_use, meanWater, area.x, area.y)) %>% 
  select(comm, acres, geo_grp:fallow) %>% 
  group_by(geo_grp) 



# Summarize for each field ------------------------------------------------ 

# Kern Geometry with ID

kernGeo <- kern %>% 
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
  left_join(kernGeo, by = "geo_grp") %>%
  st_as_sf()

# new_df <- cropRotation %>% select(-hrsYear, -jobsYear, hrsAcre) NA bc not in this df 


# Check values across all plots
metrics(cropRotation$revPerAcre)

metrics(cropRotation$revYear)

metrics(cropRotation$waterYear)

metrics(cropRotation$waterPerAcre)


# Export
write_sf(cropRotation, 
         here("data/intermediate/kernYearRotation/kernYearRotation.shp"))





























