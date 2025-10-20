
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


# #Visualization
# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)





# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_F25/0_startup/functions.R"))


# Load Kern ---------------------------------------------------------------

kern <- read_sf(here("data/intermediate/3_cleanPlots/kernID/kernID.shp")) %>%
  select(geo_grp) %>% 
  # remove duplicate geo_grps
  filter(duplicated(geo_grp) == FALSE)

# Get FID data ------------------------------------------------------------

kern2 <- read_sf(here("data/intermediate/4_cropRotationE/kernYearRotation/kernYearRotation.shp")) |> 
  select(geometry, comm_cd) |>
  st_transform(crs = 3310) |>         # Transform to a projected CRS in meters (CA Albers)
  mutate(area_m2 = st_area(geometry)) |> 
  mutate(FID = row_number()) |>      # Assign unique field ID
  relocate(FID)|>                    
  rename(commcodeLastActive = comm_cd)


# Find Nearest Neighbors -------------------------------------------------- subset first ****

# Step 1: Subset the data for quick testing (e.g. 100 random fields)
set.seed(42)  # for reproducibility
kern2_subset <- kern2 |> 
  slice_sample(n = 100)

# Step 2: Find 9 nearest neighbors (including self), then remove self
nearest_neighbors <- st_nn(kern2_subset, kern2_subset, 
                           k = 9,           # 8 + 1 (self)
                           returnDist = FALSE,
                           progress = TRUE)

# Step 3: Remove self (assumes self is always first in the list)
nearest_neighbors_clean <- lapply(nearest_neighbors, function(x) x[-1])





# Step 1: Build data frame of FID and neighbor indices
neighbor_df <- tibble(
  focal_row = 1:length(nearest_neighbors_clean),
  neighbor_rows = nearest_neighbors_clean
) |>
  unnest(neighbor_rows) |>
  mutate(
    focal_FID = kern2_subset$FID[focal_row],
    neighbor_FID = kern2_subset$FID[neighbor_rows],
    neighbor_commcode = kern2_subset$commcodeLastActive[neighbor_rows]
  ) |>
  select(focal_FID, neighbor_FID, neighbor_commcode)


# Find the most common neighbor_commcode for each focal_FID
most_common_neighbor <- neighbor_df |>
  group_by(focal_FID, neighbor_commcode) |>
  summarise(count = n(), .groups = "drop") |>
  arrange(focal_FID, desc(count)) |>
  group_by(focal_FID) |>
  slice(1) |>
  ungroup()












# Get FID data ------------------------------------------------------------




# Plots Iris Created with geometry
irisShp <- read_sf(here("data/intermediate/0_input/Iris_w_Fid/testProjn.shp")) %>%
  st_transform(crs(kern))


# Ashley Processed Iris Plots with last crop cultivated Data
splitFieldsRaw <- read_csv(here("data/intermediate/0_input/KernFieldsThruTime_David.csv"))

# Subset to 2021
splitFields <- splitFieldsRaw %>%
  filter(year == 2021)

#SJV Shapefile
sjv <- read_sf(here("data/intermediate/0_input/pp1766_cvhm_texture_regions/cvhm_texture_regions.shp")) %>%
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

write_sf(irisFields, here("data/intermediate/5_geoGroupFIDKey/irisFieldsYear/irisFieldsYear.shp"))





# Read in COMM-code crosswalk ---------------------------------------------



revenue <- read_csv(here("data/intermediate/2_masterCrosswalkE/masterCrosswalkE.csv"))



commWalk <- read_csv(here("data/intermediate/0_input/comm_codes.csv"))




# Create geoGroup : iris Comm key for fallowed plots  --------------------

# Load in annual rotated plots 

annual <- read_csv(here("data/intermediate/4_cropRotationE/annualKey_e.csv"))

# Spatial Join Kern Geogroups to iris plots to get geoGroup:fidIris key
# Then join to revenue to determine most valuable crop on plots
testRaw <- kern %>% 
  st_join(irisFields) %>% 
  left_join(revenue, by = c("commcodeLastActive" = "comm_code")) 

# Join with annual rotated data and update values where applicable
# Organize each geoGroup by most valuable crop and drop the rest of the FID's

# NOTE: changing COMM to comm to match with each df
# NOTE: changing pricePerAcre.y to price_per_acre.y (and waterUse.y to water_use.y)
# NOTE: changing geoGroup to geo_grp
# these are all likely a product of using janitor::clean_names() at some point

test <- testRaw %>% 
  left_join(annual, by = c("comm")) %>% #view()
  mutate(
    rev = ifelse(is.na(price_per_acre.y),
                 price_per_acre.x,
                 price_per_acre.y),
    water = ifelse(is.na(water_use.y),
                   water_use.x,
                   water_use.y)
    
  ) %>%
  group_by(geo_grp) %>% 
  arrange(geo_grp, desc(rev)) %>% 
  filter(duplicated(geo_grp) == FALSE)


# view all variable names in the test df
colnames(test)


# Select fidIris, geoGroup, and COMM for export
# NOTE: now selecting fidIris, geo_grp and comm
# NOTE: go back and explore why there are so many NAs
testCSV <- test %>% 
  select(fidIris, geo_grp, comm) %>% 
  st_drop_geometry()



write_csv(testCSV, here("data/intermediate/5_geoGroupFIDKey/geoGroupFIDKey.csv"))





