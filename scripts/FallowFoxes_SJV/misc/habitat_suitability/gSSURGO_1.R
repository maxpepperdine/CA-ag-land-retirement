# =============================================================================
# gSSURGO Soil Property Extraction for San Joaquin Valley
# =============================================================================
# 
# Purpose: Extract and process gSSURGO data to generate 270m resolution rasters
#          for three soil properties across the San Joaquin Valley:
#               (1) Clay content (%)
#               (2) pH (1:1 H₂O method)
#               (3) Electrical conductivity (dS/m)
#
# Methodology:
#   - Spatial unit: gSSURGO map unit raster (10m native resolution)
#   - Component selection: Dominant component per map unit (highest comppct_r)
#   - Depth interval: 0–10 cm depth-weighted mean (standard for surface soils)
#   - Upscaling: Block aggregation from 10m → 270m using arithmetic mean
#   - Study area: San Joaquin Valley (8-county definition)
#   - Output CRS: EPSG:3310 (California Albers)
#
# Key Assumptions (document in any publication):
#   1. Dominant component represents the map unit (discards within-unit variability)
#   2. 0–10 cm depth interval represents "surface soil" for modeling purposes
#   3. Block mean aggregation preserves spatial average at coarser resolution
#
# Data Source: USDA-NRCS gSSURGO (Gridded Soil Survey Geographic Database)
#
# =============================================================================


# clear workspace
rm(list = ls())

# load libraries
library(terra)
library(sf)
library(tidyverse)
library(tigris)
library(here)

# =============================================================================
# STEP 1: Load gSSURGO gdb and create lookup table of soil properties
# =============================================================================

# path to gSSURGO gdb
gdb_path <- "C:/Users/maxpe/Downloads/gSSURGO_CA/gSSURGO_CA.gdb"

# List available layers (uncomment to explore)
# st_layers(gdb_path)


# read the component table (map unit component -> horizon key)
component <- st_read(gdb_path, layer = "component")

# read the horizon table (component -> horizon properties key)
chorizon <- st_read(gdb_path, layer = "chorizon")


# get component with the highest percentage (comppct_r) per map unit (mukey)
dominant_comp <- component %>%
  group_by(mukey) %>%
  slice_max(comppct_r, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(mukey, cokey, compname, comppct_r)


# =============================================================================
# STEP 2: Calculate 0 - 10 cm (horizon) depth-weighted mean soil properties
# =============================================================================

# filter horizons that intersect the 0–10 cm depth interval
surface_0_10 <- chorizon %>%
  # a horizon intersects if: top < 10 AND bottom > 0
  filter(hzdept_r < 10, hzdepb_r > 0) %>%
  mutate(
    # clip horizon boundaries to 0–10 cm window
    top_clipped = pmax(hzdept_r, 0),
    bottom_clipped = pmin(hzdepb_r, 10),
    # calculate contributing thickness within the 0–10 cm window
    thickness = bottom_clipped - top_clipped
  ) %>%
  filter(thickness > 0)  # ensure positive thickness


# calculate depth-weighted mean for each property per component
surface_dw <- surface_0_10 %>%
  group_by(cokey) %>%
  summarise(
    # weighted.mean() handles NA values appropriately with na.rm = TRUE
    clay = weighted.mean(claytotal_r, thickness, na.rm = TRUE),
    ph = weighted.mean(ph1to1h2o_r, thickness, na.rm = TRUE),
    ec = weighted.mean(ec_r, thickness, na.rm = TRUE),
    .groups = "drop"
  )


# =============================================================================
# STEP 3: Create lookup table linking mukey to soil properties
# =============================================================================


# join them together
lookup <- dominant_comp %>%
  left_join(surface_dw, by = "cokey") %>%
  mutate(mukey = as.numeric(mukey)) %>%
  select(mukey, clay, ph, ec)

# make sure there are no duplicate mukeys
lookup <- lookup %>%
  distinct(mukey, .keep_all = TRUE)

# check it
head(lookup)
summary(lookup)


# =============================================================================
# STEP 4: Read gSSURGO map unit key raster and crop to San Joaquin Valley
# =============================================================================

# read the gSSURGO raster where each cell is a map unit key (mukey)
mukey_rast <- rast(gdb_path, "MURASTER_10m")

# project to CA Albers (EPSG 3310)
mukey_rast <- project(mukey_rast, "EPSG:3310", 
                      method = "near") # near to preserve mukey integers


# # save the projected raster to avoid reprojecting in future runs
# mukey_rast_file <- here("data/raw/gssurgo/processed/muraster_10m_3310.tif")
# writeRaster(mukey_rast, mukey_rast_file, overwrite = TRUE)


# load the saved raster (uncomment if using saved file)
mukey_rast_file <- here("data/raw/gssurgo/processed/muraster_10m_3310.tif")
mukey_rast <- rast(mukey_rast_file)
# check CRS
crs(mukey_rast)


# get California counties from Census TIGER/Line shapefiles
ca_counties <- counties(state = "CA")

# define San Joaquin Valley counties
sjv_counties <- c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                  "Fresno", "Kings", "Tulare", "Kern")

# filter and combine into one polygon
sjv <- ca_counties %>%
  filter(NAME %in% sjv_counties) %>%
  st_union() %>% 
  st_transform(crs = crs(mukey_rast)) %>% # transform to raster CRS
  vect() # convert to terra vector format


# crop and mask the raster to SJV
mukey_sjv <- mukey_rast %>%
  crop(sjv) %>%
  mask(sjv)


# =============================================================================
# STEP 5: Reclassify mukey raster to soil property rasters
# =============================================================================

max_mukey <- max(lookup$mukey, na.rm = TRUE)

# create empty vectors
ph_vec <- rep(NA_real_, max_mukey)
clay_vec <- rep(NA_real_, max_mukey)
ec_vec <- rep(NA_real_, max_mukey)

# fill in values at mukey positions
ph_vec[lookup$mukey] <- lookup$ph
clay_vec[lookup$mukey] <- lookup$clay
ec_vec[lookup$mukey] <- lookup$ec

# read raster values, reclassify, and write out
# using terra's app() function for memory-safe processing

ph_rast <- app(mukey_sjv, fun = function(x) ph_vec[x])
clay_rast <- app(mukey_sjv, fun = function(x) clay_vec[x])
ec_rast <- app(mukey_sjv, fun = function(x) ec_vec[x])

# rename layers
names(ph_rast) <- "pH_0_10cm"
names(clay_rast) <- "clay_pct_0_10cm"
names(ec_rast) <- "ec_dS_m_0_10cm"


# plot to check
plot(clay_rast, main = "Clay Content (%) 0-10 cm")
plot(ph_rast, main = "pH (1:1 H2O) 0-10 cm")
plot(ec_rast, main = "Electrical Conductivity (dS/m) 0-10 cm")


# save intermediate 10m rasters (optional)
clay_10m_file <- here("data/raw/gssurgo/processed/clay_pct_0_10cm_sjv_10m.tif")
ph_10m_file <- here("data/raw/gssurgo/processed/pH_0_10cm_sjv_10m.tif")
ec_10m_file <- here("data/raw/gssurgo/processed/ec_dS_m_0_10cm_sjv_10m.tif")

writeRaster(clay_rast, clay_10m_file, overwrite = TRUE)
writeRaster(ph_rast, ph_10m_file, overwrite = TRUE)
writeRaster(ec_rast, ec_10m_file, overwrite = TRUE)


# =============================================================================
# STEP 6: Aggregate from 10m to 270m resolution using mean and save
# =============================================================================

# calculate aggregation factor
fact <- 270 / res(clay_rast)[1]  # should be 27
# round to nearest integer in case of floating point issues
fact <- round(fact)


# aggregate using mean
clay_270m <- aggregate(clay_rast, 
                       fact = fact, 
                       fun = "mean", 
                       na.rm = TRUE)
ph_270m <- aggregate(ph_rast,
                     fact = fact, 
                     fun = "mean", 
                     na.rm = TRUE)
ec_270m <- aggregate(ec_rast,
                     fact = fact,
                     fun = "mean",
                     na.rm = TRUE)

# plot to check
plot(clay_270m, main = "Clay Content (%) 0-10 cm at 270m")
plot(ph_270m, main = "pH (1:1 H2O) 0-10 cm at 270m")
plot(ec_270m, main = "Electrical Conductivity (dS/m) 0-10 cm at 270m")


# write final rasters
clay_file <- here("data/raw/gssurgo/processed/clay_pct_0_10cm_sjv_270m.tif")
ph_file <- here("data/raw/gssurgo/processed/pH_0_10cm_sjv_270m.tif")
ec_file <- here("data/raw/gssurgo/processed/ec_dS_m_0_10cm_sjv_270m.tif")

writeRaster(clay_270m, clay_file, overwrite = TRUE)
writeRaster(ph_270m, ph_file, overwrite = TRUE)
writeRaster(ec_270m, ec_file, overwrite = TRUE)








