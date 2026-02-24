# =============================================================================
# Extracting habitat values for fields in SJV
# =============================================================================
# Purpose: Find the amount of high quality habitat for our species of interest 
#          (BNLL, GKR, SJKF) in each SJV field.
# =============================================================================


# clear environment 
rm(list = ls())


# Load Packages -----------------------------------------------------------


library(sf)
library(tidyverse)
library(terra)
library(tigris)
library(tmap)
library(smoothr)
library(exactextractr)
library(prioritizr)


# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_SJV/0_startup/functions.R"))



# =============================================================================
# STEP 1: Load SJV fields and baseline habitat rasters
# =============================================================================

# Read in Extract layer from 6_estimateFallowing_median.R
# SJV fields with revenue and water data, fully estimated
field_data <- read_sf(here("data/intermediate/6_estimateFallowing_median/sjvAddFallowMedian/sjvAddFallowMedian.shp"))
crs(field_data) # make sure EPSG:3310


# Read in blunt-nosed leopard lizard baseline habitat raster
bnll_baseline <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/bnll_sdm/4_maxent_predictions/maxent_bnll_pred_masked_lakes.tif"))
crs(bnll_baseline)

# Read in giant kangaroo rat habitat raster
gkr_baseline <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/gkr_sdm/4_maxent_predictions_gkr/maxent_gkr_pred_masked_lakes.tif"))
crs(gkr_baseline)

# Read in San Joaquin kit fox habitat raster
sjkf_baseline <- rast(here("data/intermediate/"))
crs(sjkf_baseline)


# =============================================================================
# STEP 2: Load future habitat rasters
# =============================================================================



# =============================================================================
# STEP 3: Crop habitat rasters to SJV counties
# =============================================================================

# load SJV boundary 
sjv_counties <- read_sf(here("data/raw/sjv_counties/sjv_counties.shp"))

# transform SJV boundary to EPSG:3310
sjv_counties <- st_transform(sjv_counties, crs = crs(bnll_baseline))

# convert to terra vector
sjv_vect <- vect(sjv_counties)


# Crop habitat rasters to SJV --------------------------------------------

# blunt-nosed leopard lizard
bnll_baseline_sjv <- bnll_baseline %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
plot(bnll_baseline_sjv)

# giant kangaroo rat
gkr_baseline_sjv <- gkr_baseline %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
plot(gkr_baseline_sjv)

# San Joaquin kit fox



# =============================================================================
# STEP 4: Keep only 'high quality' habitat in rasters
# =============================================================================

# Mask out 'poor quality' habitat for each species. 
# For each species, define high quality habitat using the 10th percentile training presence threshold

# BNLL: 0.23
# GKR: 0.27
# SJKF: ...

# BNLL -------------------------------------------------

# Isolate high quality habitat for baseline BNLL
# classify: values > 0.23 = 1 (high quality), else NA
bnll_baseline_hq <- ifel(bnll_baseline_sjv > 0.23, 1, NA)


# GKR -------------------------------------------------

# Isolate poor quality habitat for baseline GKR
gkr_baseline_hq <- ifel(gkr_baseline_sjv > 0.27, 1, NA)


# SJKF -------------------------------------------------

# add when SJKF habitat raster is ready




# =============================================================================
# STEP 5: Run habitat extractions
# =============================================================================

# calculate cell area
cell_area_m2 <- prod(res(bnll_baseline_hq))

# BNLL -------------------------------------------------

# extract sum of high quality habitat in each field
bnll_base_hq_areas <- exact_extract(x = bnll_baseline_hq, 
                                    y = field_data, 
                                    fun = 'sum')
# convert to acres and bind to field_data
field_data$bnll_base <- (bnll_base_hq_areas * cell_area_m2) / 4047





# Export ------------------------------------------------------------------

write_sf(habitatExtract, here("data/intermediate/7_foxExtraction/kernExtractions/kernExtractions.shp"))

















