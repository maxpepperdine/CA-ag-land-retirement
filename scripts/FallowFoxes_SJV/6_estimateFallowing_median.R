# =============================================================================
# Estimating Crop Revenue and Water Use for Fallowed Fields in the SJV
# =============================================================================
# Purpose: For fields classified as idle or fallow in the SJV dataset, estimate
#          their crop revenue and water use based on the median values from all
#          idle/fallow fields with last cultivated info. If there's a last 
#          cultivated crop as identified in `1_lastCultivatedLandIQ_2022.R`, 
#          we'll use that crop's values for estimation as calculated in 
#          2_lastCultivatedRevWater.R`.
# =============================================================================


# clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(lwgeom)
library(here)
library(nngeo)



# Load in Data ------------------------------------------------------------

# sjv Year Data with last cultivated revenue and water info (from 2_lastCultivatedRevWater.R)
sjv <- read_sf(here("data/intermediate/misc/LandIQ_processing/2_lastCultivatedRevWater/sjvLastCultivatedRevWater.shp"))
# ensure CRS is CA Albers (EPSG:3310)
st_crs(sjv)


# =============================================================================
# STEP 1: Split SJV into fallow not fallow 
# =============================================================================

sjvNoFallow <- sjv %>% 
  filter(fallow == FALSE) %>% 
  mutate(
    geoGroup = geo_grp,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    .keep = "unused"
  )

sjvFallow <- sjv %>% 
  filter(fallow == TRUE) %>% 
  mutate(
    geoGroup = geo_grp,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    .keep = "unused"
  )



# calculate area of fallowed and non fallowed land
sjvFArea <- sum(sjvFallow$acres)
sjvNFArea <- sum(sjvNoFallow$acres)

# calculate proportion of fallowed land in the SJV
propFallow <- sjvFArea / (sjvFArea + sjvNFArea)
print(paste0("Proportion of fallowed land in the SJV: ", round(propFallow * 100, 2), "%"))



# =============================================================================
# STEP 2: Split SJV into (1) cultivated land AND fallow/idle land WITH last 
#         cultivated crop info (2) fallow/idle land WITHOUT last cultivated 
#         crop info
# =============================================================================

# (1) cultivated land AND fallow land WITH last cultivated crop info
sjv_crop_fallow_w_last_cult <- sjv %>%
  # keep all cultivated land and fallow land w/o NA in last_comm
  filter(
    fallow == FALSE | 
      (fallow == TRUE & 
         !is.na(last_comm))
  ) %>%
  mutate(
    geoGroup = geo_grp,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    .keep = "unused"
  )

# (2) fallow land WITHOUT last cultivated crop info
sjv_fallow_no_last_cult <- sjv %>%
  filter(
    fallow == TRUE & 
      is.na(last_comm)
  ) %>%
  mutate(
    geoGroup = geo_grp,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    .keep = "unused"
  )



# =============================================================================
# STEP 3: Estimate crop revenue and water use of fallowed land w/ no last
#         cultivated info using median value from fields w/ last cultivated info
# =============================================================================

# find the median pricePerAcre from fallowed lands with last cultivated info
median_pricePerAcre <- median(
  sjv_crop_fallow_w_last_cult %>%
    st_drop_geometry() %>%
    filter(fallow == TRUE) %>%
    pull(pricePerAcre),
  na.rm = TRUE
)

# find the median waterUse (acre-ft/acre) from fallowed lands with last cultivated info
median_waterUse <- median(
  sjv_crop_fallow_w_last_cult %>%
    st_drop_geometry() %>%
    filter(fallow == TRUE) %>%
    pull(waterUse),
  na.rm = TRUE
)


# assign median values to fallowed lands w/o last cultivated info
sjv_fallow_est <- sjv_fallow_no_last_cult %>%
  mutate(
    pricePerAcre = median_pricePerAcre,
    waterUse = median_waterUse,
    revenue = pricePerAcre * acres,
    water = waterUse * acres
  )


# bind back to non fallowed lands and fallowed lands with last cultivated info
allsjv <- sjv_fallow_est %>%
  bind_rows(sjv_crop_fallow_w_last_cult) %>%
  # order rows by ascending geoGroup
  arrange(geoGroup) %>% 
  mutate(
    # add new row id
    id = row_number(), 
    # change true/false fallow to numeric (1 = fallow, 0 = not fallow/cultivated)
    fallow = as.numeric(fallow), 
    # indicate retired fields as those fallow for 3 or more years
    retired = ifelse(comm == "Idle - Long Term", 1, 0)
  ) %>% 
  select(id, uniqu_d, geoGroup, acres, comm, last_comm, county, annual, nass, 
         crop, fallow, retired, pricePerAcre, waterUse, revenue, water)


# ==============================================================================
# STEP 4: Summary statistics and export
# ==============================================================================


# Summary statistics for fallowed lands w/o last cultivated info ---------------

fallowSummary <- sjv_fallow_est %>%
  st_drop_geometry() %>%
  summarise(
    total_acres = sum(acres),
    total_revenue = sum(revenue),
    total_water = sum(water),
    avg_pricePerAcre = mean(pricePerAcre),
    avg_waterUse = mean(waterUse)
  )
print(fallowSummary)


# Summary statistics for fallow lands w/ last cultivated -----------------------

fallow_w_last_cult_Summary <- sjv_crop_fallow_w_last_cult %>%
  st_drop_geometry() %>%
  filter(fallow == TRUE) %>%
  summarise(
    total_acres = sum(acres),
    total_revenue = sum(revenue),
    total_water = sum(water),
    avg_pricePerAcre = mean(pricePerAcre),
    avg_waterUse = mean(waterUse)
  )
print(fallow_w_last_cult_Summary)


# Summary statistics for all cultivated lands --------------------------------

cultivatedSummary <- sjvNoFallow %>%
  st_drop_geometry() %>%
  summarise(
    total_acres = sum(acres),
    total_revenue = sum(revenue),
    total_water = sum(water),
    avg_pricePerAcre = mean(pricePerAcre),
    avg_waterUse = mean(waterUse)
  )
print(cultivatedSummary)


# Export ------------------------------------------------------------------

write_sf(allsjv, here("data/intermediate/6_estimateFallowing_median/sjvAddFallowMedian/sjvAddFallowMedian.shp"), 
         append = FALSE)










