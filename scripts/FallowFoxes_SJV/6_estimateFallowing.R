# =============================================================================
# Estimating Crop Revenue and Water Use for Fallowed Fields in the SJV
# =============================================================================
# Purpose: For fields classified as idle or fallow in the SJV dataset, estimate
#          their crop revenue and water use based on nearest neighbor analysis
#          from cultivated fields. If there's a last cultivated crop as identified
#          in `lastCultivatedLandIQ_2022.R`, use that crop's values for estimation.
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



# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_SJV/0_startup/functions.R"))



# Load in Data ------------------------------------------------------------

# sjv Year Data
sjv <- read_sf(here("data/intermediate/5_cropRotation/sjvYearRotation/sjvYearRotation.shp"))

# sjv last cultivated data (from lastCultivatedLandIQ_2022.R)
sjv_lastCult <- read_sf(here("data/intermediate/misc/lastCultivatedLandIQ_2022/landiq_2022_lastCultivated.shp")) %>% 
  select(uniqu_d, comm, last_comm, everything())


# ensure CRS is CA Albers (EPSG:3310)
st_crs(sjv)


# Split SJV into fallow not fallow ---------------------------------------

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


# Estimate crop revenue and water use of fallowed land ------------------------


# get the 8 nearest neighbors to each fallow plot
knnFallow <- nngeo::st_nn(sjvFallow, sjvNoFallow, k = 8)

# extract values from neighbors
nn_values <- lapply(knnFallow, function(idx) sjvNoFallow[idx,])

fallow_est_knn <- do.call(
  rbind,
  lapply(seq_along(sjvFallow$geometry), function(i) {
    tibble(
      geoGroup = sjvFallow$geoGroup[i],
      est_pricePerAcre = mean(nn_values[[i]]$pricePerAcre, na.rm = TRUE),
      est_waterUse = mean(nn_values[[i]]$waterUse, na.rm = TRUE)
    )
  })
)

## assign all estimated values to fallowed lands in the allsjv data frame
sjvFallowEst <- sjvFallow %>%
  left_join(fallow_est_knn, by = "geoGroup") %>%
  mutate(
    pricePerAcre = ifelse(pricePerAcre == 0, est_pricePerAcre, pricePerAcre),
    waterUse     = ifelse(waterUse == 0, est_waterUse, waterUse), 
    revenue = pricePerAcre * acres,
    water = waterUse * acres
  ) %>% 
  select(-c(est_pricePerAcre, est_waterUse))

# bind back to non fallowed lands
allsjv <- sjvFallowEst %>%
  bind_rows(sjvNoFallow) %>% 
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
  select(id, geoGroup, acres, comm, annual, nass, crop, 
         fallow, retired, pricePerAcre, waterUse, revenue, water)


# Summary statistics for fallowed lands ------------------------------------

fallowSummary <- sjvFallowEst %>%
  st_drop_geometry() %>%
  summarise(
    total_acres = sum(acres),
    total_revenue = sum(revenue),
    total_water = sum(water),
    avg_pricePerAcre = mean(pricePerAcre),
    avg_waterUse = mean(waterUse)
  )
print(fallowSummary)


# Export ------------------------------------------------------------------

write_sf(allsjv, here("data/intermediate/6_estimateFallowing/sjvAddFallow/sjvAddFallow.shp"))






