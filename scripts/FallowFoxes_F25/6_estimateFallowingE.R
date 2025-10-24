# clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(tmap)    # for static and interactive maps
library(lwgeom)
library(here)
library(nngeo)


#Visualization
# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)





# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_F25/0_startup/functions.R"))



# Load in Data ------------------------------------------------------------

# Kern Year Data
kern <- read_sf(here("data/intermediate/4_cropRotationE/kernYearRotation/kernYearRotation.shp"))

# ensure CRS is CA Albers (EPSG:3310)
st_crs(kern)

# Updated Annual Tables
annualRate <- read_csv(here("data/intermediate/4_cropRotationE/annualKey_e.csv"))


# # FID to GEO key
# 
# fidGeoKey <- read_csv(here("data/intermediate/5_geoGroupFIDKeyE/geoGroupFIDKey.csv"))
# 
# # Iris Fields
# fid <- read_sf(here("data/intermediate/5_geoGroupFIDKeyE/irisFieldsYear/irisFieldsYear.shp")) 
# 
# 
# fidRetired <-  fid %>% 
#   filter(YrsFllw > 3)



# Remove Slivers ----------------------------------------------------------


# Find perimeter and area of LandIQ fields
kernPA <- kern %>% 
  mutate(
    perim = sf::st_perimeter(.),
    area = st_area(.),
    ap = area / perim
  )


# Organize by descending area / perimeter
kernAP <- kernPA %>% 
  select(ap) %>% 
  st_drop_geometry() %>% 
  arrange(ap) %>% 
  mutate(
    ap = as.vector(ap)
  ) %>% 
  mutate(
    rank = row_number(),
    prop = ap / max(ap),
    round = rank / nrow(kernPA)
  ) 


# Value of bottom 2.5% 
cutoff <- kernAP %>% 
  filter(round < 0.025) %>% 
  {{max(.$ap)}}


## Filter LandIQ (kern) fields by ap value
apLandIQ <- kernPA %>% 
  filter(duplicated(geo_grp) == FALSE) %>% 
  mutate(
    ap = as.vector(ap)
  )

# remove slivers from LandIQ fields (fields with ap less than cutoff)
LandIQFilter <- apLandIQ %>% 
  filter(ap > cutoff)

# See how much land is lost due to filtering
LandIQLost <- apLandIQ %>% 
  filter(ap <= cutoff)

LandIQLost %>% 
  mutate(
    area = area %>% as.numeric() / 10000
  ) %>% 
  {{sum(.$area)}}


# Split Kern into fallow not fallow ---------------------------------------

kernNoFallow <- kern %>% 
  filter(fallow == FALSE) %>% 
  mutate(
    geoGroup = geo_grp,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    # hrs = hrsYear,
    # jobsPer100Acre = jbP100A,
    # jobs = jobsYer,
    .keep = "unused"
  )

kernFallow <- kern %>% 
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
kernFArea <- sum(kernFallow$acres)
kernNFArea <- sum(kernNoFallow$acres)


# Estimate crop revenue and water use of fallowed land ------------------------


# get the 8 nearest neighbors to each fallow plot
knnFallow <- nngeo::st_nn(kernFallow, kernNoFallow, k = 8)

# extract values from neighbors
nn_values <- lapply(knnFallow, function(idx) kernNoFallow[idx,])

fallow_est_knn <- do.call(
  rbind,
  lapply(seq_along(kernFallow$geometry), function(i) {
    tibble(
      geoGroup = kernFallow$geoGroup[i],
      est_pricePerAcre = mean(nn_values[[i]]$pricePerAcre, na.rm = TRUE),
      est_waterUse = mean(nn_values[[i]]$waterUse, na.rm = TRUE)
    )
  })
)

# assign all estimated values to fallowed lands in the allKern data frame
kernFallowEst <- kernFallow %>%
  left_join(fallow_est_knn, by = "geoGroup") %>%
  mutate(
    pricePerAcre = ifelse(pricePerAcre == 0, est_pricePerAcre, pricePerAcre),
    waterUse     = ifelse(waterUse == 0, est_waterUse, waterUse)
  ) %>% 
  mutate(
    revenue = pricePerAcre * acres,
    water = waterUse * acres
  ) %>% 
  select(-c(est_pricePerAcre, est_waterUse))

# bind back to non fallowed lands
allKern <- kernFallowEst %>%
  bind_rows(kernNoFallow) %>% 
  # order rows by ascending geoGroup
  arrange(geoGroup) %>% 
  # add a new row id
  mutate(
    id = row_number()
  )

# next steps:
#### figure out what to do with "Idle ? Short Term" crop types
#### add a binary identifier to difference fallowed and retired land


# Summary statistics for fallowed lands ------------------------------------

fallowSummary <- kernFallowEst %>%
  st_drop_geometry() %>%
  summarise(
    total_acres = sum(acres),
    total_revenue = sum(revenue),
    total_water = sum(water),
    avg_pricePerAcre = mean(pricePerAcre),
    avg_waterUse = mean(waterUse)
  )
print(fallowSummary)


# Join Fallowed table to non fallowed table -------------------------------


joinFields <- fallowFinal %>% 
  bind_rows(kernNoFallow) %>% 
  mutate(
    # Give new id to include non permitted fields
    id = row_number(),
    # Give land cover type to uncultivated
    retired = if_else(is.na(COMM), 1, 0),
    COMM = if_else(is.na(COMM), "UNCULTIVATED", COMM),
    nonPermit = replace_na(nonPermit, 0)
  ) %>%
  select(id, geoGroup, acres, waterUse:COMM, 
         COMM.x, revenue, water, hrs, jobs, fallow, nonPermit, retired)



joinFields2 <- fallowFinal %>% 
  bind_rows(kernNoFallow) %>% 
  mutate(
    # Give new id to include non permitted fields
    id = row_number(),
    # Give land cover type to uncultivated
    retired = if_else(YrsFllw > 3, 1, 0),
    COMM = if_else(is.na(COMM), "UNCULTIVATED", COMM),
    nonPermit = replace_na(nonPermit, 0)
  ) %>%
  select(id, geoGroup, acres, waterUse:COMM, 
         COMM.x, revenue, water, hrs, jobs, fallow, nonPermit, retired)

# Export ------------------------------------------------------------------



write_sf(joinFields2, "Data/6_estimateFallowing/kernAddFallow.shp")

