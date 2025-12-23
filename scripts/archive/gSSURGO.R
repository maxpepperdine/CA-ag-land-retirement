# clear workspace
rm(list = ls())

library(terra)
library(sf)
library(tidyverse)
library(tigris)

# =============================================================================
# STEP 1: Load gSSURGO gdb reate lookup table of soil properties
# =============================================================================

gdb_path <- "C:/Users/maxpe/Downloads/gSSURGO_CA/gSSURGO_CA.gdb"

# see all available tables
st_layers(gdb_path)

# read the tables we need
component <- st_read(gdb_path, layer = "component")
chorizon <- st_read(gdb_path, layer = "chorizon")

# get dominant component per map unit (highest comppct_r)
dominant_comp <- component %>%
  group_by(mukey) %>%
  slice_max(comppct_r, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(mukey, cokey)

# get surface horizon (top depth = 0)
surface_hz <- chorizon %>%
  filter(hzdept_r == 0) %>%
  select(cokey, ph1to1h2o_r, claytotal_r, ec_r)

# join them together
lookup <- dominant_comp %>%
  left_join(surface_hz, by = "cokey") %>%
  mutate(mukey = as.numeric(mukey)) %>%
  select(mukey, ph1to1h2o_r, claytotal_r, ec_r)

# check it
head(lookup)
summary(lookup)

# =============================================================================
# STEP 2: Read gSSURGO map unit key raster and crop to San Joaquin Valley
# =============================================================================

# get California counties with tigris
ca_counties <- counties(state = "CA")

# define San Joaquin Valley counties
sjv_counties <- c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                  "Fresno", "Kings", "Tulare", "Kern")

# filter and combine into one polygon
sjv <- ca_counties %>%
  filter(NAME %in% sjv_counties) %>%
  st_union()
plot(sjv)


# read the gSSURGO raster where each cell is a map unit key (mukey)
mukey_rast <- rast(gdb_path, "MURASTER_10m")
# project to EPSG 3310
mukey_rast <- project(mukey_rast, "EPSG:3310", 
                      method = "near")


# project SJV to raster CRS
sjv <- st_transform(sjv, crs(mukey_rast))


# convert to terra vector format
sjv_vect <- vect(sjv)


# crop and mask the raster
mukey_sjv <- crop(mukey_rast, sjv_vect)
mukey_sjv <- mask(mukey_sjv, sjv_vect)


# =============================================================================
# STEP 3: Reclassify mukey raster to soil property rasters using lookup table
# =============================================================================


# # create a classification matrix for each property
# 
# # pH raster
# ph_rast <- subst(mukey_sjv, 
#                  from = lookup$mukey, 
#                  to = lookup$ph1to1h2o_r)
# 
# # Clay raster
# clay_rast <- subst(mukey_sjv, 
#                    from = lookup$mukey, 
#                    to = lookup$claytotal_r)
# 
# # EC raster
# ec_rast <- subst(mukey_sjv, 
#                  from = lookup$mukey, 
#                  to = lookup$ec_r)


max_mukey <- max(lookup$mukey, na.rm = TRUE)

# create empty vectors
ph_vec <- rep(NA_real_, max_mukey)
clay_vec <- rep(NA_real_, max_mukey)
ec_vec <- rep(NA_real_, max_mukey)

# fill in values at mukey positions
ph_vec[lookup$mukey] <- lookup$ph1to1h2o_r
clay_vec[lookup$mukey] <- lookup$claytotal_r
ec_vec[lookup$mukey] <- lookup$ec_r

# generate rasters using terra::app() and lookup vectors
ph_rast <- app(mukey_sjv, fun = function(x) ph_vec[x])
clay_rast <- app(mukey_sjv, fun = function(x) clay_vec[x])
ec_rast <- app(mukey_sjv, fun = function(x) ec_vec[x])

# check the rasters
plot(ph_rast, main = "Soil pH")
plot(clay_rast, main = "Soil Clay Percentage")
plot(ec_rast, main = "Soil Electrical Conductivity")


# =============================================================================
# STEP 4: Resample to 270m resolution and save outputs
# =============================================================================

# build template raster at 270m resolution
template_270m <- rast(ext(ph_rast), resolution = 270, crs = crs(ph_rast))

# resample to 270m using mean aggregation
ph_270m <- resample(ph_rast, 
                    template_270m, 
                    method = "mean")
clay_270m <- resample(clay_rast, 
                      template_270m, 
                      method = "mean")
ec_270m <- resample(ec_rast, 
                    template_270m, 
                    method = "mean")

# 9. Save
writeRaster(ph_270m, "pH_sjv_270m.tif", overwrite = TRUE)
writeRaster(clay_270m, "clay_sjv_270m.tif", overwrite = TRUE)
writeRaster(ec_270m, "ec_sjv_270m.tif", overwrite = TRUE)





