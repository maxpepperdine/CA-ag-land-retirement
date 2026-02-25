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
library(smoothr)
library(exactextractr)
library(here)
library(gt)


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

# BNLL ------------------------------------------------
bnll_rcp45_2049 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/bnll_sdm/4_maxent_predictions/future/maxent_bnll_pred_rcp45_2020_2049_masked_lakes.tif"))
bnll_rcp45_2069 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/bnll_sdm/4_maxent_predictions/future/maxent_bnll_pred_rcp45_2040_2069_masked_lakes.tif"))
bnll_rcp85_2049 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/bnll_sdm/4_maxent_predictions/future/maxent_bnll_pred_rcp85_2020_2049_masked_lakes.tif"))
bnll_rcp85_2069 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/bnll_sdm/4_maxent_predictions/future/maxent_bnll_pred_rcp85_2040_2069_masked_lakes.tif"))

# GKR ------------------------------------------------
gkr_rcp45_2049 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/gkr_sdm/4_maxent_predictions_gkr/future/maxent_gkr_pred_rcp45_2020_2049_masked_lakes.tif"))
gkr_rcp45_2069 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/gkr_sdm/4_maxent_predictions_gkr/future/maxent_gkr_pred_rcp45_2040_2069_masked_lakes.tif"))
gkr_rcp85_2049 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/gkr_sdm/4_maxent_predictions_gkr/future/maxent_gkr_pred_rcp85_2020_2049_masked_lakes.tif"))
gkr_rcp85_2069 <- rast(here("data/intermediate/misc/habitat_suitability/sdm_files/gkr_sdm/4_maxent_predictions_gkr/future/maxent_gkr_pred_rcp85_2040_2069_masked_lakes.tif"))


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

# BNLL
bnll_baseline_sjv <- bnll_baseline %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
plot(bnll_baseline_sjv)

# GKR
gkr_baseline_sjv <- gkr_baseline %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
plot(gkr_baseline_sjv)

# SJKF



# Crop future habitat rasters to SJV --------------------------------------------

# BNLL
bnll_rcp45_2049_sjv <- bnll_rcp45_2049 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
bnll_rcp45_2069_sjv <- bnll_rcp45_2069 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
bnll_rcp85_2049_sjv <- bnll_rcp85_2049 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
bnll_rcp85_2069_sjv <- bnll_rcp85_2069 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)

# GKR
gkr_rcp45_2049_sjv <- gkr_rcp45_2049 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
gkr_rcp45_2069_sjv <- gkr_rcp45_2069 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
gkr_rcp85_2049_sjv <- gkr_rcp85_2049 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)
gkr_rcp85_2069_sjv <- gkr_rcp85_2069 %>% 
  crop(sjv_vect) %>% 
  mask(sjv_vect)

# SJKF


# =============================================================================
# STEP 4: Keep only 'suitable' habitat in rasters
# =============================================================================

# Mask out unsuitable habitat for each species
# We'll classify suitability using the threshold maximizing sensitivity and specificity

# BNLL: 0.23
# GKR: 0.31


# BNLL -------------------------------------------------

# Isolate suitable habitat for BNLL rasters
bnll_baseline_suit <- ifel(bnll_baseline_sjv > 0.23, 1, NA)
bnll_rcp45_2049_suit <- ifel(bnll_rcp45_2049_sjv > 0.23, 1, NA)
bnll_rcp45_2069_suit <- ifel(bnll_rcp45_2069_sjv > 0.23, 1, NA)
bnll_rcp85_2049_suit <- ifel(bnll_rcp85_2049_sjv > 0.23, 1, NA)
bnll_rcp85_2069_suit <- ifel(bnll_rcp85_2069_sjv > 0.23, 1, NA)


# GKR -------------------------------------------------

# Isolate suitable habitat for GKR rasters
gkr_baseline_suit <- ifel(gkr_baseline_sjv > 0.31, 1, NA)
gkr_rcp45_2049_suit <- ifel(gkr_rcp45_2049_sjv > 0.31, 1, NA)
gkr_rcp45_2069_suit <- ifel(gkr_rcp45_2069_sjv > 0.31, 1, NA)
gkr_rcp85_2049_suit <- ifel(gkr_rcp85_2049_sjv > 0.31, 1, NA)
gkr_rcp85_2069_suit <- ifel(gkr_rcp85_2069_sjv > 0.31, 1, NA)


# SJKF -------------------------------------------------

# add when SJKF habitat raster is ready


# =============================================================================
# STEP 5: Keep only 'high quality' habitat in rasters
# =============================================================================

# Mask out 'poor quality' habitat for each species. 
# For each species, define high quality habitat as the top quintile of the continuous distribution for each species


# BNLL -------------------------------------------------

# determine the top quintile threshold for BNLL baseline habitat values
vals_bnll <- values(bnll_baseline_sjv, na.rm = TRUE)
thresh_80_bnll <- quantile(vals_bnll, 0.8, na.rm = TRUE)

# Isolate high quality habitat for BNLL rasters
bnll_baseline_hq <- ifel(bnll_baseline_sjv > thresh_80_bnll, 1, NA)
bnll_rcp45_2049_hq <- ifel(bnll_rcp45_2049_sjv > thresh_80_bnll, 1, NA)
bnll_rcp45_2069_hq <- ifel(bnll_rcp45_2069_sjv > thresh_80_bnll, 1, NA)
bnll_rcp85_2049_hq <- ifel(bnll_rcp85_2049_sjv > thresh_80_bnll, 1, NA)
bnll_rcp85_2069_hq <- ifel(bnll_rcp85_2069_sjv > thresh_80_bnll, 1, NA)


# GKR -------------------------------------------------

# determine the top quintile threshold for GKR baseline habitat values
vals_gkr <- values(gkr_baseline_sjv, na.rm = TRUE)
thresh_80_gkr <- quantile(vals_gkr, 0.8, na.rm = TRUE)

# Isolate high quality habitat for GKR rasters
gkr_baseline_hq <- ifel(gkr_baseline_sjv > thresh_80_gkr, 1, NA)
gkr_rcp45_2049_hq <- ifel(gkr_rcp45_2049_sjv > thresh_80_gkr, 1, NA)
gkr_rcp45_2069_hq <- ifel(gkr_rcp45_2069_sjv > thresh_80_gkr, 1, NA)
gkr_rcp85_2049_hq <- ifel(gkr_rcp85_2049_sjv > thresh_80_gkr, 1, NA)
gkr_rcp85_2069_hq <- ifel(gkr_rcp85_2069_sjv > thresh_80_gkr, 1, NA)


# SJKF -------------------------------------------------

# add when SJKF habitat raster is ready



# =============================================================================
# STEP 6: Run habitat extractions for suitable habitat
# =============================================================================

# calculate cell area
cell_area_m2 <- prod(res(bnll_baseline_hq))


# BNLL -------------------------------------------------

# extract sum of suitable habitat in each field for BNLL data
# then convert from m2 to acres and bind to field_data

# Baseline
bnll_base_suit_areas <- exact_extract(x = bnll_baseline_suit,
                                      y = field_data, 
                                      fun = 'sum')
field_data$bnll_base_suit <- (bnll_base_suit_areas * cell_area_m2) / 4046.86 # convert from m2 to acres (1 acre = 4046.86 m2)


# RCP45 (2020-2049)
bnll_rcp45_2049_suit_areas <- exact_extract(x = bnll_rcp45_2049_suit, 
                                            y = field_data, 
                                            fun = 'sum')
field_data$bnll_rcp45_2049_suit <- (bnll_rcp45_2049_suit_areas * cell_area_m2) / 4046.86


# RCP45 (2040-2069)
bnll_rcp45_2069_suit_areas <- exact_extract(x = bnll_rcp45_2069_suit, 
                                            y = field_data, 
                                            fun = 'sum')
field_data$bnll_rcp45_2069_suit <- (bnll_rcp45_2069_suit_areas * cell_area_m2) / 4046.86


# RCP85 (2020-2049)
bnll_rcp85_2049_suit_areas <- exact_extract(x = bnll_rcp85_2049_suit, 
                                            y = field_data, 
                                            fun = 'sum')
field_data$bnll_rcp85_2049_suit <- (bnll_rcp85_2049_suit_areas * cell_area_m2) / 4046.86


# RCP85 (2040-2069)
bnll_rcp85_2069_suit_areas <- exact_extract(x = bnll_rcp85_2069_suit, 
                                            y = field_data, 
                                            fun = 'sum')
field_data$bnll_rcp85_2069_suit <- (bnll_rcp85_2069_suit_areas * cell_area_m2) / 4046.86


# GKR -------------------------------------------------

# Baseline
gkr_base_suit_areas <- exact_extract(x = gkr_baseline_suit,
                                     y = field_data, 
                                     fun = 'sum')
field_data$gkr_base_suit <- (gkr_base_suit_areas * cell_area_m2) / 4046.86


# RCP45 (2020-2049)
gkr_rcp45_2049_suit_areas <- exact_extract(x = gkr_rcp45_2049_suit, 
                                           y = field_data, 
                                           fun = 'sum')
field_data$gkr_rcp45_2049_suit <- (gkr_rcp45_2049_suit_areas * cell_area_m2) / 4046.86


# RCP45 (2040-2069)
gkr_rcp45_2069_suit_areas <- exact_extract(x = gkr_rcp45_2069_suit, 
                                           y = field_data, 
                                           fun = 'sum')
field_data$gkr_rcp45_2069_suit <- (gkr_rcp45_2069_suit_areas * cell_area_m2) / 4046.86


# RCP85 (2020-2049)
gkr_rcp85_2049_suit_areas <- exact_extract(x = gkr_rcp85_2049_suit, 
                                           y = field_data, 
                                           fun = 'sum')
field_data$gkr_rcp85_2049_suit <- (gkr_rcp85_2049_suit_areas * cell_area_m2) / 4046.86


# RCP85 (2040-2069)
gkr_rcp85_2069_suit_areas <- exact_extract(x = gkr_rcp85_2069_suit, 
                                           y = field_data, 
                                           fun = 'sum')
field_data$gkr_rcp85_2069_suit <- (gkr_rcp85_2069_suit_areas * cell_area_m2) / 4046.86



# =============================================================================
# STEP 7: Run habitat extractions for high quality habitat rasters
# =============================================================================


# BNLL -------------------------------------------------

# extract sum of high quality habitat in each field for BNLL data
# then convert from m2 to acres and bind to field_data

# Baseline
bnll_base_hq_areas <- exact_extract(x = bnll_baseline_hq, 
                                    y = field_data, 
                                    fun = 'sum')
field_data$bnll_base_hq <- (bnll_base_hq_areas * cell_area_m2) / 4046.86 # convert from m2 to acres (1 acre = 4046.86 m2)


# RCP45 (2020-2049)
bnll_rcp45_2049_hq_areas <- exact_extract(x = bnll_rcp45_2049_hq, 
                                          y = field_data, 
                                          fun = 'sum')
field_data$bnll_rcp45_2049_hq <- (bnll_rcp45_2049_hq_areas * cell_area_m2) / 4046.86


# RCP45 (2040-2069)
bnll_rcp45_2069_hq_areas <- exact_extract(x = bnll_rcp45_2069_hq, 
                                          y = field_data, 
                                          fun = 'sum')
field_data$bnll_rcp45_2069_hq <- (bnll_rcp45_2069_hq_areas * cell_area_m2) / 4046.86


# RCP85 (2020-2049)
bnll_rcp85_2049_hq_areas <- exact_extract(x = bnll_rcp85_2049_hq, 
                                          y = field_data, 
                                          fun = 'sum')
field_data$bnll_rcp85_2049_hq <- (bnll_rcp85_2049_hq_areas * cell_area_m2) / 4046.86


# RCP85 (2040-2069)
bnll_rcp85_2069_hq_areas <- exact_extract(x = bnll_rcp85_2069_hq, 
                                          y = field_data, 
                                          fun = 'sum')
field_data$bnll_rcp85_2069_hq <- (bnll_rcp85_2069_hq_areas * cell_area_m2) / 4046.86



# GKR -------------------------------------------------

# Baseline
gkr_base_hq_areas <- exact_extract(x = gkr_baseline_hq, 
                                   y = field_data, 
                                   fun = 'sum')
field_data$gkr_base_hq <- (gkr_base_hq_areas * cell_area_m2) / 4046.86


# RCP45 (2020-2049)
gkr_rcp45_2049_hq_areas <- exact_extract(x = gkr_rcp45_2049_hq, 
                                         y = field_data, 
                                         fun = 'sum')
field_data$gkr_rcp45_2049_hq <- (gkr_rcp45_2049_hq_areas * cell_area_m2) / 4046.86


# RCP45 (2040-2069)
gkr_rcp45_2069_hq_areas <- exact_extract(x = gkr_rcp45_2069_hq, 
                                         y = field_data, 
                                         fun = 'sum')
field_data$gkr_rcp45_2069_hq <- (gkr_rcp45_2069_hq_areas * cell_area_m2) / 4046.86


# RCP85 (2020-2049)
gkr_rcp85_2049_hq_areas <- exact_extract(x = gkr_rcp85_2049_hq, 
                                         y = field_data, 
                                         fun = 'sum')
field_data$gkr_rcp85_2049_hq <- (gkr_rcp85_2049_hq_areas * cell_area_m2) / 4046.86


# RCP85 (2040-2069)
gkr_rcp85_2069_hq_areas <- exact_extract(x = gkr_rcp85_2069_hq, 
                                         y = field_data, 
                                         fun = 'sum')
field_data$gkr_rcp85_2069_hq <- (gkr_rcp85_2069_hq_areas * cell_area_m2) / 4046.86






# Export ------------------------------------------------------------------

#write_sf(habitatExtract, here("data/intermediate/7_foxExtraction/kernExtractions/kernExtractions.shp"))



# ==============================================================================
# Step 8: Summary table
# ==============================================================================

# --- Suitable Habitat Summary Table ---
summary_table_suit <- field_data %>%
  st_drop_geometry() %>%
  summarise(
    across(c(bnll_base_suit, bnll_rcp45_2049_suit, bnll_rcp45_2069_suit, bnll_rcp85_2049_suit, bnll_rcp85_2069_suit,
             gkr_base_suit,  gkr_rcp45_2049_suit,  gkr_rcp45_2069_suit,  gkr_rcp85_2049_suit,  gkr_rcp85_2069_suit),
           ~ sum(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "column", values_to = "total_acres") %>%
  mutate(
    species  = case_when(
      startsWith(column, "bnll") ~ "Blunt-nosed Leopard Lizard",
      startsWith(column, "gkr")  ~ "Giant Kangaroo Rat"
    ),
    scenario = case_when(
      endsWith(column, "base_suit")      ~ "Baseline",
      endsWith(column, "rcp45_2049_suit") ~ "RCP 4.5 (2020–2049)",
      endsWith(column, "rcp45_2069_suit") ~ "RCP 4.5 (2040–2069)",
      endsWith(column, "rcp85_2049_suit") ~ "RCP 8.5 (2020–2049)",
      endsWith(column, "rcp85_2069_suit") ~ "RCP 8.5 (2040–2069)"
    )
  ) %>%
  dplyr::select(species, scenario, total_acres) %>%
  pivot_wider(names_from = species, values_from = total_acres)

summary_table_suit %>%
  gt() %>%
  tab_header(
    title    = "Suitable Habitat on SJV Agricultural Fields",
    subtitle = "Total acres by species and climate scenario"
  ) %>%
  cols_label(
    scenario                     = "Scenario",
    `Blunt-nosed Leopard Lizard` = "Blunt-nosed Leopard Lizard (ac)",
    `Giant Kangaroo Rat`         = "Giant Kangaroo Rat (ac)"
  ) %>%
  fmt_number(
    columns  = c(`Blunt-nosed Leopard Lizard`, `Giant Kangaroo Rat`),
    decimals = 1
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )


# --- High-Quality Habitat Summary Table ---
summary_table_hq <- field_data %>%
  st_drop_geometry() %>%
  summarise(
    across(c(bnll_base_hq, bnll_rcp45_2049_hq, bnll_rcp45_2069_hq, bnll_rcp85_2049_hq, bnll_rcp85_2069_hq,
             gkr_base_hq,  gkr_rcp45_2049_hq,  gkr_rcp45_2069_hq,  gkr_rcp85_2049_hq,  gkr_rcp85_2069_hq),
           ~ sum(.x, na.rm = TRUE))
  ) %>%
  pivot_longer(everything(), names_to = "column", values_to = "total_acres") %>%
  mutate(
    species  = case_when(
      startsWith(column, "bnll") ~ "Blunt-nosed Leopard Lizard",
      startsWith(column, "gkr")  ~ "Giant Kangaroo Rat"
    ),
    scenario = case_when(
      endsWith(column, "base_hq")      ~ "Baseline",
      endsWith(column, "rcp45_2049_hq") ~ "RCP 4.5 (2020–2049)",
      endsWith(column, "rcp45_2069_hq") ~ "RCP 4.5 (2040–2069)",
      endsWith(column, "rcp85_2049_hq") ~ "RCP 8.5 (2020–2049)",
      endsWith(column, "rcp85_2069_hq") ~ "RCP 8.5 (2040–2069)"
    )
  ) %>%
  dplyr::select(species, scenario, total_acres) %>%
  pivot_wider(names_from = species, values_from = total_acres)

summary_table_hq %>%
  gt() %>%
  tab_header(
    title    = "High-Quality Habitat on SJV Agricultural Fields",
    subtitle = "Total acres by species and climate scenario"
  ) %>%
  cols_label(
    scenario                     = "Scenario",
    `Blunt-nosed Leopard Lizard` = "Blunt-nosed Leopard Lizard (ac)",
    `Giant Kangaroo Rat`         = "Giant Kangaroo Rat (ac)"
  ) %>%
  fmt_number(
    columns  = c(`Blunt-nosed Leopard Lizard`, `Giant Kangaroo Rat`),
    decimals = 1
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )



# =============================================================================
# STEP 7: Choropleth plots
# =============================================================================

# # What proportion of fields have any habitat at all?
# mean(field_data$bnll_base > 0, na.rm = TRUE)
# mean(field_data$gkr_base  > 0, na.rm = TRUE)
# 
# # Check distribution of only the non-zero fields
# quantile(field_data$bnll_base[field_data$bnll_base > 0], 
#          probs = c(0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE)
# quantile(field_data$gkr_base[field_data$gkr_base > 0],  
#          probs = c(0.2, 0.4, 0.6, 0.8, 1.0), na.rm = TRUE)
# 
# # Histogram of non-zero fields only
# par(mfrow = c(1, 2))
# hist(field_data$bnll_base[field_data$bnll_base > 0], breaks = 50, 
#      main = "BNLL Baseline (non-zero only)", xlab = "Acres")
# hist(field_data$gkr_base[field_data$gkr_base > 0],  breaks = 50, 
#      main = "GKR Baseline (non-zero only)",  xlab = "Acres")


# --- Suitable Habitat Plots ---
field_data_long_suit <- field_data %>%
  pivot_longer(
    cols      = c(bnll_base_suit, bnll_rcp45_2049_suit, bnll_rcp45_2069_suit, bnll_rcp85_2049_suit, bnll_rcp85_2069_suit,
                  gkr_base_suit,  gkr_rcp45_2049_suit,  gkr_rcp45_2069_suit,  gkr_rcp85_2049_suit,  gkr_rcp85_2069_suit),
    names_to  = "column",
    values_to = "suit_acres"
  ) %>%
  mutate(
    species  = case_when(
      startsWith(column, "bnll") ~ "Blunt-nosed Leopard Lizard",
      startsWith(column, "gkr")  ~ "Giant Kangaroo Rat"
    ),
    scenario = case_when(
      endsWith(column, "base_suit")      ~ "Baseline",
      endsWith(column, "rcp45_2049_suit") ~ "RCP 4.5 (2020–2049)",
      endsWith(column, "rcp45_2069_suit") ~ "RCP 4.5 (2040–2069)",
      endsWith(column, "rcp85_2049_suit") ~ "RCP 8.5 (2020–2049)",
      endsWith(column, "rcp85_2069_suit") ~ "RCP 8.5 (2040–2069)"
    ),
    scenario = factor(scenario, levels = c("Baseline",
                                           "RCP 4.5 (2020–2049)", "RCP 4.5 (2040–2069)",
                                           "RCP 8.5 (2020–2049)", "RCP 8.5 (2040–2069)")),
    suit_acres_binned = cut(suit_acres,
                            breaks         = c(0, 0.001, 15, 45, 150, Inf),
                            labels         = c("None", "< 15", "15–45", "45–150", "> 150"),
                            include.lowest = TRUE,
                            right          = FALSE)
  )

for (sp in c("Blunt-nosed Leopard Lizard", "Giant Kangaroo Rat")) {
  
  p <- field_data_long_suit %>%
    filter(species == sp) %>%
    ggplot() +
    geom_sf(data = sjv_counties, fill = "grey80", color = "black", linewidth = 0.4, alpha = 0.15) +
    geom_sf(aes(fill = suit_acres_binned), color = NA) +
    geom_sf(data = sjv_counties, fill = NA, color = "black", linewidth = 0.4) +
    facet_wrap(~ scenario, ncol = 3) +
    scale_fill_viridis_d(
      name     = "Acres",
      option   = "viridis",
      na.value = "grey80"
    ) +
    coord_sf(crs = 4326) +
    labs(
      title    = paste("Suitable habitat on Land IQ fields —", sp),
      caption  = "Suitable habitat defined as threshold maximizing sensitivity + specificity"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      strip.text      = element_text(face = "bold"),
      legend.position = "right",
      axis.text       = element_text(size = 7, color = "grey30"),
      axis.ticks      = element_line(color = "grey30"),
      panel.border    = element_rect(color = "grey30", fill = NA)
    )
  
  print(p)
}


# --- High-Quality Habitat Plots ---
field_data_long_hq <- field_data %>%
  pivot_longer(
    cols      = c(bnll_base_hq, bnll_rcp45_2049_hq, bnll_rcp45_2069_hq, bnll_rcp85_2049_hq, bnll_rcp85_2069_hq,
                  gkr_base_hq,  gkr_rcp45_2049_hq,  gkr_rcp45_2069_hq,  gkr_rcp85_2049_hq,  gkr_rcp85_2069_hq),
    names_to  = "column",
    values_to = "hq_acres"
  ) %>%
  mutate(
    species  = case_when(
      startsWith(column, "bnll") ~ "Blunt-nosed Leopard Lizard",
      startsWith(column, "gkr")  ~ "Giant Kangaroo Rat"
    ),
    scenario = case_when(
      endsWith(column, "base_hq")      ~ "Baseline",
      endsWith(column, "rcp45_2049_hq") ~ "RCP 4.5 (2020–2049)",
      endsWith(column, "rcp45_2069_hq") ~ "RCP 4.5 (2040–2069)",
      endsWith(column, "rcp85_2049_hq") ~ "RCP 8.5 (2020–2049)",
      endsWith(column, "rcp85_2069_hq") ~ "RCP 8.5 (2040–2069)"
    ),
    scenario = factor(scenario, levels = c("Baseline",
                                           "RCP 4.5 (2020–2049)", "RCP 4.5 (2040–2069)",
                                           "RCP 8.5 (2020–2049)", "RCP 8.5 (2040–2069)")),
    hq_acres_binned = cut(hq_acres,
                          breaks         = c(0, 0.001, 15, 45, 150, Inf),
                          labels         = c("None", "< 15", "15–45", "45–150", "> 150"),
                          include.lowest = TRUE,
                          right          = FALSE)
  )

for (sp in c("Blunt-nosed Leopard Lizard", "Giant Kangaroo Rat")) {
  
  p <- field_data_long_hq %>%
    filter(species == sp) %>%
    ggplot() +
    geom_sf(data = sjv_counties, fill = "grey80", color = "black", linewidth = 0.4, alpha = 0.15) +
    geom_sf(aes(fill = hq_acres_binned), color = NA) +
    geom_sf(data = sjv_counties, fill = NA, color = "black", linewidth = 0.4) +
    facet_wrap(~ scenario, ncol = 3) +
    scale_fill_viridis_d(
      name     = "Acres",
      option   = "viridis",
      na.value = "grey80"
    ) +
    coord_sf(crs = 4326) +
    labs(
      title    = paste("High-quality habitat on Land IQ fields —", sp),
      caption  = "High-quality habitat defined as top quintile of baseline suitability"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5),
      strip.text      = element_text(face = "bold"),
      legend.position = "right",
      axis.text       = element_text(size = 7, color = "grey30"),
      axis.ticks      = element_line(color = "grey30"),
      panel.border    = element_rect(color = "grey30", fill = NA)
    )
  
  print(p)
}
























