# ==============================================================================
# S_net GENERATION: MECHANISTIC ETc APPROACH
# ==============================================================================
# GOAL: Calculate baseline and future net basin savings (S_net) estimates for 
#       each field. S_net will be the water savings feature in water prioritizr
#       analyses. 
#
# FORMULA CHAIN:
#   1. IE = waterUseETc / waterUseAW             (baseline efficiency from DWR)
#   2. ETc_future = waterUseETc × (PET_future / PET_baseline)  (climate-scaled)
#   3. AW_future = ETc_future / IE               (future applied water)
#   4. NC_future = AW_future - ETc_future         (non-consumptive losses)
#   5. S_net = ETc_future + (NC_future × (1 - RF))
#
# DATA SOURCES:
#   - waterUseAW: DWR applied water per acre (AF/ac) by county/crop
#   - waterUseETc: DWR crop ET per acre (AF/ac) by county/crop
#   - PET rasters: BCM baseline (1990-2020) and 4 future scenarios (270m)
#   - SAGBI: UC Davis soil recharge suitability (shapefile)
#
# OUTPUTS:
#   - GeoPackage with S_net values per field per scenario, combined with habitat
#     extraction acreages per field from 7_habitatExtraction.R, for use in 
#     prioritizr
# ==============================================================================

# clear environment
rm(list = ls())


# load packages
library(tidyverse)
library(here)
library(terra)
library(sf)
library(nngeo)


# ==============================================================================
# SECTION 1: File paths
# ==============================================================================

# LandIQ fields (main dataset with geometry from 6_estimateFallowing_median.R)
landiq_path <- "data/intermediate/6_estimateFallowing_median/sjvAddFallowMedian/sjvAddFallowMedian.gpkg"

# BCM PET rasters (annual, mm)
# baseline: 30-yr average 1990-2020
pet_baseline_path <- "data/intermediate/misc/habitat_suitability/env_predictors/baseline/pet1991_2020_ave_CA_270m.tif"

# future scenarios: 4 combinations of RCP × time period
pet_future_paths <- list(
  RCP45_near = "data/intermediate/misc/habitat_suitability/env_predictors/future/bcm_gcm_ensemble/pet_RCP45_2020_2049_gcm_ensemble.tif",
  RCP45_mid  = "data/intermediate/misc/habitat_suitability/env_predictors/future/bcm_gcm_ensemble/pet_RCP45_2040_2069_gcm_ensemble.tif",
  RCP85_near = "data/intermediate/misc/habitat_suitability/env_predictors/future/bcm_gcm_ensemble/pet_RCP85_2020_2049_gcm_ensemble.tif",
  RCP85_mid  = "data/intermediate/misc/habitat_suitability/env_predictors/future/bcm_gcm_ensemble/pet_RCP85_2040_2069_gcm_ensemble.tif"
)

# SAGBI shapefile (modified version)
sagbi_path <- "data/raw/SAGBI/sagbi_mod/sagbi_mod.shp"

# Output directory
output_dir <- "data/intermediate/8_Snet_estimation"


# ==============================================================================
# SECTION 2: Load data
# ==============================================================================

cat("Loading LandIQ fields...\n")
fields <- st_read(landiq_path)

cat("Loading PET rasters...\n")
pet_base <- rast(pet_baseline_path)
pet_futures <- lapply(pet_future_paths, rast)

cat("Loading SAGBI...\n")
sagbi <- st_read(sagbi_path)


# ==============================================================================
# SECTION 3: Prepare SAGBI
# ==============================================================================

# reproject SAGBI to match LandIQ CRS
cat("Reprojecting SAGBI to LandIQ CRS...\n")
sagbi <- st_transform(sagbi, st_crs(fields))

# compute area-weighted average SAGBI score per LandIQ field
# this handles the case where a field overlaps multiple SAGBI polygons
cat("Computing area-weighted SAGBI scores per field...\n")

# intersect fields with SAGBI
fields_sagbi <- st_intersection(fields, sagbi %>% 
                                  select(sagbi_score = sagbi))

# calculate intersection areas
fields_sagbi$isect_area <- st_area(fields_sagbi) %>% 
  as.numeric()

# area-weighted mean SAGBI per field
sagbi_by_field <- fields_sagbi %>%
  st_drop_geometry() %>%
  group_by(id) %>% # our previous unique ID in the fields data loaded at the beginning of this script
  summarise(
    sagbi_score = weighted.mean(sagbi_score, w = isect_area, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# Impute missing SAGBI: 8-nn average
# ------------------------------------------------------------------------------

# some LandIQ fields fall outside SAGBI coverage (919 of 157,912)
# for these, assign the mean SAGBI of the 8 nearest fields that DO have scores


# identify which field IDs are missing from sagbi_by_field
fields_with_sagbi <- sagbi_by_field$id
fields_missing_sagbi <- fields %>%
  filter(!(id %in% fields_with_sagbi))

cat("  Fields missing SAGBI coverage:", nrow(fields_missing_sagbi), "\n")

if (nrow(fields_missing_sagbi) > 0) {
  # get centroids for distance calculations
  fields_has <- fields %>% filter(id %in% fields_with_sagbi)
  
  centroids_missing <- st_centroid(fields_missing_sagbi)
  centroids_has     <- st_centroid(fields_has)
  
  # for each missing field, find 8 nearest neighbors among fields WITH SAGBI
  nn_indices <- st_nn(centroids_missing, centroids_has, 
                      k = 8, progress = FALSE)
  
  # compute mean SAGBI of 8 nearest neighbors
  # match sagbi_by_field scores to fields_has in the same row order
  has_scores <- sagbi_by_field$sagbi_score[match(fields_has$id, 
                                                 sagbi_by_field$id)]
  
  imputed_sagbi <- sapply(nn_indices, function(idx) {
    mean(has_scores[idx], na.rm = TRUE)
  })
  
  # build imputed rows and bind to sagbi_by_field
  imputed_df <- data.frame(
    id = fields_missing_sagbi$id,
    sagbi_score = imputed_sagbi
  )
  
  sagbi_by_field <- bind_rows(sagbi_by_field, imputed_df)
  
  cat("  Imputed SAGBI for", nrow(imputed_df), "fields via 8-NN average\n")
  cat("  Imputed score range:",
      round(min(imputed_df$sagbi_score), 1), "-",
      round(max(imputed_df$sagbi_score), 1), "\n")
}

# --------------------------------------------------------------

# join back to fields
fields <- fields %>%
  left_join(sagbi_by_field, by = "id")

cat("  Final SAGBI summary:\n")
cat("    Fields with SAGBI:", sum(!is.na(fields$sagbi_score)), "\n")
cat("    Fields missing SAGBI:", sum(is.na(fields$sagbi_score)), "\n")
cat("    Mean SAGBI:", round(mean(fields$sagbi_score, na.rm = TRUE), 1), "\n")
cat("    SAGBI range:", round(min(fields$sagbi_score), 1), "-", round(max(fields$sagbi_score), 1), "\n")


# ==============================================================================
# SECTION 4: Extract PET values per field
# ==============================================================================

# ensure fields are in the same CRS as the PET rasters
fields_pet_crs <- st_transform(fields, crs(pet_base))

cat("Extracting baseline PET per field...\n")
fields$pet_baseline_mm <- terra::extract(pet_base, vect(fields_pet_crs),
                                         fun = mean, na.rm = TRUE)[, 2]

cat("Extracting future PET per field (4 scenarios)...\n")
for (scenario_name in names(pet_futures)) {
  col_name <- paste0("pet_", scenario_name, "_mm")
  fields[[col_name]] <- terra::extract(pet_futures[[scenario_name]],
                                       vect(fields_pet_crs),
                                       fun = mean, na.rm = TRUE)[, 2]
  cat("  ", scenario_name, "done.\n")
}

# Quick QC: check for NA PET values (fields outside raster extent)
na_pet <- sum(is.na(fields$pet_baseline_mm))
if (na_pet > 0) {
  warning(paste(na_pet, "fields have NA baseline PET — likely outside raster extent."))
}



# ==============================================================================
# SECTION 5: Compute baseline IE AND S_net
# ==============================================================================

cat("Computing baseline irrigation efficiency and S_net...\n")

# Recharge fraction function from SAGBI score (0-100)
calc_RF <- function(sagbi_score) {
  rf <- 0.05 + (sagbi_score / 100) * 0.75
  return(pmin(pmax(rf, 0.05), 0.80))
}

fields <- fields %>%
  mutate(
    # baseline irrigation efficiency: ETc / AW
    # IE < 1 means some AW is non-consumptive (typical)
    # IE > 1 means ETc > AW (deficit irrigation or precip contribution) — cap at 1.0
    IE = pmin(waterUseETc / waterUseAW, 1.0),
    
    # Recharge fraction from SAGBI
    RF = calc_RF(sagbi_score),
    
    # --- Baseline S_net ---
    # Non-consumptive = AW - ETc (floored at 0 for fields where ETc > AW)
    NC_baseline = pmax(waterUseAW - waterUseETc, 0),
    # For fields where ETc > AW, use AW as the consumptive portion
    ETc_baseline_capped = pmin(waterUseETc, waterUseAW),
    Snet_baseline_AF_ac = ETc_baseline_capped + (NC_baseline * (1 - RF)),
    Snet_baseline_AF    = Snet_baseline_AF_ac * acres
  )

cat("  IE summary:\n")
print(summary(fields$IE))
cat("  Fields where ETc > AW (IE capped at 1.0):",
    sum(fields$IE == 1.0 & fields$waterUseETc > fields$waterUseAW, na.rm = TRUE),
    "of", nrow(fields),
    paste0("(", round(100 * mean(fields$IE == 1.0 & fields$waterUseETc > fields$waterUseAW, na.rm = TRUE), 1), "%)\n"))
cat("  Baseline S_net summary (AF/ac):\n")
print(summary(fields$Snet_baseline_AF_ac))
cat("  Baseline AW summary (AF/ac):\n")
print(summary(fields$waterUseAW))

# ==============================================================================
# SECTION 6: Compute future S_net — PATH A (Mechanistic ETc)
# ==============================================================================

cat("Computing future S_net for all scenarios...\n")

scenarios <- c("RCP45_near", "RCP45_mid", "RCP85_near", "RCP85_mid")

for (scen in scenarios) {
  pet_col <- paste0("pet_", scen, "_mm")
  
  # PET ratio (climate change multiplier)
  ratio_col <- paste0("PET_ratio_", scen)
  fields[[ratio_col]] <- fields[[pet_col]] / fields$pet_baseline_mm
  
  # Future ETc = baseline ETc scaled by PET change
  etc_col <- paste0("ETc_", scen)
  fields[[etc_col]] <- fields$waterUseETc * fields[[ratio_col]]
  
  # Future AW = ETc / IE
  # This amplifies AW for inefficient irrigators (low IE = high AW per unit ETc)
  aw_col <- paste0("AW_", scen)
  fields[[aw_col]] <- fields[[etc_col]] / fields$IE
  
  # Non-consumptive water (losses)
  nc_col <- paste0("NC_", scen)
  fields[[nc_col]] <- fields[[aw_col]] - fields[[etc_col]]
  
  # S_net per acre (AF/acre)
  snet_col <- paste0("Snet_", scen, "_AF_ac")
  fields[[snet_col]] <- fields[[etc_col]] + (fields[[nc_col]] * (1 - fields$RF))
  
  # S_net total volume (AF)
  snet_total_col <- paste0("Snet_", scen, "_AF")
  fields[[snet_total_col]] <- fields[[snet_col]] * fields$acres
  
  # Delta S_net vs baseline (AF/ac)
  delta_col <- paste0("Snet_delta_", scen, "_AF_ac")
  fields[[delta_col]] <- fields[[snet_col]] - fields$Snet_baseline_AF_ac
  
  cat("  ", scen, ": mean PET ratio =",
      round(mean(fields[[ratio_col]], na.rm = TRUE), 4),
      ", mean S_net =",
      round(mean(fields[[snet_col]], na.rm = TRUE), 2), "AF/ac,",
      "mean delta =",
      round(mean(fields[[delta_col]], na.rm = TRUE), 4), "AF/ac\n")
}


# ==============================================================================
# SECTION 7: QC diagnostics
# ==============================================================================

cat("\n--- QC SUMMARY ---\n")
cat("Total fields:", nrow(fields), "\n")

# Critical check: S_net <= AW for all scenarios
cat("\nS_net <= AW check:\n")
cat("  baseline:", sum(fields$Snet_baseline_AF_ac > fields$waterUseAW + 0.001, na.rm = TRUE),
    "violations\n")
for (scen in scenarios) {
  snet_col <- paste0("Snet_", scen, "_AF_ac")
  aw_col <- paste0("AW_", scen)
  violations <- sum(fields[[snet_col]] > fields[[aw_col]] + 0.001, na.rm = TRUE)
  cat("  ", scen, ":", violations, "violations\n")
}

# PET ratio distributions
cat("\nPET ratio distributions:\n")
for (scen in scenarios) {
  ratio_col <- paste0("PET_ratio_", scen)
  cat("  ", scen, ": median =",
      round(median(fields[[ratio_col]], na.rm = TRUE), 4),
      ", range =",
      round(min(fields[[ratio_col]], na.rm = TRUE), 4), "-",
      round(max(fields[[ratio_col]], na.rm = TRUE), 4), "\n")
}

# Completeness
cat("\nFields with complete S_net:\n")
cat("  baseline:", sum(!is.na(fields$Snet_baseline_AF_ac)), "/", nrow(fields), "\n")
for (scen in scenarios) {
  snet_col <- paste0("Snet_", scen, "_AF_ac")
  cat("  ", scen, ":", sum(!is.na(fields[[snet_col]])), "/", nrow(fields), "\n")
}

# Summary by crop class
cat("\nMean S_net (AF/ac) by comm class — baseline vs RCP85_mid:\n")
fields %>%
  st_drop_geometry() %>%
  group_by(comm) %>%
  summarise(
    n_fields = n(),
    mean_AW = round(mean(waterUseAW, na.rm = TRUE), 2),
    mean_ETc = round(mean(waterUseETc, na.rm = TRUE), 2),
    mean_IE = round(mean(IE, na.rm = TRUE), 3),
    mean_Snet_baseline = round(mean(Snet_baseline_AF_ac, na.rm = TRUE), 2),
    mean_Snet_RCP85mid = round(mean(Snet_RCP85_mid_AF_ac, na.rm = TRUE), 2),
    mean_delta = round(mean(Snet_delta_RCP85_mid_AF_ac, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_Snet_RCP85mid)) %>%
  print(n = 50)


# ==============================================================================
# SECTION 8: Export for prioritizr
# ==============================================================================

cat("\nExporting for prioritizr...\n")

# prioritizr_water_final <- fields %>% 
#   select(
#     # identifiers
#     id, comm, county, acres, fallow, retired,
#     # cost column & sagbi
#     revenue, sagbi_score, 
#     # irrigation efficiency & recharge fraction (for QC/decomposition)
#     IE, RF,
#     # PET values (raw climate signal)
#     pet_baseline_mm, pet_RCP45_near_mm, pet_RCP85_near_mm,
#     # PET ratios
#     starts_with("PET_ratio_"),
#     # baseline water use components
#     waterUseAW, waterAW,           # DWR applied water (per-acre and total)
#     ETc_baseline_capped,           # baseline consumptive use
#     NC_baseline,                   # baseline non-consumptive
#     # future AW (for computing demand increase -> target adjustment)
#     AW_RCP45_near, AW_RCP85_near,
#     # future ETc (for decomposition/QC)
#     ETc_RCP45_near, ETc_RCP85_near,
#     # S_net baseline (AF)
#     Snet_baseline_AF,
#     # S_net future scenarios (AF)
#     starts_with("Snet_") & ends_with("_AF"),
#   )

# final data for water only prioritizr runs
prioritizr_water_final <- fields %>% 
  select(
    # identifiers
    id, comm, county, acres, fallow, retired,
    # cost column & sagbi
    revenue, sagbi_score, 
    # PET ratios
    starts_with("PET_ratio_"),
    # S_net baseline (AF)
    Snet_baseline_AF,
    # S_net future scenarios (AF)
    starts_with("Snet_") & ends_with("_AF"),
  )

# Write GeoPackage
output_path <- file.path(output_dir, "prioritizr_water_only/fields_Snet_estimation.gpkg")
st_write(prioritizr_water_final, output_path, delete_dsn = TRUE)
cat("Written to:", output_path, "\n")

# Also write a CSV (without geometry) for quick inspection
csv_path <- file.path(output_dir, "prioritizr_water_only/fields_Snet_estimation.csv")
prioritizr_water_final %>%
  st_drop_geometry() %>%
  write_csv(csv_path)
cat("CSV written to:", csv_path, "\n")

cat("\n=== S_net ESTIMATION COMPLETE ===\n")


# combine water prioritizr data with habitat extraction data ------------------

# load habitat extraction GPKG from 7_habitatExtraction.R
habitat_field_data <- read_sf(here("data/intermediate/7_habitatExtraction/sjvHabitatExtractions/sjvHabitatExtractions.gpkg")) %>% 
  # keep only id & habitat columns for joining
  dplyr::select(id, 19:48)


# join water data by field id
combined_field_data <- prioritizr_water_final %>% 
  dplyr::left_join(st_drop_geometry(habitat_field_data), by = "id")


# save the combined data
comb_output_path <- file.path(output_dir, "prioritizr_water_habitat/fields_water_habitat_combined.gpkg")
st_write(combined_field_data, comb_output_path, delete_dsn = TRUE)
















