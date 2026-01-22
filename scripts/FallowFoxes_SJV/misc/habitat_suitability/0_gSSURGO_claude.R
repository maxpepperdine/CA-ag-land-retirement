# =============================================================================
# gSSURGO Soil Property Extraction for California
# =============================================================================
# 
# Purpose: Extract and process gSSURGO data to generate 270m resolution rasters
#          for three soil properties across California:
#               (1) Clay content (%)
#               (2) pH (1:1 H₂O method)
#               (3) Electrical conductivity (dS/m)
#
# Methodology:
#   - Spatial unit: gSSURGO map unit raster (10m native resolution)
#   - Component selection: Dominant component per map unit (highest comppct_r)
#   - Depth interval: 0–10 cm depth-weighted mean (standard for surface soils)
#   - Upscaling: Block aggregation from 10m → 270m using arithmetic mean
#   - Study area: California (or San Joaquin Valley subset)
#   - Output CRS: EPSG:3310 (California Albers)
#
# Key Assumptions (document in any publication):
#   1. Dominant component represents the map unit (discards within-unit variability)
#   2. 0–10 cm depth interval represents "surface soil" for modeling purposes
#   3. Block mean aggregation preserves spatial average at coarser resolution
#   4. Ties in dominant component selection resolved by lowest cokey (reproducible)
#   5. 270m cells require ≥50% valid 10m cells to compute mean (avoid sparse data)
#
# Data Source: USDA-NRCS gSSURGO (Gridded Soil Survey Geographic Database)
#
# Version: 2.0 (Updated with enhanced QA/QC)
# =============================================================================


# clear workspace
rm(list = ls())

# load libraries
library(terra)
library(sf)
library(tidyverse)
library(tigris)
library(here)

# create a log file for tracking issues
log_file <- here("data/raw/gssurgo/processed_test/processing_log.txt")
log_message <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))
  cat(paste0("[", Sys.time(), "] ", msg, "\n"), file = log_file, append = TRUE)
}

log_message("=== Starting gSSURGO Processing ===")

# =============================================================================
# STEP 1: Load gSSURGO gdb and create lookup table of soil properties
# =============================================================================

log_message("Step 1: Loading gSSURGO geodatabase")

# path to gSSURGO gdb
gdb_path <- here("data/raw/gssurgo/gSSURGO_CA/gSSURGO_CA.gdb")

# verify the file exists
if (!file.exists(gdb_path)) {
  stop("ERROR: gSSURGO geodatabase not found at: ", gdb_path)
}

# List available layers (uncomment to explore)
# st_layers(gdb_path)

# read the component table (map unit component -> horizon key)
component <- st_read(gdb_path, layer = "component", quiet = TRUE)
log_message(paste("Loaded", nrow(component), "components"))

# read the horizon table (component -> horizon properties key)
chorizon <- st_read(gdb_path, layer = "chorizon", quiet = TRUE)
log_message(paste("Loaded", nrow(chorizon), "horizons"))

# Check CRS of component data (if spatial)
if ("geometry" %in% names(component)) {
  log_message(paste("Component CRS:", st_crs(component)$input))
}

# =============================================================================
# STEP 1.5: Data Validation - Check for issues in raw data
# =============================================================================

log_message("Step 1.5: Validating raw data")

# Check for invalid horizon depths
invalid_horizons <- chorizon %>%
  filter(hzdept_r >= hzdepb_r | hzdept_r < 0 | is.na(hzdept_r) | is.na(hzdepb_r))

if (nrow(invalid_horizons) > 0) {
  log_message(paste("WARNING:", nrow(invalid_horizons), 
                    "horizons have invalid depth values (will be excluded)"))
}

# Check value ranges for soil properties
clay_out_range <- sum(chorizon$claytotal_r < 0 | chorizon$claytotal_r > 100, na.rm = TRUE)
ph_out_range <- sum(chorizon$ph1to1h2o_r < 0 | chorizon$ph1to1h2o_r > 14, na.rm = TRUE)
ec_out_range <- sum(chorizon$ec_r < 0, na.rm = TRUE)

if (clay_out_range > 0) {
  log_message(paste("WARNING:", clay_out_range, "clay values outside 0-100% range"))
}
if (ph_out_range > 0) {
  log_message(paste("WARNING:", ph_out_range, "pH values outside 0-14 range"))
}
if (ec_out_range > 0) {
  log_message(paste("WARNING:", ec_out_range, "EC values < 0"))
}

# =============================================================================
# STEP 2: Select dominant component per map unit
# =============================================================================

log_message("Step 2: Selecting dominant components")

# Get total number of unique map units
n_mukeys <- n_distinct(component$mukey)
log_message(paste("Total unique map units:", n_mukeys))

# Get component with the highest percentage (comppct_r) per map unit (mukey)
# Use cokey as tiebreaker for reproducibility
dominant_comp <- component %>%
  group_by(mukey) %>%
  arrange(desc(comppct_r), cokey) %>%  # Reproducible tie-breaking
  slice(1) %>%
  ungroup() %>%
  select(mukey, cokey, compname, comppct_r)

# Check for ties that were broken
ties_check <- component %>%
  group_by(mukey) %>%
  filter(comppct_r == max(comppct_r, na.rm = TRUE)) %>%
  summarise(n_tied = n(), .groups = "drop") %>%
  filter(n_tied > 1)

if (nrow(ties_check) > 0) {
  log_message(paste("INFO:", nrow(ties_check), 
                    "map units had ties in component percentage (resolved by cokey)"))
}

log_message(paste("Selected", nrow(dominant_comp), "dominant components"))

# =============================================================================
# STEP 3: Calculate 0 - 10 cm (horizon) depth-weighted mean soil properties
# =============================================================================

log_message("Step 3: Calculating depth-weighted means for 0-10 cm")

# Filter horizons that intersect the 0–10 cm depth interval
# Only include valid horizons
surface_0_10 <- chorizon %>%
  # Remove invalid horizons
  filter(!is.na(hzdept_r), !is.na(hzdepb_r), hzdept_r < hzdepb_r) %>%
  # A horizon intersects if: top < 10 AND bottom > 0
  filter(hzdept_r < 10, hzdepb_r > 0) %>%
  mutate(
    # Clip horizon boundaries to 0–10 cm window
    top_clipped = pmax(hzdept_r, 0),
    bottom_clipped = pmin(hzdepb_r, 10),
    # Calculate contributing thickness within the 0–10 cm window
    thickness = bottom_clipped - top_clipped
  ) %>%
  filter(thickness > 0)  # Ensure positive thickness

log_message(paste("Found", nrow(surface_0_10), 
                  "horizons intersecting 0-10 cm depth"))

# Identify components with surface horizon data
n_components_with_surface <- n_distinct(surface_0_10$cokey)
log_message(paste(n_components_with_surface, 
                  "components have horizons in 0-10 cm depth"))

# Calculate depth-weighted mean for each property per component
# Handle all-NA cases explicitly to return NA instead of NaN
surface_dw <- surface_0_10 %>%
  group_by(cokey) %>%
  summarise(
    # Explicit handling of all-NA cases
    clay = if_else(all(is.na(claytotal_r)), 
                   NA_real_, 
                   weighted.mean(claytotal_r, thickness, na.rm = TRUE)),
    ph = if_else(all(is.na(ph1to1h2o_r)), 
                 NA_real_, 
                 weighted.mean(ph1to1h2o_r, thickness, na.rm = TRUE)),
    ec = if_else(all(is.na(ec_r)), 
                 NA_real_, 
                 weighted.mean(ec_r, thickness, na.rm = TRUE)),
    # Track number of horizons used
    n_horizons = n(),
    # Track total thickness represented
    total_thickness = sum(thickness),
    .groups = "drop"
  )

# Check for components with incomplete 0-10 cm coverage
incomplete_coverage <- surface_dw %>%
  filter(total_thickness < 10)

if (nrow(incomplete_coverage) > 0) {
  log_message(paste("INFO:", nrow(incomplete_coverage), 
                    "components have <10 cm of horizon data in 0-10 cm interval"))
}

# Check data availability for each property
clay_available <- sum(!is.na(surface_dw$clay))
ph_available <- sum(!is.na(surface_dw$ph))
ec_available <- sum(!is.na(surface_dw$ec))

log_message(paste("Clay data available for", clay_available, "components"))
log_message(paste("pH data available for", ph_available, "components"))
log_message(paste("EC data available for", ec_available, "components"))

# =============================================================================
# STEP 4: Create lookup table linking mukey to soil properties
# =============================================================================

log_message("Step 4: Creating lookup table")

# Join dominant components with surface soil properties
lookup <- dominant_comp %>%
  left_join(surface_dw, by = "cokey") %>%
  mutate(mukey = as.numeric(mukey)) %>%
  select(mukey, clay, ph, ec, n_horizons, total_thickness)

# Identify map units without surface horizon data
mukeys_no_data <- lookup %>%
  filter(is.na(clay) & is.na(ph) & is.na(ec))

log_message(paste(nrow(mukeys_no_data), 
                  "map units have no surface horizon data (will be NA in rasters)"))

# Make sure there are no duplicate mukeys
lookup <- lookup %>%
  distinct(mukey, .keep_all = TRUE)

# Final data quality summary
log_message(paste("Final lookup table has", nrow(lookup), "map units"))
log_message(paste("  Clay: ", sum(!is.na(lookup$clay)), 
                  "values (", round(100*sum(!is.na(lookup$clay))/nrow(lookup), 1), "%)"))
log_message(paste("  pH: ", sum(!is.na(lookup$ph)), 
                  "values (", round(100*sum(!is.na(lookup$ph))/nrow(lookup), 1), "%)"))
log_message(paste("  EC: ", sum(!is.na(lookup$ec)), 
                  "values (", round(100*sum(!is.na(lookup$ec))/nrow(lookup), 1), "%)"))

# Check final value ranges
log_message("Value ranges in lookup table:")
log_message(paste("  Clay: ", round(min(lookup$clay, na.rm=TRUE), 2), "-", 
                  round(max(lookup$clay, na.rm=TRUE), 2), "%"))
log_message(paste("  pH: ", round(min(lookup$ph, na.rm=TRUE), 2), "-", 
                  round(max(lookup$ph, na.rm=TRUE), 2)))
log_message(paste("  EC: ", round(min(lookup$ec, na.rm=TRUE), 2), "-", 
                  round(max(lookup$ec, na.rm=TRUE), 2), "dS/m"))

# Save lookup table for reproducibility
lookup_file <- here("data/raw/gssurgo/processed_test/soil_property_lookup.csv")
write_csv(lookup, lookup_file)
log_message(paste("Saved lookup table to:", lookup_file))

# =============================================================================
# STEP 5: Read gSSURGO map unit key raster and crop to study area
# =============================================================================

log_message("Step 5: Loading and projecting mukey raster")

# Option A: Project from scratch (uncomment if needed)
# # read the gSSURGO raster where each cell is a map unit key (mukey)
# mukey_rast <- rast(gdb_path, "MURASTER_10m")
# log_message(paste("Original raster CRS:", crs(mukey_rast, proj=TRUE)))
# 
# # project to CA Albers (EPSG 3310)
# mukey_rast <- project(mukey_rast, "EPSG:3310", 
#                       method = "near") # near to preserve mukey integers
# log_message("Projected raster to EPSG:3310")
# 
# # save the projected raster to avoid reprojecting in future runs
# mukey_rast_file <- here("data/raw/gssurgo/processed/muraster_10m_3310.tif")
# writeRaster(mukey_rast, mukey_rast_file, overwrite = TRUE)
# log_message(paste("Saved projected raster to:", mukey_rast_file))

# Option B: Load pre-projected raster (faster for repeat runs)
mukey_rast_file <- here("data/raw/gssurgo/processed/muraster_10m_3310.tif")

if (!file.exists(mukey_rast_file)) {
  stop("ERROR: Projected mukey raster not found. Uncomment Option A to create it.")
}

mukey_rast <- rast(mukey_rast_file)

# Verify CRS
crs(mukey_rast)

# Log raster properties
log_message(paste("Raster dimensions:", paste(dim(mukey_rast), collapse=" x ")))
log_message(paste("Raster resolution:", res(mukey_rast)[1], "m"))
log_message(paste("Raster extent:", paste(as.vector(ext(mukey_rast)), collapse=", ")))

################################################################################
# Optional: Clip to San Joaquin Valley (uncomment if desired)
################################################################################

# # get California counties from Census TIGER/Line shapefiles
# log_message("Clipping to San Joaquin Valley")
# ca_counties <- counties(state = "CA")
# 
# # define San Joaquin Valley counties
# sjv_counties <- c("San Joaquin", "Stanislaus", "Merced", "Madera",
#                   "Fresno", "Kings", "Tulare", "Kern")
# 
# # filter and combine into one polygon
# sjv <- ca_counties %>%
#   filter(NAME %in% sjv_counties) %>%
#   st_union() %>%
#   st_transform(crs = crs(mukey_rast)) %>% # transform to raster CRS
#   vect() # convert to terra vector format
# 
# # crop and mask the raster to SJV
# mukey_rast <- mukey_rast %>%
#   crop(sjv) %>%
#   mask(sjv)
# 
# log_message("Clipped raster to San Joaquin Valley")

# =============================================================================
# STEP 6: Reclassify mukey raster to soil property rasters
# =============================================================================

log_message("Step 6: Reclassifying mukey raster to soil properties")

# Create reclassification vectors
max_mukey <- max(lookup$mukey, na.rm = TRUE)
log_message(paste("Maximum mukey value:", max_mukey))

# Create empty vectors filled with NA
ph_vec <- rep(NA_real_, max_mukey)
clay_vec <- rep(NA_real_, max_mukey)
ec_vec <- rep(NA_real_, max_mukey)

# Fill in values at mukey positions
ph_vec[lookup$mukey] <- lookup$ph
clay_vec[lookup$mukey] <- lookup$clay
ec_vec[lookup$mukey] <- lookup$ec

# Check how many mukeys in raster are in lookup table
unique_mukeys_raster <- unique(mukey_rast)
unique_mukeys_raster <- unique_mukeys_raster[!is.na(unique_mukeys_raster)]
mukeys_in_lookup <- sum(unique_mukeys_raster %in% lookup$mukey)

log_message(paste("Raster contains", length(unique_mukeys_raster), "unique mukeys"))
log_message(paste(mukeys_in_lookup, "mukeys found in lookup table",
                  "(", round(100*mukeys_in_lookup/length(unique_mukeys_raster), 1), "%)"))

# Reclassify rasters using terra's app() function for memory-safe processing
log_message("Creating pH raster...")
ph_rast <- app(mukey_rast, fun = function(x) ph_vec[x])

log_message("Creating clay raster...")
clay_rast <- app(mukey_rast, fun = function(x) clay_vec[x])

log_message("Creating EC raster...")
ec_rast <- app(mukey_rast, fun = function(x) ec_vec[x])

# Rename layers
names(ph_rast) <- "pH_0_10cm"
names(clay_rast) <- "clay_pct_0_10cm"
names(ec_rast) <- "ec_dS_m_0_10cm"


# Plot to check (visual QA)
png(here("data/raw/gssurgo/processed_test/qa_plots_10m.png"), 
    width=1800, height=600, res=100)
par(mfrow=c(1,3))
plot(clay_rast, main = "Clay Content (%) 0-10 cm - 10m resolution")
plot(ph_rast, main = "pH (1:1 H2O) 0-10 cm - 10m resolution")
plot(ec_rast, main = "Electrical Conductivity (dS/m) 0-10 cm - 10m resolution")
dev.off()
log_message("Saved QA plots to qa_plots_10m.png")

# Optional: save intermediate 10m rasters
# clay_10m_file <- here("data/raw/gssurgo/processed/clay_pct_0_10cm_CA_10m.tif")
# ph_10m_file <- here("data/raw/gssurgo/processed/pH_0_10cm_CA_10m.tif")
# ec_10m_file <- here("data/raw/gssurgo/processed/ec_dS_m_0_10cm_CA_10m.tif")
# 
# writeRaster(clay_rast, clay_10m_file, overwrite = TRUE)
# writeRaster(ph_rast, ph_10m_file, overwrite = TRUE)
# writeRaster(ec_rast, ec_10m_file, overwrite = TRUE)
# log_message("Saved 10m resolution rasters")

# =============================================================================
# STEP 7: Aggregate from 10m to 270m resolution with quality controls
# =============================================================================

log_message("Step 7: Aggregating to 270m resolution")

# Calculate aggregation factor
fact <- 270 / res(clay_rast)[1]  # should be 27
fact <- round(fact)  # round to nearest integer
log_message(paste("Aggregation factor:", fact, "(270m /", res(clay_rast)[1], "m)"))

# Custom aggregation function that requires minimum valid cells
# This prevents averaging just 1-2 cells across a 270m pixel
agg_with_threshold <- function(x, min_valid = 0.5) {
  # Calculate proportion of valid (non-NA) cells
  valid_pct <- sum(!is.na(x)) / length(x)
  
  # If less than threshold of cells are valid, return NA
  if (valid_pct < min_valid) {
    return(NA_real_)
  }
  
  # Otherwise return the mean of valid cells
  mean(x, na.rm = TRUE)
}

# Aggregate using custom function (requires ≥50% valid cells)
log_message("Aggregating clay (requires ≥50% valid cells per 270m pixel)...")
clay_270m <- aggregate(clay_rast, 
                       fact = fact, 
                       fun = agg_with_threshold)

log_message("Aggregating pH (requires ≥50% valid cells per 270m pixel)...")
ph_270m <- aggregate(ph_rast,
                     fact = fact, 
                     fun = agg_with_threshold)

log_message("Aggregating EC (requires ≥50% valid cells per 270m pixel)...")
ec_270m <- aggregate(ec_rast,
                     fact = fact,
                     fun = agg_with_threshold)

# Rename layers
names(clay_270m) <- "clay_pct_0_10cm"
names(ph_270m) <- "pH_0_10cm"
names(ec_270m) <- "ec_dS_m_0_10cm"

# Calculate data availability in 270m rasters
clay_270_cells <- sum(!is.na(values(clay_270m)))
ph_270_cells <- sum(!is.na(values(ph_270m)))
ec_270_cells <- sum(!is.na(values(ec_270m)))
total_270_cells <- ncell(clay_270m)

log_message(paste("270m Clay raster: ", clay_270_cells, "valid cells (", 
                  round(100*clay_270_cells/total_270_cells, 1), "%)"))
log_message(paste("270m pH raster: ", ph_270_cells, "valid cells (", 
                  round(100*ph_270_cells/total_270_cells, 1), "%)"))
log_message(paste("270m EC raster: ", ec_270_cells, "valid cells (", 
                  round(100*ec_270_cells/total_270_cells, 1), "%)"))

# Verify resolution
log_message(paste("Final resolution:", res(clay_270m)[1], "m"))

# Plot to check (visual QA)
png(here("data/raw/gssurgo/processed_test/qa_plots_270m.png"), 
    width=1800, height=600, res=100)
par(mfrow=c(1,3))
plot(clay_270m, main = "Clay Content (%) 0-10 cm - 270m resolution")
plot(ph_270m, main = "pH (1:1 H2O) 0-10 cm - 270m resolution")
plot(ec_270m, main = "Electrical Conductivity (dS/m) 0-10 cm - 270m resolution")
dev.off()
log_message("Saved QA plots to qa_plots_270m.png")

# =============================================================================
# STEP 8: Final QA checks and save outputs
# =============================================================================

log_message("Step 8: Final quality checks and saving outputs")

# Check final value ranges
clay_range <- range(values(clay_270m), na.rm=TRUE)
ph_range <- range(values(ph_270m), na.rm=TRUE)
ec_range <- range(values(ec_270m), na.rm=TRUE)

log_message(paste("Final Clay range:", round(clay_range[1], 2), "-", 
                  round(clay_range[2], 2), "%"))
log_message(paste("Final pH range:", round(ph_range[1], 2), "-", 
                  round(ph_range[2], 2)))
log_message(paste("Final EC range:", round(ec_range[1], 2), "-", 
                  round(ec_range[2], 2), "dS/m"))

# Flag any concerning values
if (clay_range[1] < 0 || clay_range[2] > 100) {
  log_message("WARNING: Clay values outside expected 0-100% range!")
}
if (ph_range[1] < 3 || ph_range[2] > 11) {
  log_message("WARNING: pH values outside typical 3-11 range (may be valid for extreme soils)")
}
if (ec_range[1] < 0) {
  log_message("WARNING: Negative EC values detected!")
}

# Write final rasters
clay_file <- here("data/raw/gssurgo/processed_test/clay_pct_0_10cm_CA_270m.tif")
ph_file <- here("data/raw/gssurgo/processed_test/pH_0_10cm_CA_270m.tif")
ec_file <- here("data/raw/gssurgo/processed_test/ec_dS_m_0_10cm_CA_270m.tif")

log_message("Writing final rasters...")
writeRaster(clay_270m, clay_file, overwrite = TRUE)
log_message(paste("Saved:", clay_file))

writeRaster(ph_270m, ph_file, overwrite = TRUE)
log_message(paste("Saved:", ph_file))

writeRaster(ec_270m, ec_file, overwrite = TRUE)
log_message(paste("Saved:", ec_file))

# =============================================================================
# STEP 9: Generate summary report
# =============================================================================

log_message("Step 9: Generating summary report")

# Create summary statistics
summary_stats <- data.frame(
  Property = c("Clay (%)", "pH", "EC (dS/m)"),
  Min_10m = c(min(values(clay_rast), na.rm=TRUE),
              min(values(ph_rast), na.rm=TRUE),
              min(values(ec_rast), na.rm=TRUE)),
  Max_10m = c(max(values(clay_rast), na.rm=TRUE),
              max(values(ph_rast), na.rm=TRUE),
              max(values(ec_rast), na.rm=TRUE)),
  Mean_10m = c(mean(values(clay_rast), na.rm=TRUE),
               mean(values(ph_rast), na.rm=TRUE),
               mean(values(ec_rast), na.rm=TRUE)),
  Valid_Cells_10m = c(clay_cells, ph_cells, ec_cells),
  Pct_Valid_10m = round(100 * c(clay_cells, ph_cells, ec_cells) / total_cells, 1),
  Min_270m = c(min(values(clay_270m), na.rm=TRUE),
               min(values(ph_270m), na.rm=TRUE),
               min(values(ec_270m), na.rm=TRUE)),
  Max_270m = c(max(values(clay_270m), na.rm=TRUE),
               max(values(ph_270m), na.rm=TRUE),
               max(values(ec_270m), na.rm=TRUE)),
  Mean_270m = c(mean(values(clay_270m), na.rm=TRUE),
                mean(values(ph_270m), na.rm=TRUE),
                mean(values(ec_270m), na.rm=TRUE)),
  Valid_Cells_270m = c(clay_270_cells, ph_270_cells, ec_270_cells),
  Pct_Valid_270m = round(100 * c(clay_270_cells, ph_270_cells, ec_270_cells) / 
                           total_270_cells, 1)
)

# Round numeric columns
summary_stats <- summary_stats %>%
  mutate(across(where(is.numeric) & !contains("Cells") & !contains("Pct"), 
                ~round(., 2)))

# Save summary
summary_file <- here("data/raw/gssurgo/processed_test/processing_summary.csv")
write_csv(summary_stats, summary_file)
log_message(paste("Saved summary statistics to:", summary_file))

# Print summary to console
cat("\n=== PROCESSING SUMMARY ===\n")
print(summary_stats)
cat("\n")

log_message("=== Processing complete ===")
log_message(paste("Log file saved to:", log_file))

# Print final message
cat("\n")
cat("================================================================================\n")
cat("gSSURGO Processing Complete!\n")
cat("================================================================================\n")
cat("\nOutput files:\n")
cat("  - Clay:  ", clay_file, "\n")
cat("  - pH:    ", ph_file, "\n")
cat("  - EC:    ", ec_file, "\n")
cat("  - Log:   ", log_file, "\n")
cat("  - Lookup:", lookup_file, "\n")
cat("  - Summary:", summary_file, "\n")
cat("\nQA plots saved to: data/raw/gssurgo/processed_test/qa_plots_*.png\n")
cat("================================================================================\n")






