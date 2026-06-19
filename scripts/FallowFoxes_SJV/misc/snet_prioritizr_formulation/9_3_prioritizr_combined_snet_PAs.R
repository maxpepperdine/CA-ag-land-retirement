# =============================================================================
# PRIORITIZR COMBINED OPTIMIZATION: WATER SAVINGS + HABITAT CREATION
#                                   WITH LOCKED-IN PROTECTED AREAS
# =============================================================================
# Purpose: Identify optimal field retirement configurations that simultaneously
#          meet cross-temporal habitat targets AND water savings targets while
#          minimizing foregone agricultural revenue, with existing protected
#          areas (PAs) incorporated as locked-in planning units.
#
# Approach: Mirrors the original 9_3_prioritizr_combined.R workflow, with the
#           same PA design pattern applied in 9_1_prioritizr_habitat_only_PAs.R:
#
#           1. Planning units include the same non-retired fields within the
#              15 SJV groundwater basins as the original combined workflow.
#
#           2. Protected areas (PAs) from the CPAD 2025b Holdings dataset are
#              added to the planning unit pool. Only PAs that intersect or
#              touch the union of the 15 SJV basins are retained, then
#              topologically cleaned (PA-PA overlap dissolved, field footprint
#              erased from PAs) so the resulting PA layer does not spatially
#              overlap fields or other PAs. Field geometry is never altered.
#
#           3. PAs are assigned habitat = 0, S_net = 0, and revenue = 0 so
#              they do not contribute toward habitat creation or water savings
#              targets. They are LOCKED IN to every solution via
#              `add_locked_in_constraints()`. PAs influence the optimization
#              solely through the boundary penalty: by appearing in the
#              boundary matrix, they pull retired fields toward existing
#              protected landscapes.
#
#           A new `is_pa` logical column distinguishes PA planning units (TRUE)
#           from field planning units (FALSE) for downstream reporting.
#
# Scenarios: 6 problems (3 water scenarios × 2 habitat quality levels)
#   Water: Baseline, RCP4.5 (2020-2049), RCP8.5 (2020-2049)
#   Habitat quality: Suitable, High Quality
#
# Water savings targets:
#   Valley-wide overdraft reduction targets adjusted for climate change
#   following the PPIC/BCM scaling methodology:
#     Baseline: 1,849 TAF (PPIC SGMA-only estimate)
#     RCP4.5:   Baseline × 1.063 (PPIC 6.3% increase)
#     RCP8.5:   Baseline × (1 + 0.063 × 1.72) (PPIC × BCM PET scaling = 10.3%)
#
# Habitat targets:
#   25,000 acres per habitat feature (15 features, cross-temporal)
#
# Cost layer: revenue per field (annual foregone revenue); PAs assigned cost
#             of 1 (scaled), nominal value since PAs are locked in.
# Decision variable: binary (retire or not)
# BLM: re-calibrated on the Baseline + Suitable habitat problem WITH locked-in
#      PAs. Applied to all 6 problems.
# =============================================================================


# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(prioritizr)
library(sf)
library(terra)
library(here)


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)


# =============================================================================
# SECTION 1: Load Data
# =============================================================================

# --- Combined planning unit data (water + habitat columns) ---
field_data_all <- read_sf(here("data/intermediate/8_Snet_estimation/prioritizr_water_habitat/fields_water_habitat_combined.gpkg"))

# --- DWR groundwater basin boundaries ---
basins_raw <- st_read(here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp"))

# --- CPAD Holdings (protected areas) ---
cpad_raw <- st_read(here("data/raw/protected_areas/cpad_release_2025b/CPAD_Release_2025b/CPAD_2025b_Holdings/CPAD_2025b_Holdings.shp"))


# =============================================================================
# SECTION 1b: Assign Fields to Groundwater Basins
# =============================================================================
# Limit planning units to the same 15 SJV basins used in the water savings
# analysis to ensure water targets are applied to the correct spatial extent.

cat("Assigning fields to groundwater basins...\n")

# Filter basins to SJV only — remove basins without PPIC targets
sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>%
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA",
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN",
                            "SAN JOAQUIN VALLEY - COSUMNES",
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY"))

cat("  SJV basins found:", nrow(sjv_basins), "\n")
cat("  Basin names:\n")
print(sort(unique(sjv_basins$Basin_Su_1)))

# Reproject basins to match fields CRS
sjv_basins <- st_transform(sjv_basins, st_crs(field_data_all))

# Spatial join: assign each field to the basin it falls within
# Use centroid of field for clean 1:1 matching
fields_centroids <- st_centroid(field_data_all)
basin_join <- st_join(fields_centroids, sjv_basins %>% select(Basin_Su_1), left = TRUE)

# Extract basin name and add to fields
field_data_all$basin_raw <- basin_join$Basin_Su_1

# Clean basin names
field_data_all <- field_data_all %>%
  mutate(
    basin = case_when(
      grepl("KERN COUNTY", basin_raw)         ~ "Kern",
      grepl("KINGS", basin_raw)               ~ "Kings",
      grepl("TULE", basin_raw)                ~ "Tule",
      grepl("KAWEAH", basin_raw)              ~ "Kaweah",
      grepl("MADERA", basin_raw)              ~ "Madera",
      grepl("MERCED", basin_raw)              ~ "Merced",
      grepl("TURLOCK", basin_raw)             ~ "Turlock",
      grepl("DELTA-MENDOTA", basin_raw)       ~ "Delta-Mendota",
      grepl("DELTA MENDOTA", basin_raw)       ~ "Delta-Mendota",
      grepl("EASTERN SAN JOAQUIN", basin_raw) ~ "Eastern San Joaquin",
      grepl("CHOWCHILLA", basin_raw)          ~ "Chowchilla",
      grepl("TULARE LAKE", basin_raw)         ~ "Tulare Lake",
      grepl("MODESTO", basin_raw)             ~ "Modesto",
      grepl("WESTSIDE", basin_raw)            ~ "Westside",
      grepl("TRACY", basin_raw)               ~ "Tracy",
      grepl("WHITE WOLF", basin_raw)          ~ "White Wolf",
      TRUE                                     ~ NA_character_
    )
  )

# QC: check basin assignment
cat("\nBasin assignment summary:\n")
basin_counts <- field_data_all %>%
  st_drop_geometry() %>%
  count(basin) %>%
  arrange(desc(n)) %>%
  as.data.frame()
print(basin_counts)
cat("  Fields without basin assignment:", sum(is.na(field_data_all$basin)), "\n")


# =============================================================================
# SECTION 1c: Define Field Planning Units
# =============================================================================
# Planning units: non-retired fields within the 15 SJV groundwater basins.

candidate_ids <- field_data_all %>%
  filter(retired == 0, !is.na(basin)) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% candidate_ids)

cat("\nField planning units (non-retired, in 15 SJV basins):", nrow(field_data), "\n")
cat("  - Fallow:", sum(field_data$fallow == 1), "\n")
cat("  - Cultivated:", sum(field_data$fallow == 0), "\n")


# =============================================================================
# SECTION 1d: Filter Protected Areas to the 15 SJV Basins, then Resolve
#             Overlap with Fields and Other PAs
# =============================================================================
# Same topology cleanup approach as 9_1_prioritizr_habitat_only_PAs.R:
#
#   Step 1: Keep CPAD Holdings that intersect the union of the 15 SJV basins
#           (single `st_intersects` predicate covers within / overlapping /
#           touching).
#
#   Step 2: Resolve geometric overlap. CPAD Holdings frequently overlap each
#           other and agricultural fields. Spatially overlapping planning
#           units cause `prioritizr` to compute incorrect boundary lengths.
#           Field geometry is NEVER altered — full field acreage must be
#           preserved for accurate retirement reporting. We therefore:
#             (a) Dissolve all in-SJV PAs into a single union (PA-PA overlap).
#             (b) Erase the field union from the dissolved PA layer (PA-field
#                 overlap, by reshaping PAs only).
#             (c) Cast to single-part polygons so each spatially-distinct PA
#                 patch becomes one planning unit.
#             (d) Drop tiny sliver polygons (<1 acre) from edge mismatches.
#
# The boundary matrix still captures PA-field shared borders wherever the
# original PA footprint touched (rather than overlapped) a field, so the
# BLM penalty continues to pull retirement toward PAs.

cat("\nFiltering CPAD Holdings to the 15 SJV basins...\n")

# Match CPAD to fields CRS
cpad <- st_transform(cpad_raw, st_crs(field_data))
cat("  CPAD Holdings (statewide):", nrow(cpad), "\n")

# Union of 15 SJV basins (single multipolygon for the intersects test)
sjv_basins_union <- st_union(sjv_basins)

# Keep PAs that intersect (covers within / overlapping / touching)
cpad_in_sjv <- cpad %>%
  filter(lengths(st_intersects(., sjv_basins_union)) > 0)

cat("  CPAD Holdings retained (intersecting 15 SJV basins):", nrow(cpad_in_sjv), "\n")


# --- Resolve PA overlap topology ---
cat("\nResolving PA overlap topology (preserving field geometry)...\n")

# (a) Dissolve all PAs into a single multipolygon — removes PA-PA overlap
pa_union <- st_union(cpad_in_sjv) %>% st_make_valid()

# (b) Erase the field footprint from the PA union — removes PA-field overlap
field_union <- st_union(field_data) %>% st_make_valid()
pa_clean <- st_difference(pa_union, field_union) %>% st_make_valid()

# (c) Cast to single-part polygons so each contiguous PA patch is one PU
pa_patches <- st_cast(pa_clean, "POLYGON", warn = FALSE)

# (d) Drop tiny slivers (< 1 acre)
pa_patches_sf <- st_sf(geometry = pa_patches) %>%
  mutate(acres_pa = as.numeric(st_area(.)) / 4046.8564224) %>%
  filter(acres_pa >= 1)

cat("  PA patches after topology cleanup:", nrow(pa_patches_sf), "\n")
cat("  Total PA acres (after cleanup):",
    format(round(sum(pa_patches_sf$acres_pa)), big.mark = ","), "\n")

# Replace cpad_in_sjv with the cleaned patch layer for downstream use
cpad_in_sjv <- pa_patches_sf


# =============================================================================
# SECTION 1e: Build the Combined Planning Unit Layer (Fields + PAs)
# =============================================================================
# Append PAs to the field planning units. PAs are assigned:
#   - habitat = 0 for every habitat feature column (no contribution to targets)
#   - S_net   = 0 for every water feature column (no contribution to targets)
#   - revenue = 0, cost = nominal (so the solver isn't selecting them "for
#     free" in a way that messes with cost reporting; the locked-in constraint
#     forces selection regardless)
#   - is_pa = TRUE for PAs, FALSE for fields (logical, required by
#            add_locked_in_constraints())
#   - id    = unique negative IDs (so they can't collide with field IDs)

cat("\nBuilding combined planning unit layer (fields + PAs)...\n")

# --- Identify habitat & water columns we'll need to zero out for PAs ---
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr|sjkf)_", names(field_data))]

# Water columns: both raw S_net (in AF) and any pre-existing TAF versions
# (the TAF versions are computed in Section 2 below; here we just zero out
# the raw AF columns the PAs need to carry).
water_cols_AF <- intersect(
  c("Snet_baseline_AF", "Snet_RCP45_near_AF", "Snet_RCP85_near_AF",
    "AW_baseline_AF",   "AW_RCP45_near_AF",   "AW_RCP85_near_AF",
    "ETc_baseline_AF",  "ETc_RCP45_near_AF",  "ETc_RCP85_near_AF"),
  names(field_data)
)

# PET ratio columns are needed for the RCP8.5 scaling derivation in Section 3.
# Setting these to 1 on PAs means they contribute identity ratios (no climate
# adjustment) to the weighted mean — but since PAs are weighted by their
# acres (not field acres), and PAs have no agricultural water demand, we
# zero out ALL PA water-related columns and explicitly exclude PAs from the
# PET ratio calculation in Section 3.
pet_ratio_cols <- intersect(
  c("PET_ratio_RCP45_near", "PET_ratio_RCP85_near"),
  names(field_data)
)

# --- Add is_pa = FALSE to fields ---
field_data <- field_data %>%
  mutate(is_pa = FALSE)

# --- Build PA planning units, matching the field schema ---
n_pa <- nrow(cpad_in_sjv)

pa_data <- cpad_in_sjv %>%
  # acres_pa is already computed in Section 1d topology cleanup
  select(acres_pa) %>%
  # Assign matching schema
  mutate(
    id        = -seq_len(n_pa),       # negative IDs to avoid collision with fields
    is_pa     = TRUE,                 # logical, required by add_locked_in_constraints()
    fallow    = 0L,                   # PAs are not fallow fields
    retired   = 0L,                   # PAs are not "retired" agricultural fields
    revenue   = 0,
    acres     = acres_pa,
    basin     = NA_character_,        # PAs are not assigned to a single basin
    basin_raw = NA_character_
  ) %>%
  rename(geom = geometry) %>% 
  select(-acres_pa)

# Add NA-filled versions of any other field-only attributes so rbind() doesn't
# drop them (e.g., county, last_comm). Only add columns that exist on fields
# but not yet on PAs. We'll handle the type-matching for important columns
# explicitly below.
pa_data <- pa_data %>%
  mutate(
    county    = NA_character_,
    last_comm = NA_character_, 
    waterAW   = NA_real_
  )

# Add zeroed habitat columns
for (col in habitat_cols_all) {
  pa_data[[col]] <- 0
}

# Add zeroed water columns (raw AF)
for (col in water_cols_AF) {
  pa_data[[col]] <- 0
}

# Add NA PET ratio columns (these are excluded from the Section 3 calculation
# anyway — explicitly NA so they don't sneak into any future weighted means).
for (col in pet_ratio_cols) {
  pa_data[[col]] <- NA_real_
}

# --- Align column sets and rbind ---
common_cols <- intersect(names(field_data), names(pa_data))
common_cols <- c(setdiff(common_cols, attr(field_data, "sf_column")),
                 attr(field_data, "sf_column"))

field_data_pu <- field_data %>% select(all_of(common_cols))
pa_data_pu    <- pa_data    %>% select(all_of(common_cols))

# Combine
pu <- rbind(field_data_pu, pa_data_pu)

cat("  Field planning units:", sum(!pu$is_pa), "\n")
cat("  PA planning units:   ", sum(pu$is_pa), "\n")
cat("  Total planning units:", nrow(pu), "\n")


# =============================================================================
# SECTION 2: Prepare Cost, Habitat, and Water Columns
# =============================================================================

# --- Cost: revenue scaled to ten-thousands of USD; PAs get nominal cost ---
cost_scale_factor <- 1e4

pu <- pu %>%
  mutate(
    cost_raw = case_when(
      is_pa                                     ~ 1,                # nominal (PA, locked in)
      is.na(revenue) | revenue <= 0             ~ 1,                # nominal
      TRUE                                      ~ revenue
    ),
    cost = cost_raw / cost_scale_factor
  )

cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(pu$cost), 4), ",",
    round(max(pu$cost), 2), "]\n")


# --- Habitat: replace NAs with 0 (PAs already 0 by construction) ---
pu <- pu %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))


# --- Water: scale S_net from AF to TAF ---
pu <- pu %>%
  mutate(
    Snet_baseline_TAF   = Snet_baseline_AF / 1000,
    Snet_RCP45_near_TAF = Snet_RCP45_near_AF / 1000,
    Snet_RCP85_near_TAF = Snet_RCP85_near_AF / 1000
  )


# --- Diagnostics ---
cat("\nTotal fields (all, incl. retired):", nrow(field_data_all), "\n")
cat("Field planning units (non-retired, within 15 basins):", sum(!pu$is_pa), "\n")
cat("  - Fallow:", sum(!pu$is_pa & pu$fallow == 1), "\n")
cat("  - Cultivated:", sum(!pu$is_pa & pu$fallow == 0), "\n")
cat("PA planning units (locked in):", sum(pu$is_pa), "\n")
cat("Retired fields (excluded):", sum(field_data_all$retired == 1), "\n")
cat("Fields outside 15 basins (excluded):",
    sum(is.na(field_data_all$basin) & field_data_all$retired == 0), "\n")


# =============================================================================
# SECTION 2b: Boundary Matrix (Fields + PAs)
# =============================================================================
# Build the boundary matrix on the FULL planning unit set (fields + PAs).
# This is what allows the BLM penalty to reward retiring fields adjacent to
# locked-in PAs.

cat("\nBuilding boundary matrix on fields + PAs...\n")
bm <- boundary_matrix(pu)
cat("  Boundary matrix dimensions:", dim(bm), "\n")


# =============================================================================
# SECTION 3: Define Water Savings Targets
# =============================================================================
# Climate-adjusted overdraft reduction targets following the PPIC/BCM
# methodology established in the water-only analysis. PET ratios are computed
# using FIELD planning units only (PAs excluded — they have no agricultural
# water use and would bias the weighted mean if included).

field_pu_df <- pu %>% st_drop_geometry() %>% filter(!is_pa)

mean_pet_ratio_rcp45 <- weighted.mean(field_pu_df$PET_ratio_RCP45_near,
                                      field_pu_df$acres, na.rm = TRUE)
mean_pet_ratio_rcp85 <- weighted.mean(field_pu_df$PET_ratio_RCP85_near,
                                      field_pu_df$acres, na.rm = TRUE)
rcp85_scaling <- (mean_pet_ratio_rcp85 - 1) / (mean_pet_ratio_rcp45 - 1)

cat("\nRCP8.5 scaling factor derivation (from BCMv8 PET, fields only):\n")
cat("  Mean PET ratio RCP4.5:", round(mean_pet_ratio_rcp45, 4), "\n")
cat("  Mean PET ratio RCP8.5:", round(mean_pet_ratio_rcp85, 4), "\n")
cat("  RCP8.5/RCP4.5 scaling:", round(rcp85_scaling, 2), "\n")

# --- Climate multipliers ---
ppic_rcp45_increase <- 0.063
ppic_rcp85_increase <- ppic_rcp45_increase * rcp85_scaling

multiplier_rcp45 <- 1 + ppic_rcp45_increase
multiplier_rcp85 <- 1 + ppic_rcp85_increase

# --- Valley-wide targets (TAF) ---
valley_overdraft_taf <- 1849

water_target_baseline_taf <- valley_overdraft_taf
water_target_rcp45_taf    <- valley_overdraft_taf * multiplier_rcp45
water_target_rcp85_taf    <- valley_overdraft_taf * multiplier_rcp85

cat("\nValley-wide water savings targets (TAF):\n")
cat("  Baseline:          ", round(water_target_baseline_taf, 1), "\n")
cat("  RCP4.5 (2020-2049):", round(water_target_rcp45_taf, 1), "\n")
cat("  RCP8.5 (2020-2049):", round(water_target_rcp85_taf, 1), "\n")


# =============================================================================
# SECTION 4: Define Scenario Matrix (6 Problems)
# =============================================================================
# Each problem combines 15 cross-temporal habitat features + 1 water feature.

habitat_target <- 25000

# --- Habitat features ---
suit_features <- c(
  "bnll_base_suit",       "bnll_rcp45_2049_suit", "bnll_rcp45_2069_suit",
  "bnll_rcp85_2049_suit", "bnll_rcp85_2069_suit",
  "gkr_base_suit",        "gkr_rcp45_2049_suit",  "gkr_rcp45_2069_suit",
  "gkr_rcp85_2049_suit",  "gkr_rcp85_2069_suit",
  "sjkf_base_suit",       "sjkf_rcp45_2049_suit", "sjkf_rcp45_2069_suit",
  "sjkf_rcp85_2049_suit", "sjkf_rcp85_2069_suit"
)

hq_features <- c(
  "bnll_base_hq",       "bnll_rcp45_2049_hq", "bnll_rcp45_2069_hq",
  "bnll_rcp85_2049_hq", "bnll_rcp85_2069_hq",
  "gkr_base_hq",        "gkr_rcp45_2049_hq",  "gkr_rcp45_2069_hq",
  "gkr_rcp85_2049_hq",  "gkr_rcp85_2069_hq",
  "sjkf_base_hq",       "sjkf_rcp45_2049_hq", "sjkf_rcp45_2069_hq",
  "sjkf_rcp85_2049_hq", "sjkf_rcp85_2069_hq"
)

# --- Water scenario definitions ---
water_scenarios <- tibble(
  water_label     = c("Baseline", "RCP45", "RCP85"),
  snet_col        = c("Snet_baseline_TAF", "Snet_RCP45_near_TAF", "Snet_RCP85_near_TAF"),
  water_target    = c(water_target_baseline_taf, water_target_rcp45_taf, water_target_rcp85_taf)
)

# --- Full scenario matrix: 3 water × 2 habitat quality = 6 problems ---
scenarios <- expand_grid(
  water_scenarios,
  tibble(
    quality      = c("Suitable", "High Quality"),
    hab_features = list(suit_features, hq_features)
  )
) %>%
  mutate(
    scenario_name = paste0("combined_", tolower(water_label), "_",
                           ifelse(quality == "Suitable", "suit", "hq"))
  )

cat("\n--- Scenario Matrix (6 problems) ---\n")
for (i in 1:nrow(scenarios)) {
  cat(sprintf("  %d. %-35s | Water: %-8s (%.1f TAF) | Habitat: %s (15 features x 25,000 ac)\n",
              i, scenarios$scenario_name[i],
              scenarios$water_label[i], scenarios$water_target[i],
              scenarios$quality[i]))
}


# --- Verify all columns exist ---
all_feature_cols <- c(suit_features, hq_features, water_scenarios$snet_col)
missing <- setdiff(all_feature_cols, names(pu))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
cat("\nAll feature columns verified.\n")


# =============================================================================
# SECTION 5: Diagnostic — Check Feature Availability
# =============================================================================
# Available habitat and water on FIELD planning units only (PAs contribute 0).

cat("\n========== FEATURE AVAILABILITY ON FIELD PLANNING UNITS ==========\n")
cat("Field PUs:", sum(!pu$is_pa),
    "(fallow:", sum(!pu$is_pa & pu$fallow == 1),
    "| cultivated:", sum(!pu$is_pa & pu$fallow == 0), ")\n")
cat("PA PUs (locked in):", sum(pu$is_pa), "\n\n")

cat("HABITAT FEATURES (target: 25,000 acres each):\n")
for (feat in c(suit_features, hq_features)) {
  avail <- sum(pu[[feat]], na.rm = TRUE)  # PAs are 0 so this is field-only
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nWATER FEATURES (S_net in TAF):\n")
for (i in 1:nrow(water_scenarios)) {
  col <- water_scenarios$snet_col[i]
  avail <- sum(pu[[col]], na.rm = TRUE)  # PAs are 0
  target <- water_scenarios$water_target[i]
  cat(sprintf("  %-25s: %8.1f TAF available | target: %8.1f TAF (%s)\n",
              col, avail, target, ifelse(avail >= target, "OK", "!!")))
}

cat("\nTotal field-PU revenue: $",
    format(sum(pu$revenue[!pu$is_pa], na.rm = TRUE), big.mark = ","), "\n")
cat("Total field-PU acres:   ",
    format(round(sum(pu$acres[!pu$is_pa], na.rm = TRUE)), big.mark = ","), "\n")
cat("Total PA acres (locked in):",
    format(round(sum(pu$acres[pu$is_pa], na.rm = TRUE)), big.mark = ","), "\n")


# =============================================================================
# SECTION 6: BLM Calibration (Baseline + Suitable Problem, with locked-in PAs)
# =============================================================================
# Calibrate on the Baseline + Suitable problem (16 features) WITH locked-in
# PAs. The chosen BLM will be applied to all 6 problems.

cat("\n========== BLM CALIBRATION (Baseline + Suitable, 16 features, w/ PAs) ==========\n\n")

# Build feature list and targets for calibration problem
cal_features <- c(suit_features, "Snet_baseline_TAF")
cal_targets  <- c(rep(habitat_target, length(suit_features)),
                  water_target_baseline_taf)

blm_values <- c(0, 0.0001, 0.0005, 0.001, 0.003, 0.005, 0.01, 0.05)

blm_results <- tibble(
  blm         = numeric(),
  n_selected  = numeric(),  # excluding locked-in PAs
  total_cost  = numeric(),
  total_acres = numeric(),
  boundary    = numeric()
)

for (blm_val in blm_values) {
  
  cat("  Solving BLM =", blm_val, "... ")
  
  p_cal <- problem(
    x            = pu,
    features     = cal_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = cal_targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    add_boundary_penalties(penalty = blm_val, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, numeric_focus = TRUE)
  
  sol_cal <- tryCatch(
    solve(p_cal, run_checks = FALSE),
    error = function(e) {
      cat("FAILED:", e$message, "\n")
      return(NULL)
    }
  )
  
  if (!is.null(sol_cal)) {
    # Selected NEW retirements (exclude PAs which are locked in by construction)
    sel <- sol_cal %>% filter(solution_1 == 1, !is_pa)
    boundary_length <- eval_boundary_summary(p_cal, sol_cal[, "solution_1"])$boundary
    
    blm_results <- blm_results %>%
      add_row(
        blm         = blm_val,
        n_selected  = nrow(sel),
        total_cost  = sum(sel$revenue, na.rm = TRUE),
        total_acres = sum(sel$acres, na.rm = TRUE),
        boundary    = boundary_length
      )
    cat("done (", nrow(sel), "fields, $",
        format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","), ")\n")
  }
}

cat("\n--- BLM Calibration Results ---\n")
print(blm_results)

# --- Plot BLM tradeoff ---
blm_plot <- ggplot(blm_results, aes(x = total_cost, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length (with locked-in PAs)",
    subtitle = "Calibrated on combined Baseline + Suitable problem (16 features)",
    x        = "Total Revenue Cost ($)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)

# --- SELECT THE BEST BLM ---
# Inspect the plot above and set the "elbow" value.
# (Original 9_3 used 0.005; expect a similar value here, but verify.)
chosen_blm <- 0.005

cat("\nChosen BLM:", chosen_blm, "\n")


# =============================================================================
# SECTION 7: Run All 6 Combined Optimizations (with locked-in PAs)
# =============================================================================

cat("\n========== RUNNING 6 COMBINED OPTIMIZATIONS (w/ locked-in PAs) ==========\n\n")

solutions <- list()

summary_all <- tibble(
  scenario_name    = character(),
  water_scenario   = character(),
  quality          = character(),
  n_features       = numeric(),
  n_pa_locked_in   = numeric(),
  n_selected       = numeric(),  # excluding PAs
  total_cost       = numeric(),
  total_acres      = numeric(),
  total_Snet_AF    = numeric(),
  boundary         = numeric()
)

for (i in 1:nrow(scenarios)) {
  
  scen       <- scenarios$scenario_name[i]
  water_lab  <- scenarios$water_label[i]
  quality    <- scenarios$quality[i]
  hab_feats  <- scenarios$hab_features[[i]]
  snet_col   <- scenarios$snet_col[i]
  water_targ <- scenarios$water_target[i]
  
  # Combine features: 15 habitat + 1 water = 16
  all_features <- c(hab_feats, snet_col)
  all_targets  <- c(rep(habitat_target, length(hab_feats)), water_targ)
  
  cat(sprintf("--- Scenario %d/6: %s ---\n", i, scen))
  cat(sprintf("    Water: %s | Target: %.1f TAF\n", water_lab, water_targ))
  cat(sprintf("    Habitat: %s | 15 features x %s acres\n",
              quality, format(habitat_target, big.mark = ",")))
  cat(sprintf("    Total features: %d | Locked-in PAs: %d\n",
              length(all_features), sum(pu$is_pa)))
  
  # Build problem
  p <- problem(
    x            = pu,
    features     = all_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = all_targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    add_boundary_penalties(penalty = chosen_blm, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, numeric_focus = TRUE)
  
  # Solve
  sol <- tryCatch(
    solve(p, run_checks = FALSE),
    error = function(e) {
      cat("    FAILED:", e$message, "\n\n")
      return(NULL)
    }
  )
  
  if (is.null(sol)) next
  
  # Selected NEW retirements (exclude PAs)
  sel <- sol %>% filter(solution_1 == 1, !is_pa)
  
  # Calculate boundary length (on full solution)
  boundary_length <- eval_boundary_summary(p, sol[, "solution_1"])$boundary
  
  # Store solution
  solutions[[scen]] <- sol
  
  # Append summary
  summary_all <- summary_all %>%
    add_row(
      scenario_name    = scen,
      water_scenario   = water_lab,
      quality          = quality,
      n_features       = length(all_features),
      n_pa_locked_in   = sum(pu$is_pa),
      n_selected       = nrow(sel),
      total_cost       = sum(sel$revenue, na.rm = TRUE),
      total_acres      = sum(sel$acres, na.rm = TRUE),
      total_Snet_AF    = sum(sel[[snet_col]], na.rm = TRUE) * 1000,
      boundary         = boundary_length
    )
  
  # Print summary
  cat(sprintf("    Selected (new): %d fields | Cost: $%s | Acres: %s | S_net: %s AF\n",
              nrow(sel),
              format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel$acres, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel[[snet_col]], na.rm = TRUE) * 1000), big.mark = ",")))
  
  # Per-feature target achievement (PAs contribute 0 by construction)
  cat("\n    --- Target Achievement ---\n")
  
  # Habitat features
  for (feat in hab_feats) {
    achieved <- sum(sel[[feat]], na.rm = TRUE)
    met <- ifelse(achieved >= habitat_target, "YES", "NO")
    cat(sprintf("    %-30s: %10.1f / %s acres [%s]\n",
                feat, achieved, format(habitat_target, big.mark = ","), met))
  }
  
  # Water feature
  achieved_taf <- sum(sel[[snet_col]], na.rm = TRUE)
  met <- ifelse(achieved_taf >= water_targ, "YES", "NO")
  cat(sprintf("    %-30s: %10.1f / %.1f TAF    [%s]\n",
              snet_col, achieved_taf, water_targ, met))
  
  cat(sprintf("\n    Boundary: %.1f\n\n", boundary_length))
}


# =============================================================================
# SECTION 8: Compare Solutions
# =============================================================================

cat("\n========== COMBINED SOLUTION COMPARISON ==========\n\n")
print(summary_all, width = Inf)


# --- Detailed target achievement for all problems ---
target_detail <- tibble(
  scenario       = character(),
  water_scenario = character(),
  quality        = character(),
  feature        = character(),
  feature_type   = character(),
  species        = character(),
  climate        = character(),
  target         = numeric(),
  target_unit    = character(),
  achieved       = numeric()
)

for (i in 1:nrow(scenarios)) {
  scen       <- scenarios$scenario_name[i]
  water_lab  <- scenarios$water_label[i]
  quality    <- scenarios$quality[i]
  hab_feats  <- scenarios$hab_features[[i]]
  snet_col   <- scenarios$snet_col[i]
  water_targ <- scenarios$water_target[i]
  
  if (!(scen %in% names(solutions))) next
  # Field-only achievement (PAs contribute 0)
  sel <- solutions[[scen]] %>% filter(solution_1 == 1, !is_pa)
  
  # Habitat features
  for (feat in hab_feats) {
    target_detail <- target_detail %>%
      add_row(
        scenario       = scen,
        water_scenario = water_lab,
        quality        = quality,
        feature        = feat,
        feature_type   = "habitat",
        species        = case_when(
          grepl("^bnll", feat) ~ "BNLL",
          grepl("^gkr", feat)  ~ "GKR",
          grepl("^sjkf", feat) ~ "SJKF"
        ),
        climate = case_when(
          grepl("base", feat)       ~ "Baseline",
          grepl("rcp45_2049", feat) ~ "RCP 4.5 (2020-2049)",
          grepl("rcp45_2069", feat) ~ "RCP 4.5 (2040-2069)",
          grepl("rcp85_2049", feat) ~ "RCP 8.5 (2020-2049)",
          grepl("rcp85_2069", feat) ~ "RCP 8.5 (2040-2069)"
        ),
        target       = habitat_target,
        target_unit  = "acres",
        achieved     = sum(sel[[feat]], na.rm = TRUE)
      )
  }
  
  # Water feature
  target_detail <- target_detail %>%
    add_row(
      scenario       = scen,
      water_scenario = water_lab,
      quality        = quality,
      feature        = snet_col,
      feature_type   = "water",
      species        = NA_character_,
      climate        = water_lab,
      target         = water_targ,
      target_unit    = "TAF",
      achieved       = sum(sel[[snet_col]], na.rm = TRUE)
    )
}

cat("\n--- Detailed Target Achievement (field PUs only; PAs contribute 0) ---\n")
print(target_detail, n = Inf)


# =============================================================================
# SECTION 9: Export Results
# =============================================================================

output_dir <- here("data/intermediate/misc/snet_prioritizr_formulation/9_3_prioritizr_combined_snet_PAs/")

# --- Save all objects for figures ---
save(
  solutions,
  summary_all,
  target_detail,
  scenarios,
  water_scenarios,
  suit_features,
  hq_features,
  pu,                  # combined planning unit layer (fields + PAs)
  field_data,
  field_data_all,
  pa_data,
  sjv_basins,
  habitat_target,
  water_target_baseline_taf,
  water_target_rcp45_taf,
  water_target_rcp85_taf,
  chosen_blm,
  blm_results,
  bm,
  cost_scale_factor,
  multiplier_rcp45,
  multiplier_rcp85,
  rcp85_scaling,
  file = file.path(output_dir, "prioritizr_combined_PAs_results.RData")
)

# --- Export individual solutions as geopackages ---
# Solutions retain the full PU layer (fields + PAs) with solution_1 and is_pa
# columns so downstream figures can distinguish locked-in PAs from newly
# retired fields.
for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  # Try to attach crop/comm classification to fields, leave PAs as-is
  if (exists("convertCommRetired")) {
    sol_classified <- tryCatch(
      convertCommRetired(sol) %>% dplyr::select(-any_of("crop")),
      error = function(e) sol
    )
  } else {
    sol_classified <- sol
  }
  
  write_sf(sol_classified,
           file.path(output_dir, paste0("solution_", scen, ".gpkg")))
}

# --- Export summary tables ---
write_csv(summary_all,
          file.path(output_dir, "combined_PAs_solution_comparison.csv"))
write_csv(target_detail,
          file.path(output_dir, "combined_PAs_target_detail.csv"))
write_csv(blm_results,
          file.path(output_dir, "combined_PAs_blm_calibration.csv"))

# --- Export target methodology ---
target_methodology <- data.frame(
  parameter = c("PPIC RCP4.5 increase", "BCM RCP8.5 scaling factor",
                "RCP4.5 multiplier", "RCP8.5 multiplier",
                "Mean PET ratio RCP4.5", "Mean PET ratio RCP8.5",
                "Valley baseline overdraft (TAF)",
                "Valley RCP4.5 target (TAF)", "Valley RCP8.5 target (TAF)",
                "Habitat target (acres per feature)",
                "PAs locked in (count)",
                "PA acres locked in"),
  value = c(round(ppic_rcp45_increase, 4), round(rcp85_scaling, 2),
            round(multiplier_rcp45, 4), round(multiplier_rcp85, 4),
            round(mean_pet_ratio_rcp45, 4), round(mean_pet_ratio_rcp85, 4),
            valley_overdraft_taf,
            round(water_target_rcp45_taf, 1), round(water_target_rcp85_taf, 1),
            habitat_target,
            sum(pu$is_pa),
            round(sum(pu$acres[pu$is_pa], na.rm = TRUE)))
)

write_csv(target_methodology, file.path(output_dir, "combined_PAs_target_methodology.csv"))


cat("\n========== COMBINED OPTIMIZATION (w/ PAs) COMPLETE — RESULTS SAVED ==========\n")
cat("Output directory:", output_dir, "\n")
cat("Load results for figures with:\n")
cat('  load(here("data/intermediate/misc/snet_prioritizr_formulation/9_3_prioritizr_combined_PAs/prioritizr_combined_PAs_results.RData"))\n')

























