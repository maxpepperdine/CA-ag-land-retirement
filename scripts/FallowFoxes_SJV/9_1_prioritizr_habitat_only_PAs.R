# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS-TEMPORAL) OPTIMIZATION — W/ PROTECTED AREAS
# =============================================================================
# Purpose: Use prioritizr to find optimal retirement configurations that meet
#          habitat targets for BNLL, GKR, and SJKF across ALL climate scenarios
#          / time periods simultaneously, with existing protected areas (PAs)
#          incorporated as locked-in planning units.
#
# Approach: 
#           Build problems where every climate projection is a separate feature 
#           with a 25,000-acre target. This forces the solver to select fields that 
#           provide adequate habitat for all three species under ALL futures —
#           ensuring that land retired today remains suitable as climate shifts.
#
#           Incorporate PAs as planning units:
#
#           1. Planning units are restricted to non-retired fields within the
#              same 15 SJV groundwater basins used in the water-only analysis,
#              ensuring spatial consistency between the habitat- and water-only
#              optimizations.
#
#           2. Protected areas (PAs) from the CPAD 2025b Holdings dataset are
#              added to the planning unit pool. Only PAs that intersect or
#              touch the union of the 15 SJV basins are retained. PAs are
#              then topologically cleaned (PA-PA overlap dissolved, field
#              footprint erased from PAs) so the resulting PA layer does not
#              spatially overlap fields or other PAs — required for valid
#              boundary matrix calculations in `prioritizr`. Field geometry
#              is never altered.
#
#           3. PAs are assigned habitat = 0 and water = 0 (so they do not
#              contribute toward habitat creation or water savings targets),
#              and are LOCKED IN to every solution. This means the boundary
#              penalty (BLM) rewards retiring fields adjacent to existing PAs,
#              encouraging new habitat to cluster around existing protected
#              landscapes — a foundational principle in conservation planning.
#
# Two optimization problems:
#   1. Suitable habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
#   2. High quality habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
#
#   Each feature has a 25,000-acre target.
#
# Cost layer: annual revenue per field (`revenue`); PAs assigned cost = 0.0001
#             (nominal, to avoid solver issues with zero-cost locked-in PUs).
# Decision variable: binary (retire/protect or not)
# BLM: calibrated on the suitable habitat problem WITH locked-in PAs, then
#      applied to both problems.
# =============================================================================

# clear environment
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
# STEP 1: Load Data
# =============================================================================

# --- Planning units: SJV fields with habitat extractions ---
field_data_all <- read_sf(here("data/intermediate/7_habitat_extraction/sjvHabitatExtractions/sjvHabitatExtractions.gpkg"))

# --- DWR groundwater basin boundaries ---
basins_raw <- st_read(here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp"))

# --- CPAD Holdings (protected areas) ---
cpad_raw <- st_read(here("data/raw/protected_areas/cpad_release_2025b/CPAD_Release_2025b/CPAD_2025b_Holdings/CPAD_2025b_Holdings.shp"))


# =============================================================================
# STEP 1b: Filter Basins to the 15 SJV Groundwater Basins
# =============================================================================
# Same filter applied in 9_2_prioritizr_water_only.R: SJV basins only,
# excluding the 4 basins without PPIC overdraft estimates (East Contra Costa,
# Kettleman Plain, Cosumnes, Pleasant Valley).

cat("Filtering DWR basins to the 15 SJV basins used in the water-only analysis...\n")

sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>%
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA",
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN",
                            "SAN JOAQUIN VALLEY - COSUMNES",
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY"))

cat("  SJV basins retained:", nrow(sjv_basins), "\n")
print(sort(unique(sjv_basins$Basin_Su_1)))

# reproject basins to match fields CRS
sjv_basins <- st_transform(sjv_basins, st_crs(field_data_all))


# =============================================================================
# STEP 1c: Restrict Field Planning Units to the 15 SJV Basins
# =============================================================================
# Assign each field to a basin via centroid spatial join, then keep only fields 
# that fall within one of the 15 SJV basins.

cat("\nAssigning fields to groundwater basins...\n")

# spatial join via centroid for clean 1:1 matching
field_centroids <- st_centroid(field_data_all)
basin_join <- st_join(field_centroids, sjv_basins %>% 
                        select(Basin_Su_1), left = TRUE)

# attach basin name
field_data_all$basin_raw <- basin_join$Basin_Su_1

# clean basin names (same crosswalk as water-only script)
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

cat("\nField basin assignment summary:\n")
print(field_data_all %>% st_drop_geometry() %>% count(basin) %>% arrange(desc(n)) %>% as.data.frame())
cat("  Fields outside the 15 SJV basins (will be dropped):",
    sum(is.na(field_data_all$basin)), "\n")


# --- Subset to non-retired fields within the 15 SJV basins ---
candidate_ids <- field_data_all %>%
  filter(retired == 0, !is.na(basin)) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% candidate_ids)

cat("\nField planning units (non-retired, in 15 SJV basins):", nrow(field_data), "\n")
cat("  - Fallow:", sum(field_data$fallow == 1), "\n")
cat("  - Cultivated:", sum(field_data$fallow == 0), "\n")


# =============================================================================
# STEP 1d: Filter Protected Areas to the 15 SJV Basins
# =============================================================================
# Step 1: Keep PAs that are within, intersecting, OR touching the boundary of
#         the 15 SJV basins.
#
# Step 2: Resolve geometric overlap. CPAD Holdings frequently overlap each
#         other (multiple agencies covering the same land) and overlap
#         agricultural fields (conservation easements on cropland). Spatially
#         overlapping planning units cause `prioritizr` to compute incorrect
#         boundary lengths, so we must produce a non-overlapping PA layer.
#
#         CRITICAL: Field geometry is NEVER altered — full field acreage must
#         be preserved for accurate retirement reporting. Instead, we:
#           (a) Dissolve all in-SJV PAs into a single union (removes PA-PA
#               overlap).
#           (b) Erase the field union from the dissolved PA layer (removes
#               PA-field overlap by reshaping PAs only).
#           (c) Cast to single-part polygons so each spatially-distinct PA
#               patch becomes one planning unit.
#           (d) Drop tiny sliver polygons (<1 acre) from edge mismatches.
#
#         The boundary matrix still captures PA-field shared borders wherever
#         the original PA footprint TOUCHED (rather than overlapped) a field,
#         so the BLM penalty continues to pull retirement toward PAs.

cat("\nFiltering CPAD Holdings to the 15 SJV basins...\n")

# match CPAD to fields CRS
cpad <- st_transform(cpad_raw, st_crs(field_data))
cat("  CPAD Holdings (statewide):", nrow(cpad), "\n")

# union of 15 SJV basins (single multipolygon for the intersects test)
sjv_basins_union <- st_union(sjv_basins)

# keep PAs that intersect (covers within / overlapping / touching)
cpad_in_sjv <- cpad %>%
  filter(lengths(st_intersects(., sjv_basins_union)) > 0)

cat("  CPAD Holdings retained (intersecting 15 SJV basins):", nrow(cpad_in_sjv), "\n")


# --- Resolve PA overlap topology ---
cat("\nResolving PA overlap topology (preserving field geometry)...\n")

# (a) Dissolve all PAs into a single multipolygon — removes PA-PA overlap
pa_union <- st_union(cpad_in_sjv) %>% st_make_valid()

# (b) Erase the field footprint from the PA union — removes PA-field overlap
#     (reshapes PAs only; fields are untouched)
field_union <- st_union(field_data) %>% st_make_valid()
pa_clean <- st_difference(pa_union, field_union) %>% st_make_valid()

# (c) Cast to single-part polygons so each contiguous PA patch is one PU
pa_patches <- st_cast(pa_clean, "POLYGON", warn = FALSE)

# (d) Drop tiny slivers (< 1 acre) — typically arise from edge mismatches
#     between CPAD and ag-field digitizations
pa_patches_sf <- st_sf(geometry = pa_patches) %>%
  mutate(acres_pa = as.numeric(st_area(.)) / 4046.8564224) %>%
  filter(acres_pa >= 1)

cat("  PA patches after topology cleanup:", nrow(pa_patches_sf), "\n")
cat("  Total PA acres (after cleanup):",
    format(round(sum(pa_patches_sf$acres_pa)), big.mark = ","), "\n")

# Replace cpad_in_sjv with the cleaned patch layer for downstream use
cpad_in_sjv <- pa_patches_sf


# =============================================================================
# STEP 1e: Build the Combined Planning Unit Layer (Fields + PAs)
# =============================================================================
# Append PAs to the field planning units. PAs are assigned:
#   - habitat = 0 for every habitat feature column (no contribution to targets)
#   - water  = 0 (no contribution to water savings, for downstream consistency)
#   - revenue = 0, (cost = nominal; so the solver isn't selecting them "for free"
#     in a way that messes with cost reporting; the locked-in constraint
#     forces selection regardless)
#   - is_pa = TRUE for PAs, FALSE for fields (logical, required by
#            add_locked_in_constraints())
#   - id    = unique negative IDs (so they can't collide with field IDs)

cat("\nBuilding combined planning unit layer (fields + PAs)...\n")

# --- Identify habitat & water columns we'll need to zero out for PAs ---
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr|sjkf)_", names(field_data))]
# water columns for downstream consistency
water_cols <- intersect(c("water", "waterUs", "waterAW"), names(field_data))

# --- Add is_pa = FALSE to fields ---
# `add_locked_in_constraints()` requires a logical column, not integer
field_data <- field_data %>%
  mutate(is_pa = FALSE)

# --- Build PA planning units, matching the field schema ---
# Start with a skeleton that has only the geometry and pre-computed acres
# from the topology cleanup
n_pa <- nrow(cpad_in_sjv)

# then add all the columns the field layer carries (zeroed)
pa_data <- cpad_in_sjv %>%
  # acres_pa is already computed in Section 1d topology cleanup, so keep it
  # Drop everything else we don't need
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
  # rename for joining
  rename(geom = geometry) %>% 
  select(-acres_pa)

# Add field-only attributes as NA so rbind() preserves them
# We'll also use these for plotting/post-prioritizr analyses
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

# Add zeroed water columns (if present in the field layer)
for (col in water_cols) {
  pa_data[[col]] <- 0
}

# --- Align column sets and rbind ---
# Use only columns common to both layers (drops any field-level extras that
# don't apply to PAs and vice versa).
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
# STEP 2: Cost & Habitat Column Prep
# =============================================================================

# Replace NAs in habitat columns with 0 (PAs already 0 by construction)
pu <- pu %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))

# Cost: revenue for fields, nominal $1 for PAs (locked in anyway, but a
# strictly positive cost avoids weird solver behavior)
pu <- pu %>%
  mutate(
    cost_raw = case_when(
      is_pa                                     ~ 1,                # nominal (PA, locked in)
      is.na(revenue) | revenue <= 0             ~ 1,                # nominal
      TRUE                                      ~ revenue
    )
  )

# Scale costs by a factor of 10,000
# prioritizr can act weird when costs are too high, and we have high costs
cost_scale_factor <- 1e4
pu <- pu %>%
  mutate(cost = cost_raw / cost_scale_factor)

cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(pu$cost), 4), ",",
    round(max(pu$cost), 2), "]\n")


# =============================================================================
# STEP 2b: Boundary Matrix (Fields + PAs)
# =============================================================================
# Build the boundary matrix on the FULL planning unit set (fields + PAs).

# This is what allows the BLM penalty to reward retiring fields adjacent to
# locked-in PAs.

cat("\nBuilding boundary matrix on fields + PAs...\n")
bm <- boundary_matrix(pu)
cat("  Boundary matrix dimensions:", dim(bm), "\n")


# =============================================================================
# STEP 3: Define the 2 Cross-Temporal Optimization Problems
# =============================================================================

# 25,000 ac habitat target for each feature
habitat_target <- 25000

# Suitable habitat features (15 total)
suit_features <- c(
  "bnll_base_suit",      "bnll_rcp45_2049_suit", "bnll_rcp45_2069_suit",
  "bnll_rcp85_2049_suit", "bnll_rcp85_2069_suit",
  "gkr_base_suit",       "gkr_rcp45_2049_suit",  "gkr_rcp45_2069_suit",
  "gkr_rcp85_2049_suit",  "gkr_rcp85_2069_suit",
  "sjkf_base_suit",      "sjkf_rcp45_2049_suit", "sjkf_rcp45_2069_suit",
  "sjkf_rcp85_2049_suit", "sjkf_rcp85_2069_suit"
)

# High quality habitat features (15 total)
hq_features <- c(
  "bnll_base_hq",      "bnll_rcp45_2049_hq", "bnll_rcp45_2069_hq",
  "bnll_rcp85_2049_hq", "bnll_rcp85_2069_hq",
  "gkr_base_hq",       "gkr_rcp45_2049_hq",  "gkr_rcp45_2069_hq",
  "gkr_rcp85_2049_hq",  "gkr_rcp85_2069_hq",
  "sjkf_base_hq",      "sjkf_rcp45_2049_hq", "sjkf_rcp45_2069_hq",
  "sjkf_rcp85_2049_hq", "sjkf_rcp85_2069_hq"
)

# Targets: 25,000 acres for each feature
suit_targets <- rep(habitat_target, length(suit_features))
hq_targets   <- rep(habitat_target, length(hq_features))

# Scenario metadata
scenarios <- tibble(
  scenario_name = c("cross_temporal_suit", "cross_temporal_hq"),
  quality       = c("Suitable", "High Quality"),
  features      = list(suit_features, hq_features),
  targets       = list(suit_targets, hq_targets)
)

# Verify columns exist
stopifnot(all(suit_features %in% names(pu)))
stopifnot(all(hq_features %in% names(pu)))


# =============================================================================
# STEP 4: Quick Diagnostic — Available Habitat & PA Locked-In Summary
# =============================================================================

# This just makes sure we have enough habitat available on PUs to meet the 
# defined habitat targets

cat("\n--- Available habitat on field PUs (PAs contribute 0) (acres) ---\n")
cat("--- Field PUs:", sum(!pu$is_pa),
    "(fallow:", sum(!pu$is_pa & pu$fallow == 1),
    "| cultivated:", sum(!pu$is_pa & pu$fallow == 0), ") ---\n")
cat("--- PA PUs (locked in):", sum(pu$is_pa), "---\n")
cat("--- Target per feature: 25,000 acres ---\n\n")

cat("SUITABLE HABITAT FEATURES:\n")
for (feat in suit_features) {
  avail <- sum(pu[[feat]], na.rm = TRUE)  # PAs are 0 so this is field-only habitat
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nHIGH QUALITY HABITAT FEATURES:\n")
for (feat in hq_features) {
  avail <- sum(pu[[feat]], na.rm = TRUE)
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nTotal field-PU revenue: $",
    format(sum(pu$revenue[!pu$is_pa], na.rm = TRUE), big.mark = ","), "\n")
cat("Total field-PU acres:   ",
    format(round(sum(pu$acres[!pu$is_pa], na.rm = TRUE)), big.mark = ","), "\n")
cat("Total PA acres (locked in):",
    format(round(sum(pu$acres[pu$is_pa], na.rm = TRUE)), big.mark = ","), "\n")


# =============================================================================
# STEP 5: BLM Calibration (Suitable Habitat Problem, with locked-in PAs)
# =============================================================================
# Calibrate BLM on the suitable habitat problem with PAs locked in. The
# locked-in constraint is part of the calibration so the chosen BLM reflects
# the same constraint structure used in the final runs.

cat("\n========== BLM CALIBRATION (cross-temporal suitable, w/ locked-in PAs) ==========\n\n")

blm_values <- c(0, 0.001, 0.003, 0.005, 0.0075, 0.01, 0.05)

blm_results <- tibble(
  blm         = numeric(),
  n_selected  = numeric(),  # excluding locked-in PAs
  total_cost  = numeric(),
  total_acres = numeric(),
  boundary    = numeric()
)

for (blm_val in blm_values) {
  
  cat("  Solving BLM =", blm_val, "... ")
  
  # define the calibration problem
  p_cal <- problem(
    x            = pu,
    features     = suit_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = suit_targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    add_boundary_penalties(penalty = blm_val, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, numeric_focus = TRUE)
  
  # solve the calibration problem
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

# Plot BLM tradeoff
blm_plot <- ggplot(blm_results, aes(x = total_cost, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length (with locked-in PAs)",
    subtitle = "Calibrated on cross-temporal suitable habitat problem (15 features)",
    x        = "Total Revenue Cost ($)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)

# --- SELECT THE BEST BLM ---
# this is the "inflection" point (elbow) on the curve
chosen_blm <- 0.005

cat("\nChosen BLM:", chosen_blm, "\n")


# =============================================================================
# STEP 6: Run Both Cross-Temporal Optimizations
# =============================================================================

cat("\n========== RUNNING 2 CROSS-TEMPORAL OPTIMIZATIONS (w/ locked-in PAs) ==========\n\n")

solutions <- list()

summary_all <- tibble(
  scenario_name   = character(),
  quality         = character(),
  n_features      = numeric(),
  n_pa_locked_in  = numeric(),
  n_selected      = numeric(),  # excluding PAs
  total_cost      = numeric(),
  total_acres     = numeric(),
  total_water_AW  = numeric(),
  boundary        = numeric()
)

for (i in 1:nrow(scenarios)) {
  
  scen     <- scenarios$scenario_name[i]
  quality  <- scenarios$quality[i]
  feats    <- scenarios$features[[i]]
  targs    <- scenarios$targets[[i]]
  
  cat(sprintf("--- Scenario %d/2: %s (%s) ---\n", i, scen, quality))
  cat(sprintf("    Features: %d (BNLL + GKR + SJKF × 5 time periods)\n", length(feats)))
  cat(sprintf("    Target: %s acres per feature\n", format(habitat_target, big.mark = ",")))
  cat(sprintf("    Locked-in PAs: %d\n", sum(pu$is_pa)))
  
  # build problem
  p <- problem(
    x            = pu,
    features     = feats,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = targs) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    add_boundary_penalties(penalty = chosen_blm, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, numeric_focus = TRUE)
  
  # solve problem
  sol <- tryCatch(
    solve(p, run_checks = FALSE),
    error = function(e) {
      cat("    FAILED:", e$message, "\n\n")
      return(NULL)
    }
  )
  
  if (is.null(sol)) next
  
  # selected NEW retirements (exclude PAs)
  sel <- sol %>% 
    filter(solution_1 == 1, !is_pa)
  
  # boundary length on the full solution
  boundary_length <- eval_boundary_summary(p, sol[, "solution_1"])$boundary
  
  # store solution
  solutions[[scen]] <- sol
  
  # append summary
  summary_all <- summary_all %>%
    add_row(
      scenario_name   = scen,
      quality         = quality,
      n_features      = length(feats),
      n_pa_locked_in  = sum(pu$is_pa),
      n_selected      = nrow(sel),
      total_cost      = sum(sel$revenue, na.rm = TRUE),
      total_acres     = sum(sel$acres, na.rm = TRUE),
      total_water_AW  = if ("waterAW" %in% names(sel)) sum(sel$waterAW, na.rm = TRUE) else NA_real_,
      boundary        = boundary_length
    )
  
  cat(sprintf("    Selected (new): %d fields | Cost: $%s | Acres: %s\n",
              nrow(sel),
              format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel$acres, na.rm = TRUE)), big.mark = ",")))
  
  # per-feature target achievement (PAs contribute 0 by construction)
  cat("\n    --- Target Achievement ---\n")
  for (feat in feats) {
    achieved <- sum(sel[[feat]], na.rm = TRUE)
    met <- ifelse(achieved >= habitat_target, "YES", "NO")
    cat(sprintf("    %-30s: %10.1f / %s acres [%s]\n",
                feat, achieved, format(habitat_target, big.mark = ","), met))
  }
  cat(sprintf("\n    Boundary: %.1f\n\n", boundary_length))
}


# =============================================================================
# STEP 7: Compare the Two Solutions
# =============================================================================

cat("\n========== CROSS-TEMPORAL SOLUTION COMPARISON ==========\n\n")
print(summary_all, width = Inf)


# --- Detailed target achievement for both problems ---
target_detail <- tibble(
  scenario = character(),
  quality  = character(),
  feature  = character(),
  species  = character(),
  climate  = character(),
  target   = numeric(),
  achieved = numeric()
)

for (i in 1:nrow(scenarios)) {
  scen  <- scenarios$scenario_name[i]
  feats <- scenarios$features[[i]]
  
  if (!(scen %in% names(solutions))) next
  # Field-only achievement (PAs contribute 0)
  sel <- solutions[[scen]] %>% filter(solution_1 == 1, !is_pa)
  
  for (feat in feats) {
    target_detail <- target_detail %>%
      add_row(
        scenario = scen,
        quality  = scenarios$quality[i],
        feature  = feat,
        species  = case_when(
          grepl("^bnll", feat) ~ "BNLL",
          grepl("^gkr", feat)  ~ "GKR",
          grepl("^sjkf", feat) ~ "SJKF"
        ),
        climate  = case_when(
          grepl("base", feat)       ~ "Baseline",
          grepl("rcp45_2049", feat) ~ "RCP 4.5 (2020-2049)",
          grepl("rcp45_2069", feat) ~ "RCP 4.5 (2040-2069)",
          grepl("rcp85_2049", feat) ~ "RCP 8.5 (2020-2049)",
          grepl("rcp85_2069", feat) ~ "RCP 8.5 (2040-2069)"
        ),
        target   = habitat_target,
        achieved = sum(sel[[feat]], na.rm = TRUE)
      )
  }
}

cat("\n--- Detailed Target Achievement (field PUs only; PAs contribute 0) ---\n")
print(target_detail, n = Inf)


# =============================================================================
# SECTION 8: Export Results
# =============================================================================

# Output directory
out_dir <- here("data/intermediate/9_1_prioritizr_habitat_only_PAs")

# --- Save objects needed for figures ---
save(
  solutions,
  summary_all,
  target_detail,
  scenarios,
  suit_features,
  hq_features,
  pu,                  # combined planning unit layer (fields + PAs)
  field_data,
  field_data_all,
  pa_data,
  sjv_basins,
  habitat_target,
  chosen_blm,
  blm_results,
  bm,
  cost_scale_factor,
  file = file.path(out_dir, "prioritizr_cross_temporal_PAs_results.RData")
)

# --- Export individual solutions as geopackages ---
# Solutions retain the full PU layer (fields + PAs) with solution_1 and is_pa
# columns so downstream figures can distinguish locked-in PAs from newly
# retired fields.
for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  # attach crop/comm classification to fields, leave PAs as-is
  if (exists("convertCommRetired")) {
    sol_classified <- tryCatch(
      convertCommRetired(sol) %>% dplyr::select(-any_of("crop")),
      error = function(e) sol
    )
  } else {
    sol_classified <- sol
  }
  
  write_sf(sol_classified,
           file.path(out_dir, paste0("solution_", scen, ".gpkg")))
}

# --- Export summary tables ---
write_csv(summary_all,    file.path(out_dir, "cross_temporal_PAs_comparison.csv"))
write_csv(target_detail,  file.path(out_dir, "cross_temporal_PAs_target_detail.csv"))
write_csv(blm_results,    file.path(out_dir, "cross_temporal_PAs_blm_calibration.csv"))

























