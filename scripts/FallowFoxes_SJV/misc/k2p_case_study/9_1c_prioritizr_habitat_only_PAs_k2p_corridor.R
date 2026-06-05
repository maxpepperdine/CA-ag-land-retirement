# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS-TEMPORAL) — KERN-PIXLEY CORRIDOR CASE STUDY
# =============================================================================
# Purpose: Apply the cross-temporal, habitat-only, locked-in-PA optimization
#          framework to a targeted case study: a conceptual restoration
#          corridor connecting Kern and Pixley National Wildlife Refuges
#          (extending slightly north of Pixley NWR).
#
# Approach: Identical to 9_1_prioritizr_habitat_only_PAs.R, with one change:
#           planning units are restricted to non-retired fields and PAs
#           within (or touching) the TNC conceptual corridor polygon instead
#           of the full 15 SJV groundwater basins.
#
#           - Field PUs:  non-retired fields intersecting the
#                         corridor polygon.
#           - PA PUs:     CPAD Holdings that intersect or touch the corridor
#                         border of the corridor
#
#           All other framework choices preserved:
#             * Cross-temporal structure (2 problems × 15 features each)
#             * 25,000-acre target per feature (matches the basin-wide run)
#             * PAs zeroed for habitat and water, locked in
#             * Cost scale 1e4, BLM = 0.005 (calibrated previously; this
#               script also re-runs the calibration on the corridor PU set
#               for completeness — the corridor elbow may sit elsewhere)
#             * Gurobi with numeric_focus = TRUE, run_checks = FALSE
#
# Two optimization problems:
#   1. Suitable habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
#   2. High quality habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
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

# --- Planning units: SJV fields with habitat extractions ---
field_data_all <- read_sf(here("data/intermediate/7_habitatExtraction/sjvHabitatExtractions/sjvHabitatExtractions.gpkg"))

# --- CPAD Holdings (protected areas) ---
cpad_raw <- st_read(here("data/raw/protected_areas/cpad_release_2025b/CPAD_Release_2025b/CPAD_2025b_Holdings/CPAD_2025b_Holdings.shp"))

# --- TNC Kern-Pixley Conceptual Restoration Corridor ---
corridor_raw <- st_read(here("data/raw/tnc_corridor/ConceptualRestorationArea.shp"))


# =============================================================================
# SECTION 1b: Prepare the Corridor Polygon
# =============================================================================
# Match the corridor CRS to the fields layer, dissolve to a single
# multipolygon (handles multi-feature shapefiles), and validate geometry.

cat("Preparing the Kern-Pixley conceptual corridor polygon...\n")

corridor <- corridor_raw %>%
  st_transform(st_crs(field_data_all)) %>%
  st_make_valid()

# Dissolve to a single multipolygon for the intersects/within tests
corridor_union <- st_union(corridor) %>% st_make_valid()

cat("  Corridor features (raw):", nrow(corridor_raw), "\n")
cat("  Corridor area:",
    format(round(as.numeric(st_area(corridor_union)) / 4046.8564224),
           big.mark = ","), "acres\n")


# =============================================================================
# SECTION 1c: Restrict Field Planning Units to the Corridor
# =============================================================================
# Use st_intersects() on the full field geometries to keep any field that is
# within, overlapping, or touching the corridor boundary. This is more
# inclusive than a centroid-based filter — a field is retained if any part
# of its geometry touches the corridor, including fields that straddle the
# boundary or share only an edge with it.

cat("\nAssigning fields to the corridor (st_intersects on field geometries)...\n")

in_corridor <- lengths(st_intersects(field_data_all, corridor_union)) > 0

field_data_all$in_corridor <- in_corridor

cat("  Fields intersecting/touching the corridor:", sum(in_corridor), "\n")
cat("  Fields outside the corridor (dropped):", sum(!in_corridor), "\n")


# --- Subset to non-retired fields within the corridor ---
candidate_ids <- field_data_all %>%
  filter(retired == 0, in_corridor) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% candidate_ids)

cat("\nField planning units (non-retired, in corridor):", nrow(field_data), "\n")
cat("  - Fallow:", sum(field_data$fallow == 1), "\n")
cat("  - Cultivated:", sum(field_data$fallow == 0), "\n")


# =============================================================================
# SECTION 1d: Filter Protected Areas to the Corridor
# =============================================================================
# Keep PAs that are within, intersecting, OR touching the corridor border
# using st_intersects()
#
# Then resolve overlap topology exactly as in the basin-wide script:
#   (a) Dissolve PAs into a single union (removes PA-PA overlap)
#   (b) Erase the field footprint from the PA union (removes PA-field overlap)
#   (c) Cast to single-part polygons
#   (d) Drop sliver polygons < 1 acre
#
# Field geometry is NEVER altered.

cat("\nFiltering CPAD Holdings to the corridor...\n")

# Match CPAD to fields CRS
cpad <- st_transform(cpad_raw, st_crs(field_data))
cat("  CPAD Holdings (statewide):", nrow(cpad), "\n")

# Keep PAs that intersect the corridor (covers within / overlapping / touching)
cpad_in_corridor <- cpad %>%
  filter(lengths(st_intersects(., corridor_union)) > 0)

cat("  CPAD Holdings retained (intersecting/touching corridor):",
    nrow(cpad_in_corridor), "\n")

# Quick look at which named PAs are locked in (Kern & Pixley NWRs expected).
# CPAD Holdings carry several name fields; print the most informative ones
# that exist.
pa_name_cols <- intersect(c("UNIT_NAME", "HOLDING_NA", "MNG_AGNCY", "MNG_AG_LEV"),
                          names(cpad_in_corridor))
if (length(pa_name_cols) > 0) {
  cat("\n  PA holdings locked in (top by area):\n")
  print(cpad_in_corridor %>%
          st_drop_geometry() %>%
          mutate(acres = as.numeric(st_area(cpad_in_corridor)) / 4046.8564224) %>%
          select(all_of(pa_name_cols), acres) %>%
          arrange(desc(acres)) %>%
          head(15))
}


# --- Resolve PA overlap topology (same logic as basin-wide script) ---
cat("\nResolving PA overlap topology (preserving field geometry)...\n")

# (a) Dissolve all PAs into a single multipolygon
pa_union <- st_union(cpad_in_corridor) %>% st_make_valid()

# (b) Erase the field footprint from the PA union
field_union <- st_union(field_data) %>% st_make_valid()
pa_clean <- st_difference(pa_union, field_union) %>% st_make_valid()

# (c) Cast to single-part polygons
pa_patches <- st_cast(pa_clean, "POLYGON", warn = FALSE)

# (d) Drop tiny slivers (< 1 acre)
pa_patches_sf <- st_sf(geometry = pa_patches) %>%
  mutate(acres_pa = as.numeric(st_area(.)) / 4046.8564224) %>%
  filter(acres_pa >= 1)

cat("  PA patches after topology cleanup:", nrow(pa_patches_sf), "\n")
cat("  Total PA acres (after cleanup):",
    format(round(sum(pa_patches_sf$acres_pa)), big.mark = ","), "\n")

# Replace cpad_in_corridor with the cleaned patch layer for downstream use
cpad_in_corridor <- pa_patches_sf


# =============================================================================
# SECTION 1e: Build the Combined Planning Unit Layer (Fields + PAs)
# =============================================================================
# Append PAs to the field planning units with:
#   - habitat = 0 for every habitat feature column
#   - water  = 0 (downstream consistency)
#   - revenue = 0, cost = nominal (locked in regardless)
#   - is_pa = TRUE (logical, required by add_locked_in_constraints())
#   - id    = unique negative IDs to avoid collision with field IDs

cat("\nBuilding combined planning unit layer (fields + PAs)...\n")

# Identify habitat & water columns to zero out for PAs
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr|sjkf)_", names(field_data))]
water_cols       <- intersect(c("water", "waterUs", "waterAW"), names(field_data))

# Add is_pa = FALSE to fields
field_data <- field_data %>%
  mutate(is_pa = FALSE)

# Build PA planning units matching the field schema
n_pa <- nrow(cpad_in_corridor)

pa_data <- cpad_in_corridor %>%
  select(acres_pa) %>%
  mutate(
    id        = -seq_len(n_pa),       # negative IDs to avoid collision
    is_pa     = TRUE,
    fallow    = 0L,
    retired   = 0L,
    revenue   = 0,
    acres     = acres_pa,
    in_corridor = TRUE                # all PAs here are corridor-relevant by filter
  ) %>%
  rename(geom = geometry) %>%
  select(-acres_pa)

# Add field-only attributes as NA so rbind() preserves them
# We'll use the comm variables for plotting/exploring crop classes on selected
# fields.
pa_data <- pa_data %>%
  mutate(
    county    = NA_character_,
    comm      = NA_character_,
    last_comm = NA_character_,
    waterAW   = NA_real_
  )

# Zeroed habitat columns
for (col in habitat_cols_all) {
  pa_data[[col]] <- 0
}

# Zeroed water columns
for (col in water_cols) {
  pa_data[[col]] <- 0
}

# Align column sets and rbind
common_cols <- intersect(names(field_data), names(pa_data))
common_cols <- c(setdiff(common_cols, attr(field_data, "sf_column")),
                 attr(field_data, "sf_column"))

field_data_pu <- field_data %>% select(all_of(common_cols))
pa_data_pu    <- pa_data    %>% select(all_of(common_cols))

pu <- rbind(field_data_pu, pa_data_pu)

cat("  Field planning units:", sum(!pu$is_pa), "\n")
cat("  PA planning units:   ", sum(pu$is_pa), "\n")
cat("  Total planning units:", nrow(pu), "\n")


# =============================================================================
# SECTION 2: Cost & Habitat Column Prep
# =============================================================================

# Replace NAs in habitat columns with 0
pu <- pu %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))

# Cost: revenue for fields, nominal $1 for PAs (locked in, but strictly
# positive cost avoids degenerate solver behavior)
pu <- pu %>%
  mutate(
    cost_raw = case_when(
      is_pa                                     ~ 1,
      is.na(revenue) | revenue <= 0             ~ 1,
      TRUE                                      ~ revenue
    )
  )

# Scale costs by 10,000 (same as basin-wide script)
cost_scale_factor <- 1e4
pu <- pu %>%
  mutate(cost = cost_raw / cost_scale_factor)

cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(pu$cost), 4), ",",
    round(max(pu$cost), 2), "]\n")


# =============================================================================
# SECTION 2b: Boundary Matrix (Fields + PAs)
# =============================================================================
# Build the boundary matrix on the FULL corridor PU set so the BLM penalty
# can reward retiring fields adjacent to locked-in PAs (Kern/Pixley NWRs and
# any other CPAD holdings touching the corridor).

cat("\nBuilding boundary matrix on fields + PAs...\n")
bm <- boundary_matrix(pu)
cat("  Boundary matrix dimensions:", dim(bm), "\n")


# =============================================================================
# SECTION 3: Define the 2 Cross-Temporal Optimization Problems
# =============================================================================

habitat_target <- 25000

# Suitable habitat features (15 total: 3 species × 5 climate periods)
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

# Targets: 25,000 acres per feature
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
stopifnot(all(hq_features   %in% names(pu)))


# =============================================================================
# SECTION 4: Diagnostic — Available Habitat & PA Locked-In Summary
# =============================================================================
# IMPORTANT: The corridor is a much smaller PU pool than the full 15 SJV
# basins. Some features may not have 25,000 acres of available habitat on
# field PUs within the corridor. The diagnostic below flags any feature
# where the target is unachievable, so you can decide whether to (a) reduce
# the target for this case study, (b) expand the corridor, or (c) report
# infeasibility as a finding.

cat("\n--- Available habitat on field PUs (PAs contribute 0) (acres) ---\n")
cat("--- Field PUs:", sum(!pu$is_pa),
    "(fallow:", sum(!pu$is_pa & pu$fallow == 1),
    "| cultivated:", sum(!pu$is_pa & pu$fallow == 0), ") ---\n")
cat("--- PA PUs (locked in):", sum(pu$is_pa), "---\n")
cat("--- Target per feature: 25,000 acres ---\n\n")

cat("SUITABLE HABITAT FEATURES:\n")
for (feat in suit_features) {
  avail <- sum(pu[[feat]], na.rm = TRUE)
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
# SECTION 5: BLM Calibration (Suitable Habitat Problem, with locked-in PAs)
# =============================================================================
# Re-run the calibration on the corridor PU set. The elbow on a small,
# spatially-constrained PU pool may differ from the basin-wide run, so it's
# worth inspecting the curve before defaulting to 0.005.

cat("\n========== BLM CALIBRATION (corridor, cross-temporal suitable, w/ locked-in PAs) ==========\n\n")

blm_values <- c(0, 0.001, 0.003, 0.005, 0.0075, 0.01, 0.05)

k_neighbors <- 5

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
    features     = suit_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = suit_targets) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    add_neighbor_constraints(k = k_neighbors) %>% 
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
    # Selected NEW retirements (exclude locked-in PAs)
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

cat("\n--- BLM Calibration Results (corridor) ---\n")
print(blm_results)

# Plot BLM tradeoff
blm_plot <- ggplot(blm_results, aes(x = total_cost, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length (corridor, with locked-in PAs)",
    subtitle = "Calibrated on cross-temporal suitable habitat problem (15 features)",
    x        = "Total Revenue Cost ($)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)

# --- SELECT THE BEST BLM ---
# Look for the "elbow" of the curve
chosen_blm <- 0.0075

cat("\nChosen BLM:", chosen_blm, "\n")


# =============================================================================
# SECTION 6: Run Both Cross-Temporal Optimizations
# =============================================================================

cat("\n========== RUNNING 2 CROSS-TEMPORAL OPTIMIZATIONS (corridor, w/ locked-in PAs) ==========\n\n")

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
  
  # Build problem
  p <- problem(
    x            = pu,
    features     = feats,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = targs) %>%
    add_binary_decisions() %>%
    add_locked_in_constraints(locked_in = "is_pa") %>%
    # add_neighbor_constraints(k = k_neighbors) %>%
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
  
  # Boundary length on the full solution
  boundary_length <- eval_boundary_summary(p, sol[, "solution_1"])$boundary
  
  # Store solution
  solutions[[scen]] <- sol
  
  # Append summary
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
  
  # Per-feature target achievement (PAs contribute 0 by construction)
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
# SECTION 7: Compare the Two Solutions
# =============================================================================

cat("\n========== CORRIDOR CROSS-TEMPORAL SOLUTION COMPARISON ==========\n\n")
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

# Output directory (separate from the basin-wide PA results)
out_dir <- here("data/intermediate/misc/k2p_case_study/9_1c_prioritizr_habitat_only_PAs_k2p_corridor/blm_0.0075")


# --- Save objects needed for figures ---
save(
  solutions,
  summary_all,
  target_detail,
  scenarios,
  suit_features,
  hq_features,
  pu,                  # combined planning unit layer (corridor fields + PAs)
  field_data,
  field_data_all,
  pa_data,
  corridor,
  corridor_union,
  habitat_target,
  chosen_blm,
  #k_neighbors,
  blm_results,
  bm,
  cost_scale_factor,
  file = file.path(out_dir, "prioritizr_corridor_cross_temporal_PAs_results.RData")
)

# --- Export individual solutions as geopackages ---
for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  if (exists("convertCommRetired")) {
    sol_classified <- tryCatch(
      convertCommRetired(sol) %>% dplyr::select(-any_of("crop")),
      error = function(e) sol
    )
  } else {
    sol_classified <- sol
  }
  
  write_sf(sol_classified,
           file.path(out_dir, paste0("solution_corridor_", scen, ".gpkg")))
}

# --- Export the corridor polygon used (for figures / reproducibility) ---
write_sf(corridor, file.path(out_dir, "corridor_used.gpkg"))

# --- Export summary tables ---
write_csv(summary_all,    file.path(out_dir, "corridor_cross_temporal_PAs_comparison.csv"))
write_csv(target_detail,  file.path(out_dir, "corridor_cross_temporal_PAs_target_detail.csv"))
write_csv(blm_results,    file.path(out_dir, "corridor_cross_temporal_PAs_blm_calibration.csv"))


cat("\n========== CORRIDOR CROSS-TEMPORAL OPTIMIZATION (w/ PAs) COMPLETE — RESULTS SAVED ==========\n")
cat("Load results for figures with:\n")
cat('  load(here("data/intermediate/misc/k2p_case_study/9_1c_prioritizr_habitat_only_PAs_k2p_corridor/blm_0.0075/prioritizr_corridor_cross_temporal_PAs_results.RData"))\n')




















