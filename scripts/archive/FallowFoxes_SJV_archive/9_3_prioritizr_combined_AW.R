# =============================================================================
# PRIORITIZR COMBINED OPTIMIZATION: WATER SAVINGS + HABITAT CREATION
# APPLIED WATER (AW) VERSION
# =============================================================================
# Purpose: Identify optimal field retirement configurations that simultaneously
#          meet cross-temporal habitat targets AND applied water (AW) savings 
#          targets while minimizing foregone agricultural revenue.
#
# PURPOSE OF THIS SCRIPT (vs. 9_3_prioritizr_combined.R):
#   Parallel analysis to the S_net combined version, using per-field annual
#   applied water (AW) estimates as the water feature instead of derived 
#   S_net consumption estimates. Intended as a secondary/SI-friendly 
#   formulation that relies only on widely understood AW accounting.
#
# Approach: For each climate scenario, combine the 15 cross-temporal habitat 
#           features (BNLL + GKR + SJKF × 5 time periods) with 1 water savings
#           feature (AW for that scenario) = 16 features per problem.
#           This runs as 6 separate optimizations:
#             3 water scenarios × 2 habitat quality levels
#
# Scenarios:
#   Water: Baseline, RCP4.5 (2020-2049), RCP8.5 (2020-2049)
#   Habitat quality: Suitable, High Quality
#
# Water savings targets (parallel to 9_2_prioritizr_water_only_AW.R):
#   Valley-wide AW reduction targets from PPIC with climate adjustment:
#     Baseline: 2,675 TAF (PPIC-derived, sum of 15 basin-specific targets 
#                          from the "Corrected reduction in groundwater
#                          supplies no negative" field, Escriva-Bou et al. 2023)
#     RCP4.5:   Baseline × 1.063 (PPIC 6.3% increase from 2.68 to 2.85 MAF)
#     RCP8.5:   Baseline × (1 + 0.063 × BCM PET scaling factor)
#
# Habitat targets:
#   25,000 acres per habitat feature (15 features, cross-temporal)
#
# Cost layer: revenue per field (annual foregone revenue)
# Planning units: non-retired agricultural fields (retired == 0)
# Decision variable: binary (retire or not)
# BLM: re-calibrated on the Baseline + Suitable habitat problem (AW version)
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
# SECTION 1c: Define Planning Units
# =============================================================================
# Planning units: non-retired fields within the 15 SJV groundwater basins.

candidate_ids <- field_data_all %>%
  filter(retired == 0, !is.na(basin)) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% candidate_ids)


# --- Boundary matrix ---
blm_shapes <- field_data_all %>%
  filter(id %in% candidate_ids)

stopifnot(nrow(field_data) == nrow(blm_shapes))
stopifnot(all(field_data$id == blm_shapes$id))

bm <- boundary_matrix(blm_shapes)


# =============================================================================
# SECTION 2: Prepare Cost, Habitat, and Water Columns
# =============================================================================

# --- Cost: revenue scaled to ten-thousands of USD ---
cost_scale_factor <- 1e4

field_data <- field_data %>%
  mutate(
    cost_raw = ifelse(is.na(revenue) | revenue <= 0, 1, revenue),
    cost = cost_raw / cost_scale_factor
  )

cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(field_data$cost), 4), ",",
    round(max(field_data$cost), 2), "]\n")


# --- Habitat: replace NAs with 0 ---
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr|sjkf)_", names(field_data))]
field_data <- field_data %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))


# --- Water: scale AW from AF to TAF ---
field_data <- field_data %>%
  mutate(
    AW_baseline_TAF   = AW_baseline_AF   / 1000,
    AW_RCP45_near_TAF = AW_RCP45_near_AF / 1000,
    AW_RCP85_near_TAF = AW_RCP85_near_AF / 1000
  )


# --- Diagnostics ---
cat("\nTotal fields (all):", nrow(field_data_all), "\n")
cat("Planning units (non-retired, within 15 basins):", nrow(field_data), "\n")
cat("  - Fallow:", sum(field_data$fallow == 1), "\n")
cat("  - Cultivated:", sum(field_data$fallow == 0), "\n")
cat("Retired fields (excluded):", sum(field_data_all$retired == 1), "\n")
cat("Fields outside 15 basins (excluded):", sum(is.na(field_data_all$basin) & field_data_all$retired == 0), "\n")
cat("Boundary matrix dimensions:", dim(bm), "\n")


# =============================================================================
# SECTION 3: Define Water Savings Targets
# =============================================================================
# Climate-adjusted AW reduction targets following the PPIC/BCM methodology
# established in the AW water-only analysis (9_2_prioritizr_water_only_AW.R).
#
# Baseline valley-wide target (2,675 TAF) comes from the PPIC technical 
# appendix dataset (Escriva-Bou et al. 2023), summing the subunit-scale 
# "Corrected reduction in groundwater supplies no negative" field to the 
# 15 SJV groundwater basins.
#
# RCP4.5 scaling: +6.3% from PPIC's published SGMA + climate change target 
# (2.68 -> 2.85 MAF) under median RCP4.5 conditions.
#
# RCP8.5 scaling: RCP4.5 increment scaled by the ratio of ensemble mean PET 
# change between RCP scenarios in the BCMv8 projections.

# --- Valley-wide baseline target from PPIC dataset ---
# Sum of basin-specific AW reduction targets (see 9_2 script for breakdown)
valley_aw_reduction_taf <- 2675.3  # = sum(721.9, 340.9, 210.5, ..., 6.5, 5.2)

# --- Derive RCP8.5 scaling factor from BCM PET data ---
nonretired_df <- field_data %>% st_drop_geometry()

mean_pet_ratio_rcp45 <- weighted.mean(nonretired_df$PET_ratio_RCP45_near,
                                      nonretired_df$acres, na.rm = TRUE)
mean_pet_ratio_rcp85 <- weighted.mean(nonretired_df$PET_ratio_RCP85_near,
                                      nonretired_df$acres, na.rm = TRUE)
rcp85_scaling <- (mean_pet_ratio_rcp85 - 1) / (mean_pet_ratio_rcp45 - 1)

cat("\nRCP8.5 scaling factor derivation (from BCMv8 PET):\n")
cat("  Mean PET ratio RCP4.5:", round(mean_pet_ratio_rcp45, 4), "\n")
cat("  Mean PET ratio RCP8.5:", round(mean_pet_ratio_rcp85, 4), "\n")
cat("  RCP8.5/RCP4.5 scaling:", round(rcp85_scaling, 2), "\n")

# --- Climate multipliers ---
ppic_rcp45_increase <- 0.063  # 6.3% (PPIC: 2.68 -> 2.85 MAF)
ppic_rcp85_increase <- ppic_rcp45_increase * rcp85_scaling

multiplier_rcp45 <- 1 + ppic_rcp45_increase
multiplier_rcp85 <- 1 + ppic_rcp85_increase

# --- Valley-wide targets (TAF) ---
water_target_baseline_taf <- valley_aw_reduction_taf
water_target_rcp45_taf    <- valley_aw_reduction_taf * multiplier_rcp45
water_target_rcp85_taf    <- valley_aw_reduction_taf * multiplier_rcp85

cat("\nValley-wide AW savings targets (TAF):\n")
cat("  Baseline:          ", round(water_target_baseline_taf, 1), "\n")
cat("  RCP4.5 (2020-2049):", round(water_target_rcp45_taf, 1),
    " (multiplier:", round(multiplier_rcp45, 3), ")\n")
cat("  RCP8.5 (2020-2049):", round(water_target_rcp85_taf, 1),
    " (multiplier:", round(multiplier_rcp85, 3), ")\n")


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
  water_label  = c("Baseline", "RCP45", "RCP85"),
  aw_col       = c("AW_baseline_TAF", "AW_RCP45_near_TAF", "AW_RCP85_near_TAF"),
  water_target = c(water_target_baseline_taf, water_target_rcp45_taf, water_target_rcp85_taf)
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
all_feature_cols <- c(suit_features, hq_features, water_scenarios$aw_col)
missing <- setdiff(all_feature_cols, names(field_data))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
cat("\nAll feature columns verified.\n")


# =============================================================================
# SECTION 5: Diagnostic — Check Feature Availability
# =============================================================================

cat("\n========== FEATURE AVAILABILITY ON PLANNING UNITS ==========\n")
cat("Planning units:", nrow(field_data), "\n\n")

cat("HABITAT FEATURES (target: 25,000 acres each):\n")
for (feat in c(suit_features, hq_features)) {
  avail <- sum(field_data[[feat]], na.rm = TRUE)
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nWATER FEATURES (AW in TAF):\n")
for (i in 1:nrow(water_scenarios)) {
  col <- water_scenarios$aw_col[i]
  avail <- sum(field_data[[col]], na.rm = TRUE)
  target <- water_scenarios$water_target[i]
  cat(sprintf("  %-25s: %8.1f TAF available | target: %8.1f TAF (%s)\n",
              col, avail, target, ifelse(avail >= target, "OK", "!!")))
}

cat("\nTotal planning unit revenue: $",
    format(sum(field_data$revenue, na.rm = TRUE), big.mark = ","), "\n")
cat("Total planning unit acres:",
    format(sum(field_data$acres, na.rm = TRUE), big.mark = ","), "\n")


# =============================================================================
# SECTION 6: BLM Calibration (Baseline + Suitable Problem)
# =============================================================================
# Recalibrate BLM for the AW-based combined problem. Calibrated on the 
# Baseline + Suitable problem (16 features). The chosen BLM will be applied 
# to all 6 problems.
#
# Note: The S_net combined script uses BLM = 0.005, but because the water 
# feature and target here are different (AW vs S_net), the cost-boundary 
# trade-off curve may differ. Inspect the printed calibration plot and 
# resulting table, then set `chosen_blm` below.

cat("\n========== BLM CALIBRATION (Baseline + Suitable, 16 features) ==========\n\n")

# Build feature list and targets for calibration problem
cal_features <- c(suit_features, "AW_baseline_TAF")
cal_targets  <- c(rep(habitat_target, length(suit_features)),
                  water_target_baseline_taf)

blm_values <- c(0, 0.0001, 0.0005, 0.001, 0.003, 0.005, 0.01, 0.05)

blm_results <- tibble(
  blm         = numeric(),
  n_selected  = numeric(),
  total_cost  = numeric(),
  total_acres = numeric(),
  boundary    = numeric()
)

for (blm_val in blm_values) {
  
  cat("  Solving BLM =", blm_val, "... ")
  
  p_cal <- problem(
    x            = field_data,
    features     = cal_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = cal_targets) %>%
    add_binary_decisions() %>%
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
    sel <- sol_cal %>% filter(solution_1 == 1)
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
    title    = "BLM Calibration: Cost vs. Boundary Length (AW version)",
    subtitle = "Calibrated on combined Baseline + Suitable problem (16 features)",
    x        = "Total Revenue Cost ($)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)

# --- SELECT THE BEST BLM ---
# Inspect the plot above and set the "elbow" value.
# Starting value matches the S_net version; revise after inspecting the plot.
chosen_blm <- 0.005

cat("\nChosen BLM:", chosen_blm, "\n")


# =============================================================================
# SECTION 7: Run All 6 Combined Optimizations
# =============================================================================

cat("\n========== RUNNING 6 COMBINED OPTIMIZATIONS ==========\n\n")

solutions <- list()

summary_all <- tibble(
  scenario_name    = character(),
  water_scenario   = character(),
  quality          = character(),
  n_features       = numeric(),
  n_selected       = numeric(),
  total_cost       = numeric(),
  total_acres      = numeric(),
  total_AW_AF      = numeric(),
  boundary         = numeric()
)

for (i in 1:nrow(scenarios)) {
  
  scen       <- scenarios$scenario_name[i]
  water_lab  <- scenarios$water_label[i]
  quality    <- scenarios$quality[i]
  hab_feats  <- scenarios$hab_features[[i]]
  aw_col     <- scenarios$aw_col[i]
  water_targ <- scenarios$water_target[i]
  
  # Combine features: 15 habitat + 1 water = 16
  all_features <- c(hab_feats, aw_col)
  all_targets  <- c(rep(habitat_target, length(hab_feats)), water_targ)
  
  cat(sprintf("--- Scenario %d/6: %s ---\n", i, scen))
  cat(sprintf("    Water: %s | Target: %.1f TAF\n", water_lab, water_targ))
  cat(sprintf("    Habitat: %s | 15 features x %s acres\n",
              quality, format(habitat_target, big.mark = ",")))
  cat(sprintf("    Total features: %d\n", length(all_features)))
  
  # Build problem
  p <- problem(
    x            = field_data,
    features     = all_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = all_targets) %>%
    add_binary_decisions() %>%
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
  
  # Extract selected fields
  sel <- sol %>% filter(solution_1 == 1)
  
  # Calculate boundary length
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
      n_selected       = nrow(sel),
      total_cost       = sum(sel$revenue, na.rm = TRUE),
      total_acres      = sum(sel$acres, na.rm = TRUE),
      total_AW_AF      = sum(sel[[aw_col]], na.rm = TRUE) * 1000,
      boundary         = boundary_length
    )
  
  # Print summary
  cat(sprintf("    Selected: %d fields | Cost: $%s | Acres: %s | AW: %s AF\n",
              nrow(sel),
              format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel$acres, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel[[aw_col]], na.rm = TRUE) * 1000), big.mark = ",")))
  
  # Per-feature target achievement
  cat("\n    --- Target Achievement ---\n")
  
  # Habitat features
  for (feat in hab_feats) {
    achieved <- sum(sel[[feat]], na.rm = TRUE)
    met <- ifelse(achieved >= habitat_target, "YES", "NO")
    cat(sprintf("    %-30s: %10.1f / %s acres [%s]\n",
                feat, achieved, format(habitat_target, big.mark = ","), met))
  }
  
  # Water feature
  achieved_taf <- sum(sel[[aw_col]], na.rm = TRUE)
  met <- ifelse(achieved_taf >= water_targ, "YES", "NO")
  cat(sprintf("    %-30s: %10.1f / %.1f TAF    [%s]\n",
              aw_col, achieved_taf, water_targ, met))
  
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
  aw_col     <- scenarios$aw_col[i]
  water_targ <- scenarios$water_target[i]
  
  if (!(scen %in% names(solutions))) next
  sel <- solutions[[scen]] %>% filter(solution_1 == 1)
  
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
      feature        = aw_col,
      feature_type   = "water",
      species        = NA_character_,
      climate        = water_lab,
      target         = water_targ,
      target_unit    = "TAF",
      achieved       = sum(sel[[aw_col]], na.rm = TRUE)
    )
}

cat("\n--- Detailed Target Achievement ---\n")
print(target_detail, n = Inf)


# =============================================================================
# SECTION 9: Export Results
# =============================================================================

output_dir <- here("data/intermediate/FallowFoxes_SJV_archive/9_3_prioritizr_combined_AW/")

# --- Save all objects for figures ---
save(
  solutions,
  summary_all,
  target_detail,
  scenarios,
  water_scenarios,
  suit_features,
  hq_features,
  field_data,
  field_data_all,
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
  valley_aw_reduction_taf,
  file = file.path(output_dir, "prioritizr_combined_AW_results.RData")
)

# --- Export individual solutions as geopackages ---
for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  sol_key <- sol %>%
    st_drop_geometry() %>%
    dplyr::select(id, solution_1)
  
  sol_complete <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(sol_key, by = "id") %>%
    mutate(solution_1 = replace_na(solution_1, 0))
  
  sol_classified <- convertCommRetired(sol_complete) %>%
    dplyr::select(-any_of("crop"))
  
  write_sf(sol_classified,
           file.path(output_dir, paste0("solution_", scen, ".gpkg")))
}

# --- Export summary tables ---
write_csv(summary_all,
          file.path(output_dir, "combined_AW_solution_comparison.csv"))
write_csv(target_detail,
          file.path(output_dir, "combined_AW_target_detail.csv"))
write_csv(blm_results,
          file.path(output_dir, "combined_AW_blm_calibration.csv"))

# --- Export target methodology ---
target_methodology <- data.frame(
  parameter = c("PPIC RCP4.5 increase", "BCM RCP8.5 scaling factor",
                "RCP4.5 multiplier", "RCP8.5 multiplier",
                "Mean PET ratio RCP4.5", "Mean PET ratio RCP8.5",
                "Valley baseline AW reduction (TAF)",
                "Valley RCP4.5 target (TAF)", 
                "Valley RCP8.5 target (TAF)",
                "Habitat target (acres per feature)",
                "PPIC reported valley baseline (TAF)",
                "Chosen BLM"),
  value = c(round(ppic_rcp45_increase, 4), round(rcp85_scaling, 2),
            round(multiplier_rcp45, 4), round(multiplier_rcp85, 4),
            round(mean_pet_ratio_rcp45, 4), round(mean_pet_ratio_rcp85, 4),
            round(valley_aw_reduction_taf, 1),
            round(water_target_rcp45_taf, 1), 
            round(water_target_rcp85_taf, 1),
            habitat_target,
            2680,
            chosen_blm)
)

write_csv(target_methodology, file.path(output_dir, "combined_AW_target_methodology.csv"))


cat("\n========== COMBINED AW OPTIMIZATION COMPLETE — RESULTS SAVED ==========\n")
cat("Output directory:", output_dir, "\n")
cat("Load results for figures with:\n")
cat('  load(here("data/intermediate/FallowFoxes_SJV_archive/9_3_prioritizr_combined_AW/prioritizr_combined_AW_results.RData"))\n')






















