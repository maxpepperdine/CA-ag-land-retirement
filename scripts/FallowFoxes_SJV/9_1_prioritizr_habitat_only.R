# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS TEMPORAL) OPTIMIZATION
# =============================================================================
# Purpose: Use prioritizr to find optimal retirement configurations that
#          meet habitat targets for BNLL, GKR, and SJKF across ALL climate
#          scenarios/time periods simultaneously.
#
# Approach: Instead of running separate optimizations per time period
#           (as in 10_prioritizr_sjv.R), we build problems where every
#           climate projection is a separate feature with a 25,000-acre
#           target. This forces the solver to select fields that provide
#           adequate habitat for all three species under ALL futures —
#           ensuring that land retired today remains suitable as climate shifts.
#
# Two optimization problems:
#   1. Suitable habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
#   2. High quality habitat: 15 features (BNLL + GKR + SJKF × 5 time periods)
#   Each feature has a 25,000-acre target
#
# Cost layer: annual revenue per field (`revenue`)
# Planning units: non-retired agricultural fields (sf polygons) (retired == 0)
# Decision variable: binary (retire or not)
# BLM: calibrated on the suitable habitat problem, then applied to both
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
field_data_all <- read_sf(here("data/intermediate/8_blm/cleanShapes/cleanShapes.gpkg")) %>%
  st_make_valid()

# --- BLM shapes for boundary matrix ---
blm_shapes_all <- read_sf(here("data/intermediate/8_blm/blmShapes/blmShapes.shp"))


# --- Subset to non-retired fields ---
# Planning units include all fallow AND cultivated fields.
# Only permanently retired fields (retired == 1) are excluded.

candidate_ids <- field_data_all %>%
  filter(retired == 0) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% candidate_ids)

blm_shapes <- blm_shapes_all %>%
  filter(id %in% candidate_ids)

# Verify alignment
stopifnot(nrow(field_data) == nrow(blm_shapes))
stopifnot(all(field_data$id == blm_shapes$id))

# Build boundary matrix
bm <- boundary_matrix(blm_shapes)


# Prepare cost and habitat columns
field_data <- field_data %>%
  mutate(
    cost_raw = ifelse(is.na(revenue) | revenue <= 0, 1, revenue)
  )

# Replace NAs in habitat columns with 0
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr|sjkf)_", names(field_data))]
field_data <- field_data %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))


# Scale costs
cost_scale_factor <- 1e4

field_data <- field_data %>%
  mutate(cost = cost_raw / cost_scale_factor)

cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(field_data$cost), 4), ",",
    round(max(field_data$cost), 2), "]\n")
hab_vals <- field_data %>% st_drop_geometry() %>% dplyr::select(all_of(habitat_cols_all))
cat("Habitat range: [", round(min(hab_vals, na.rm = TRUE), 4), ",",
    round(max(hab_vals, na.rm = TRUE), 2), "]\n")


# Quick summary
cat("\nTotal fields (all):", nrow(field_data_all), "\n")
cat("Planning units (non-retired):", nrow(field_data), "\n")
cat("  - Fallow:", sum(field_data$fallow == 1), "\n")
cat("  - Cultivated:", sum(field_data$fallow == 0), "\n")
cat("Retired fields (excluded):", sum(field_data_all$retired == 1), "\n")
cat("Boundary matrix dimensions:", dim(bm), "\n")


# =============================================================================
# SECTION 2: Define the 2 Cross-Temporal Optimization Problems
# =============================================================================

# Each problem includes ALL 5 time periods for all three species = 15 features.
# Every feature gets a 25,000-acre target, so the selected fields must
# simultaneously meet habitat thresholds under all climate projections.

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

# Targets: 25,000 acres for each of the 15 features
suit_targets <- rep(habitat_target, length(suit_features))
hq_targets   <- rep(habitat_target, length(hq_features))

# Scenario metadata for labeling
scenarios <- tibble(
  scenario_name = c("cross_temporal_suit", "cross_temporal_hq"),
  quality       = c("Suitable", "High Quality"),
  features      = list(suit_features, hq_features),
  targets       = list(suit_targets, hq_targets)
)

# Verify columns exist
stopifnot(all(suit_features %in% names(field_data)))
stopifnot(all(hq_features %in% names(field_data)))


# =============================================================================
# SECTION 3: Diagnostic — Check Habitat Availability on Plnning Units
# =============================================================================

cat("\n--- Available habitat on ALL non-retired fields (acres) ---\n")
cat("--- Planning units:", nrow(field_data), "(fallow:", sum(field_data$fallow == 1),
    "| cultivated:", sum(field_data$fallow == 0), ") ---\n")
cat("--- Target per feature: 25,000 acres ---\n\n")

cat("SUITABLE HABITAT FEATURES:\n")
for (feat in suit_features) {
  avail <- sum(field_data[[feat]], na.rm = TRUE)
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nHIGH QUALITY HABITAT FEATURES:\n")
for (feat in hq_features) {
  avail <- sum(field_data[[feat]], na.rm = TRUE)
  cat(sprintf("  %-30s: %10.1f ac (%s)\n",
              feat, avail, ifelse(avail >= habitat_target, "OK", "!!")))
}

cat("\nTotal planning unit revenue: $",
    format(sum(field_data$revenue, na.rm = TRUE), big.mark = ","), "\n")
cat("Total planning unit acres:",
    format(sum(field_data$acres, na.rm = TRUE), big.mark = ","), "\n")


# =============================================================================
# SECTION 4: BLM Calibration (Suitable Habitat Problem)
# =============================================================================

# Calibrate BLM using the suitable habitat problem (15 features).
# The chosen BLM will be applied to both problems.

cat("\n========== BLM CALIBRATION (cross-temporal suitable) ==========\n\n")

blm_values <- c(0, 0.0001, 0.0005, 0.001, 0.0025, 0.005, 0.01, 0.05)

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
    features     = suit_features,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = suit_targets) %>%
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

# Plot BLM tradeoff
blm_plot <- ggplot(blm_results, aes(x = total_cost, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length",
    subtitle = "Calibrated on cross-temporal suitable habitat problem (15 features)",
    x        = "Total Revenue Cost ($)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)

# --- SELECT THE BEST BLM ---
# this is the "elbow" in the cost-boundary tradeoff.
chosen_blm <- 0.0025 

cat("\nChosen BLM:", chosen_blm, "\n")


# =============================================================================
# SECTION 5: Run Both Cross-Temporal Optimizations
# =============================================================================

cat("\n========== RUNNING 2 CROSS-TEMPORAL OPTIMIZATIONS ==========\n\n")

solutions <- list()

summary_all <- tibble(
  scenario_name = character(),
  quality       = character(),
  n_features    = numeric(),
  n_selected    = numeric(),
  total_cost    = numeric(),
  total_acres   = numeric(),
  total_water   = numeric(),
  boundary      = numeric()
)

for (i in 1:nrow(scenarios)) {
  
  scen     <- scenarios$scenario_name[i]
  quality  <- scenarios$quality[i]
  feats    <- scenarios$features[[i]]
  targs    <- scenarios$targets[[i]]
  
  cat(sprintf("--- Scenario %d/2: %s (%s) ---\n", i, scen, quality))
  cat(sprintf("    Features: %d (BNLL + GKR + SJKF × 5 time periods)\n", length(feats)))
  cat(sprintf("    Target: %s acres per feature\n", format(habitat_target, big.mark = ",")))
  
  # Build problem
  p <- problem(
    x            = field_data,
    features     = feats,
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = targs) %>%
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
      scenario_name = scen,
      quality       = quality,
      n_features    = length(feats),
      n_selected    = nrow(sel),
      total_cost    = sum(sel$revenue, na.rm = TRUE),
      total_acres   = sum(sel$acres, na.rm = TRUE),
      total_water   = sum(sel$water, na.rm = TRUE),
      boundary      = boundary_length
    )
  
  # Print summary
  cat(sprintf("    Selected: %d fields | Cost: $%s | Acres: %s\n",
              nrow(sel),
              format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel$acres, na.rm = TRUE)), big.mark = ",")))
  
  # Print per-feature target achievement
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
# SECTION 6: Compare the Two Solutions
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
  sel <- solutions[[scen]] %>% filter(solution_1 == 1)
  
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
          grepl("base", feat)      ~ "Baseline",
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

cat("\n--- Detailed Target Achievement ---\n")
print(target_detail, n = Inf)


# =============================================================================
# SECTION 7: Export Results
# =============================================================================

# --- Save all objects needed for figures ---
save(
  solutions,
  summary_all,
  target_detail,
  scenarios,
  suit_features,
  hq_features,
  field_data,
  field_data_all,
  habitat_target,
  chosen_blm,
  blm_results,
  bm,
  cost_scale_factor,
  file = here("data/intermediate/9_1_prioritizr_habitat_only/prioritizr_cross_temporal_results.RData")
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
           here(paste0("data/intermediate/9_1_prioritizr_habitat_only/solution_", scen, ".gpkg")))
}

# --- Export summary tables ---
write_csv(summary_all,
          here("data/intermediate/9_1_prioritizr_habitat_only/cross_temporal_comparison.csv"))
write_csv(target_detail,
          here("data/intermediate/9_1_prioritizr_habitat_only/cross_temporal_target_detail.csv"))
write_csv(blm_results,
          here("data/intermediate/9_1_prioritizr_habitat_only/cross_temporal_blm_calibration.csv"))


cat("\n========== CROSS-TEMPORAL OPTIMIZATION COMPLETE — RESULTS SAVED ==========\n")
cat("Load results for figures with:\n")
cat('  load(here("data/intermediate/9_1_prioritizr_habitat_only/prioritizr_cross_temporal_results.RData"))\n')




















