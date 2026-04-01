# =============================================================================
# Spatial Optimization of Agricultural Land Retirement in San Joaquin Valley
# =============================================================================
# Purpose: Use prioritizr to find optimal configurations of fallow land that
#          minimize costs to the agricultural economy while meeting minimum
#          habitat creation targets for BNLL and GKR.
#
# Approach: We "toss up" all currently fallow fields and use a minimum-cost
#           optimization to reconfigure them into retirement patterns that
#           meet habitat targets (25,000 acres per species) while minimizing
#           the total annual revenue lost from retiring those fields.
#
# We run 10 separate optimizations:
#   5 climate scenarios × 2 habitat quality levels
#   - Scenarios: baseline, rcp45_2049, rcp45_2069, rcp85_2049, rcp85_2069
#   - Quality:   suitable (suit), high quality (hq)
#   Each optimization has 2 features: BNLL habitat + GKR habitat
#   Each feature has a 25,000-acre target
#
# Cost layer: annual revenue per field (`revenue`)
# Planning units: individual fallow agricultural fields (sf polygons)
# Decision variable: binary (retire or not)
# BLM: calibrated once on the baseline suitable scenario, then applied to all
# =============================================================================



# clear environment
rm(list = ls())


# load Packages -----------------------------------------------------------

library(tidyverse)
library(prioritizr)
library(sf)
library(terra)
library(here)


# load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# print full numbers
options(scipen = 999)


# =============================================================================
# STEP 1: Load & clean data
# =============================================================================

# load SJV boundary 
sjv_boundary <- read_sf(here("data/raw/sjv_counties/sjv_counties.shp"))

# --- Planning units: SJV fields with habitat extractions ---
# output from 8_blm.R (fields cleaned of rasterization artifacts)
field_data_all <- read_sf(here("data/intermediate/8_blm/cleanShapes/cleanShapes.gpkg")) %>%
  st_make_valid()

# --- BLM shapes for boundary length matrix (from 8_blm.R) ---
blm_shapes_all <- read_sf(here("data/intermediate/8_blm/blmShapes/blmShapes.shp"))


# subset to fallow fields only
fallow_ids <- field_data_all %>%
  filter(fallow == 1) %>%
  pull(id)

field_data <- field_data_all %>%
  filter(id %in% fallow_ids)

# subset BLM shapes to match fallow fields and rebuild boundary matrix
blm_shapes <- blm_shapes_all %>%
  filter(id %in% fallow_ids)

# verify alignment (IDs and rows must match for prioritizr)
stopifnot(nrow(field_data) == nrow(blm_shapes))
stopifnot(all(field_data$id == blm_shapes$id))

# build boundary matrix from fallow-only shapes
bm <- boundary_matrix(blm_shapes)


# prepare cost and habitat columns
field_data <- field_data %>%
  mutate(
    cost_raw = ifelse(is.na(revenue) | revenue <= 0, 1, revenue)
  )

# replace NAs in habitat columns with 0
habitat_cols_all <- names(field_data)[grepl("^(bnll|gkr)_", names(field_data))]
field_data <- field_data %>%
  mutate(across(all_of(habitat_cols_all), ~ replace_na(.x, 0)))


# scale costs to avoid Gurobi presolve numeric issues ---------------------

# scale revenue by a constact factor to keep costs in reasonable range
cost_scale_factor <- 1e4

field_data <- field_data %>%
  mutate(cost = cost_raw / cost_scale_factor)

# print summary of cost and habitat ranges after scaling
cat("\nCost scaling factor:", cost_scale_factor, "\n")
cat("Cost range (scaled): [", round(min(field_data$cost), 4), ",",
    round(max(field_data$cost), 2), "]\n")
hab_vals <- field_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(all_of(habitat_cols_all))
cat("Habitat range: [", round(min(hab_vals, na.rm = TRUE), 4), ",",
    round(max(hab_vals, na.rm = TRUE), 2), "]\n")


# quick summary
cat("\nTotal fields (all):", nrow(field_data_all), "\n")
cat("Fallow fields (planning units):", nrow(field_data), "\n")
cat("Cultivated fields (excluded):", nrow(field_data_all) - nrow(field_data), "\n")
cat("Boundary matrix dimensions:", dim(bm), "\n")



# =============================================================================
# STEP 2: Define the 10 Optimization Scenarios (for now)
# =============================================================================

# each scenario is defined by a climate projection and a habitat quality level
# 5 scenarios x 2 habitat levels = 10 total optimization runs

scenarios <- tibble(
  scenario_name = c(
    "base_suit",      "rcp45_2049_suit", "rcp45_2069_suit",
    "rcp85_2049_suit", "rcp85_2069_suit",
    "base_hq",        "rcp45_2049_hq",   "rcp45_2069_hq",
    "rcp85_2049_hq",  "rcp85_2069_hq"
  ),
  bnll_col = c(
    "bnll_base_suit",      "bnll_rcp45_2049_suit", "bnll_rcp45_2069_suit",
    "bnll_rcp85_2049_suit", "bnll_rcp85_2069_suit",
    "bnll_base_hq",        "bnll_rcp45_2049_hq",   "bnll_rcp45_2069_hq",
    "bnll_rcp85_2049_hq",  "bnll_rcp85_2069_hq"
  ),
  gkr_col = c(
    "gkr_base_suit",      "gkr_rcp45_2049_suit", "gkr_rcp45_2069_suit",
    "gkr_rcp85_2049_suit", "gkr_rcp85_2069_suit",
    "gkr_base_hq",        "gkr_rcp45_2049_hq",   "gkr_rcp45_2069_hq",
    "gkr_rcp85_2049_hq",  "gkr_rcp85_2069_hq"
  ),
  climate = c(
    "Baseline", "RCP 4.5 (2020-2049)", "RCP 4.5 (2040-2069)",
    "RCP 8.5 (2020-2049)", "RCP 8.5 (2040-2069)",
    "Baseline", "RCP 4.5 (2020-2049)", "RCP 4.5 (2040-2069)",
    "RCP 8.5 (2020-2049)", "RCP 8.5 (2040-2069)"
  ),
  quality = c(
    rep("Suitable", 5),
    rep("High Quality", 5)
  )
)


# habitat target: 25,000 acres per species per scenario
habitat_target <- 25000



# =============================================================================
# STEP 3: Diagnostic — Check Habitat Availability on Fallow Fields
# =============================================================================

# since field_data now contains only fallow fields, we can check directly
# make sure we have enough habitat available to meet targets before running optimizations
cat("\n--- Available habitat on ALL fallow fields (acres) ---\n")
cat("--- Target per species: 25,000 acres ---\n\n")

for (i in 1:nrow(scenarios)) {
  bnll_avail <- sum(field_data[[scenarios$bnll_col[i]]], na.rm = TRUE)
  gkr_avail  <- sum(field_data[[scenarios$gkr_col[i]]], na.rm = TRUE)
  cat(sprintf("  %-20s | BNLL: %10.1f ac (%s) | GKR: %10.1f ac (%s)\n",
              scenarios$scenario_name[i],
              bnll_avail, ifelse(bnll_avail >= habitat_target, "OK", "!!"),
              gkr_avail,  ifelse(gkr_avail >= habitat_target, "OK", "!!")))
}

# total fallow revenue in 2022
cat("\nTotal fallow revenue: $",
    format(sum(field_data$revenue, na.rm = TRUE), big.mark = ","), "\n")

# total fallow acreage in 2022
cat("Total fallow acres:",
    format(sum(field_data$acres, na.rm = TRUE), big.mark = ","), "\n")



# =============================================================================
# STEP 4: BLM Calibration (Baseline Suitable Scenario)
# =============================================================================

# calibrate boundary length modifier using the baseline suitable habitat scenario
# examine tradeoff between cost and boundary length across a range of BLM values
# the chosen BLM will be applied to all 10 optimization runs.

cat("\n========== BLM CALIBRATION (baseline suitable) ==========\n\n")

blm_values <- c(0, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05)

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
    features     = c("bnll_base_suit", "gkr_base_suit"),
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = c(habitat_target, habitat_target)) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = blm_val, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, time_limit = 300, numeric_focus = TRUE)
  
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
    title = "BLM tradeoff: cost vs. boundary Length",
    subtitle = "Calibrated on baseline suitable habitat scenario",
    x     = "Total revenue cost ($)",
    y     = "Boundary length (lower = more cohesive)"
  ) +
  theme_minimal()

print(blm_plot)


# --- SELECT THE BEST BLM ---
chosen_blm <- 0.005  # elbow of cost-boundary tradeoff curve



# =============================================================================
# STEP 5: Run All 10 Optimizations
# =============================================================================

cat("\n========== RUNNING 10 OPTIMIZATION SCENARIOS ==========\n\n")

# store all solutions in a list
solutions <- list()

# store summary stats for comparison
summary_all <- tibble(
  scenario_name = character(),
  climate       = character(),
  quality       = character(),
  n_selected    = numeric(),
  total_cost    = numeric(),
  total_acres   = numeric(),
  total_water   = numeric(),
  bnll_achieved = numeric(),
  gkr_achieved  = numeric(),
  boundary      = numeric()
)

for (i in 1:nrow(scenarios)) {
  
  scen <- scenarios$scenario_name[i]
  bnll <- scenarios$bnll_col[i]
  gkr  <- scenarios$gkr_col[i]
  
  cat(sprintf("--- Scenario %d/10: %s ---\n", i, scen))
  cat(sprintf("    Features: %s, %s\n", bnll, gkr))
  
  # build problem
  p <- problem(
    x            = field_data,
    features     = c(bnll, gkr),
    cost_column  = "cost"
  ) %>%
    add_min_set_objective() %>%
    add_absolute_targets(targets = c(habitat_target, habitat_target)) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = chosen_blm, data = bm) %>%
    add_gurobi_solver(verbose = FALSE, time_limit = 600, numeric_focus = TRUE)
  
  # solve (run_checks = FALSE to bypass prioritizr presolve)
  sol <- tryCatch(
    solve(p, run_checks = FALSE),
    error = function(e) {
      cat("    FAILED:", e$message, "\n\n")
      return(NULL)
    }
  )
  
  if (is.null(sol)) next
  
  # extract selected fields
  sel <- sol %>% filter(solution_1 == 1)
  
  # calculate boundary length
  boundary_length <- eval_boundary_summary(p, sol[, "solution_1"])$boundary
  
  # store solution
  solutions[[scen]] <- sol
  
  # append summary
  summary_all <- summary_all %>%
    add_row(
      scenario_name = scen,
      climate       = scenarios$climate[i],
      quality       = scenarios$quality[i],
      n_selected    = nrow(sel),
      total_cost    = sum(sel$revenue, na.rm = TRUE),
      total_acres   = sum(sel$acres, na.rm = TRUE),
      total_water   = sum(sel$water, na.rm = TRUE),
      bnll_achieved = sum(sel[[bnll]], na.rm = TRUE),
      gkr_achieved  = sum(sel[[gkr]], na.rm = TRUE),
      boundary      = boundary_length
    )
  
  # print summary of selected fields, cost, and acres
  cat(sprintf("    Selected: %d fields | Cost: $%s | Acres: %s\n",
              nrow(sel),
              format(round(sum(sel$revenue, na.rm = TRUE)), big.mark = ","),
              format(round(sum(sel$acres, na.rm = TRUE)), big.mark = ",")))
  # print habitat achieved and boundary length
  cat(sprintf("    BNLL: %.1f ac | GKR: %.1f ac | Boundary: %.1f\n\n",
              sum(sel[[bnll]], na.rm = TRUE),
              sum(sel[[gkr]], na.rm = TRUE),
              boundary_length))
}



# =============================================================================
# STEP 6: Export results
# =============================================================================

# --- Save all objects needed for figures (11_prioritizrFigures.R) ---
save(
  solutions,        # list of 10 solution sf objects
  summary_all,      # cross-scenario comparison table
  scenarios,        # scenario definitions (names, columns, labels)
  field_data,       # fallow fields with selection frequency (added in figures script)
  field_data_all,   # all fields (for mapping context)
  habitat_target,   # 25,000 acres
  chosen_blm,       # BLM value used
  blm_results,      # BLM calibration table
  bm,               # boundary matrix (for eval_boundary_summary if needed)
  cost_scale_factor,# for reference
  file = here("data/intermediate/FallowFoxes_SJV_archive/9_1_prioritizr_habitat_separate/prioritizr_results.RData")
)

# --- Also export individual solutions as geopackages ---
for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  # Extract just id and solution_1 from the fallow-only solution
  sol_key <- sol %>%
    st_drop_geometry() %>%
    dplyr::select(id, solution_1)
  
  # Join to all fields, ensuring no duplicate columns
  sol_complete <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(sol_key, by = "id") %>%
    mutate(solution_1 = replace_na(solution_1, 0))
  
  # Classify crops (convertCommRetired adds a "Crop" column)
  # Drop original "crop" column to avoid case-insensitive name collision in gpkg
  sol_classified <- convertCommRetired(sol_complete) %>%
    dplyr::select(-any_of("crop"))
  
  write_sf(sol_classified,
           here(paste0("data/intermediate/FallowFoxes_SJV_archive/9_1_prioritizr_habitat_separate/solution_", scen, ".gpkg")))
}

# --- Export summary tables as CSVs ---
write_csv(summary_all,
          here("data/intermediate/FallowFoxes_SJV_archive/9_1_prioritizr_habitat_separate/scenario_comparison.csv"))
write_csv(blm_results,
          here("data/intermediate/FallowFoxes_SJV_archive/9_1_prioritizr_habitat_separate/blm_calibration.csv"))

















