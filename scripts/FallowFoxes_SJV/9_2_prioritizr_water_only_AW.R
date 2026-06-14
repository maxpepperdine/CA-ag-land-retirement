# ==============================================================================
# PRIORITIZR WATER SAVINGS OPTIMIZATION — APPLIED WATER (AW) VERSION
# ==============================================================================
# GOAL: Identify optimal sets of agricultural fields to retire to meet the
#       PPIC-estimated applied water (AW) reduction targets for SGMA
#       implementation, while minimizing foregone agricultural revenue.
#
# SCENARIOS:
#   - 3 climate scenarios: Baseline, RCP4.5 (2020-2049), RCP8.5 (2020-2049)
#   - 2 spatial scales per scenario:
#       (a) Valley-wide: meet scenario-specific target using any SJV non-retired field
#       (b) Basin-specific: meet each basin's scenario-specific target independently
#
# TARGETS:
#   Baseline targets come directly from the PPIC technical appendix dataset
#   (Escriva-Bou et al. 2023, "PPIC Water Supply Constraints at the Local
#   Scale in the San Joaquin Valley"), specifically the field
#   "Corrected reduction in groundwater supplies no negative" aggregated
#   from 49 subunits to the 15 SJV groundwater basins. This field represents
#   the subunit-scale applied water reduction required to bring each basin 
#   into sustainable balance, accounting for irrigation efficiency and 
#   redistribution of excess groundwater from surplus subunits.
#
#   Valley-wide baseline target (2,675 TAF) = sum of basin-specific targets.
#   This differs slightly from PPIC's rounded 2.68 MAF headline number and
#   ensures internal consistency between valley-wide and basin-specific runs.
#
# TARGET ADJUSTMENT FOR FUTURE SCENARIOS:
#   Under future climate, the AW reduction target increases because of 
#   rising crop water demand and declining surface water deliveries.
#
#   Approach: Scale PPIC-derived baseline AW reduction targets using the 
#   proportional increase in applied water reductions reported by 
#   Escriva-Bou et al. (2023), who estimated a 6.3% increase under median 
#   RCP4.5 conditions relative to the SGMA-only baseline (from 2.68 to 
#   2.85 MAF). The RCP8.5 increment is scaled relative to RCP4.5 by a 
#   factor derived from the ratio of ensemble mean PET change between RCP 
#   scenarios in the BCMv8 projections used in this analysis:
#     scaling = (mean PET_ratio_RCP85 - 1) / (mean PET_ratio_RCP45 - 1)
#
#     RCP4.5 target = Baseline target × 1.063
#     RCP8.5 target = Baseline target × (1 + 0.063 × BCM scaling factor)
#
# OPTIMIZATION:
#   - Objective: minimize total revenue of selected fields (min-set)
#   - Planning units: non-retired fields only (retired = 0)
#   - Feature: AW (TAF) for the relevant climate scenario
#   - Target: scenario-specific AW reduction in TAF/yr
#   - Solver: Gurobi
#
# SCALING:
#   - Revenue: USD / 10,000 -> "ten-thousands of USD" (to keep cost < 1e6)
#   - AW: AF / 1,000 -> TAF (to keep targets < 1e6)
#   - Results are reported in original units (USD and AF)
#
# INPUTS:
#   - fields_Snet_estimation.gpkg (fields with AW, S_net, revenue, fallow status)
#     from 8_Snet_estimation.R
#   - DWR groundwater basin boundaries shapefile
#
# OUTPUTS:
#   - Solution GeoPackages with selected fields per scenario
#   - Summary tables of results (CSV)
# ==============================================================================

# clear workspace
rm(list = ls())

# load packages
library(sf)
library(dplyr)
library(readr)
library(prioritizr)
library(here)


# ==============================================================================
# STEP 1: Load data 
# ==============================================================================

# File paths -----------------------------------------------------------

# Fields from S_net workflow (contains AW_* and Snet_* columns + revenue)
fields_path <- here("data/intermediate/8_Snet_estimation/prioritizr_water_only/fields_Snet_estimation.gpkg")

# DWR groundwater basin boundaries
basins_path <- here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp")

# Output directory
output_dir <- here("data/intermediate/9_2_prioritizr_water_only_AW/")


# Load data ------------------------------------------------------------

# load fields data
fields <- st_read(fields_path)

# load basin boundaries
basins_raw <- st_read(basins_path)


# ==============================================================================
# STEP 2: Assign fields to groundwater basins
# ==============================================================================

cat("Assigning fields to groundwater basins...\n")

# Filter basins to SJV only (Basin_Su_1 starts with "SAN JOAQUIN VALLEY")
# Also remove basins we don't have PPIC targets for (East CC, Kettleman, 
# Consumnes, Pleasant Valley)
sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>% 
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA", 
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN", 
                            "SAN JOAQUIN VALLEY - COSUMNES", 
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY"))

cat("  SJV basins found:", nrow(sjv_basins), "\n")
cat("  Basin names:\n")
print(sort(unique(sjv_basins$Basin_Su_1)))

# reproject basins to match fields CRS
sjv_basins <- st_transform(sjv_basins, st_crs(fields))

# spatial join: assign each field to the basin it falls within
# use centroid of field for clean 1:1 matching
fields_centroids <- st_centroid(fields)
basin_join <- st_join(fields_centroids, sjv_basins %>% select(Basin_Su_1), left = TRUE)

# extract basin name and add to fields
fields$basin_raw <- basin_join$Basin_Su_1

# Clean basin names: extract the short name from "SAN JOAQUIN VALLEY - [NAME]"
fields <- fields %>%
  mutate(
    basin = case_when(
      grepl("KERN COUNTY", basin_raw)        ~ "Kern",
      grepl("KINGS", basin_raw)              ~ "Kings",
      grepl("TULE", basin_raw)               ~ "Tule",
      grepl("KAWEAH", basin_raw)             ~ "Kaweah",
      grepl("MADERA", basin_raw)             ~ "Madera",
      grepl("MERCED", basin_raw)             ~ "Merced",
      grepl("TURLOCK", basin_raw)            ~ "Turlock",
      grepl("DELTA-MENDOTA", basin_raw)      ~ "Delta-Mendota",
      grepl("DELTA MENDOTA", basin_raw)      ~ "Delta-Mendota",
      grepl("EASTERN SAN JOAQUIN", basin_raw) ~ "Eastern San Joaquin",
      grepl("CHOWCHILLA", basin_raw)         ~ "Chowchilla",
      grepl("TULARE LAKE", basin_raw)        ~ "Tulare Lake",
      grepl("MODESTO", basin_raw)            ~ "Modesto",
      grepl("WESTSIDE", basin_raw)           ~ "Westside",
      grepl("TRACY", basin_raw)              ~ "Tracy",
      grepl("WHITE WOLF", basin_raw)         ~ "White Wolf",
      grepl("PLEASANT VALLEY", basin_raw)    ~ "Pleasant Valley",
      TRUE                                    ~ NA_character_
    )
  )

# QC: check basin assignment
cat("\nBasin assignment summary:\n")
basin_counts <- fields %>%
  st_drop_geometry() %>%
  count(basin) %>%
  arrange(desc(n)) %>%
  as.data.frame()
print(basin_counts)
cat("  Fields without basin assignment:", sum(is.na(fields$basin)), "\n")


# ==============================================================================
# STEP 3: Define baseline AW reduction targets (in TAF)
# ==============================================================================
# These are the PPIC-estimated SGMA AW reduction targets for each of the 15 
# SJV groundwater basins, derived from the PPIC technical appendix dataset 
# ("PPIC Water Supply Constraints at the Local Scale in the San Joaquin 
# Valley", Escriva-Bou et al. 2023). Values are aggregated from the 49 
# subunits to the 15 basins using the field:
#   "Corrected reduction in groundwater supplies no negative"
# which represents the AW reduction required at the subunit scale to bring 
# each basin into sustainable balance, after accounting for irrigation 
# efficiency and the redistribution of excess groundwater from surplus 
# subunits (per PPIC Box A2).
#
# Valley-wide baseline target = sum of basin targets = 2,675 TAF. This 
# approximates PPIC's headline 2.68 MAF figure (difference due to rounding) 
# and ensures valley-wide and basin-specific targets are internally 
# consistent. Future scenario targets are computed in STEP 3b below.

# Basin-specific AW reduction targets (TAF/yr) from the PPIC dataset
basin_aw_reduction <- data.frame(
  basin = c("Kern", "Kings", "Tule", "Merced", "Kaweah",
            "Madera", "Turlock", "Delta-Mendota", "Chowchilla", "Tulare Lake",
            "Eastern San Joaquin", "Modesto", "Westside", "White Wolf", "Tracy"),
  aw_reduction_taf = c(721.9, 340.9, 210.5, 198.4, 197.1,
                       185.5, 177.6, 172.2, 135.5, 125.7,
                       120.4,  42.3,  35.6,   6.5,   5.2)
)

# Valley-wide baseline AW reduction (sum of basin-specific values)
valley_aw_reduction_taf <- sum(basin_aw_reduction$aw_reduction_taf)

cat("\nBaseline basin-specific AW reduction targets (TAF/yr):\n")
print(basin_aw_reduction)
cat("\nValley-wide baseline AW reduction:",
    round(valley_aw_reduction_taf, 1), "TAF (",
    round(valley_aw_reduction_taf / 1000, 3), "MAF)\n")
cat("PPIC reported valley-wide target: 2,680 TAF (2.68 MAF)\n")


# ==============================================================================
# STEP 3b: Compute climate-adjusted targets
# ==============================================================================
# Baseline AW reduction targets are scaled upward under future climate to 
# account for the combined effects of increased crop water demand and reduced 
# surface water deliveries.
#
# RCP4.5 scaling: Escriva-Bou et al. (2023) estimated that the combined
# effects of climate change increased the required applied water reduction 
# by 6.3% (from 2.68 to 2.85 MAF) under median RCP4.5 conditions. We apply
# this same proportional increase directly to our PPIC-derived baseline AW 
# reduction targets.
#
# RCP8.5 scaling: The RCP4.5 climate increment is scaled by a factor
# derived from the ratio of ensemble mean PET change between RCP scenarios
# in the BCMv8 projections used in this analysis:
#   scaling = (mean PET_ratio_RCP85 - 1) / (mean PET_ratio_RCP45 - 1)
#
# This provides an empirically grounded multiplier that captures the 
# stronger radiative forcing under RCP8.5 without requiring an independent 
# water operations model.
#
#   RCP4.5 target = Baseline target × 1.063
#   RCP8.5 target = Baseline target × (1 + 0.063 × BCM scaling factor)
# ==============================================================================

cat("\n========================================\n")
cat("COMPUTING CLIMATE-ADJUSTED TARGETS\n")
cat("========================================\n")

# --- Derive RCP8.5 scaling factor from BCM PET data ---
nonretired <- fields %>%
  st_drop_geometry() %>%
  filter(retired == 0, !is.na(basin))

mean_pet_ratio_rcp45 <- weighted.mean(nonretired$PET_ratio_RCP45_near,
                                      nonretired$acres, na.rm = TRUE)
mean_pet_ratio_rcp85 <- weighted.mean(nonretired$PET_ratio_RCP85_near,
                                      nonretired$acres, na.rm = TRUE)
rcp85_scaling <- (mean_pet_ratio_rcp85 - 1) / (mean_pet_ratio_rcp45 - 1)

cat("\nRCP8.5 scaling factor derivation (from BCMv8 PET):\n")
cat("  Mean PET ratio RCP4.5:", round(mean_pet_ratio_rcp45, 4), "\n")
cat("  Mean PET ratio RCP8.5:", round(mean_pet_ratio_rcp85, 4), "\n")
cat("  RCP8.5/RCP4.5 scaling factor:", round(rcp85_scaling, 2), "\n")

# --- Define climate adjustment multipliers ---
# PPIC RCP4.5 proportional increase in applied water reduction target
ppic_rcp45_increase <- 0.063  # 6.3% (from 2.68 to 2.85 MAF)

# RCP8.5 increase = RCP4.5 increase x empirical scaling factor
ppic_rcp85_increase <- ppic_rcp45_increase * rcp85_scaling

# Full multipliers applied to baseline targets
multiplier_rcp45 <- 1 + ppic_rcp45_increase
multiplier_rcp85 <- 1 + ppic_rcp85_increase

cat("\nClimate adjustment multipliers:\n")
cat("  Baseline:  1.000 (no adjustment)\n")
cat("  RCP4.5:   ", round(multiplier_rcp45, 3),
    "(+", round(ppic_rcp45_increase * 100, 1), "%, from PPIC)\n")
cat("  RCP8.5:   ", round(multiplier_rcp85, 3),
    "(+", round(ppic_rcp85_increase * 100, 1), "%, PPIC x BCM scaling)\n")

# --- Compute scenario-specific targets ---

# Valley-wide targets
valley_target_baseline_taf <- valley_aw_reduction_taf
valley_target_rcp45_taf    <- valley_aw_reduction_taf * multiplier_rcp45
valley_target_rcp85_taf    <- valley_aw_reduction_taf * multiplier_rcp85

# Basin-specific targets (same multiplier applied to each basin's baseline target)
basin_targets <- basin_aw_reduction %>%
  mutate(
    target_baseline_taf = aw_reduction_taf,
    target_rcp45_taf    = aw_reduction_taf * multiplier_rcp45,
    target_rcp85_taf    = aw_reduction_taf * multiplier_rcp85
  )

cat("\n========================================\n")
cat("FINAL SCENARIO-SPECIFIC TARGETS\n")
cat("========================================\n")

cat("\nValley-wide targets (TAF):\n")
cat("  Baseline:          ", round(valley_target_baseline_taf, 1), "\n")
cat("  RCP4.5 (2020-2049):", round(valley_target_rcp45_taf, 1), "\n")
cat("  RCP8.5 (2020-2049):", round(valley_target_rcp85_taf, 1), "\n")

cat("\nBasin-specific targets (TAF):\n")
print(as.data.frame(basin_targets))


# Get some baseline retirement stats by basin --------------------
retired_fields <- fields %>% 
  st_drop_geometry() %>% 
  filter(retired == 1)

# retirement by basin
retirement_by_basin <- retired_fields %>% 
  group_by(basin) %>% 
  summarise(field_count = n(), 
            sum_acres = sum(acres))


# ==============================================================================
# STEP 4: Define AW scenario columns & map to targets (scaled to TAF)
# ==============================================================================

# Scenario definitions: name, AW column, and corresponding valley/basin targets
scenarios <- data.frame(
  scenario_name = c("Baseline", "RCP45_2020_2049", "RCP85_2020_2049"),
  aw_col = c("AW_baseline_TAF", "AW_RCP45_near_TAF", "AW_RCP85_near_TAF"),
  aw_col_orig = c("AW_baseline_AF", "AW_RCP45_near_AF", "AW_RCP85_near_AF"),
  valley_target_taf = c(valley_target_baseline_taf, valley_target_rcp45_taf, valley_target_rcp85_taf),
  basin_target_col = c("target_baseline_taf", "target_rcp45_taf", "target_rcp85_taf"),
  stringsAsFactors = FALSE
)

cat("\nScenario configuration:\n")
print(scenarios)


# ==============================================================================
# STEP 5: Prepare planning units (non-retired fields only) & scale values
# ==============================================================================

cat("\nPreparing planning units (non-retired fields only)...\n")
planning_units <- fields %>% 
  filter(retired == 0, !is.na(basin))
cat("  Total PUs:", nrow(planning_units), "\n")
cat("  Total PU revenue: $", format(sum(planning_units$revenue, na.rm = TRUE),
                                    big.mark = ","), "\n")

# Scale values to avoid numerical issues with solver
# Revenue: USD -> ten-thousands of USD (/ 10,000)
# AW: AF -> TAF (/ 1,000)
planning_units <- planning_units %>%
  mutate(
    revenue_10k = revenue / 10000,
    AW_baseline_TAF   = AW_baseline_AF   / 1000,
    AW_RCP45_near_TAF = AW_RCP45_near_AF / 1000,
    AW_RCP85_near_TAF = AW_RCP85_near_AF / 1000
  )


# ==============================================================================
# STEP 5b: Export basin reference table
# ==============================================================================

cat("\nBuilding basin reference table...\n")

# Calculate total AW available from non-retired fields per basin per scenario
basin_available <- planning_units %>%
  st_drop_geometry() %>%
  filter(!is.na(basin)) %>%
  group_by(basin) %>%
  summarise(
    n_planning_units = n(),
    total_acres = round(sum(acres, na.rm = TRUE)),
    total_revenue = round(sum(revenue, na.rm = TRUE)),
    available_baseline_taf = round(sum(AW_baseline_TAF, na.rm = TRUE), 1),
    available_rcp45_taf    = round(sum(AW_RCP45_near_TAF, na.rm = TRUE), 1),
    available_rcp85_taf    = round(sum(AW_RCP85_near_TAF, na.rm = TRUE), 1),
    .groups = "drop"
  )

# Join with scenario-specific targets
basin_ref_table <- basin_targets %>%
  left_join(basin_available, by = "basin") %>%
  mutate(
    shortfall_baseline_taf = pmax(target_baseline_taf - available_baseline_taf, 0),
    shortfall_rcp45_taf    = pmax(target_rcp45_taf    - available_rcp45_taf,    0),
    shortfall_rcp85_taf    = pmax(target_rcp85_taf    - available_rcp85_taf,    0),
    feasible_baseline = available_baseline_taf >= target_baseline_taf,
    feasible_rcp45    = available_rcp45_taf    >= target_rcp45_taf,
    feasible_rcp85    = available_rcp85_taf    >= target_rcp85_taf
  )

cat("\nBasin reference table:\n")
print(as.data.frame(basin_ref_table))

write_csv(basin_ref_table, file.path(output_dir, "basin_reference_table.csv"))

# Export target methodology details for documentation
target_methodology <- data.frame(
  parameter = c("PPIC RCP4.5 increase", "BCM RCP8.5 scaling factor",
                "RCP4.5 multiplier", "RCP8.5 multiplier",
                "Mean PET ratio RCP4.5", "Mean PET ratio RCP8.5",
                "Valley baseline AW reduction (TAF)",
                "Valley RCP4.5 target (TAF)", 
                "Valley RCP8.5 target (TAF)",
                "PPIC reported valley baseline (TAF)"),
  value = c(round(ppic_rcp45_increase, 4), round(rcp85_scaling, 2),
            round(multiplier_rcp45, 4), round(multiplier_rcp85, 4),
            round(mean_pet_ratio_rcp45, 4), round(mean_pet_ratio_rcp85, 4),
            round(valley_target_baseline_taf, 1),
            round(valley_target_rcp45_taf, 1),
            round(valley_target_rcp85_taf, 1),
            2680)
)

write_csv(target_methodology, file.path(output_dir, "target_methodology.csv"))


# ==============================================================================
# STEP 6: Helper function — build and solve prioritizr problems
# ==============================================================================

# create the helper function to build and solve prioritizr problems

solve_water_savings <- function(pu,               # planning units (sf, non-retired fields)
                                aw_col,           # name of SCALED AW column (TAF)
                                target_taf,       # target in TAF
                                scenario_label) { # for labeling output
  
  cat("\n--- Solving:", scenario_label, "---\n")
  cat("  Planning units:", nrow(pu), "\n")
  cat("  AW column:", aw_col, "\n")
  cat("  Target:", round(target_taf, 1), "TAF\n")
  
  # Check if target is achievable
  total_available_taf <- sum(pu[[aw_col]], na.rm = TRUE)
  cat("  Total AW available:", round(total_available_taf, 1), "TAF\n")
  
  if (total_available_taf < target_taf) {
    cat("  WARNING: Target exceeds available AW! Skipping.\n")
    cat("  Shortfall:", round(target_taf - total_available_taf, 1), "TAF\n")
    return(NULL)
  }
  
  # Build the prioritizr problem
  # - Cost: revenue_10k (revenue in ten-thousands of USD)
  # - Feature: AW in TAF
  # - Target: AW reduction in TAF
  p <- problem(pu, 
               features = aw_col, 
               cost_column = "revenue_10k") %>%
    add_min_set_objective() %>%
    add_absolute_targets(target_taf) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(verbose = FALSE)
  
  # Solve (force = TRUE to bypass single-feature warning)
  solution <- solve(p, force = TRUE)
  
  # Extract results
  sol_col <- "solution_1"
  selected <- solution %>%
    st_drop_geometry() %>%
    filter(!!sym(sol_col) == 1)
  
  # Report in original units
  cat("  Fields selected:", nrow(selected), "\n")
  cat("  Acres selected:", format(round(sum(selected$acres)), big.mark = ","), "\n")
  cat("  Revenue cost: $", format(round(sum(selected$revenue)), big.mark = ","), "\n")
  cat("  AW achieved:", format(round(sum(selected[[aw_col]]) * 1000),
                               big.mark = ","), "AF (",
      round(sum(selected[[aw_col]]), 1), "TAF)\n")
  
  # Add metadata columns
  solution$scenario <- scenario_label
  solution$aw_col_used <- aw_col
  solution$target_taf <- target_taf
  
  return(solution)
}


# ==============================================================================
# STEP 7: Run valley-wide optimizations
# ==============================================================================

cat("\n========================================\n")
cat("VALLEY-WIDE OPTIMIZATIONS\n")
cat("========================================\n")

valley_results <- list()

for (i in 1:nrow(scenarios)) {
  scen_name <- scenarios$scenario_name[i]
  aw_col    <- scenarios$aw_col[i]
  target    <- scenarios$valley_target_taf[i]
  label     <- paste0("Valley_", scen_name)
  
  result <- solve_water_savings(
    pu = planning_units,
    aw_col = aw_col,
    target_taf = target,
    scenario_label = label
  )
  
  if (!is.null(result)) {
    valley_results[[label]] <- result
  }
}


# ==============================================================================
# STEP 8: Run basin-specific optimizations
# ==============================================================================

cat("\n========================================\n")
cat("BASIN-SPECIFIC OPTIMIZATIONS\n")
cat("========================================\n")

basin_results <- list()

for (i in 1:nrow(scenarios)) {
  scen_name       <- scenarios$scenario_name[i]
  aw_col          <- scenarios$aw_col[i]
  target_col_name <- scenarios$basin_target_col[i]
  
  cat("\n--- Scenario:", scen_name, "---\n")
  
  for (j in 1:nrow(basin_targets)) {
    basin_name <- basin_targets$basin[j]
    target_taf <- basin_targets[[target_col_name]][j]
    
    # Skip basins with 0 or near-0 target
    if (target_taf <= 0) {
      cat("  Skipping", basin_name, "(target <= 0)\n")
      next
    }
    
    # Filter planning units to this basin
    basin_pu <- planning_units %>% 
      filter(basin == basin_name)
    
    if (nrow(basin_pu) == 0) {
      cat("  Skipping", basin_name, "(no planning units in basin)\n")
      next
    }
    
    label <- paste0("Basin_", gsub(" ", "_", basin_name), "_", scen_name)
    
    result <- solve_water_savings(
      pu = basin_pu,
      aw_col = aw_col,
      target_taf = target_taf,
      scenario_label = label
    )
    
    if (!is.null(result)) {
      basin_results[[label]] <- result
    }
  }
}


# ==============================================================================
# STEP 9: COMPILE AND EXPORT RESULTS
# ==============================================================================

cat("\n========================================\n")
cat("COMPILING RESULTS\n")
cat("========================================\n")

# --- Summary table function ---
summarize_solution <- function(solution, aw_col, label) {
  sol_col <- "solution_1"
  selected <- solution %>%
    st_drop_geometry() %>%
    filter(!!sym(sol_col) == 1)
  
  data.frame(
    scenario = label,
    n_fields_selected = nrow(selected),
    acres_selected = round(sum(selected$acres, na.rm = TRUE)),
    # Convert back to original units
    revenue_cost_usd = round(sum(selected$revenue, na.rm = TRUE)),
    aw_achieved_af = round(sum(selected[[aw_col]], na.rm = TRUE) * 1000),
    target_af = round(unique(solution$target_taf) * 1000),
    stringsAsFactors = FALSE
  )
}

# Valley-wide summary
valley_summary <- bind_rows(lapply(names(valley_results), function(label) {
  sol <- valley_results[[label]]
  aw_col <- unique(sol$aw_col_used)
  summarize_solution(sol, aw_col, label)
}))

cat("\nValley-wide results:\n")
print(as.data.frame(valley_summary))

# Basin-specific summary
basin_summary <- bind_rows(lapply(names(basin_results), function(label) {
  sol <- basin_results[[label]]
  aw_col <- unique(sol$aw_col_used)
  summarize_solution(sol, aw_col, label)
}))

cat("\nBasin-specific results:\n")
print(as.data.frame(basin_summary))

# --- Export ---
cat("\nExporting results...\n")

# Valley-wide solutions
for (label in names(valley_results)) {
  out_path <- file.path(output_dir, paste0(label, ".gpkg"))
  st_write(valley_results[[label]], out_path, delete_dsn = TRUE)
  cat("  Written:", out_path, "\n")
}

# Basin-specific solutions
for (label in names(basin_results)) {
  out_path <- file.path(output_dir, paste0(label, ".gpkg"))
  st_write(basin_results[[label]], out_path, delete_dsn = TRUE)
  cat("  Written:", out_path, "\n")
}

# Summary CSVs
write_csv(valley_summary, file.path(output_dir, "valley_wide_summary.csv"))
write_csv(basin_summary,  file.path(output_dir, "basin_specific_summary.csv"))
















