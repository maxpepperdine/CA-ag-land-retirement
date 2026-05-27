# ==============================================================================
# PRIORITIZR WATER SAVINGS (APPLIED WATER) — RESULTS VISUALIZATIONS
# ==============================================================================
# Creating figures from the AW-based prioritizr optimization results
# (9_2_prioritizr_water_only_AW.R).
#
# This is the AW-feature counterpart to 10_2_prioritizr_water_only_figures.R,
# providing a parallel visual analysis using applied water (AW) reductions 
# rather than S_net consumption reductions as the water feature.
#
# FIGURES:
#   1. Basin-level results: acres retired, revenue cost, and AW achieved
#      across climate scenarios (grouped bar chart, faceted by metric)
#   2. Valley-wide scenario comparison: acres, revenue, AW across scenarios
#   3. Spatial maps:
#      3a-c. Selected fields per scenario (Baseline, RCP4.5, RCP8.5)
#      3d.   Selection frequency overlay (0-3 scenarios) with basin boundaries
#   5. Retirement totals by crop type across climate scenarios
#
# TABLES (kableExtra, exported as HTML + CSV):
#   1. Basin-level results by scenario (target, AW, acres, fields, revenue)
#   2. Valley-wide scenario summary
#   3. Selection consistency across scenarios
#
# INPUTS:
#   - valley_wide_summary.csv
#   - basin_specific_summary.csv
#   - basin_reference_table.csv
#   - target_methodology.csv
#   - Valley-wide solution GeoPackages (Valley_Baseline.gpkg, etc.)
#   - DWR groundwater basin boundaries shapefile
#
# OUTPUTS:
#   - Individual figure files (PNG) in output directory
#   - Tables as HTML + CSV files in output directory
# ==============================================================================

# clear environment
rm(list = ls())

library(tidyverse)
library(scales)
library(sf)
library(here)
library(patchwork)
library(kableExtra)


# ==============================================================================
# SECTION 1: Load data
# ==============================================================================

# file paths ------------------------------------------------------

# Prioritizr output directory
prioritizr_dir <- here("data/intermediate/misc/aw_prioritizr_formulation/9_2_prioritizr_water_only_AW/")

# Figure output directory
fig_dir <- here("data/intermediate/misc/aw_prioritizr_formulation/10_2_prioritizr_water_only_figures_AW/")

# Basin boundaries
basins_path <- here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp")

# Valley-wide solution GeoPackages
valley_gpkg_paths <- list(
  Baseline        = file.path(prioritizr_dir, "Valley_Baseline.gpkg"),
  RCP45_2020_2049 = file.path(prioritizr_dir, "Valley_RCP45_2020_2049.gpkg"),
  RCP85_2020_2049 = file.path(prioritizr_dir, "Valley_RCP85_2020_2049.gpkg")
)


# load data -------------------------------------------------------

# load summary data
valley_summary <- read_csv(file.path(prioritizr_dir, "valley_wide_summary.csv"))
basin_summary  <- read_csv(file.path(prioritizr_dir, "basin_specific_summary.csv"))
basin_ref      <- read_csv(file.path(prioritizr_dir, "basin_reference_table.csv"))

# load basin boundaries
basins_raw <- st_read(basins_path, quiet = TRUE)
sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>% 
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA", 
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN", 
                            "SAN JOAQUIN VALLEY - COSUMNES", 
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY"))

# load valley-wide solution GeoPackages
valley_solutions <- lapply(valley_gpkg_paths, function(path) {
  if (file.exists(path)) {
    st_read(path, quiet = TRUE)
  } else {
    cat("  WARNING: File not found:", path, "\n")
    NULL
  }
})

# Reproject basins to match solution CRS
if (!is.null(valley_solutions[[1]])) {
  sjv_basins <- st_transform(sjv_basins, st_crs(valley_solutions[[1]]))
}


# ==============================================================================
# SECTION 2: Clean and prepare data
# ==============================================================================

# --- Basin summary: parse scenario and basin from the label ---
basin_results <- basin_summary %>%
  mutate(
    # Extract scenario: everything after the last underscore-delimited scenario token
    scenario_label = case_when(
      grepl("Baseline$", scenario)        ~ "Baseline",
      grepl("RCP45_2020_2049$", scenario) ~ "RCP4.5 (2020-2049)",
      grepl("RCP85_2020_2049$", scenario) ~ "RCP8.5 (2020-2049)"
    ),
    # Extract basin name: everything between "Basin_" and the scenario suffix
    basin = scenario %>%
      str_remove("^Basin_") %>%
      str_remove("_(Baseline|RCP45_2020_2049|RCP85_2020_2049)$") %>%
      str_replace_all("_", " "),
    scenario_label = factor(scenario_label,
                            levels = c("Baseline", "RCP4.5 (2020-2049)", "RCP8.5 (2020-2049)"))
  )

# Order basins by baseline target (descending)
basin_ref_clean <- basin_ref %>% filter(aw_reduction_taf > 0)
basin_order <- basin_ref_clean %>% arrange(aw_reduction_taf) %>% pull(basin)
basin_results$basin <- factor(basin_results$basin, levels = basin_order)

# --- Valley summary: clean scenario labels ---
valley_plot <- valley_summary %>%
  mutate(
    scenario_label = case_when(
      grepl("Baseline", scenario) ~ "Baseline",
      grepl("RCP45", scenario)    ~ "RCP4.5\n(2020-2049)",
      grepl("RCP85", scenario)    ~ "RCP8.5\n(2020-2049)"
    ),
    scenario_label = factor(scenario_label,
                            levels = c("Baseline", "RCP4.5\n(2020-2049)", "RCP8.5\n(2020-2049)"))
  )


# ==============================================================================
# FIGURE 1: Basin-level results across scenarios
# ==============================================================================
# Grouped bar chart showing acres retired, revenue cost, and AW achieved
# per basin, colored by scenario.

cat("Creating Figure 1: Basin-level results across scenarios...\n")

# Reshape basin results for faceted plotting
fig1_data <- basin_results %>%
  filter(!is.na(basin)) %>%
  select(basin, scenario_label, acres_selected, revenue_cost_usd, aw_achieved_af) %>%
  mutate(
    revenue_cost_millions = revenue_cost_usd / 1e6,
    aw_achieved_taf = aw_achieved_af / 1000
  ) %>%
  pivot_longer(
    cols = c(acres_selected, revenue_cost_millions, aw_achieved_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_selected", "revenue_cost_millions", "aw_achieved_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Applied water saved (TAF)"))
  )

# Color palette for scenarios
scenario_colors <- c(
  "Baseline"            = "#3266ad",
  "RCP4.5 (2020-2049)"  = "#E8A825",
  "RCP8.5 (2020-2049)"  = "#C7432B"
)

fig1 <- ggplot(fig1_data, aes(x = basin, y = value, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = label_comma()) +
  labs(
    title = "Basin-level optimization results across climate scenarios (applied water target only)",
    subtitle = "Acres retired, foregone revenue, and applied water saved by groundwater basin",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA)
  )
fig1

ggsave(file.path(fig_dir, "fig1_basin_results_by_scenario.png"), fig1,
       width = 13, height = 8, dpi = 600, bg = "white")
cat("  Saved: fig1_basin_results_by_scenario.png\n")


# ==============================================================================
# FIGURE 2: Valley-wide scenario comparison (acres + revenue + fields)
# ==============================================================================

cat("Creating Figure 2: Valley-wide scenario comparison...\n")

# Read methodology so subtitle can reflect actual targets
target_method <- read_csv(file.path(prioritizr_dir, "target_methodology.csv"),
                          show_col_types = FALSE)
valley_base_taf  <- as.numeric(target_method$value[target_method$parameter == "Valley baseline AW reduction (TAF)"])
valley_rcp45_taf <- as.numeric(target_method$value[target_method$parameter == "Valley RCP4.5 target (TAF)"])
valley_rcp85_taf <- as.numeric(target_method$value[target_method$parameter == "Valley RCP8.5 target (TAF)"])

# Acres panel
fig2a <- ggplot(valley_plot, aes(x = scenario_label, y = acres_selected)) +
  geom_col(fill = "#854F0B", width = 0.6) +
  geom_text(aes(label = format(acres_selected, big.mark = ",")),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "A", x = NULL, y = "Acres") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18)
  )

# Revenue panel
fig2b <- ggplot(valley_plot, aes(x = scenario_label, y = revenue_cost_usd / 1e9)) +
  geom_col(fill = "#0F6E56", width = 0.6) +
  geom_text(aes(label = paste0("$", round(revenue_cost_usd / 1e9, 2), "B")),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0("$", x, "B"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "B", x = NULL, y = "$ billions") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18)
  )

# Applied water savings panel (show achieved)
fig2c <- ggplot(valley_plot, aes(x = scenario_label)) +
  geom_col(aes(y = aw_achieved_af / 1000), fill = "#3266ad", width = 0.6) +
  geom_text(aes(y = aw_achieved_af / 1000,
                label = paste0(format(round(aw_achieved_af / 1000), big.mark = ","), " TAF")),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = label_comma()) +
  labs(title = "C", x = NULL, y = "TAF/yr") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.caption = element_text(size = 9, color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 18)
  )

# Build subtitle dynamically from targets
fig2_subtitle <- sprintf(
  "Each RCP scenario uses a climate-adjusted applied water reduction target (Baseline: %s TAF; RCP4.5: %s TAF; RCP8.5: %s TAF)",
  format(round(valley_base_taf), big.mark = ","),
  format(round(valley_rcp45_taf), big.mark = ","),
  format(round(valley_rcp85_taf), big.mark = ",")
)

fig2 <- fig2a + fig2b + fig2c +
  plot_annotation(
    title = "Valley-wide optimization results across climate scenarios (applied water)",
    subtitle = fig2_subtitle,
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )
fig2

ggsave(file.path(fig_dir, "fig2_valley_scenario_comparison.png"), fig2,
       width = 15, height = 8, dpi = 600, bg = "white")
cat("  Saved: fig2_valley_scenario_comparison.png\n")


# ==============================================================================
# FIGURE 3: Spatial maps — selected fields per scenario + selection frequency
# ==============================================================================

cat("Creating Figure 3: Spatial maps...\n")

# --- 3a-c: Individual scenario maps ---

# Build a combined sf object with selection status from each scenario
# Start with the baseline solution as the spatial template
base_sol <- valley_solutions[["Baseline"]]

if (!is.null(base_sol)) {
  
  # Extract selection columns from each scenario
  selection_data <- base_sol %>%
    select(id) %>%
    mutate(
      selected_baseline = valley_solutions[["Baseline"]]$solution_1,
      selected_rcp45    = valley_solutions[["RCP45_2020_2049"]]$solution_1,
      selected_rcp85    = valley_solutions[["RCP85_2020_2049"]]$solution_1,
      # Selection frequency: how many scenarios selected this field
      n_selected = selected_baseline + selected_rcp45 + selected_rcp85
    )
  
  # Define consistent bbox from the data
  map_bbox <- st_bbox(selection_data)
  
  # Scenario label mapping (dynamic targets)
  scenario_map_labels <- c(
    "selected_baseline" = sprintf("A: Baseline (target: %s TAF)",
                                  format(round(valley_base_taf), big.mark = ",")),
    "selected_rcp45"    = sprintf("B: RCP 4.5 (target: %s TAF)",
                                  format(round(valley_rcp45_taf), big.mark = ",")),
    "selected_rcp85"    = sprintf("C: RCP 8.5 (target: %s TAF)",
                                  format(round(valley_rcp85_taf), big.mark = ","))
  )
  
  # --- Individual scenario maps (3a, 3b, 3c) ---
  make_scenario_map <- function(data, sel_col, title_text) {
    
    # Separate selected vs not-selected for layering
    plot_data <- data %>%
      mutate(status = ifelse(!!sym(sel_col) == 1, "Selected", "Not selected"))
    
    ggplot() +
      # Basin boundaries (background)
      geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
      # Non-selected fields (light gray, subtle)
      geom_sf(data = plot_data %>% filter(status == "Not selected"),
              fill = "gray85", color = NA, linewidth = 0) +
      # Selected fields (blue)
      geom_sf(data = plot_data %>% filter(status == "Selected"),
              fill = "#2166AC", color = NA, linewidth = 0) +
      # Basin boundaries on top
      geom_sf(data = sjv_basins, fill = NA, color = "gray40", linewidth = 0.3) +
      coord_sf(xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
               ylim = c(map_bbox["ymin"], map_bbox["ymax"])) +
      labs(title = title_text) +
      theme_void(base_size = 10) +
      theme(
        plot.title = element_text(face = "bold", size = 12, hjust = 0),
        plot.margin = margin(5, 5, 5, 5), 
        panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        axis.text        = element_text(size = 10, color = "grey30"),
        axis.ticks       = element_line(color = "grey30")
      ) + 
      scale_y_continuous(n.breaks = 7) + 
      scale_x_continuous(n.breaks = 4)
  }
  
  map_baseline <- make_scenario_map(selection_data, "selected_baseline",
                                    scenario_map_labels["selected_baseline"])
  map_rcp45    <- make_scenario_map(selection_data, "selected_rcp45",
                                    scenario_map_labels["selected_rcp45"])
  map_rcp85    <- make_scenario_map(selection_data, "selected_rcp85",
                                    scenario_map_labels["selected_rcp85"])
  
  # --- Selection frequency map (3d) ---
  
  # Define color palette: 0 = light gray, 1-3 = viridis gradient
  freq_colors <- c(
    "0" = "gray85",
    "1" = "#FDE725",
    "2" = "#21918C",
    "3" = "#440154"
  )
  
  freq_labels <- c(
    "0" = "Not selected (0)",
    "1" = "1 scenario",
    "2" = "2 scenarios",
    "3" = "3 scenarios"
  )
  
  selection_data <- selection_data %>%
    mutate(n_selected_fct = factor(n_selected, levels = c("0", "1", "2", "3")))
  
  map_frequency <- ggplot() +
    geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
    geom_sf(data = selection_data,
            aes(fill = n_selected_fct), color = NA, linewidth = 0) +
    geom_sf(data = sjv_basins, fill = NA, color = "gray40", linewidth = 0.3) +
    coord_sf(xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
             ylim = c(map_bbox["ymin"], map_bbox["ymax"])) +
    scale_fill_manual(values = freq_colors, labels = freq_labels,
                      name = "Selection\nfrequency", drop = FALSE) +
    labs(title = "D: Selection frequency") +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 12, hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 12),
      plot.margin = margin(5, 5, 5, 5), 
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 10, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"), 
      legend.margin = margin(t = 10),
      legend.spacing.y = unit(0.2, "cm")
    ) +
    scale_y_continuous(n.breaks = 7) + 
    scale_x_continuous(n.breaks = 4) + 
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.3, color = "gray40")))
  
  # --- Combine into 2x2 grid ---
  fig3 <- (map_baseline + map_rcp45) / (map_rcp85 + map_frequency) +
    plot_annotation(
      title = "Spatial distribution of fields selected for retirement (applied water target only)",
      subtitle = "Valley-wide optimization under three climate scenarios with scenario-specific targets",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 11)
      )
    )
  fig3
  
  ggsave(file.path(fig_dir, "fig3_spatial_maps_frequency.png"), fig3,
         width = 10, height = 10, dpi = 600, bg = "white")
  cat("  Saved: fig3_spatial_maps_frequency.png\n")
}


# ==============================================================================
# FIGURE 5: Retirement totals by crop type across climate scenarios
# ==============================================================================

cat("Creating Figure 5: Retirement by crop type...\n")

# Check that the GeoPackages include a 'comm' column (commodity name)
crop_results_list <- list()

scenario_labels <- list(
  Baseline        = "Baseline",
  RCP45_2020_2049 = "RCP4.5 (2020-2049)",
  RCP85_2020_2049 = "RCP8.5 (2020-2049)"
)

# Map scenario -> AW column name
scenario_aw_col <- list(
  Baseline        = "AW_baseline_AF",
  RCP45_2020_2049 = "AW_RCP45_near_AF",
  RCP85_2020_2049 = "AW_RCP85_near_AF"
)

for (scen in names(valley_gpkg_paths)) {
  
  if (is.null(valley_solutions[[scen]])) next
  
  aw_col <- scenario_aw_col[[scen]]
  
  selected <- valley_solutions[[scen]] %>%
    st_drop_geometry() %>%
    filter(solution_1 == 1) %>%
    mutate(
      scenario = scenario_labels[[scen]],
      aw_af = .data[[aw_col]]
    ) %>%
    select(comm, acres, revenue, aw_af, scenario)
  
  crop_results_list[[scen]] <- selected
}

crop_results <- bind_rows(crop_results_list)

# clean up comm names
crop_results <- crop_results %>% 
  mutate(comm = case_match(comm,
                           "Alfalfa & Alfalfa Mixtures" ~ "Alfalfa",
                           "Miscellaneous Grasses"      ~ "Misc. Grasses",
                           .default = comm
  ))

# --- Identify top 10 crops by baseline acres retired ---
top_crops <- crop_results %>%
  filter(scenario == "Baseline") %>%
  group_by(comm) %>%
  summarise(total_acres = sum(acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_acres)) %>%
  slice_head(n = 10) %>%
  pull(comm)

# --- Aggregate by crop type, lumping non-top-10 into "All other crops" ---
crop_summary <- crop_results %>%
  mutate(
    crop_group = ifelse(comm %in% top_crops, comm, "All other crops")
  ) %>%
  group_by(crop_group, scenario) %>%
  summarise(
    acres_retired = sum(acres, na.rm = TRUE),
    revenue_millions = sum(revenue, na.rm = TRUE) / 1e6,
    aw_taf = sum(aw_af, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# --- Set crop order: top 10 by baseline acres, "All other crops" at bottom ---
crop_order <- crop_results %>%
  filter(scenario == "Baseline") %>%
  mutate(crop_group = ifelse(comm %in% top_crops, comm, "All other crops")) %>%
  group_by(crop_group) %>%
  summarise(total_acres = sum(acres, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_acres) %>%
  pull(crop_group)

# Ensure "All other crops" is at the bottom (first in factor for coord_flip)
crop_order <- c("All other crops", crop_order[crop_order != "All other crops"])

crop_summary$crop_group <- factor(crop_summary$crop_group, levels = crop_order)
crop_summary$scenario <- factor(crop_summary$scenario,
                                levels = c("Baseline", "RCP4.5 (2020-2049)", "RCP8.5 (2020-2049)"))

# --- Reshape for faceted plot ---
fig5_data <- crop_summary %>%
  pivot_longer(
    cols = c(acres_retired, revenue_millions, aw_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_retired", "revenue_millions", "aw_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Applied water saved (TAF)"))
  )

# --- Build figure ---
fig5 <- ggplot(fig5_data, aes(x = crop_group, y = value, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = label_comma()) +
  labs(
    title = "Retirement totals by crop type across climate scenarios (applied water target)",
    subtitle = "Top 10 crop types by baseline acres retired, with remaining types grouped",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text.y = element_text(
      face = ifelse(levels(crop_summary$crop_group) == "All other crops", "italic", "plain"),
      size = 12
    ), 
    axis.text.x = element_text(size = 12), 
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    panel.spacing.x = unit(1.5, "lines")
  )
fig5

ggsave(file.path(fig_dir, "fig5_retirement_by_crop_type.png"), fig5,
       width = 12, height = 7, dpi = 600, bg = "white")
cat("  Saved: fig5_retirement_by_crop_type.png\n")



# ==============================================================================
# TABLE 1: Basin-level results by scenario
# ==============================================================================
# One row per basin, columns grouped by scenario: target (TAF), AW achieved
# (TAF), acres retired, fields selected, revenue ($M). Valley-wide totals at bottom.

cat("\n========================================\n")
cat("GENERATING TABLES\n")
cat("========================================\n")

cat("Creating Table 1: Basin-level results by scenario...\n")

# Parse basin names and scenarios from the label column
basin_parsed <- basin_summary %>%
  mutate(
    scenario_short = case_when(
      grepl("Baseline$", scenario)        ~ "Baseline",
      grepl("RCP45_2020_2049$", scenario) ~ "RCP45",
      grepl("RCP85_2020_2049$", scenario) ~ "RCP85"
    ),
    basin = scenario %>%
      str_remove("^Basin_") %>%
      str_remove("_(Baseline|RCP45_2020_2049|RCP85_2020_2049)$") %>%
      str_replace_all("_", " ")
  )

# Get targets from basin_ref
basin_targets_long <- basin_ref %>%
  filter(aw_reduction_taf > 0) %>%
  select(basin, target_baseline_taf, target_rcp45_taf, target_rcp85_taf) %>%
  pivot_longer(cols = starts_with("target_"),
               names_to = "scenario_short",
               values_to = "target_taf") %>%
  mutate(
    scenario_short = case_when(
      grepl("baseline", scenario_short) ~ "Baseline",
      grepl("rcp45", scenario_short)    ~ "RCP45",
      grepl("rcp85", scenario_short)    ~ "RCP85"
    )
  )

# Join and compute display values
table1_long <- basin_parsed %>%
  left_join(basin_targets_long, by = c("basin", "scenario_short")) %>%
  mutate(
    revenue_millions = round(revenue_cost_usd / 1e6, 1),
    aw_taf = round(aw_achieved_af / 1000, 1),
    target_taf = round(target_taf, 1)
  ) %>%
  select(basin, scenario_short, target_taf, aw_taf, acres_selected,
         n_fields_selected, revenue_millions)

# Pivot wide: one row per basin
table1_wide <- table1_long %>%
  pivot_wider(
    names_from = scenario_short,
    values_from = c(target_taf, aw_taf, acres_selected, n_fields_selected, revenue_millions),
    names_glue = "{scenario_short}_{.value}"
  ) %>%
  select(
    basin,
    Baseline_target_taf, Baseline_aw_taf, Baseline_acres_selected,
    Baseline_n_fields_selected, Baseline_revenue_millions,
    RCP45_target_taf, RCP45_aw_taf, RCP45_acres_selected,
    RCP45_n_fields_selected, RCP45_revenue_millions,
    RCP85_target_taf, RCP85_aw_taf, RCP85_acres_selected,
    RCP85_n_fields_selected, RCP85_revenue_millions
  ) %>%
  arrange(desc(Baseline_target_taf))

# Valley-wide totals row
valley_row <- valley_summary %>%
  mutate(
    scenario_short = case_when(
      grepl("Baseline", scenario) ~ "Baseline",
      grepl("RCP45", scenario)    ~ "RCP45",
      grepl("RCP85", scenario)    ~ "RCP85"
    ),
    target_taf = round(target_af / 1000, 1),
    aw_taf = round(aw_achieved_af / 1000, 1),
    revenue_millions = round(revenue_cost_usd / 1e6, 1)
  ) %>%
  select(scenario_short, target_taf, aw_taf, acres_selected,
         n_fields_selected, revenue_millions) %>%
  pivot_wider(
    names_from = scenario_short,
    values_from = c(target_taf, aw_taf, acres_selected, n_fields_selected, revenue_millions),
    names_glue = "{scenario_short}_{.value}"
  ) %>%
  mutate(basin = "Valley-wide total") %>%
  select(
    basin,
    Baseline_target_taf, Baseline_aw_taf, Baseline_acres_selected,
    Baseline_n_fields_selected, Baseline_revenue_millions,
    RCP45_target_taf, RCP45_aw_taf, RCP45_acres_selected,
    RCP45_n_fields_selected, RCP45_revenue_millions,
    RCP85_target_taf, RCP85_aw_taf, RCP85_acres_selected,
    RCP85_n_fields_selected, RCP85_revenue_millions
  )

table1_final <- bind_rows(table1_wide, valley_row)

# Format numbers for display
table1_display <- table1_final %>%
  rename(Basin = basin) %>%
  mutate(across(contains("acres") | contains("fields"), ~format(., big.mark = ",")))

# Build kableExtra table
kbl_table1 <- table1_display %>%
  kbl(
    col.names = c("Basin",
                  "Target", "AW", "Acres", "Fields", "Rev ($M)",
                  "Target", "AW", "Acres", "Fields", "Rev ($M)",
                  "Target", "AW", "Acres", "Fields", "Rev ($M)"),
    align = c("l", rep("r", 15)),
    caption = "Table 1. Basin-level optimization results by climate scenario (applied water target). Targets and AW in TAF/yr; revenue in millions USD."
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  add_header_above(c(" " = 1,
                     "Baseline" = 5,
                     "RCP 4.5 (2020-2049)" = 5,
                     "RCP 8.5 (2020-2049)" = 5)) %>%
  row_spec(nrow(table1_display), bold = TRUE,
           extra_css = "border-top: 2px solid #333;") %>%
  kableExtra::footnote(
    general = "Baseline targets derived from PPIC San Joaquin Valley dataset (Escriva-Bou et al. 2023; 'Corrected reduction in groundwater supplies no negative' field, aggregated to basin level). RCP 4.5 targets are baseline targets scaled by 6.3% following PPIC's published climate-change adjustment (2.68 -> 2.85 MAF). RCP 8.5 targets apply an additional multiplier derived from the ratio of ensemble mean PET change between RCP scenarios in the BCMv8 projections. Valley-wide totals reflect the valley-wide optimization, not the sum of basin-specific runs.",
    general_title = "Note: "
  )

save_kable(kbl_table1, file.path(fig_dir, "table1_basin_results.html"))
cat("  Saved: table1_basin_results.html\n")

# Also save as CSV for flexibility
write_csv(table1_final, file.path(fig_dir, "table1_basin_results.csv"))
cat("  Saved: table1_basin_results.csv\n")


# ==============================================================================
# TABLE 2: Valley-wide scenario summary (compact)
# ==============================================================================

cat("Creating Table 2: Valley-wide scenario summary...\n")

# Get multipliers from target methodology
rcp45_mult <- as.numeric(target_method$value[target_method$parameter == "RCP4.5 multiplier"])
rcp85_mult <- as.numeric(target_method$value[target_method$parameter == "RCP8.5 multiplier"])

table2 <- valley_summary %>%
  mutate(
    Scenario = case_when(
      grepl("Baseline", scenario) ~ "Baseline",
      grepl("RCP45", scenario)    ~ "RCP 4.5 (2020-2049)",
      grepl("RCP85", scenario)    ~ "RCP 8.5 (2020-2049)"
    ),
    `Target multiplier` = case_when(
      grepl("Baseline", scenario) ~ "1.000",
      grepl("RCP45", scenario)    ~ as.character(round(rcp45_mult, 4)),
      grepl("RCP85", scenario)    ~ as.character(round(rcp85_mult, 4))
    ),
    `Target (TAF)` = round(target_af / 1000, 1),
    `AW saved (TAF)` = round(aw_achieved_af / 1000, 1),
    `Acres retired` = format(acres_selected, big.mark = ","),
    `Fields selected` = format(n_fields_selected, big.mark = ","),
    `Revenue cost ($M)` = round(revenue_cost_usd / 1e6, 1)
  ) %>%
  select(Scenario, `Target multiplier`, `Target (TAF)`, `AW saved (TAF)`,
         `Acres retired`, `Fields selected`, `Revenue cost ($M)`)

kbl_table2 <- table2 %>%
  kbl(
    align = c("l", rep("r", 7)),
    caption = "Table 2. Valley-wide optimization results across climate scenarios (applied water target)."
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13
  ) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(
    general = sprintf(
      paste0("Target multiplier reflects the proportional scaling of the baseline PPIC SGMA applied water reduction target: ",
             "%.3f for RCP 4.5 (Escriva-Bou et al., 2023; reflecting the 6.3%% increase from 2.68 to 2.85 MAF under median RCP 4.5 conditions) ",
             "and %.3f for RCP 8.5 (the RCP 4.5 increment scaled by the ratio of ensemble mean PET change between RCP scenarios ",
             "in the BCMv8 projections)."),
      rcp45_mult, rcp85_mult
    ),
    general_title = "Note: "
  )

save_kable(kbl_table2, file.path(fig_dir, "table2_valley_summary.html"))
cat("  Saved: table2_valley_summary.html\n")

write_csv(table2, file.path(fig_dir, "table2_valley_summary.csv"))
cat("  Saved: table2_valley_summary.csv\n")


# ==============================================================================
# TABLE 3: Selection consistency across scenarios
# ==============================================================================
# How many fields are selected in 0, 1, 2, or all 3 scenarios?
# Tabular companion to the selection frequency map (Figure 3d).

cat("Creating Table 3: Selection consistency across scenarios...\n")

if (all(file.exists(unlist(valley_gpkg_paths)))) {
  
  # Build selection frequency from already-loaded solutions
  consistency <- valley_solutions[["Baseline"]] %>%
    st_drop_geometry() %>%
    select(id, acres, revenue) %>%
    mutate(
      sel_baseline = valley_solutions[["Baseline"]]$solution_1,
      sel_rcp45    = valley_solutions[["RCP45_2020_2049"]]$solution_1,
      sel_rcp85    = valley_solutions[["RCP85_2020_2049"]]$solution_1,
      n_selected   = sel_baseline + sel_rcp45 + sel_rcp85
    )
  
  table3_data <- consistency %>%
    group_by(n_selected) %>%
    summarise(
      n_fields = n(),
      total_acres = round(sum(acres, na.rm = TRUE)),
      total_revenue_m = round(sum(revenue, na.rm = TRUE) / 1e6, 1),
      .groups = "drop"
    ) %>%
    mutate(
      pct_fields = round(n_fields / sum(n_fields) * 100, 1),
      pct_acres = round(total_acres / sum(total_acres) * 100, 1)
    )
  
  table3_display <- table3_data %>%
    mutate(
      Category = case_when(
        n_selected == 0 ~ "Not selected in any scenario",
        n_selected == 1 ~ "Selected in 1 scenario",
        n_selected == 2 ~ "Selected in 2 scenarios",
        n_selected == 3 ~ "Selected in all 3 scenarios"
      ),
      Fields = format(n_fields, big.mark = ","),
      `% Fields` = paste0(pct_fields, "%"),
      Acres = format(total_acres, big.mark = ","),
      `% Acres` = paste0(pct_acres, "%"),
      `Revenue ($M)` = total_revenue_m
    ) %>%
    select(Category, Fields, `% Fields`, Acres, `% Acres`, `Revenue ($M)`)
  
  kbl_table3 <- table3_display %>%
    kbl(
      align = c("l", "r", "r", "r", "r", "r"),
      caption = "Table 3. Selection consistency of fields across three climate scenarios (valley-wide optimization, applied water target)."
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      font_size = 13
    ) %>%
    row_spec(nrow(table3_display), bold = TRUE,
             extra_css = "background-color: #E8F0FE;") %>%
    footnote(
      general = "Fields selected in all 3 scenarios represent a 'core portfolio' of no-regret retirements that are optimal regardless of climate trajectory. Fields selected in only 1 scenario are climate-sensitive selections whose inclusion depends on the severity of future conditions.",
      general_title = "Note: "
    )
  
  save_kable(kbl_table3, file.path(fig_dir, "table3_selection_consistency.html"))
  cat("  Saved: table3_selection_consistency.html\n")
  
  write_csv(table3_data, file.path(fig_dir, "table3_selection_consistency.csv"))
  cat("  Saved: table3_selection_consistency.csv\n")
  
} else {
  cat("  Skipping Table 3: solution GeoPackages not found.\n")
}



# ==============================================================================
# WATER-ONLY (AW) FIGURES: VALLEY-WIDE vs. BASIN-SPECIFIC COMPARISON
# ==============================================================================
# This block builds three new figures and an updated version of Figure 1 to 
# highlight the operational tension between valley-wide and basin-specific 
# optimization (i.e., regional efficiency vs. local SGMA implementation, where 
# each GSA is independently responsible for its basin's overdraft reduction).
#
# NEW / UPDATED:
#   - Figure 1b: same basin-level bars as Fig 1, plus a "Total" row that sums
#                across basins for each scenario. The Total row is directly
#                comparable to Figure 2 (valley-wide).
#   - Figure 4:  Spatial maps comparing valley-wide vs. basin-specific
#                solutions side-by-side, across 3 climate scenarios (2x3 grid).
#   - Figure 6:  Per-basin retirement totals comparing basin-specific runs
#                vs. each basin's share of the valley-wide run. Reveals how
#                valley-wide optimization redistributes effort (e.g. drops
#                some Kern retirements onto cheaper basins elsewhere).
#   - Figure 7:  Retirement efficiency (acres/TAF, $/TAF, fields/TAF) comparing
#                valley-wide vs. basin-specific approaches across scenarios.
#
# Assumes the following objects already exist from the upstream script:
#   - prioritizr_dir, fig_dir, basins_path
#   - valley_summary, basin_summary, basin_ref, basin_results, valley_plot
#   - valley_gpkg_paths, valley_solutions
#   - sjv_basins, scenario_colors
#   - valley_base_taf, valley_rcp45_taf, valley_rcp85_taf
# ==============================================================================


# ==============================================================================
# SECTION A: Load all basin-specific solution GeoPackages
# ==============================================================================
# 15 basins x 3 scenarios = 45 files (or whatever the optimization produced).
# Each file is a single-basin solution; we combine them into one sf object per
# scenario, with a `basin` column attached for downstream grouping.

cat("\n========================================\n")
cat("LOADING BASIN-SPECIFIC SOLUTIONS\n")
cat("========================================\n")

# Discover all basin-specific GPKGs in the prioritizr output directory
basin_gpkgs <- list.files(prioritizr_dir,
                          pattern = "^Basin_.*\\.gpkg$",
                          full.names = TRUE)

cat("  Found", length(basin_gpkgs), "basin-specific GeoPackages\n")

# Parse basin + scenario from filename, load, and tag
parse_basin_scenario <- function(path) {
  fname <- tools::file_path_sans_ext(basename(path))
  # Format: Basin_{basin_with_underscores}_{Baseline|RCP45_2020_2049|RCP85_2020_2049}
  scen <- case_when(
    grepl("_Baseline$",        fname) ~ "Baseline",
    grepl("_RCP45_2020_2049$", fname) ~ "RCP45_2020_2049",
    grepl("_RCP85_2020_2049$", fname) ~ "RCP85_2020_2049"
  )
  basin <- fname %>%
    str_remove("^Basin_") %>%
    str_remove("_(Baseline|RCP45_2020_2049|RCP85_2020_2049)$") %>%
    str_replace_all("_", " ")
  list(basin = basin, scenario = scen)
}

# Load all and bind per scenario
basin_sols_by_scenario <- list(
  Baseline        = list(),
  RCP45_2020_2049 = list(),
  RCP85_2020_2049 = list()
)

for (gpath in basin_gpkgs) {
  meta <- parse_basin_scenario(gpath)
  if (is.na(meta$scenario)) next
  
  sol <- st_read(gpath, quiet = TRUE)
  # Force the basin tag from the filename (in case the saved attribute lost it)
  sol$basin <- meta$basin
  basin_sols_by_scenario[[meta$scenario]][[meta$basin]] <- sol
}

# Combine each scenario's basin solutions into one sf object
combine_basin_sols <- function(sol_list) {
  if (length(sol_list) == 0) return(NULL)
  # Different basins have slightly different schemas; keep the safe subset
  keeper_cols <- c("id", "basin", "acres", "revenue", "comm",
                   "AW_baseline_AF", "AW_RCP45_near_AF", "AW_RCP85_near_AF",
                   "solution_1", "scenario", "aw_col_used", "target_taf")
  sol_list %>%
    map(~ select(.x, any_of(keeper_cols))) %>%
    bind_rows()
}

basin_combined <- list(
  Baseline        = combine_basin_sols(basin_sols_by_scenario$Baseline),
  RCP45_2020_2049 = combine_basin_sols(basin_sols_by_scenario$RCP45_2020_2049),
  RCP85_2020_2049 = combine_basin_sols(basin_sols_by_scenario$RCP85_2020_2049)
)

for (scen in names(basin_combined)) {
  if (!is.null(basin_combined[[scen]])) {
    n_sel <- sum(basin_combined[[scen]]$solution_1 == 1)
    cat("  ", scen, ": ", nrow(basin_combined[[scen]]),
        " planning units across ",
        length(unique(basin_combined[[scen]]$basin)),
        " basins (", n_sel, " selected)\n", sep = "")
  }
}


# ==============================================================================
# FIGURE 1b: Basin-level results across scenarios, WITH a "Total" row
# ==============================================================================
# Same as Fig 1 but adds a "Total" row (sum across basins, per scenario).
# This makes the basin-specific summed total directly comparable to Figure 2
# (valley-wide).

cat("\nCreating Figure 1b: Basin-level results with Total row...\n")

# Compute totals across basins, per scenario
basin_totals <- basin_results %>%
  filter(!is.na(basin)) %>%
  group_by(scenario_label) %>%
  summarise(
    acres_selected   = sum(acres_selected,   na.rm = TRUE),
    revenue_cost_usd = sum(revenue_cost_usd, na.rm = TRUE),
    aw_achieved_af   = sum(aw_achieved_af,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(basin = "Total (basin-specific)")

# Add the Total row to a copy of the basin results
basin_results_with_total <- basin_results %>%
  select(basin, scenario_label, acres_selected, revenue_cost_usd, aw_achieved_af) %>%
  mutate(basin = as.character(basin)) %>%
  bind_rows(basin_totals %>% select(basin, scenario_label, acres_selected,
                                    revenue_cost_usd, aw_achieved_af))

# Reorder factor: Total at the top (which becomes the bottom after coord_flip's
# reversal, so we want it FIRST in the factor levels for it to appear at top
# after the flip — actually the easiest way is to set it as the LAST level)
fig1b_basin_order <- c(levels(basin_results$basin), "Total (basin-specific)")
basin_results_with_total$basin <- factor(basin_results_with_total$basin,
                                         levels = fig1b_basin_order)

# Reshape for faceted plotting
fig1b_data <- basin_results_with_total %>%
  filter(!is.na(basin)) %>%
  mutate(
    revenue_cost_millions = revenue_cost_usd / 1e6,
    aw_achieved_taf       = aw_achieved_af / 1000
  ) %>%
  pivot_longer(
    cols = c(acres_selected, revenue_cost_millions, aw_achieved_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_selected", "revenue_cost_millions", "aw_achieved_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Applied water saved (TAF)"))
  )

# Build figure with a horizontal rule separating Total from individual basins
fig1b <- ggplot(fig1b_data, aes(x = basin, y = value, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
  # Separator: dotted line between Total and the rest
  geom_vline(xintercept = length(fig1b_basin_order) - 0.5,
             linetype = "dashed", color = "gray40", linewidth = 0.5) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = label_comma()) +
  labs(
    title = "Basin-level results across climate scenarios, with summed totals (applied water)",
    subtitle = "Total row shows the sum of basin-specific retirements; compare directly to Figure 2 (valley-wide)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text.y = element_text(
      face = ifelse(levels(basin_results_with_total$basin) == "Total (basin-specific)",
                    "bold", "plain")
    )
  )

print(fig1b)

ggsave(file.path(fig_dir, "fig1b_basin_results_with_total.png"), fig1b,
       width = 13, height = 8.5, dpi = 600, bg = "white")
cat("  Saved: fig1b_basin_results_with_total.png\n")


# ==============================================================================
# FIGURE 1c: Valley totals (from basin-specific runs) + basin-level breakdown
# ==============================================================================
# Top panel (A): valley totals computed by summing basin-specific results,
#                analogous to Figure 2 but using the summed basin-specific runs.
# Bottom panel (B): basin-level breakdown (same as Fig 1, no Total row).
# Both panels use the same scenario color scheme.

cat("\nCreating Figure 1c: Valley totals (basin sum) + basin breakdown...\n")

# --- Top panel (A): valley totals from summed basin-specific runs ---
# Same 3-metric grid as Fig 2, but bars colored by scenario for direct
# visual comparison with the bottom panel.

# Pretty scenario labels for the top panel (multi-line so x-axis text matches Fig 2 style)
basin_totals_top <- basin_totals %>%
  mutate(
    scenario_label_multi = factor(case_when(
      scenario_label == "Baseline"           ~ "Baseline",
      scenario_label == "RCP4.5 (2020-2049)" ~ "RCP4.5\n(2020-2049)",
      scenario_label == "RCP8.5 (2020-2049)" ~ "RCP8.5\n(2020-2049)"
    ), levels = c("Baseline", "RCP4.5\n(2020-2049)", "RCP8.5\n(2020-2049)"))
  )

# Sub-panel 1A: acres
fig1c_top_acres <- ggplot(basin_totals_top,
                          aes(x = scenario_label_multi, y = acres_selected,
                              fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(acres_selected, big.mark = ",")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = scenario_colors, guide = "none") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Acres") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  )

# Sub-panel 1B: revenue
fig1c_top_rev <- ggplot(basin_totals_top,
                        aes(x = scenario_label_multi, y = revenue_cost_usd / 1e6,
                            fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", format(round(revenue_cost_usd / 1e6), big.mark = ","), "M")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = scenario_colors, guide = "none") +
  scale_y_continuous(labels = function(x) paste0("$", x, "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "$ millions") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  )

# Sub-panel 1C: AW saved
fig1c_top_aw <- ggplot(basin_totals_top,
                       aes(x = scenario_label_multi, y = aw_achieved_af / 1000,
                           fill = scenario_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(format(round(aw_achieved_af / 1000), big.mark = ","), " TAF")),
            vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_fill_manual(values = scenario_colors, guide = "none") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "TAF/yr") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 13)
  )

# Assemble top panel A (3 sub-panels side by side)
fig1c_top <- fig1c_top_acres + fig1c_top_rev + fig1c_top_aw +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "A. Summed basin-specific totals across climate scenarios",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

# --- Bottom panel (B): basin-level breakdown (Fig 1 without Total row) ---
# Reshape basin_results (already excludes the Total row) for faceted plotting
fig1c_bot_data <- basin_results %>%
  filter(!is.na(basin)) %>%
  select(basin, scenario_label, acres_selected, revenue_cost_usd, aw_achieved_af) %>%
  mutate(
    revenue_cost_millions = revenue_cost_usd / 1e6,
    aw_achieved_taf       = aw_achieved_af / 1000
  ) %>%
  pivot_longer(
    cols = c(acres_selected, revenue_cost_millions, aw_achieved_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_selected", "revenue_cost_millions", "aw_achieved_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Applied water saved (TAF)"))
  )

fig1c_bot <- ggplot(fig1c_bot_data, aes(x = basin, y = value, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = label_comma()) +
  labs(
    title = "B. Basin-level results across climate scenarios",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

# --- Combine top + bottom with patchwork ---
fig1c <- wrap_elements(fig1c_top) / fig1c_bot +
  plot_layout(heights = c(1, 2.2)) +
  plot_annotation(
    title = "Basin-specific optimization results: valley totals (A) and basin-level breakdown (B), applied water",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(fig1c)

ggsave(file.path(fig_dir, "fig1c_basin_totals_and_breakdown.png"), fig1c,
       width = 14, height = 14, dpi = 600, bg = "white")
cat("  Saved: fig1c_basin_totals_and_breakdown.png\n")



# ==============================================================================
# FIGURE 1d: Revenue efficiency by basin and scenario ($/AF saved)
# ==============================================================================

cat("Creating Figure 1d: Revenue efficiency by basin...\n")

fig1d_data <- basin_results %>%
  filter(!is.na(basin), aw_achieved_af > 0) %>%
  mutate(
    efficiency = revenue_cost_usd / aw_achieved_af
  )

fig1d <- ggplot(fig1d_data, aes(x = basin, y = efficiency, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Revenue efficiency of land retirement by basin",
    subtitle = "Cost per acre-foot of AW savings ($/AF) — lower values indicate more efficient retirement",
    x = NULL,
    y = "Revenue cost per AF saved ($/AF)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14), 
    axis.title.x = element_text(size = 16), 
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    # legend.margin removes space around the legend items
    legend.margin = margin(b = -3), 
    # legend.box.margin adjusts the space between the legend box and the plot area
    legend.box.margin = margin(t = 0, b = -5),
  )
fig1d

ggsave(file.path(fig_dir, "fig1d_revenue_efficiency.png"), fig1d,
       width = 10, height = 9, dpi = 600, bg = "white")
cat("  Saved: fig1d_revenue_efficiency.png\n")



# ==============================================================================
# FIGURE 4: Valley-wide vs. basin-specific spatial maps
# ==============================================================================
# 2x3 grid: rows = approach (Valley-wide, Basin-specific), columns = scenarios.
# Same coloring as Fig 3a-c (selected = blue, not selected = gray).

cat("\nCreating Figure 4: Valley-wide vs. basin-specific spatial maps...\n")

# Reuse the bounding box from Fig 3 (valley_solutions[["Baseline"]])
base_sol_for_bbox <- valley_solutions[["Baseline"]]
map_bbox_4 <- st_bbox(base_sol_for_bbox)

# Map helper (matches Fig 3 styling)
make_compare_map <- function(plot_data, title_text) {
  ggplot() +
    # Basin boundaries (background)
    geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
    # Non-selected fields
    geom_sf(data = plot_data %>% filter(solution_1 == 0),
            fill = "gray85", color = NA, linewidth = 0) +
    # Selected fields
    geom_sf(data = plot_data %>% filter(solution_1 == 1),
            fill = "#2166AC", color = NA, linewidth = 0) +
    # Basin boundaries on top
    geom_sf(data = sjv_basins, fill = NA, color = "gray40", linewidth = 0.3) +
    coord_sf(xlim = c(map_bbox_4["xmin"], map_bbox_4["xmax"]),
             ylim = c(map_bbox_4["ymin"], map_bbox_4["ymax"])) +
    labs(title = title_text) +
    theme_void(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 11, hjust = 0),
      plot.margin      = margin(5, 5, 5, 5),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 9, color = "grey30"),
      axis.ticks       = element_line(color = "grey30")
    ) +
    scale_y_continuous(n.breaks = 7) +
    scale_x_continuous(n.breaks = 4)
}

# Targets for titles
target_lookup <- c(
  Baseline        = valley_base_taf,
  RCP45_2020_2049 = valley_rcp45_taf,
  RCP85_2020_2049 = valley_rcp85_taf
)

scenario_pretty <- c(
  Baseline        = "Baseline",
  RCP45_2020_2049 = "RCP 4.5",
  RCP85_2020_2049 = "RCP 8.5"
)

# Build the 6 panels
fig4_panels <- list()

for (scen in c("Baseline", "RCP45_2020_2049", "RCP85_2020_2049")) {
  
  # Valley-wide map
  vw_title <- sprintf("Valley-wide  |  %s", scenario_pretty[scen])
  fig4_panels[[paste0("vw_", scen)]] <- make_compare_map(
    valley_solutions[[scen]], vw_title
  )
  
  # Basin-specific map (assembled from per-basin solutions)
  bs_data <- basin_combined[[scen]]
  if (!is.null(bs_data)) {
    bs_title <- sprintf("Basin-specific  |  %s", scenario_pretty[scen])
    fig4_panels[[paste0("bs_", scen)]] <- make_compare_map(bs_data, bs_title)
  } else {
    fig4_panels[[paste0("bs_", scen)]] <- patchwork::plot_spacer()
  }
}

# Assemble 2x3 grid: row 1 = valley-wide, row 2 = basin-specific
fig4 <- (fig4_panels$vw_Baseline + fig4_panels$vw_RCP45_2020_2049 + fig4_panels$vw_RCP85_2020_2049) /
  (fig4_panels$bs_Baseline + fig4_panels$bs_RCP45_2020_2049 + fig4_panels$bs_RCP85_2020_2049) +
  plot_annotation(
    title = "Spatial distribution of selected fields: valley-wide vs. basin-specific optimization (applied water)",
    subtitle = "Top row: valley-wide target. Bottom row: each basin meets its own PPIC target. Selected fields shown in blue.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 11)
    )
  )

print(fig4)

ggsave(file.path(fig_dir, "fig4_valley_vs_basin_maps.png"), fig4,
       width = 14, height = 11, dpi = 600, bg = "white")
cat("  Saved: fig4_valley_vs_basin_maps.png\n")


# ==============================================================================
# FIGURE 6: Per-basin comparison — basin-specific vs. valley-wide
# ==============================================================================
# For each basin, plot side-by-side bars showing what was retired in
#   (a) the basin-specific run (selecting fields only from that basin to meet
#       that basin's own target), vs.
#   (b) the basin's share of the valley-wide run (selecting fields valley-wide
#       to meet the summed target; how much fell into each basin?).
# Three panels: acres, revenue, AW achieved.

cat("\nCreating Figure 6: Per-basin comparison (basin-specific vs. valley-wide share)...\n")

# --- (a) Basin-specific totals (already have these in basin_results) ---
fig6_basin_specific <- basin_results %>%
  filter(!is.na(basin)) %>%
  select(basin, scenario_label, acres_selected, revenue_cost_usd, aw_achieved_af) %>%
  mutate(
    approach = "Basin-specific run",
    revenue_millions = revenue_cost_usd / 1e6,
    aw_taf           = aw_achieved_af / 1000
  ) %>%
  select(basin, scenario_label, approach, acres_selected, revenue_millions, aw_taf)

# --- (b) Valley-wide solutions, broken down by basin ---
# Need to know which AW column corresponds to each scenario
scenario_aw_col_map <- c(
  Baseline        = "AW_baseline_AF",
  RCP45_2020_2049 = "AW_RCP45_near_AF",
  RCP85_2020_2049 = "AW_RCP85_near_AF"
)

valley_by_basin_list <- list()

for (scen_key in names(valley_solutions)) {
  
  sol <- valley_solutions[[scen_key]]
  if (is.null(sol)) next
  
  aw_col <- scenario_aw_col_map[scen_key]
  
  scen_label_pretty <- case_when(
    scen_key == "Baseline"        ~ "Baseline",
    scen_key == "RCP45_2020_2049" ~ "RCP4.5 (2020-2049)",
    scen_key == "RCP85_2020_2049" ~ "RCP8.5 (2020-2049)"
  )
  
  vw_per_basin <- sol %>%
    st_drop_geometry() %>%
    filter(solution_1 == 1, !is.na(basin)) %>%
    group_by(basin) %>%
    summarise(
      acres_selected   = sum(acres, na.rm = TRUE),
      revenue_millions = sum(revenue, na.rm = TRUE) / 1e6,
      aw_taf           = sum(.data[[aw_col]], na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    mutate(
      scenario_label = scen_label_pretty,
      approach = "Valley-wide run (basin share)"
    )
  
  valley_by_basin_list[[scen_key]] <- vw_per_basin
}

fig6_valley_share <- bind_rows(valley_by_basin_list) %>%
  select(basin, scenario_label, approach, acres_selected, revenue_millions, aw_taf)

# --- Combine ---
fig6_data <- bind_rows(fig6_basin_specific, fig6_valley_share) %>%
  mutate(
    scenario_label = factor(scenario_label,
                            levels = c("Baseline", "RCP4.5 (2020-2049)", "RCP8.5 (2020-2049)")),
    approach = factor(approach,
                      levels = c("Basin-specific run", "Valley-wide run (basin share)"))
  )

# Use the same basin ordering as Fig 1 (by baseline target, descending)
fig6_data$basin <- factor(fig6_data$basin, levels = basin_order)

# Reshape for facets
fig6_long <- fig6_data %>%
  filter(!is.na(basin)) %>%
  pivot_longer(
    cols = c(acres_selected, revenue_millions, aw_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_selected", "revenue_millions", "aw_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Applied water saved (TAF)"))
  )

# Color: encode approach via fill, scenario via facet column
# Use a 6-color palette: approach x scenario
approach_scen_colors <- c(
  "Basin-specific run | Baseline"             = "#3266ad",
  "Basin-specific run | RCP4.5 (2020-2049)"   = "#E8A825",
  "Basin-specific run | RCP8.5 (2020-2049)"   = "#C7432B",
  "Valley-wide run (basin share) | Baseline"           = "#88B1D4",
  "Valley-wide run (basin share) | RCP4.5 (2020-2049)" = "#F5D17F",
  "Valley-wide run (basin share) | RCP8.5 (2020-2049)" = "#E89685"
)

fig6_long <- fig6_long %>%
  mutate(approach_scen = paste(approach, scenario_label, sep = " | "))

# Faceted plot — facets by scenario (columns) x metric (rows), basin on y-axis
fig6 <- ggplot(fig6_long, aes(x = basin, y = value, fill = approach)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  coord_flip() +
  facet_grid(scenario_label ~ metric, scales = "free_x", switch = "y") +
  scale_fill_manual(
    values = c("Basin-specific run" = "#1f4e79",
               "Valley-wide run (basin share)" = "#a8c8e8"),
    name = "Optimization approach"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10)),
                     labels = label_comma()) +
  labs(
    title = "Per-basin retirement: basin-specific run vs. share from valley-wide run (applied water)",
    subtitle = "Reveals how valley-wide optimization redistributes retirement effort across basins relative to local SGMA targets",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.placement = "outside",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0.5, "lines")
  )

print(fig6)

ggsave(file.path(fig_dir, "fig6_basin_specific_vs_valley_share.png"), fig6,
       width = 14, height = 11, dpi = 600, bg = "white")
cat("  Saved: fig6_basin_specific_vs_valley_share.png\n")


# Also export the comparison data as CSV for the manuscript supplement
fig6_export <- fig6_data %>%
  select(scenario_label, basin, approach, acres_selected, revenue_millions, aw_taf) %>%
  arrange(scenario_label, basin, approach)

write_csv(fig6_export, file.path(fig_dir, "fig6_basin_comparison_data.csv"))
cat("  Saved: fig6_basin_comparison_data.csv\n")


# ==============================================================================
# FIGURE 7: Retirement efficiency — valley-wide vs. basin-specific
# ==============================================================================
# Efficiency = output per unit input. Lower = better.
# Three metrics: acres / TAF saved, $ / TAF saved, fields / TAF saved.
# Computed at the valley aggregate level (summed across all basins for the
# basin-specific approach).

cat("\nCreating Figure 7: Retirement efficiency comparison...\n")

# --- Valley-wide aggregate efficiency (one row per scenario) ---
valley_efficiency <- valley_summary %>%
  mutate(
    scenario_label = case_when(
      grepl("Baseline", scenario) ~ "Baseline",
      grepl("RCP45", scenario)    ~ "RCP4.5 (2020-2049)",
      grepl("RCP85", scenario)    ~ "RCP8.5 (2020-2049)"
    ),
    approach = "Valley-wide",
    aw_taf = aw_achieved_af / 1000,
    acres_per_taf  = acres_selected / aw_taf,
    dollars_per_af = revenue_cost_usd / aw_achieved_af,
    fields_per_taf = n_fields_selected / aw_taf
  ) %>%
  select(scenario_label, approach, acres_per_taf, dollars_per_af, fields_per_taf)

# --- Basin-specific aggregate efficiency (sum across basins, per scenario) ---
basin_specific_efficiency <- basin_results %>%
  filter(!is.na(basin)) %>%
  group_by(scenario_label) %>%
  summarise(
    acres_selected   = sum(acres_selected,   na.rm = TRUE),
    revenue_cost_usd = sum(revenue_cost_usd, na.rm = TRUE),
    aw_achieved_af   = sum(aw_achieved_af,   na.rm = TRUE),
    n_fields         = sum(n_fields_selected, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    approach = "Basin-specific (summed)",
    aw_taf = aw_achieved_af / 1000,
    acres_per_taf  = acres_selected / aw_taf,
    dollars_per_af = revenue_cost_usd / aw_achieved_af,
    fields_per_taf = n_fields / aw_taf,
    scenario_label = as.character(scenario_label)
  ) %>%
  select(scenario_label, approach, acres_per_taf, dollars_per_af, fields_per_taf)

efficiency_combined <- bind_rows(valley_efficiency, basin_specific_efficiency) %>%
  mutate(
    scenario_label = factor(scenario_label,
                            levels = c("Baseline", "RCP4.5 (2020-2049)", "RCP8.5 (2020-2049)")),
    approach = factor(approach, levels = c("Valley-wide", "Basin-specific (summed)"))
  )

# Total fields selected across all basin-specific runs, per scenario
cat("\nTotal fields selected across basin-specific runs (per scenario):\n")
basin_results %>%
  filter(!is.na(basin)) %>%
  group_by(scenario_label) %>%
  summarise(total_fields = sum(n_fields_selected, na.rm = TRUE), .groups = "drop") %>%
  print()


# Reshape for facets — drop fields_per_taf, keep only acres and dollars
fig7_long <- efficiency_combined %>%
  pivot_longer(
    cols = c(acres_per_taf, dollars_per_af),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_per_taf", "dollars_per_af"),
                    labels = c("A: ac/TAF", "B: $/AF"))
  )

# Build figure
fig7 <- ggplot(fig7_long, aes(x = scenario_label, y = value, fill = approach)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = ifelse(metric == "B: $/AF",
                               paste0("$", round(value)),
                               round(value, 1))),
            position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  scale_fill_manual(
    values = c("Valley-wide" = "#1f4e79",
               "Basin-specific (summed)" = "#a8c8e8"),
    name = "Optimization approach"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)),
                     labels = label_comma()) +
  labs(
    title = "Retirement efficiency: valley-wide vs. basin-specific optimization (applied water)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "top",
    legend.justification = "left",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.spacing.x = unit(1.5, "lines")
  )

print(fig7)

ggsave(file.path(fig_dir, "fig7_efficiency_comparison.png"), fig7,
       width = 9, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig7_efficiency_comparison.png\n")


# Export underlying numbers
write_csv(efficiency_combined %>%
            mutate(across(where(is.numeric), ~ round(., 2))),
          file.path(fig_dir, "fig7_efficiency_data.csv"))
cat("  Saved: fig7_efficiency_data.csv\n")


cat("\n=== VALLEY vs. BASIN COMPARISON FIGURES COMPLETE ===\n")


cat("\n=== ALL FIGURE AND TABLE GENERATION COMPLETE (AW analysis) ===\n")








