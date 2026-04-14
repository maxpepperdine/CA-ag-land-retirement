# ==============================================================================
# PRIORITIZR WATER SAVINGS — RESULTS VISUALIZATIONS
# ==============================================================================
# Creating figures from the prioritizr optimization results.
#
# FIGURES:
#   1. Basin-level results: acres retired, revenue cost, and S_net achieved
#      across climate scenarios (grouped bar chart, faceted by metric)
#   2. Valley-wide scenario comparison: acres, revenue, fields across scenarios
#      (updated for differentiated targets)
#   3. Spatial maps:
#      3a-c. Selected fields per scenario (Baseline, RCP4.5, RCP8.5)
#      3d.   Selection frequency overlay (0-3 scenarios) with basin boundaries
#
# TABLES (kableExtra, exported as HTML + PNG):
#   1. Basin-level results by scenario (target, S_net, acres, fields, revenue)
#   2. Valley-wide scenario summary
#   3. Selection consistency across scenarios
#
# INPUTS:
#   - valley_wide_summary.csv
#   - basin_specific_summary.csv
#   - basin_reference_table.csv
#   - Valley-wide solution GeoPackages (Valley_Baseline.gpkg, etc.)
#   - DWR groundwater basin boundaries shapefile
#
# OUTPUTS:
#   - Individual figure files (PNG) in output directory
#   - Tables as HTML files in output directory
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
prioritizr_dir <- here("data/intermediate/9_2_prioritizr_water_only/")

# Figure output directory
fig_dir <- here("data/intermediate/10_2_prioritizr_water_only_figures/")

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
basin_ref_clean <- basin_ref %>% filter(overdraft_taf > 0)
basin_order <- basin_ref_clean %>% arrange(overdraft_taf) %>% pull(basin)
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
# Grouped bar chart showing acres retired, revenue cost, and S_net achieved
# per basin, colored by scenario.

cat("Creating Figure 1: Basin-level results across scenarios...\n")

# Reshape basin results for faceted plotting
fig1_data <- basin_results %>%
  filter(!is.na(basin)) %>%
  select(basin, scenario_label, acres_selected, revenue_cost_usd, snet_achieved_af) %>%
  mutate(
    revenue_cost_millions = revenue_cost_usd / 1e6,
    snet_achieved_taf = snet_achieved_af / 1000
  ) %>%
  pivot_longer(
    cols = c(acres_selected, revenue_cost_millions, snet_achieved_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_selected", "revenue_cost_millions", "snet_achieved_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Water savings (TAF)"))
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
    title = "Basin-level optimization results across climate scenarios (water savings target only)",
    subtitle = "Acres retired, foregone revenue, and water savings by groundwater basin",
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
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), # Adjusts x-axis tick labels
    axis.text.y = element_text(size = 16), # Adjusts y-axis tick labels
    axis.title.y = element_text(size = 18) # Adjusts "Acres" title size
  )

# Revenue panel
fig2b <- ggplot(valley_plot, aes(x = scenario_label, y = revenue_cost_usd / 1e9)) +
  geom_col(fill = "#0F6E56", width = 0.6) +
  geom_text(aes(label = paste0("$", round(revenue_cost_usd / 1e6, 0), "M")),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(labels = function(x) paste0("$", x, "B"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "B", x = NULL, y = "$ millions") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), # Adjusts x-axis tick labels
    axis.text.y = element_text(size = 16), # Adjusts y-axis tick labels
    axis.title.y = element_text(size = 18) # Adjusts "Acres" title size
  )

# Water savings panel (show target vs achieved)
fig2c <- ggplot(valley_plot, aes(x = scenario_label)) +
  geom_col(aes(y = snet_achieved_af / 1000), fill = "#3266ad", width = 0.6) +
  geom_text(aes(y = snet_achieved_af / 1000,
                label = paste0(round(snet_achieved_af / 1000), " TAF")),
            vjust = -0.5, size = 5.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "C", x = NULL, y = "TAF/yr") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    plot.caption = element_text(size = 9, color = "gray50"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1), # Adjusts x-axis tick labels
    axis.text.y = element_text(size = 16), # Adjusts y-axis tick labels
    axis.title.y = element_text(size = 18) # Adjusts "Acres" title size
  )

fig2 <- fig2a + fig2b + fig2c +
  plot_annotation(
    title = "Valley-wide optimization results across climate scenarios",
    subtitle = "Each RCP scenario uses a climate-adjusted overdraft reduction target (Baseline: 1,849 TAF; RCP4.5: ~1,966 TAF; RCP8.5: ~2,049 TAF)",
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
  
  # Scenario label mapping
  scenario_map_labels <- c(
    "selected_baseline" = "A: Baseline (target: 1,849 TAF)",
    "selected_rcp45"    = "B: RCP 4.5 (target: ~1,966 TAF)",
    "selected_rcp85"    = "C: RCP 8.5 (target: ~2,049 TAF)"
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
      scale_y_continuous(n.breaks = 4) + 
      scale_x_continuous(n.breaks = 4)
  }
  
  map_baseline <- make_scenario_map(selection_data, "selected_baseline",
                                    scenario_map_labels["selected_baseline"])
  map_rcp45    <- make_scenario_map(selection_data, "selected_rcp45",
                                    scenario_map_labels["selected_rcp45"])
  map_rcp85    <- make_scenario_map(selection_data, "selected_rcp85",
                                    scenario_map_labels["selected_rcp85"])
  
  # --- Selection frequency map (3d) ---
  
  # Define color palette: 0 = light gray, 1-3 = blue gradient
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
    "3" = "All 3 scenarios"
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
                      name = "Selection frequency", drop = FALSE) +
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
    scale_y_continuous(n.breaks = 4) + 
    scale_x_continuous(n.breaks = 4) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.3, color = "gray40")))
  
  # --- Combine into 4-panel figure ---
  fig3 <- (map_baseline + map_rcp45 + map_rcp85 + map_frequency) +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Spatial distribution of fields selected for retirement (water savings only)",
      subtitle = "Valley-wide optimization under three climate scenarios with scenario-specific targets",
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(color = "gray40", size = 11)
      )
    )
  fig3
  
  ggsave(file.path(fig_dir, "fig3_spatial_maps.png"), fig3,
         width = 10, height = 10, dpi = 600, bg = "white")
  cat("  Saved: fig3_spatial_maps.png\n")
  
} else {
  cat("  Skipping Figure 3: solution GeoPackages not found.\n")
}


# ==============================================================================
# FIGURE 4: Revenue efficiency by basin and scenario ($/AF saved)
# ==============================================================================

cat("Creating Figure 4: Revenue efficiency by basin...\n")

fig4_data <- basin_results %>%
  filter(!is.na(basin), snet_achieved_af > 0) %>%
  mutate(
    efficiency = revenue_cost_usd / snet_achieved_af
  )

fig4 <- ggplot(fig4_data, aes(x = basin, y = efficiency, fill = scenario_label)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = scenario_colors, name = "Scenario") +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Revenue efficiency of land retirement by basin",
    subtitle = "Cost per acre-foot of net basin water savings ($/AF) — lower values indicate more efficient retirement",
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
fig4

ggsave(file.path(fig_dir, "fig4_revenue_efficiency.png"), fig4,
       width = 10, height = 9, dpi = 600, bg = "white")
cat("  Saved: fig4_revenue_efficiency.png\n")


# ==============================================================================
# FIGURE 5: Retirement totals by crop type across scenarios
# ==============================================================================
# Top 10 crop types by acres retired (baseline), with "All other crops" category.
# Three-panel faceted horizontal bar chart: acres, revenue, water savings.

cat("Creating Figure 5: Retirement totals by crop type...\n")

# --- Extract selected fields by scenario with correct S_net column ---
crop_results_list <- list()

scenario_snet_cols <- c(
  Baseline        = "Snet_baseline_AF",
  RCP45_2020_2049 = "Snet_RCP45_near_AF",
  RCP85_2020_2049 = "Snet_RCP85_near_AF"
)

scenario_labels <- c(
  Baseline        = "Baseline",
  RCP45_2020_2049 = "RCP4.5 (2020-2049)",
  RCP85_2020_2049 = "RCP8.5 (2020-2049)"
)

for (scen in names(valley_solutions)) {
  sol <- valley_solutions[[scen]]
  if (is.null(sol)) next
  
  snet_col <- scenario_snet_cols[[scen]]
  
  selected <- sol %>%
    st_drop_geometry() %>%
    filter(solution_1 == 1) %>%
    mutate(
      scenario = scenario_labels[[scen]],
      snet_af = .data[[snet_col]]
    ) %>%
    select(comm, acres, revenue, snet_af, scenario)
  
  crop_results_list[[scen]] <- selected
}

crop_results <- bind_rows(crop_results_list)

# clean up comm names
crop_results <- crop_results %>% 
  mutate(comm = case_match(comm,
                           "Alfalfa & Alfalfa Mixtures" ~ "Alfalfa",
                           "Miscellaneous Grasses"      ~ "Misc. Grasses",
                           .default = comm  # This keeps all other values unchanged
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
    snet_taf = sum(snet_af, na.rm = TRUE) / 1000,
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
    cols = c(acres_retired, revenue_millions, snet_taf),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric,
                    levels = c("acres_retired", "revenue_millions", "snet_taf"),
                    labels = c("Acres retired", "Foregone revenue ($ millions)", "Water savings (TAF)"))
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
    title = "Retirement totals by crop type across climate scenarios",
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
    # Italicize "All other crops" to visually distinguish it
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
# One row per basin, columns grouped by scenario: target (TAF), S_net achieved
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
  filter(overdraft_taf > 0) %>%
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
    snet_taf = round(snet_achieved_af / 1000, 1),
    target_taf = round(target_taf, 1)
  ) %>%
  select(basin, scenario_short, target_taf, snet_taf, acres_selected,
         n_fields_selected, revenue_millions)

# Pivot wide: one row per basin
table1_wide <- table1_long %>%
  pivot_wider(
    names_from = scenario_short,
    values_from = c(target_taf, snet_taf, acres_selected, n_fields_selected, revenue_millions),
    names_glue = "{scenario_short}_{.value}"
  ) %>%
  select(
    basin,
    Baseline_target_taf, Baseline_snet_taf, Baseline_acres_selected,
    Baseline_n_fields_selected, Baseline_revenue_millions,
    RCP45_target_taf, RCP45_snet_taf, RCP45_acres_selected,
    RCP45_n_fields_selected, RCP45_revenue_millions,
    RCP85_target_taf, RCP85_snet_taf, RCP85_acres_selected,
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
    snet_taf = round(snet_achieved_af / 1000, 1),
    revenue_millions = round(revenue_cost_usd / 1e6, 1)
  ) %>%
  select(scenario_short, target_taf, snet_taf, acres_selected,
         n_fields_selected, revenue_millions) %>%
  pivot_wider(
    names_from = scenario_short,
    values_from = c(target_taf, snet_taf, acres_selected, n_fields_selected, revenue_millions),
    names_glue = "{scenario_short}_{.value}"
  ) %>%
  mutate(basin = "Valley-wide total") %>%
  select(
    basin,
    Baseline_target_taf, Baseline_snet_taf, Baseline_acres_selected,
    Baseline_n_fields_selected, Baseline_revenue_millions,
    RCP45_target_taf, RCP45_snet_taf, RCP45_acres_selected,
    RCP45_n_fields_selected, RCP45_revenue_millions,
    RCP85_target_taf, RCP85_snet_taf, RCP85_acres_selected,
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
                  "Target", "S_net", "Acres", "Fields", "Rev ($M)",
                  "Target", "S_net", "Acres", "Fields", "Rev ($M)",
                  "Target", "S_net", "Acres", "Fields", "Rev ($M)"),
    align = c("l", rep("r", 15)),
    caption = "Table 1. Basin-level optimization results by climate scenario. Targets and S_net in TAF/yr; revenue in millions USD."
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
    general = "Targets derived from PPIC 2003-2010 overdraft estimates scaled by climate scenario (see Methods). Valley-wide totals reflect the valley-wide optimization, not the sum of basin-specific runs.",
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
target_method <- read_csv(file.path(prioritizr_dir, "target_methodology.csv"), show_col_types = FALSE)
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
      grepl("RCP45", scenario)    ~ as.character(round(rcp45_mult, 3)),
      grepl("RCP85", scenario)    ~ as.character(round(rcp85_mult, 3))
    ),
    `Target (TAF)` = round(target_af / 1000, 1),
    `S_net achieved (TAF)` = round(snet_achieved_af / 1000, 1),
    `Acres retired` = format(acres_selected, big.mark = ","),
    `Fields selected` = format(n_fields_selected, big.mark = ","),
    `Revenue cost ($M)` = round(revenue_cost_usd / 1e6, 1)
  ) %>%
  select(Scenario, `Target multiplier`, `Target (TAF)`, `S_net achieved (TAF)`,
         `Acres retired`, `Fields selected`, `Revenue cost ($M)`)

kbl_table2 <- table2 %>%
  kbl(
    align = c("l", rep("r", 7)),
    caption = "Table 2. Valley-wide optimization results across climate scenarios."
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 13
  ) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(
    general = "Target multiplier reflects the proportional scaling of the baseline SGMA overdraft target: 6.3% for RCP 4.5 (Escriva-Bou et al., 2023) and 10.8% for RCP 8.5 (scaled by BCMv8 PET ratio of 1.72).",
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
  # (valley_solutions was loaded in Section 2 for the spatial maps)
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
      caption = "Table 3. Selection consistency of fields across three climate scenarios (valley-wide optimization)."
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


cat("\n=== FIGURE AND TABLE GENERATION COMPLETE ===\n")


















