# =============================================================================
# PRIORITIZR COMBINED OPTIMIZATION (APPLIED WATER) WITH PROTECTED AREAS —
# RESULTS VISUALIZATIONS
# =============================================================================
# Purpose: Generate figures from the AW-based combined water + habitat
#          prioritizr results (with locked-in PAs) produced by
#          9_3_prioritizr_combined_AW_PAs.R.
#
# This is the AW-feature counterpart to 10_3_prioritizr_combined_PAs_figures.R,
# providing a parallel visual analysis using applied water (AW) as the water
# feature rather than S_net consumption, with the same locked-in PA design.
#
# FIGURES:
#   1. Scenario comparison grid: Acres, Revenue, AW Saved (6 scenarios)
#   2. Target achievement heatmap: 16 features × 6 scenarios
#   3. Spatial maps: 3 water scenarios (HQ) — basin polygons + PAs in green +
#                    newly retired fields in brick red
#   3b. 4-panel spatial maps: 3 scenarios + selection frequency (HQ)
#   4. Selection frequency: Suitable + HQ across water scenarios
#   5. Cost premium: combined vs. AW water-only vs. habitat-only-with-PAs
#
# INPUTS:
#   - prioritizr_combined_AW_PAs_results.RData    (from 9_3_*_AW_PAs.R)
#   - prioritizr_cross_temporal_PAs_results.RData (from 9_1_*_PAs.R)
#   - valley_wide_summary.csv                     (from 9_2 AW)
#
# OUTPUTS:
#   - Individual figure files (PNG) in fig_dir
#   - Summary tables (CSV)
# =============================================================================


# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(prioritizr)
library(sf)
library(here)
library(scales)
library(patchwork)
library(kableExtra)
library(cowplot)


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)


# Output directory
fig_dir <- here("data/intermediate/10_3_prioritizr_combined_AW_PAs_figures/")


# =============================================================================
# SECTION 1: Load Results
# =============================================================================

# --- Combined AW results (with PAs) ---
load(here("data/intermediate/9_3_prioritizr_combined_AW_PAs/prioritizr_combined_AW_PAs_results.RData"))


# --- Extract cleaned PA layer from the saved `pu` object so the figures show
#     exactly what the optimization saw (post topology cleanup). ---
pa_sf <- pu %>% filter(is_pa) %>% st_make_valid()


# --- Habitat-only results (with PAs) for cost premium figure ---
habitat_results <- tryCatch({
  env <- new.env()
  load(here("data/intermediate/9_1_prioritizr_habitat_only_PAs/prioritizr_cross_temporal_PAs_results.RData"),
       envir = env)
  env
}, error = function(e) {
  cat("  NOTE: Habitat-only PAs results not found — Figure 5 will skip habitat comparison.\n")
  NULL
})

# --- AW water-only results (for cost premium figure) ---
# Pulls from the AW water-only analysis to keep the comparison methodologically
# consistent. Water-only does NOT use PAs (per manuscript), so we use the
# original AW water-only output.
water_summary <- tryCatch({
  read_csv(here("data/intermediate/9_2_prioritizr_water_only_AW/valley_wide_summary.csv"),
           show_col_types = FALSE)
}, error = function(e) {
  cat("  NOTE: AW water-only summary not found — Figure 5 will skip water comparison.\n")
  NULL
})


# --- Project the basin layer to match field data CRS (defensive) ---
sjv_basins <- st_transform(sjv_basins, st_crs(field_data))


# --- Diagnostics ---
cat("Loaded combined AW prioritizr results (with PAs):\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Scenarios:", paste(names(solutions), collapse = ", "), "\n")
cat("  Field planning units:", sum(!pu$is_pa), "\n")
cat("    - Fallow:", sum(!pu$is_pa & pu$fallow == 1), "\n")
cat("    - Cultivated:", sum(!pu$is_pa & pu$fallow == 0), "\n")
cat("  PA planning units (locked in):", nrow(pa_sf), "\n")
cat("  All fields (incl. pre-retired):", nrow(field_data_all), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Water targets (TAF):", round(water_target_baseline_taf, 1),
    "/", round(water_target_rcp45_taf, 1),
    "/", round(water_target_rcp85_taf, 1), "\n")
cat("  Chosen BLM:", chosen_blm, "\n")


# =============================================================================
# COLOR PALETTES
# =============================================================================
# Centralized so the spatial maps and bar charts read as a coherent story.

# Spatial map colors (consistent with habitat-only PAs figures)
col_selected   <- "#8B2A1A"   # brick red — newly retired (selected) fields
col_unselected <- "wheat1"    # tan — non-selected fields
col_pa         <- "darkgreen" # existing protected areas (locked in)
col_retired    <- "grey70"    # fields already retired prior to optimization
col_basin_fill <- "grey95"    # 15 SJV basin polygons backdrop
col_basin_line <- "grey40"    # basin boundary lines

# Water scenario colors (consistent with water-only figures)
water_colors <- c(
  "Baseline" = "#3266ad",
  "RCP45"    = "#E8A825",
  "RCP85"    = "#C7432B"
)

# Habitat quality colors (consistent with habitat-only figures)
quality_colors <- c(
  "Suitable"     = "darkseagreen",
  "High Quality" = "darkgreen"
)

# Combined palette for 6 scenarios (water × quality)
scenario_colors_6 <- c(
  "combined_baseline_suit" = "#6BAED6",
  "combined_baseline_hq"   = "#08519C",
  "combined_rcp45_suit"    = "#FDD835",
  "combined_rcp45_hq"      = "#F9A825",
  "combined_rcp85_suit"    = "#EF9A9A",
  "combined_rcp85_hq"      = "#C62828"
)


# Pre-format target labels for use in figure titles/subtitles
baseline_taf_lbl <- format(round(water_target_baseline_taf), big.mark = ",")
rcp45_taf_lbl    <- format(round(water_target_rcp45_taf),    big.mark = ",")
rcp85_taf_lbl    <- format(round(water_target_rcp85_taf),    big.mark = ",")


# =============================================================================
# FIGURE 1: Scenario Comparison Grid
# =============================================================================
# Grouped bar chart: 3 panels (Acres, Revenue, AW Saved)
# Bars grouped by water scenario, colored by habitat quality.
# NOTE: Acres/cost are FIELD-ONLY (PAs excluded; total_acres in summary_all
# was already computed with !is_pa filter in the optimization script).

cat("\nCreating Figure 1: Scenario comparison grid...\n")

fig1_data <- summary_all %>%
  mutate(
    water_scenario = factor(water_scenario,
                            levels = c("Baseline", "RCP45", "RCP85"),
                            labels = c("Baseline", "RCP 4.5\n(2020-2049)", "RCP 8.5\n(2020-2049)")),
    quality = factor(quality, levels = c("Suitable", "High Quality"))
  )

# Panel A: Acres retired (field PUs only)
fig1a <- ggplot(fig1_data, aes(x = water_scenario, y = total_acres, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "A", x = NULL, y = "Acres") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  )

# Panel B: Foregone revenue
fig1b <- ggplot(fig1_data, aes(x = water_scenario, y = total_cost / 1e9, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = dollar_format(suffix = "B"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "B", x = NULL, y = "$ billions") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  )

# Panel C: Applied water saved
fig1c <- ggplot(fig1_data, aes(x = water_scenario, y = total_AW_AF / 1000, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "C", x = NULL, y = "TAF/yr") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  )

# Combine
fig1 <- fig1a + fig1b + fig1c +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Combined optimization results across climate scenarios (applied water, with locked-in PAs)",
    subtitle = paste0("Meeting 25,000-acre habitat targets (BNLL, GKR, SJKF x 5 climate periods) + ",
                      "valley-wide AW savings targets | BLM = ", chosen_blm),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  ) &
  theme(legend.position = "bottom") &
  scale_fill_manual(values = quality_colors, name = "Habitat Quality")

print(fig1)

ggsave(file.path(fig_dir, "fig1_scenario_comparison.png"), fig1,
       width = 12, height = 8, dpi = 600, bg = "white")
cat("  Saved: fig1_scenario_comparison.png\n")


# =============================================================================
# FIGURE 2: Target Achievement Heatmap
# =============================================================================
# Tile plot: features (rows) × scenarios (columns), colored by % of target.
# target_detail was already computed with !is_pa filter, so achieved values
# reflect field-only contributions (PAs contribute 0 by construction).

cat("\nCreating Figure 2: Target achievement heatmap...\n")

heatmap_data <- target_detail %>%
  mutate(
    pct_achieved = achieved / target * 100,
    # Clean feature labels
    feature_label = case_when(
      feature_type == "water" ~ case_when(
        grepl("baseline", feature) ~ "Water: Baseline",
        grepl("RCP45", feature)    ~ "Water: RCP 4.5",
        grepl("RCP85", feature)    ~ "Water: RCP 8.5"
      ),
      TRUE ~ feature
    ),
    # Clean scenario labels
    scenario_label = case_when(
      grepl("baseline_suit", scenario) ~ "Baseline\nSuitable",
      grepl("baseline_hq", scenario)   ~ "Baseline\nHigh Quality",
      grepl("rcp45_suit", scenario)    ~ "RCP 4.5\nSuitable",
      grepl("rcp45_hq", scenario)      ~ "RCP 4.5\nHigh Quality",
      grepl("rcp85_suit", scenario)    ~ "RCP 8.5\nSuitable",
      grepl("rcp85_hq", scenario)      ~ "RCP 8.5\nHigh Quality"
    )
  )

# Order scenario labels
scenario_order <- c("Baseline\nSuitable", "Baseline\nHigh Quality",
                    "RCP 4.5\nSuitable", "RCP 4.5\nHigh Quality",
                    "RCP 8.5\nSuitable", "RCP 8.5\nHigh Quality")
heatmap_data$scenario_label <- factor(heatmap_data$scenario_label, levels = scenario_order)

# Order feature labels: water features at top, then habitat by species
habitat_feature_order <- c(
  "bnll_base_suit", "bnll_rcp45_2049_suit", "bnll_rcp45_2069_suit",
  "bnll_rcp85_2049_suit", "bnll_rcp85_2069_suit",
  "gkr_base_suit", "gkr_rcp45_2049_suit", "gkr_rcp45_2069_suit",
  "gkr_rcp85_2049_suit", "gkr_rcp85_2069_suit",
  "sjkf_base_suit", "sjkf_rcp45_2049_suit", "sjkf_rcp45_2069_suit",
  "sjkf_rcp85_2049_suit", "sjkf_rcp85_2069_suit"
)

# For the heatmap, use the _suit features as labels (shared across quality levels)
feature_label_order <- c(
  "Water: Baseline", "Water: RCP 4.5", "Water: RCP 8.5",
  rev(habitat_feature_order)
)

# For hq scenarios, map hq feature names to suit equivalents for consistent ordering
heatmap_data <- heatmap_data %>%
  mutate(
    feature_label = case_when(
      feature_type == "water" ~ feature_label,
      TRUE ~ gsub("_hq$", "_suit", feature)
    )
  )

heatmap_data$feature_label <- factor(heatmap_data$feature_label,
                                     levels = feature_label_order)

# Build heatmap
fig2 <- ggplot(heatmap_data, aes(x = scenario_label, y = feature_label, fill = pct_achieved)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct_achieved), "%")),
            size = 4, fontface = "bold") +
  scale_fill_gradient2(
    low = "#D32F2F", mid = "#FFF9C4", high = "#388E3C",
    midpoint = 100,
    limits = c(
      min(floor(min(heatmap_data$pct_achieved) / 10) * 10, 90),
      max(ceiling(max(heatmap_data$pct_achieved) / 10) * 10, 110)
    ),
    name = "% of target"
  ) +
  labs(
    title = "Target achievement across all combined scenarios (applied water, with locked-in PAs)",
    subtitle = "Each cell shows % of target achieved by newly retired fields (PAs contribute 0 by construction)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  geom_hline(yintercept = 3.5, linewidth = 1, color = "gray30")

print(fig2)

ggsave(file.path(fig_dir, "fig2_target_heatmap.png"), fig2,
       width = 10, height = 9, dpi = 600, bg = "white")
cat("  Saved: fig2_target_heatmap.png\n")


# =============================================================================
# FIGURE 3: Spatial Maps — High Quality across 3 Water Scenarios
# =============================================================================
# Three maps showing Baseline, RCP 4.5, and RCP 8.5 combined solutions.
# Layer order (bottom -> top):
#   1. Basin polygons (grey95 fill, grey40 outline)
#   2. Field statuses (selected = brick red, unselected = wheat, retired = grey)
#   3. Protected areas (darkgreen overlay, included in fill aesthetic for
#      legend)
#   4. Basin polygon outlines (re-drawn on top)

cat("\nCreating Figure 3: Spatial maps (HQ across water scenarios)...\n")

# Bounding box with small buffer so basins aren't clipped at edges
field_bbox_raw <- st_bbox(field_data)
buffer_frac <- 0.03
plot_bbox <- field_bbox_raw
plot_bbox[c("xmin", "xmax")] <- field_bbox_raw[c("xmin", "xmax")] +
  c(-1, 1) * diff(field_bbox_raw[c("xmin", "xmax")]) * buffer_frac
plot_bbox[c("ymin", "ymax")] <- field_bbox_raw[c("ymin", "ymax")] +
  c(-1, 1) * diff(field_bbox_raw[c("ymin", "ymax")]) * buffer_frac

# --- Scenario definitions ---
fig3_scenarios <- tibble(
  scen  = c("combined_baseline_hq", "combined_rcp45_hq", "combined_rcp85_hq"),
  title = c(paste0("Baseline (", baseline_taf_lbl, " TAF)"),
            paste0("RCP 4.5 (",   rcp45_taf_lbl,    " TAF)"),
            paste0("RCP 8.5 (",   rcp85_taf_lbl,    " TAF)"))
)

# --- Helper: build a scenario map (basin + fields + PAs) ---
build_scenario_map <- function(scen, title_text) {
  
  sol <- solutions[[scen]]
  
  # Restrict fields to the 15 SJV basins (basin column non-NA), join solution,
  # and assign status.
  sol_join <- field_data_all %>%
    filter(!is.na(basin)) %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% filter(!is_pa) %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1    ~ "Retired",
        solution_1 == 1 ~ "Cultivated or short-term fallow (selected)",
        TRUE            ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  ggplot() +
    # Basin polygons backdrop
    geom_sf(data = sjv_basins, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.3) +
    # Field planning units (selected / not selected / retired)
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    # Protected areas overlay (mapped to fill so it appears in the legend)
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    # Basin outlines on top
    geom_sf(data = sjv_basins, fill = NA, color = col_basin_line, linewidth = 0.3) +
    coord_sf(xlim = plot_bbox[c("xmin", "xmax")],
             ylim = plot_bbox[c("ymin", "ymax")],
             expand = FALSE, datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated or short-term fallow (selected)"     = col_selected,
        "Cultivated or short-term fallow (not selected)" = col_unselected,
        "Retired"                                        = col_retired,
        "Existing protected area"                        = col_pa
      ),
      breaks = c(
        "Cultivated or short-term fallow (selected)",
        "Cultivated or short-term fallow (not selected)",
        "Retired",
        "Existing protected area"
      ),
      labels = c(
        "Cultivated or short-term fallow (selected)"     = "Cultivated or short-term\nfallow (selected)",
        "Cultivated or short-term fallow (not selected)" = "Cultivated or short-term\nfallow (not selected)",
        "Retired"                                        = "Retired",
        "Existing protected area"                        = "Existing protected\narea (locked in)"
      ),
      name = "Planning Unit Status"
    ) +
    labs(title = title_text) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 11),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 10, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.key.height = unit(1, "cm")
    )
}

# Build 3 maps
map_base  <- build_scenario_map("combined_baseline_hq", fig3_scenarios$title[1])
map_rcp45 <- build_scenario_map("combined_rcp45_hq",    fig3_scenarios$title[2])
map_rcp85 <- build_scenario_map("combined_rcp85_hq",    fig3_scenarios$title[3])

fig3 <- map_base + map_rcp45 + map_rcp85 +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Optimal retirement configuration across water scenarios (combined AW, high quality habitat, with locked-in PAs)",
    subtitle = paste0("Minimizing foregone revenue | 25,000 ac habitat target x 15 features + ",
                      "scenario-specific AW targets | BLM = ", chosen_blm),
    theme = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(fig3)

ggsave(file.path(fig_dir, "fig3_spatial_maps.png"), fig3,
       width = 15, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig3_spatial_maps.png\n")


# =============================================================================
# FIGURE 3b: 4-Panel Spatial Maps — 3 Scenarios + Selection Frequency (HQ)
# =============================================================================
# Panels A–C: Baseline, RCP 4.5, RCP 8.5 (HQ) with Planning Unit Status legend
# Panel D:    Selection frequency across 3 water scenarios (HQ)

cat("\nCreating Figure 3b: 4-panel spatial maps + selection frequency...\n")

# --- Wider buffer for the 4-panel layout ---
field_bbox_raw <- st_bbox(field_data)
buffer_frac_3b <- 0.02
plot_bbox_3b <- field_bbox_raw
plot_bbox_3b[c("xmin", "xmax")] <- field_bbox_raw[c("xmin", "xmax")] +
  c(-1, 1) * diff(field_bbox_raw[c("xmin", "xmax")]) * buffer_frac_3b
plot_bbox_3b[c("ymin", "ymax")] <- field_bbox_raw[c("ymin", "ymax")] +
  c(-1, 1) * diff(field_bbox_raw[c("ymin", "ymax")]) * buffer_frac_3b

# --- Scenario definitions ---
fig3b_scenarios <- tibble(
  scen  = c("combined_baseline_hq", "combined_rcp45_hq", "combined_rcp85_hq"),
  title = c(paste0("A: Baseline (", baseline_taf_lbl, " TAF)"),
            paste0("B: RCP 4.5 (",   rcp45_taf_lbl,    " TAF)"),
            paste0("C: RCP 8.5 (",   rcp85_taf_lbl,    " TAF)"))
)

# --- Helper: scenario map (selected/not selected/retired/PA) ---
build_status_map <- function(scen, title_text) {
  
  sol <- solutions[[scen]]
  
  sol_join <- field_data_all %>%
    filter(!is.na(basin)) %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% filter(!is_pa) %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1    ~ "Retired",
        solution_1 == 1 ~ "Cultivated or short-term fallow (selected)",
        TRUE            ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  ggplot() +
    geom_sf(data = sjv_basins, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    geom_sf(data = sjv_basins, fill = NA, color = col_basin_line, linewidth = 0.3) +
    coord_sf(xlim = plot_bbox_3b[c("xmin", "xmax")],
             ylim = plot_bbox_3b[c("ymin", "ymax")],
             expand = FALSE, datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated or short-term fallow (selected)"     = col_selected,
        "Cultivated or short-term fallow (not selected)" = col_unselected,
        "Retired"                                        = col_retired,
        "Existing protected area"                        = col_pa
      ),
      breaks = c(
        "Cultivated or short-term fallow (selected)",
        "Cultivated or short-term fallow (not selected)",
        "Retired",
        "Existing protected area"
      ),
      labels = c(
        "Cultivated or short-term fallow (selected)"     = "Planning unit\n(selected)",
        "Cultivated or short-term fallow (not selected)" = "Planning unit\n(not selected)",
        "Retired"                                        = "Retired",
        "Existing protected area"                        = "Protected area\n(locked in)"
      ),
      name = "Field\nstatus"
    ) +
    labs(title = title_text) +
    theme_void(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, hjust = 0),
      plot.margin      = margin(5, 5, 5, 5),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 11, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_text(face = "bold", size = 10),
      legend.text      = element_text(size = 10)
    ) +
    scale_y_continuous(n.breaks = 7) +
    scale_x_continuous(n.breaks = 4) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.3, color = "gray40")))
}

# Build 3 scenario maps
map3b_base  <- build_status_map("combined_baseline_hq", fig3b_scenarios$title[1])
map3b_rcp45 <- build_status_map("combined_rcp45_hq",    fig3b_scenarios$title[2])
map3b_rcp85 <- build_status_map("combined_rcp85_hq",    fig3b_scenarios$title[3])

# --- Build selection frequency (HQ only, fields only — exclude PAs) ---
hq_scenarios <- c("combined_baseline_hq", "combined_rcp45_hq", "combined_rcp85_hq")

freq_hq_3b <- field_data %>%
  filter(!is.na(basin)) %>%
  dplyr::select(id) %>%
  mutate(n_selected = 0L)

for (scen in hq_scenarios) {
  if (scen %in% names(solutions)) {
    # Field selections only (exclude PAs which are locked in for all scenarios)
    sel_ids <- solutions[[scen]] %>%
      filter(solution_1 == 1, !is_pa) %>%
      st_drop_geometry() %>%
      pull(id)
    freq_hq_3b <- freq_hq_3b %>%
      mutate(n_selected = n_selected + as.integer(id %in% sel_ids))
  }
}

freq_hq_3b <- freq_hq_3b %>%
  mutate(n_selected_fct = factor(n_selected, levels = c("0", "1", "2", "3")))

# Frequency color palette
freq_colors_3b <- c(
  "0" = "gray85",
  "1" = "#FDE725",
  "2" = "#21918C",
  "3" = "#440154"
)

freq_labels_3b <- c(
  "0" = "Not selected (0)",
  "1" = "1 scenario",
  "2" = "2 scenarios",
  "3" = "3 scenarios"
)

# Frequency map: also overlay PAs in dark green to keep visual continuity
map3b_freq <- ggplot() +
  geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
  geom_sf(data = freq_hq_3b, aes(fill = n_selected_fct),
          color = NA, linewidth = 0) +
  geom_sf(data = pa_sf, fill = col_pa, color = NA, alpha = 0.85) +
  geom_sf(data = sjv_basins, fill = NA, color = "gray40", linewidth = 0.3) +
  coord_sf(xlim = plot_bbox_3b[c("xmin", "xmax")],
           ylim = plot_bbox_3b[c("ymin", "ymax")],
           expand = FALSE, datum = sf::st_crs(4326)) +
  scale_fill_manual(values = freq_colors_3b, labels = freq_labels_3b,
                    name = "Selection\nfrequency", drop = FALSE) +
  labs(title = "D: Selection frequency") +
  theme_void(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0),
    plot.margin      = margin(5, 5, 5, 5),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 11, color = "grey30"),
    axis.ticks       = element_line(color = "grey30"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 10),
    legend.margin    = margin(t = 10),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(n.breaks = 4) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                             override.aes = list(linewidth = 0.3, color = "gray40")))

# --- Combine into 2×2 grid with separate legends ---
map3b_base_nl  <- map3b_base  + theme(legend.position = "none")
map3b_rcp45_nl <- map3b_rcp45 + theme(legend.position = "none")
map3b_rcp85_nl <- map3b_rcp85 + theme(legend.position = "none")
map3b_freq_nl  <- map3b_freq  + theme(legend.position = "none")

# Extract Planning Unit Status legend
status_legend <- get_legend(
  map3b_base +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.3, color = "gray40"))) +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 9),
          legend.key.height = unit(0.6, "cm"))
)

# Extract Selection Frequency legend
freq_legend <- get_legend(
  map3b_freq +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(linewidth = 0.3, color = "gray40"))) +
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold", size = 10),
          legend.text = element_text(size = 9),
          legend.key.height = unit(0.6, "cm"))
)

# 2×2 map grid
map_grid <- (map3b_base_nl + map3b_rcp45_nl + map3b_rcp85_nl + map3b_freq_nl) +
  plot_layout(ncol = 2) &
  theme(plot.margin = margin(8, 14, 8, 14))

# Legend row — center the two legends in the available width with a margin
# so legend titles aren't clipped at the figure edges
legend_row <- plot_grid(
  NULL, status_legend, freq_legend, NULL,
  ncol = 4,
  rel_widths = c(0.08, 1, 1, 0.08)
)

# Stack maps + legends
fig3b <- plot_grid(
  map_grid, legend_row,
  ncol = 1, rel_heights = c(1, 0.1)
)

# # Add title/subtitle via ggdraw
# fig3b_titled <- ggdraw() +
#   draw_plot(fig3b, y = 0, height = 0.94) +
#   draw_label("Spatial distribution of fields selected for retirement (combined AW + habitat, high quality, with locked-in PAs)",
#              x = 0.02, y = 0.98, hjust = 0, vjust = 1, fontface = "bold", size = 14) +
#   draw_label(paste0("Valley-wide optimization under three climate scenarios with scenario-specific targets | BLM = ", chosen_blm),
#              x = 0.02, y = 0.95, hjust = 0, vjust = 1, color = "gray40", size = 11)

#print(fig3b_titled)

ggsave(file.path(fig_dir, "fig3b_spatial_maps_frequency.png"), fig3b,
       width = 10, height = 11, dpi = 600, bg = "white")
cat("  Saved: fig3b_spatial_maps_frequency.png\n")


# =============================================================================
# FIGURE 4: Selection Frequency Across Water Scenarios (Suit + HQ)
# =============================================================================
# Fields-only frequency (PAs excluded — they're locked in across all scenarios).
# Fields selected in all 3 are "no-regret" retirements.

cat("\nCreating Figure 4: Selection frequency maps...\n")

# --- Build selection frequency for Suitable ---
suit_scenarios <- c("combined_baseline_suit", "combined_rcp45_suit", "combined_rcp85_suit")

freq_suit <- field_data %>%
  filter(!is.na(basin)) %>%
  dplyr::select(id) %>%
  mutate(n_selected = 0L)

for (scen in suit_scenarios) {
  if (scen %in% names(solutions)) {
    sel_ids <- solutions[[scen]] %>%
      filter(solution_1 == 1, !is_pa) %>%
      st_drop_geometry() %>%
      pull(id)
    freq_suit <- freq_suit %>%
      mutate(n_selected = n_selected + as.integer(id %in% sel_ids))
  }
}

freq_suit <- freq_suit %>%
  mutate(n_selected_fct = factor(n_selected, levels = c("0", "1", "2", "3")))

# --- Build selection frequency for High Quality ---
freq_hq <- field_data %>%
  filter(!is.na(basin)) %>%
  dplyr::select(id) %>%
  mutate(n_selected = 0L)

for (scen in hq_scenarios) {
  if (scen %in% names(solutions)) {
    sel_ids <- solutions[[scen]] %>%
      filter(solution_1 == 1, !is_pa) %>%
      st_drop_geometry() %>%
      pull(id)
    freq_hq <- freq_hq %>%
      mutate(n_selected = n_selected + as.integer(id %in% sel_ids))
  }
}

freq_hq <- freq_hq %>%
  mutate(n_selected_fct = factor(n_selected, levels = c("0", "1", "2", "3")))


# --- Color palette ---
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

map_bbox <- st_bbox(field_data)

# --- Helper function for frequency maps (with PA overlay) ---
build_freq_map <- function(freq_data, title_text) {
  ggplot() +
    geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
    geom_sf(data = freq_data, aes(fill = n_selected_fct),
            color = NA, linewidth = 0) +
    geom_sf(data = pa_sf, fill = col_pa, color = NA, alpha = 0.85) +
    geom_sf(data = sjv_basins, fill = NA, color = "gray40", linewidth = 0.3) +
    coord_sf(xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
             ylim = c(map_bbox["ymin"], map_bbox["ymax"])) +
    scale_fill_manual(values = freq_colors, labels = freq_labels,
                      name = "Selection frequency", drop = FALSE) +
    labs(title = title_text) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      plot.margin = margin(5, 5, 5, 5),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 7, color = "grey30"),
      axis.ticks = element_line(color = "grey30")
    ) +
    guides(fill = guide_legend(nrow = 1, override.aes = list(linewidth = 0.3, color = "gray40")))
}

map_freq_suit <- build_freq_map(freq_suit, "Suitable Habitat + Water")
map_freq_hq   <- build_freq_map(freq_hq, "High Quality Habitat + Water")

fig4 <- map_freq_suit + map_freq_hq +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Selection frequency across water scenarios (combined AW optimization, with locked-in PAs)",
    subtitle = "Fields selected in all 3 water scenarios are 'no-regret' retirements; existing PAs shown in dark green",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  ) &
  theme(legend.position = "bottom")

print(fig4)

ggsave(file.path(fig_dir, "fig4_selection_frequency.png"), fig4,
       width = 11, height = 7, dpi = 600, bg = "white")
cat("  Saved: fig4_selection_frequency.png\n")


# --- Selection frequency summary table ---
freq_summary <- bind_rows(
  freq_suit %>%
    st_drop_geometry() %>%
    group_by(n_selected) %>%
    summarise(n_fields = n(), .groups = "drop") %>%
    mutate(quality = "Suitable"),
  freq_hq %>%
    st_drop_geometry() %>%
    group_by(n_selected) %>%
    summarise(n_fields = n(), .groups = "drop") %>%
    mutate(quality = "High Quality")
) %>%
  group_by(quality) %>%
  mutate(pct = round(n_fields / sum(n_fields) * 100, 1)) %>%
  ungroup()

cat("\n--- Selection Frequency Summary ---\n")
print(freq_summary, n = Inf)

write_csv(freq_summary, file.path(fig_dir, "selection_frequency_summary.csv"))


# =============================================================================
# FIGURE 5: Cost Premium of Dual Objectives
# =============================================================================
# Compare combined solutions (with PAs) against individual water-only and
# habitat-only-with-PAs solutions to quantify the "cost of co-optimization."
# All costs are field-only (PAs contribute $0 in all formulations).

cat("\nCreating Figure 5: Cost premium of dual objectives...\n")

# --- Build comparison table ---
comparison_rows <- list()

# Combined results (6 scenarios)
for (i in 1:nrow(summary_all)) {
  row <- summary_all[i, ]
  comparison_rows[[length(comparison_rows) + 1]] <- tibble(
    analysis    = "Combined",
    water_label = row$water_scenario,
    quality     = row$quality,
    total_cost  = row$total_cost,
    total_acres = row$total_acres,
    total_AW_AF = row$total_AW_AF
  )
}

# Habitat-only results with PAs (2 scenarios)
if (!is.null(habitat_results)) {
  hab_summary <- habitat_results$summary_all
  for (i in 1:nrow(hab_summary)) {
    row <- hab_summary[i, ]
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      analysis    = "Habitat only",
      water_label = "N/A",
      quality     = row$quality,
      total_cost  = row$total_cost,
      total_acres = row$total_acres,
      total_AW_AF = NA_real_
    )
  }
}

# Water-only results (3 scenarios) — note: water-only does NOT use PAs
if (!is.null(water_summary)) {
  for (i in 1:nrow(water_summary)) {
    row <- water_summary[i, ]
    water_lab <- case_when(
      grepl("Baseline", row$scenario) ~ "Baseline",
      grepl("RCP45", row$scenario)    ~ "RCP45",
      grepl("RCP85", row$scenario)    ~ "RCP85"
    )
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      analysis    = "Water only",
      water_label = water_lab,
      quality     = "N/A",
      total_cost  = row$revenue_cost_usd,
      total_acres = row$acres_selected,
      total_AW_AF = row$snet_achieved_af  # column name from water-only output
    )
  }
}

comparison <- bind_rows(comparison_rows)

cat("\n--- Full Comparison Table ---\n")
print(comparison, width = Inf)

write_csv(comparison, file.path(fig_dir, "fig5_cost_comparison_data.csv"))


# --- Build figure: Baseline (top row) + RCP 4.5 (bottom row) ---
fig5_data <- comparison %>%
  filter(
    (analysis == "Water only" & water_label %in% c("Baseline", "RCP45")) |
      (analysis == "Habitat only") |
      (analysis == "Combined" & water_label %in% c("Baseline", "RCP45"))
  ) %>%
  mutate(
    bar_label = case_when(
      analysis == "Water only"   ~ "Water only",
      analysis == "Habitat only" & quality == "Suitable"     ~ "Habitat only\n(Suitable)",
      analysis == "Habitat only" & quality == "High Quality" ~ "Habitat only\n(High Quality)",
      analysis == "Combined"     & quality == "Suitable"     ~ "Combined\n(Suitable)",
      analysis == "Combined"     & quality == "High Quality" ~ "Combined\n(High Quality)"
    ),
    bar_label = factor(bar_label, levels = c(
      "Water only",
      "Habitat only\n(Suitable)", "Habitat only\n(High Quality)",
      "Combined\n(Suitable)", "Combined\n(High Quality)"
    )),
    bar_fill = case_when(
      analysis == "Water only"   ~ "#3266ad",
      analysis == "Habitat only" & quality == "Suitable"     ~ "darkseagreen",
      analysis == "Habitat only" & quality == "High Quality" ~ "darkgreen",
      analysis == "Combined"     & quality == "Suitable"     ~ "#B07CD8",
      analysis == "Combined"     & quality == "High Quality" ~ "#6A1B9A"
    ),
    scenario_row = case_when(
      water_label == "Baseline" | (analysis == "Habitat only") ~ "Baseline",
      water_label == "RCP45" ~ "RCP 4.5 (2020-2049)"
    )
  )

# Split into Baseline and RCP 4.5 subsets
fig5_baseline <- fig5_data %>% filter(scenario_row == "Baseline")
fig5_rcp45    <- fig5_data %>%
  filter(scenario_row == "RCP 4.5 (2020-2049)" | analysis == "Habitat only")

fill_values <- setNames(fig5_data$bar_fill, fig5_data$bar_label)

# --- Helper functions for cost/acres panels ---
make_revenue_panel <- function(df, title_text) {
  ggplot(df, aes(x = bar_label, y = total_cost / 1e6, fill = bar_label)) +
    geom_col(width = 0.65, color = "black", linewidth = 0.3) +
    geom_text(aes(label = paste0("$", format(round(total_cost / 1e6), big.mark = ","), "M")),
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = fill_values, guide = "none") +
    labs(title = title_text, x = NULL, y = "$ millions") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 12, margin = margin(r = 10))
    )
}

make_acres_panel <- function(df, title_text) {
  ggplot(df, aes(x = bar_label, y = total_acres, fill = bar_label)) +
    geom_col(width = 0.65, color = "black", linewidth = 0.3) +
    geom_text(aes(label = format(round(total_acres), big.mark = ",")),
              vjust = -0.5, size = 4, fontface = "bold") +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
    scale_fill_manual(values = fill_values, guide = "none") +
    labs(title = title_text, x = NULL, y = "Acres") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
      axis.title.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 12, margin = margin(r = 10))
    )
}

# Build 4 panels
fig5a <- make_revenue_panel(fig5_baseline, "A. Foregone revenue (Baseline)")
fig5b <- make_acres_panel(fig5_baseline,   "B. Acres retired (Baseline)")
fig5c_panel <- make_revenue_panel(fig5_rcp45, "C. Foregone revenue (RCP 4.5)")
fig5d <- make_acres_panel(fig5_rcp45,      "D. Acres retired (RCP 4.5)")

fig5 <- (fig5a + fig5b) / (fig5c_panel + fig5d) +
  plot_annotation(
    title = "Cost of co-optimization: combined vs. individual objective solutions (applied water, with locked-in PAs)",
    subtitle = "Comparing AW water-only, habitat-only-with-PAs, and combined-with-PAs optimization under Baseline and RCP 4.5 scenarios",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  )

print(fig5)

ggsave(file.path(fig_dir, "fig5_cost_premium.png"), fig5,
       width = 10, height = 10, dpi = 600, bg = "white")
cat("  Saved: fig5_cost_premium.png\n")


# --- Also build the full scenario comparison (all water scenarios) ---
fig5_full_data <- comparison %>%
  filter(analysis %in% c("Water only", "Combined")) %>%
  mutate(
    water_label_clean = case_when(
      water_label == "Baseline" ~ "Baseline",
      water_label == "RCP45"    ~ "RCP 4.5",
      water_label == "RCP85"    ~ "RCP 8.5",
      TRUE                      ~ water_label
    ),
    water_label_clean = factor(water_label_clean,
                               levels = c("Baseline", "RCP 4.5", "RCP 8.5")),
    bar_group = case_when(
      analysis == "Water only"                           ~ "Water only",
      analysis == "Combined" & quality == "Suitable"     ~ "Combined (Suitable)",
      analysis == "Combined" & quality == "High Quality" ~ "Combined (High Quality)"
    ),
    bar_group = factor(bar_group, levels = c("Water only",
                                             "Combined (Suitable)",
                                             "Combined (High Quality)"))
  )

premium_colors <- c(
  "Water only"              = "#3266ad",
  "Combined (Suitable)"     = "#B07CD8",
  "Combined (High Quality)" = "#6A1B9A"
)

fig5b_full <- ggplot(fig5_full_data, aes(x = water_label_clean, y = total_cost / 1e6,
                                         fill = bar_group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("$", format(round(total_cost / 1e6), big.mark = ","), "M")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = premium_colors, name = "Analysis") +
  labs(
    title = "Cost premium of adding habitat constraints across all water scenarios (applied water)",
    subtitle = "Combined-with-PAs optimization costs vs. AW water-only baseline for each climate scenario",
    x = "Water savings scenario",
    y = "Foregone revenue ($ millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(fig5b_full)

ggsave(file.path(fig_dir, "fig5b_cost_premium_all_scenarios.png"), fig5b_full,
       width = 10, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig5b_cost_premium_all_scenarios.png\n")


# =============================================================================
# SUMMARY TABLE: Overall Comparison (kableExtra)
# =============================================================================

cat("\nCreating summary table...\n")

# Total PA acres locked in (constant across scenarios)
total_pa_acres <- sum(pa_sf$acres, na.rm = TRUE)

overall_table <- summary_all %>%
  mutate(
    `Water Scenario`     = water_scenario,
    Quality              = quality,
    `PAs Locked In`      = format(n_pa_locked_in, big.mark = ","),
    `PA Acres Locked In` = format(round(total_pa_acres), big.mark = ","),
    `Fields Selected`    = format(n_selected, big.mark = ","),
    `Acres Retired`      = format(round(total_acres), big.mark = ","),
    `Revenue Cost`       = paste0("$", format(round(total_cost / 1e6, 1), big.mark = ","), "M"),
    `AW Saved (TAF)`     = round(total_AW_AF / 1000, 1),
    `Boundary`           = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(`Water Scenario`, Quality, `PAs Locked In`, `PA Acres Locked In`,
                `Fields Selected`, `Acres Retired`,
                `Revenue Cost`, `AW Saved (TAF)`, Boundary)

kbl_overall <- overall_table %>%
  kbl(
    caption = "Combined optimization results: applied water savings + habitat creation (with locked-in PAs)",
    align = c("l", "l", rep("r", 7))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  pack_rows("Baseline", 1, 2) %>%
  pack_rows("RCP 4.5 (2020-2049)", 3, 4) %>%
  pack_rows("RCP 8.5 (2020-2049)", 5, 6) %>%
  footnote(
    general = paste0("All scenarios meet 25,000-acre habitat targets for BNLL, GKR, and SJKF ",
                     "across 5 climate periods (15 features) plus valley-wide AW savings targets. ",
                     "Baseline AW target (", round(water_target_baseline_taf), " TAF) derived from ",
                     "PPIC San Joaquin Valley dataset; RCP 4.5 and RCP 8.5 targets scaled using ",
                     "PPIC's 6.3% climate adjustment + BCMv8 PET ratio. Existing PAs locked in via ",
                     "add_locked_in_constraints(); they contribute 0 to habitat and water targets ",
                     "and influence the optimization solely through the boundary penalty (BLM = ",
                     chosen_blm, ")."),
    general_title = "Note: "
  )

save_kable(kbl_overall, file.path(fig_dir, "table_combined_AW_PAs_summary.html"))
cat("  Saved: table_combined_AW_PAs_summary.html\n")

write_csv(overall_table, file.path(fig_dir, "table_combined_AW_PAs_summary.csv"))
cat("  Saved: table_combined_AW_PAs_summary.csv\n")


cat("\n========== ALL FIGURES GENERATED (AW combined w/ PAs) ==========\n")
cat("Output directory:", fig_dir, "\n")




















# =============================================================================
# =============================================================================
# METRIC VERSIONS (SI UNITS) OF ALL FIGURES AND TABLES
# =============================================================================
# =============================================================================
# Regenerates every figure/table above in metric units and saves a parallel
# copy to a "metric/" subfolder, with "_metric" appended to the filename. The
# imperial outputs above are left untouched.
#
# Conversions:
#   - Area retired / habitat targets: acres -> hectares (ha)
#   - Water savings/target:           TAF/yr / AF -> km^3/yr
#
# (This script has no efficiency-ratio figures, so the ha/Mm^3 and $/m^3
#  conventions used in the water-only script do not apply here.)
#
# All source data objects persist from the script body above.
# =============================================================================

cat("\n========== GENERATING METRIC (SI UNIT) VERSIONS ==========\n")

# --- Unit conversion constants -------------------------------------------------
ACRE_TO_HA <- 0.40468564224           # 1 acre = 0.40468564224 hectares
AF_TO_M3   <- 1233.48183754752        # 1 acre-foot = 1233.48 cubic metres
AF_TO_KM3  <- AF_TO_M3 / 1e9          # 1 acre-foot in km^3
TAF_TO_KM3 <- (AF_TO_M3 * 1000) / 1e9 # 1 TAF (1000 AF) in km^3

# --- Converters + formatter ----------------------------------------------------
ac_to_ha  <- function(acres) acres * ACRE_TO_HA
af_to_km3 <- function(af)    af    * AF_TO_KM3
fmt_km3   <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")

# --- Metric output directory + filename helper --------------------------------
metric_dir <- file.path(fig_dir, "metric")

metric_path <- function(filename) {
  stem <- tools::file_path_sans_ext(filename)
  ext  <- tools::file_ext(filename)
  file.path(metric_dir, paste0(stem, "_metric.", ext))
}

cat("  Metric output directory:", metric_dir, "\n")

# --- km^3 scenario-target labels (for figure titles) --------------------------
baseline_km3_lbl <- fmt_km3(water_target_baseline_taf * TAF_TO_KM3, 2)
rcp45_km3_lbl    <- fmt_km3(water_target_rcp45_taf    * TAF_TO_KM3, 2)
rcp85_km3_lbl    <- fmt_km3(water_target_rcp85_taf    * TAF_TO_KM3, 2)

# --- Habitat target expressed in hectares (for subtitles/footnotes) -----------
habitat_target_ha_lbl <- format(round(ac_to_ha(habitat_target)), big.mark = ",")


# =============================================================================
# FIGURE 1 (metric): Scenario Comparison Grid
# =============================================================================
cat("Creating Figure 1 (metric)...\n")

# Panel A: hectares retired
fig1a_m <- ggplot(fig1_data, aes(x = water_scenario, y = ac_to_ha(total_acres), fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "A", x = NULL, y = "Hectares") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  )

# Panel B: foregone revenue (unchanged — reuse imperial object)
fig1b_m <- fig1b

# Panel C: applied water saved (km^3/yr)
fig1c_m <- ggplot(fig1_data, aes(x = water_scenario, y = af_to_km3(total_AW_AF), fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "C", x = NULL, y = "km³/yr") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, margin = margin(r = 10)),
    axis.title.x = element_text(size = 16),
    legend.title = element_text(face = "bold", size = 16),
    legend.text = element_text(size = 14)
  )

fig1_metric <- fig1a_m + fig1b_m + fig1c_m +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Combined optimization results across climate scenarios (applied water, with locked-in PAs)",
    subtitle = paste0("Meeting ", habitat_target_ha_lbl, "-ha habitat targets (BNLL, GKR, SJKF x 5 climate periods) + ",
                      "valley-wide AW savings targets | BLM = ", chosen_blm),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  ) &
  theme(legend.position = "bottom") &
  scale_fill_manual(values = quality_colors, name = "Habitat Quality")

ggsave(metric_path("fig1_scenario_comparison.png"), fig1_metric,
       width = 12, height = 8, dpi = 600, bg = "white")
cat("  Saved: fig1_scenario_comparison_metric.png\n")


# =============================================================================
# FIGURE 2 (metric): Target Achievement Heatmap
# =============================================================================
# Cells show % of target achieved — unit-independent. Saved under the metric
# filename so the metric/ folder mirrors the imperial set.
if (exists("fig2")) {
  cat("Creating Figure 2 (metric)...\n")
  ggsave(metric_path("fig2_target_heatmap.png"), fig2,
         width = 10, height = 9, dpi = 600, bg = "white")
  cat("  Saved: fig2_target_heatmap_metric.png\n")
}


# =============================================================================
# FIGURE 3 (metric): Spatial Maps — HQ across 3 water scenarios
# =============================================================================
# Maps are unit-independent; only the TAF targets in titles convert to km^3.
if (exists("build_scenario_map")) {
  
  cat("Creating Figure 3 (metric)...\n")
  
  map_base_m  <- build_scenario_map("combined_baseline_hq",
                                    paste0("Baseline (", baseline_km3_lbl, " km³)"))
  map_rcp45_m <- build_scenario_map("combined_rcp45_hq",
                                    paste0("RCP 4.5 (",   rcp45_km3_lbl,    " km³)"))
  map_rcp85_m <- build_scenario_map("combined_rcp85_hq",
                                    paste0("RCP 8.5 (",   rcp85_km3_lbl,    " km³)"))
  
  fig3_m <- map_base_m + map_rcp45_m + map_rcp85_m +
    plot_layout(ncol = 3, guides = "collect") +
    plot_annotation(
      title = "Optimal retirement configuration across water scenarios (combined AW, high quality habitat, with locked-in PAs)",
      subtitle = paste0("Minimizing foregone revenue | ", habitat_target_ha_lbl,
                        " ha habitat target x 15 features + scenario-specific AW targets | BLM = ", chosen_blm),
      theme = theme(
        plot.title    = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 10)
      )
    )
  
  ggsave(metric_path("fig3_spatial_maps.png"), fig3_m,
         width = 15, height = 6, dpi = 600, bg = "white")
  cat("  Saved: fig3_spatial_maps_metric.png\n")
}


# =============================================================================
# FIGURE 3b (metric): 4-panel spatial maps + selection frequency
# =============================================================================
# Status/frequency maps are unit-independent; only scenario titles convert to
# km^3. Reuses the persisted frequency map, both legends, and the legend row.
if (exists("build_status_map") && exists("map3b_freq_nl") && exists("legend_row")) {
  
  cat("Creating Figure 3b (metric)...\n")
  
  map3b_base_m  <- build_status_map("combined_baseline_hq",
                                    paste0("A: Baseline (", baseline_km3_lbl, " km³)"))
  map3b_rcp45_m <- build_status_map("combined_rcp45_hq",
                                    paste0("B: RCP 4.5 (",   rcp45_km3_lbl,    " km³)"))
  map3b_rcp85_m <- build_status_map("combined_rcp85_hq",
                                    paste0("C: RCP 8.5 (",   rcp85_km3_lbl,    " km³)"))
  
  map3b_base_m_nl  <- map3b_base_m  + theme(legend.position = "none")
  map3b_rcp45_m_nl <- map3b_rcp45_m + theme(legend.position = "none")
  map3b_rcp85_m_nl <- map3b_rcp85_m + theme(legend.position = "none")
  
  map_grid_m <- (map3b_base_m_nl + map3b_rcp45_m_nl + map3b_rcp85_m_nl + map3b_freq_nl) +
    plot_layout(ncol = 2) &
    theme(plot.margin = margin(8, 14, 8, 14))
  
  fig3b_m <- plot_grid(
    map_grid_m, legend_row,
    ncol = 1, rel_heights = c(1, 0.1)
  )
  
  ggsave(metric_path("fig3b_spatial_maps_frequency.png"), fig3b_m,
         width = 10, height = 11, dpi = 600, bg = "white")
  cat("  Saved: fig3b_spatial_maps_frequency_metric.png\n")
}


# =============================================================================
# FIGURE 4 (metric): Selection frequency across water scenarios
# =============================================================================
# Frequency maps are unit-independent (titles carry no units). Saved as a copy.
if (exists("fig4")) {
  cat("Creating Figure 4 (metric)...\n")
  ggsave(metric_path("fig4_selection_frequency.png"), fig4,
         width = 11, height = 7, dpi = 600, bg = "white")
  cat("  Saved: fig4_selection_frequency_metric.png\n")
}

# Selection frequency summary CSV is counts/percentages only (no units) — copy
# it to the metric folder so the folder mirrors the imperial set.
if (exists("freq_summary")) {
  write_csv(freq_summary, metric_path("selection_frequency_summary.csv"))
  cat("  Saved: selection_frequency_summary_metric.csv\n")
}


# # =============================================================================
# # FIGURE 5 (metric): Cost premium — revenue ($) panels unchanged, acres -> ha
# # =============================================================================
# cat("Creating Figure 5 (metric)...\n")
# 
# # Metric acres-panel builder (identical to make_acres_panel but y = Hectares;
# # the supplied data frame carries hectares in its `total_acres` column).
# make_acres_panel_metric <- function(df, title_text) {
#   ggplot(df, aes(x = bar_label, y = total_acres, fill = bar_label)) +
#     geom_col(width = 0.65, color = "black", linewidth = 0.3) +
#     geom_text(aes(label = format(round(total_acres), big.mark = ",")),
#               vjust = -0.5, size = 4, fontface = "bold") +
#     scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
#     scale_fill_manual(values = fill_values, guide = "none") +
#     labs(title = title_text, x = NULL, y = "Hectares") +
#     theme_minimal() +
#     theme(
#       plot.title = element_text(face = "bold", size = 13),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
#       axis.title.x = element_text(size = 12),
#       axis.text.y = element_text(size = 11),
#       axis.title.y = element_text(size = 12, margin = margin(r = 10))
#     )
# }
# 
# # Hectares versions of the per-scenario data frames
# fig5_baseline_m <- fig5_baseline %>% mutate(total_acres = ac_to_ha(total_acres))
# fig5_rcp45_m    <- fig5_rcp45    %>% mutate(total_acres = ac_to_ha(total_acres))
# 
# fig5a_m       <- make_revenue_panel(fig5_baseline, "A. Foregone revenue (Baseline)")
# fig5b_panel_m <- make_acres_panel_metric(fig5_baseline_m, "B. Hectares retired (Baseline)")
# fig5c_panel_m <- make_revenue_panel(fig5_rcp45,    "C. Foregone revenue (RCP 4.5)")
# fig5d_m       <- make_acres_panel_metric(fig5_rcp45_m,    "D. Hectares retired (RCP 4.5)")
# 
# fig5_metric <- (fig5a_m + fig5b_panel_m) / (fig5c_panel_m + fig5d_m) +
#   plot_annotation(
#     title = "Cost of co-optimization: combined vs. individual objective solutions (applied water, with locked-in PAs)",
#     subtitle = "Comparing AW water-only, habitat-only-with-PAs, and combined-with-PAs optimization under Baseline and RCP 4.5 scenarios",
#     theme = theme(
#       plot.title    = element_text(face = "bold", size = 13),
#       plot.subtitle = element_text(color = "gray40", size = 10)
#     )
#   )
# 
# ggsave(metric_path("fig5_cost_premium.png"), fig5_metric,
#        width = 10, height = 10, dpi = 600, bg = "white")
# cat("  Saved: fig5_cost_premium_metric.png\n")


# =============================================================================
# FIGURE 5 (metric): Cost premium — 2 panels, bars colored by climate scenario
# =============================================================================
# Compares three optimization problems (Water only, Combined Suitable, Combined
# High Quality) across all three climate scenarios. Habitat-only is excluded.
# Panel A = foregone revenue ($ billions); Panel B = hectares retired.
cat("Creating Figure 5 (metric)...\n")

# Build plotting data straight from `comparison` (drops Habitat only; keeps all
# three climate scenarios). x-axis = optimization problem; fill = climate scenario.
fig5_data_m <- comparison %>%
  filter(analysis %in% c("Water only", "Combined")) %>%
  mutate(
    opt_problem = case_when(
      analysis == "Water only"                              ~ "Water only",
      analysis == "Combined" & quality == "Suitable"        ~ "Combined\n(Suitable)",
      analysis == "Combined" & quality == "High Quality"    ~ "Combined\n(High Quality)"
    ),
    opt_problem = factor(opt_problem,
                         levels = c("Water only", "Combined\n(Suitable)", "Combined\n(High Quality)")),
    climate          = factor(water_label, levels = c("Baseline", "RCP45", "RCP85")),
    revenue_billions = total_cost / 1e9,
    hectares_retired = ac_to_ha(total_acres)
  )

# Shared fill scale (same water_colors as the rest of the script)
climate_fill_scale <- scale_fill_manual(
  values = water_colors,
  breaks = c("Baseline", "RCP45", "RCP85"),
  labels = c("Baseline", "RCP 4.5\n(2020-2049)", "RCP 8.5\n(2020-2049)"),
  name   = "Climate scenario"
)

fig5_panel_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.key.spacing.y = unit(2, "pt")
  )

# Panel A: foregone revenue ($ billions)
fig5a_m <- ggplot(fig5_data_m, aes(x = opt_problem, y = revenue_billions, fill = climate)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = label_comma(accuracy = 0.001), expand = expansion(mult = c(0, 0.08))) +
  climate_fill_scale +
  labs(title = "A. foregone revenue", x = NULL, y = "$ billions") +
  fig5_panel_theme

# Panel B: hectares retired
fig5b_panel_m <- ggplot(fig5_data_m, aes(x = opt_problem, y = hectares_retired, fill = climate)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.08))) +
  climate_fill_scale +
  labs(title = "B. land retired", x = NULL, y = "hectares") +
  fig5_panel_theme

fig5_metric <- fig5a_m + fig5b_panel_m +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(
    title = "Cost of co-optimization across climate scenarios (applied water, with locked-in PAs)",
    subtitle = "Water-only vs. combined (Suitable / High Quality) optimization; bars colored by climate scenario",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  ) &
  theme(legend.position = "right")

ggsave(metric_path("fig5_cost_premium.png"), fig5_metric,
       width = 12, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig5_cost_premium_metric.png\n")


# =============================================================================
# FIGURE 5b (metric): Cost premium across all scenarios
# =============================================================================
# Revenue ($ millions) only — unit-independent. Saved as a copy.
if (exists("fig5b_full")) {
  cat("Creating Figure 5b (metric)...\n")
  ggsave(metric_path("fig5b_cost_premium_all_scenarios.png"), fig5b_full,
         width = 10, height = 6, dpi = 600, bg = "white")
  cat("  Saved: fig5b_cost_premium_all_scenarios_metric.png\n")
}

# Cost comparison data CSV (metric): acres -> ha, AW (AF) -> km^3
comparison_m <- comparison %>%
  mutate(
    total_hectares = ac_to_ha(total_acres),
    total_AW_km3   = total_AW_AF * AF_TO_KM3
  ) %>%
  select(analysis, water_label, quality, total_cost, total_hectares, total_AW_km3)
write_csv(comparison_m, metric_path("fig5_cost_comparison_data.csv"))
cat("  Saved: fig5_cost_comparison_data_metric.csv\n")


# =============================================================================
# SUMMARY TABLE (metric)
# =============================================================================
cat("Creating summary table (metric)...\n")

total_pa_hectares <- ac_to_ha(total_pa_acres)

overall_table_m <- summary_all %>%
  mutate(
    `Water Scenario`        = water_scenario,
    Quality                 = quality,
    `PAs Locked In`         = format(n_pa_locked_in, big.mark = ","),
    `PA Hectares Locked In` = format(round(total_pa_hectares), big.mark = ","),
    `Fields Selected`       = format(n_selected, big.mark = ","),
    `Hectares Retired`      = format(round(ac_to_ha(total_acres)), big.mark = ","),
    `Revenue Cost`          = paste0("$", format(round(total_cost / 1e6, 1), big.mark = ","), "M"),
    `AW Saved (km³)`   = round(total_AW_AF * AF_TO_KM3, 4),
    `Boundary`              = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(`Water Scenario`, Quality, `PAs Locked In`, `PA Hectares Locked In`,
                `Fields Selected`, `Hectares Retired`,
                `Revenue Cost`, `AW Saved (km³)`, Boundary)

kbl_overall_m <- overall_table_m %>%
  kbl(
    caption = "Combined optimization results: applied water savings + habitat creation (with locked-in PAs)",
    align = c("l", "l", rep("r", 7))
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  pack_rows("Baseline", 1, 2) %>%
  pack_rows("RCP 4.5 (2020-2049)", 3, 4) %>%
  pack_rows("RCP 8.5 (2020-2049)", 5, 6) %>%
  footnote(
    general = paste0("All scenarios meet ", habitat_target_ha_lbl, "-ha habitat targets for BNLL, GKR, and SJKF ",
                     "across 5 climate periods (15 features) plus valley-wide AW savings targets. ",
                     "Baseline AW target (", fmt_km3(water_target_baseline_taf * TAF_TO_KM3, 2), " km³) derived from ",
                     "PPIC San Joaquin Valley dataset; RCP 4.5 and RCP 8.5 targets scaled using ",
                     "PPIC's 6.3% climate adjustment + BCMv8 PET ratio. Existing PAs locked in via ",
                     "add_locked_in_constraints(); they contribute 0 to habitat and water targets ",
                     "and influence the optimization solely through the boundary penalty (BLM = ",
                     chosen_blm, ")."),
    general_title = "Note: "
  )

save_kable(kbl_overall_m, metric_path("table_combined_AW_PAs_summary.html"))
cat("  Saved: table_combined_AW_PAs_summary_metric.html\n")

write_csv(overall_table_m, metric_path("table_combined_AW_PAs_summary.csv"))
cat("  Saved: table_combined_AW_PAs_summary_metric.csv\n")


cat("\n========== METRIC (SI UNIT) VERSIONS COMPLETE (AW combined w/ PAs) ==========\n")
cat("Metric output directory:", metric_dir, "\n")



















