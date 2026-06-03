# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS-TEMPORAL) WITH PROTECTED AREAS —
# RESULTS VISUALIZATIONS
# =============================================================================
# Purpose: Generate figures and summary tables from the cross-temporal
#          prioritizr results produced by 9_1_prioritizr_habitat_only_PAs.R.
#
# Inputs:  prioritizr_cross_temporal_PAs_results.RData
#          Contains: solutions, summary_all, target_detail, scenarios,
#                    suit_features, hq_features, pu (fields + PAs),
#                    field_data, field_data_all, pa_data, sjv_basins,
#                    habitat_target, chosen_blm, blm_results, bm,
#                    cost_scale_factor
#
# Outputs (in fig_dir):
#   fig1_*.png/csv     — Overall summary tables
#   fig2_retirement_by_county.png
#   fig3_retirement_by_basin.png
#   fig4_total_revenue_acreage.png
#   fig5_spatial_maps.png         (full SJV extent, basin polygons + PAs)
#   fig6_spatial_maps_zoom.png    (Kern/Fresno/Kings/Tulare focus)
#   fig7_blm_calibration.png
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


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)

# Figure output directory
fig_dir <- here("data/intermediate/10_1_prioritizr_habitat_only_PAs_figures/blm_0.005/")


# =============================================================================
# LOAD RESULTS
# =============================================================================

load(here("data/intermediate/9_1_prioritizr_habitat_only_PAs/prioritizr_cross_temporal_PAs_results.RData"))

# Extract the cleaned PA layer from the combined planning unit object so the
# figures show exactly what the optimization saw (post topology cleanup).
pa_sf <- pu %>% filter(is_pa) %>% st_make_valid()

cat("Loaded cross-temporal prioritizr results (with PAs):\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Field planning units:", sum(!pu$is_pa), "\n")
cat("    - Fallow:", sum(!pu$is_pa & pu$fallow == 1), "\n")
cat("    - Cultivated:", sum(!pu$is_pa & pu$fallow == 0), "\n")
cat("  PA planning units (locked in):", nrow(pa_sf), "\n")
cat("  All fields (including pre-retired):", nrow(field_data_all), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Chosen BLM:", chosen_blm, "\n")


# Project the basin layer to match the field data CRS (defensive — already
# done upstream, but cheap to re-do here for clarity).
sjv_basins <- st_transform(sjv_basins, st_crs(field_data))


# load the TNC Pixley-Kern wildlife refuge corridor
kern_pix_cor <- st_read(here("data/raw/tnc_corridor/ConceptualRestorationArea.shp"))
# match the field data crs
kern_pix_cor <- st_transform(kern_pix_cor, st_crs(field_data))

# =============================================================================
# COLOR PALETTE
# =============================================================================
# Centralized so the spatial maps and bar charts read as a coherent story.

col_selected   <- "#8B2A1A"   # brick red — newly retired (selected) fields
col_unselected <- "wheat1"    # tan — non-selected fields
col_pa         <- "darkgreen" # existing protected areas (locked in)
col_retired    <- "grey70"    # fields already retired prior to optimization
col_basin_fill <- "grey95"    # 15 SJV basin polygons backdrop
col_basin_line <- "grey40"    # basin boundary lines

# Bar-chart palette (preserved from original script)
col_quality <- c("Suitable" = "darkseagreen", "High Quality" = "darkgreen")


# =============================================================================
# FIGURE 1: Overall Summary Tables
# =============================================================================

cat("\n========== FIGURE 1: OVERALL SUMMARY ==========\n\n")

# Total PA acres locked in (constant across scenarios — PAs are the same in both)
total_pa_acres <- sum(pa_sf$acres, na.rm = TRUE)

overall_summary <- summary_all %>%
  mutate(
    Quality              = quality,
    `PAs Locked In`      = format(n_pa_locked_in, big.mark = ","),
    `PA Acres Locked In` = format(round(total_pa_acres), big.mark = ","),
    `Fields Selected`    = format(n_selected, big.mark = ","),
    `Acres Retired`      = format(round(total_acres), big.mark = ","),
    `Revenue Cost ($)`   = paste0("$", format(round(total_cost), big.mark = ",")),
    `Water Saved (af)`   = ifelse(is.na(total_water_AW), "—",
                                  format(round(total_water_AW), big.mark = ",")),
    `Boundary Length`    = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(Quality, `PAs Locked In`, `PA Acres Locked In`,
                `Fields Selected`, `Acres Retired`,
                `Revenue Cost ($)`, `Water Saved (af)`, `Boundary Length`)

overall_summary %>%
  kbl(caption = "Cross-Temporal Optimization Summary (with locked-in PAs)",
      align = c("l", rep("r", 7))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  print()


# Detailed target achievement by species and climate period
target_summary <- target_detail %>%
  mutate(
    surplus       = achieved - target,
    pct_of_target = round(achieved / target * 100, 1)
  )

target_summary %>%
  dplyr::select(Quality = quality, Species = species, `Climate Scenario` = climate,
                Target = target, Achieved = achieved, Surplus = surplus,
                `% of Target` = pct_of_target) %>%
  mutate(
    Target   = format(round(Target), big.mark = ","),
    Achieved = format(round(Achieved), big.mark = ","),
    Surplus  = format(round(Surplus), big.mark = ",")
  ) %>%
  kbl(caption = "Target Achievement by Species and Climate Scenario",
      align = c("l", "l", "l", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Suitable Habitat", 1, 15) %>%
  pack_rows("High Quality Habitat", 16, 30) %>%
  print()


# =============================================================================
# FIGURE 2: Retirement by County
# =============================================================================

cat("\n========== FIGURE 2: RETIREMENT BY COUNTY ==========\n\n")

# Build county summaries for both solutions (field PUs only — exclude PAs)
county_summaries <- list()

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  county_summaries[[scen]] <- sol %>%
    filter(solution_1 == 1, !is_pa) %>%
    st_drop_geometry() %>%
    group_by(county) %>%
    summarise(
      n_fields    = n(),
      total_acres = sum(acres, na.rm = TRUE),
      total_rev   = sum(revenue, na.rm = TRUE),
      total_water_AW = if ("waterAW" %in% names(.)) sum(waterAW, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(scenario = scen) %>%
    arrange(desc(total_acres))
}

county_summary_all <- bind_rows(county_summaries) %>%
  mutate(
    quality = case_when(
      grepl("suit", scenario) ~ "Suitable",
      grepl("hq", scenario)   ~ "High Quality"
    ),
    quality = factor(quality, levels = c("Suitable", "High Quality"))
  )

# Zero-fill: ensure every county appears in both quality levels
all_combos_county <- expand.grid(
  county  = unique(county_summary_all$county),
  quality = levels(county_summary_all$quality),
  stringsAsFactors = FALSE
)

county_summary_all <- county_summary_all %>%
  right_join(all_combos_county, by = c("county", "quality")) %>%
  mutate(across(c(n_fields, total_acres, total_rev, total_water_AW),
                ~ replace_na(.x, 0)))

# Pin a single y-axis ordering across all three facets: descending total acres
# retired (summed across both quality scenarios). With coord_flip(), ascending
# factor levels put the largest at the top.
county_order <- county_summary_all %>%
  group_by(county) %>%
  summarise(total_acres_all = sum(total_acres), .groups = "drop") %>%
  arrange(total_acres_all) %>%
  pull(county)

county_summary_all <- county_summary_all %>%
  mutate(county = factor(county, levels = county_order))

# Pivot to long format for faceting
county_long <- county_summary_all %>%
  pivot_longer(
    cols      = c(total_acres, total_rev, n_fields),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_acres" ~ "Acres Retired",
      metric == "total_rev"   ~ "Foregone Revenue ($)",
      metric == "n_fields"    ~ "Fields Retired"
    ),
    metric = factor(metric, levels = c("Acres Retired", "Foregone Revenue ($)", "Fields Retired"))
  )

fig2_county_plot <- ggplot(county_long,
                           aes(x = county, y = value, fill = quality)) +
  geom_col(position = "dodge", col = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
           format(round(x), big.mark = ","))
  }) +
  scale_fill_manual(values = col_quality) +
  labs(
    title    = "Optimal retirement by county (habitat only, with locked-in PAs)",
    subtitle = "Fields must meet 25,000-acre targets for BNLL, GKR & SJKF across all climate scenarios",
    x        = NULL,
    y        = NULL,
    fill     = "Habitat Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 9),
    strip.text    = element_text(face = "bold", size = 10, hjust = 0),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 10),
    legend.title  = element_text(face = "bold", size = 11),
    legend.text   = element_text(size = 10),
    legend.position = "bottom"
  )

print(fig2_county_plot)

ggsave(file.path(fig_dir, "fig2_retirement_by_county.png"), fig2_county_plot,
       width = 9, height = 6, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 3: Retirement by Basin
# =============================================================================
# Mirror of Figure 2, using groundwater basin assignments (added to the
# planning units in 9_1_prioritizr_habitat_only_PAs.R).

cat("\n========== FIGURE 3: RETIREMENT BY BASIN ==========\n\n")

basin_summaries <- list()

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  basin_summaries[[scen]] <- sol %>%
    filter(solution_1 == 1, !is_pa) %>%
    st_drop_geometry() %>%
    group_by(basin) %>%
    summarise(
      n_fields    = n(),
      total_acres = sum(acres, na.rm = TRUE),
      total_rev   = sum(revenue, na.rm = TRUE),
      total_water_AW = if ("waterAW" %in% names(.)) sum(waterAW, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(scenario = scen) %>%
    arrange(desc(total_acres))
}

basin_summary_all <- bind_rows(basin_summaries) %>%
  filter(!is.na(basin)) %>%   # safety: drop any unassigned PUs
  mutate(
    quality = case_when(
      grepl("suit", scenario) ~ "Suitable",
      grepl("hq", scenario)   ~ "High Quality"
    ),
    quality = factor(quality, levels = c("Suitable", "High Quality"))
  )

# Zero-fill: ensure every basin appears in both quality levels
all_combos_basin <- expand.grid(
  basin   = unique(basin_summary_all$basin),
  quality = levels(basin_summary_all$quality),
  stringsAsFactors = FALSE
)

basin_summary_all <- basin_summary_all %>%
  right_join(all_combos_basin, by = c("basin", "quality")) %>%
  mutate(across(c(n_fields, total_acres, total_rev, total_water_AW),
                ~ replace_na(.x, 0)))

# Pin a single y-axis ordering across all three facets: descending total acres
# retired (summed across both quality scenarios). With coord_flip(), ascending
# factor levels put the largest at the top.
basin_order <- basin_summary_all %>%
  group_by(basin) %>%
  summarise(total_acres_all = sum(total_acres), .groups = "drop") %>%
  arrange(total_acres_all) %>%
  pull(basin)

basin_summary_all <- basin_summary_all %>%
  mutate(basin = factor(basin, levels = basin_order))

basin_long <- basin_summary_all %>%
  pivot_longer(
    cols      = c(total_acres, total_rev, n_fields),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_acres" ~ "Acres Retired",
      metric == "total_rev"   ~ "Foregone Revenue ($)",
      metric == "n_fields"    ~ "Fields Retired"
    ),
    metric = factor(metric, levels = c("Acres Retired", "Foregone Revenue ($)", "Fields Retired"))
  )

fig3_basin_plot <- ggplot(basin_long,
                          aes(x = basin, y = value, fill = quality)) +
  geom_col(position = "dodge", col = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
           format(round(x), big.mark = ","))
  }) +
  scale_fill_manual(values = col_quality) +
  labs(
    title    = "Optimal retirement by groundwater basin (habitat only, with locked-in PAs)",
    subtitle = "Fields must meet 25,000-acre targets for BNLL, GKR & SJKF across all climate scenarios",
    x        = NULL,
    y        = NULL,
    fill     = "Habitat Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 9),
    strip.text    = element_text(face = "bold", size = 10, hjust = 0),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 9),
    legend.title  = element_text(face = "bold", size = 11),
    legend.text   = element_text(size = 10),
    legend.position = "bottom"
  )

print(fig3_basin_plot)

ggsave(file.path(fig_dir, "fig3_retirement_by_basin.png"), fig3_basin_plot,
       width = 10, height = 6.5, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 4: Cost and Acres Comparison — Suitable vs. High Quality
# =============================================================================

cat("\n========== FIGURE 4: COST & ACRES COMPARISON ==========\n\n")

comparison_data <- summary_all %>%
  dplyr::select(quality, total_cost, total_acres, n_selected) %>%
  mutate(quality = factor(quality, levels = c("Suitable", "High Quality")))

# Revenue cost
fig4a_cost_plot <- ggplot(comparison_data, aes(x = quality, y = total_cost, fill = quality)) +
  geom_col(width = 0.6, col = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("$", format(round(total_cost / 1e6, 1), big.mark = ","), "M")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = col_quality, guide = "none") +
  labs(title = "A", x = NULL, y = "Foregone revenue ($M)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    axis.title.y  = element_text(margin = margin(r = 10))
  )

# Acres
fig4b_acres_plot <- ggplot(comparison_data, aes(x = quality, y = total_acres, fill = quality)) +
  geom_col(width = 0.6, col = "black", linewidth = 0.3) +
  geom_text(aes(label = format(round(total_acres), big.mark = ",")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = col_quality, guide = "none") +
  labs(title = "B", x = NULL, y = "Acres retired") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    axis.title.y  = element_text(margin = margin(r = 10))
  )

fig4_cost_acres <- fig4a_cost_plot + fig4b_acres_plot +
  plot_annotation(
    title    = "Retirement optimization: suitable vs. high quality habitat",
    subtitle = "Meeting 25,000-acre targets for BNLL, GKR & SJKF across all 5 climate scenarios",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

print(fig4_cost_acres)

ggsave(file.path(fig_dir, "fig4_total_revenue_acreage.png"), fig4_cost_acres,
       width = 7, height = 5, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 5: Side-by-Side Spatial Maps — Suitable vs. High Quality (Full SJV)
# =============================================================================
# Layer order (bottom -> top):
#   1. Basin polygons (grey95 fill, grey40 outline)
#   2. Field statuses (selected = brick red, unselected = wheat, retired = grey)
#   3. Protected areas (darkgreen overlay)
#   4. Basin polygon outlines (re-drawn on top for crispness)

cat("\n========== FIGURE 5: SPATIAL MAPS (FULL SJV) ==========\n\n")

# Bounding box from field data
field_bbox <- st_bbox(field_data)

map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  # Build a complete field layer for plotting (all fields incl. pre-retired)
  # so the visual story is: every cropland parcel is colored according to
  # its status.
  sol_join <- field_data_all %>%
    filter(!is.na(basin)) %>%   # restrict to fields inside the 15 SJV basins
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% filter(!is_pa) %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1                   ~ "Retired",
        solution_1 == 1                ~ "Cultivated or short-term fallow (selected)",
        TRUE                           ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  p <- ggplot() +
    # Basin polygons backdrop
    geom_sf(data = sjv_basins, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.3) +
    # Fields colored by status
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    # PAs overlaid in green — mapped via fill so they appear in the legend
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    # Re-draw basin outlines on top so they're not hidden by fills
    geom_sf(data = sjv_basins, fill = NA, color = col_basin_line, linewidth = 0.3) +
    coord_sf(xlim = field_bbox[c(1, 3)], ylim = field_bbox[c(2, 4)],
             expand = TRUE, datum = sf::st_crs(4326)) +
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
      name = "Field status"
    ) +
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 9, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title     = element_text(face = "bold"),
      legend.key.height = unit(1, "cm"), 
    ) + 
    scale_y_continuous(n.breaks = 7) + 
    scale_x_continuous(n.breaks = 4)
  
  map_list[[quality_label]] <- p
}

# All four planning-unit statuses are now in the fill aesthetic, so the
# patchwork shared legend will include the PA color automatically.

fig5_combined_map <- map_list[["Suitable"]] + map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only, with locked-in PAs)",
    subtitle = paste0(
      "minimizing foregone revenue | 25,000 ac target per species × 5 climate periods | BLM = ", chosen_blm
    ),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(fig5_combined_map)

ggsave(file.path(fig_dir, "fig5_spatial_maps.png"), fig5_combined_map,
       width = 11, height = 6, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 6: Zoomed Spatial Maps — Kern, Fresno, Kings, Tulare focus
# =============================================================================

cat("\n========== FIGURE 6: SPATIAL MAPS (ZOOM) ==========\n\n")

# Bounding box: Kern/Fresno/Kings/Tulare focus area
zoom_bbox <- st_bbox(c(xmin = -121, ymin = 34.75, xmax = -118.5, ymax = 37.25),
                     crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  st_transform(st_crs(field_data)) %>%
  st_bbox()

zoom_map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  sol_join <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% filter(!is_pa) %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1                   ~ "Retired",
        solution_1 == 1                ~ "Cultivated or short-term fallow (selected)",
        TRUE                           ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  p <- ggplot() +
    geom_sf(data = sjv_basins, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    geom_sf(data = sjv_basins, fill = NA, color = col_basin_line, linewidth = 0.3) +
    geom_sf(data = kern_pix_cor, fill = NA, color = "black", linewidth = 0.2) + 
    coord_sf(xlim = zoom_bbox[c(1, 3)], ylim = zoom_bbox[c(2, 4)],
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
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.key.height = unit(1, "cm")
    )
  
  zoom_map_list[[quality_label]] <- p
}

fig6_zoom_map <- zoom_map_list[["Suitable"]] + zoom_map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only, with locked-in PAs)",
    subtitle = "Kern, Fresno, Kings, and Tulare counties",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(fig6_zoom_map)

ggsave(file.path(fig_dir, "fig6_spatial_maps_zoom.png"), fig6_zoom_map,
       width = 10, height = 6, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 7: BLM Calibration Plot
# =============================================================================

cat("\n========== FIGURE 7: BLM CALIBRATION ==========\n\n")

blm_results <- blm_results %>%
  mutate(total_cost_M = round(total_cost / 1e6, 3))

fig7_blm_plot <- ggplot(blm_results, aes(x = total_cost_M, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length",
    subtitle = "Calibrated on cross-temporal suitable habitat problem with locked-in PAs",
    x        = "Foregone revenue ($M)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_classic()

print(fig7_blm_plot)

ggsave(file.path(fig_dir, "fig7_blm_calibration.png"), fig7_blm_plot,
       width = 8, height = 5.5, dpi = 600, bg = "white")


# =============================================================================
# EXPORT SUMMARY TABLES
# =============================================================================

write_csv(county_summary_all,
          file.path(fig_dir, "county_summaries.csv"))
write_csv(basin_summary_all,
          file.path(fig_dir, "basin_summaries.csv"))
write_csv(target_summary,
          file.path(fig_dir, "target_achievement.csv"))


cat("\n========== ALL FIGURES GENERATED ==========\n")
cat("Outputs written to:\n  ", fig_dir, "\n")













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
#   - Area retired / habitat targets / species targets: acres -> hectares (ha)
#   - Incidental water reported in the summary:          AF -> km^3
#   - Cost ($), boundary length, BLM, and field-status maps are unit-free.
#
# All source data objects persist from the script body above.
# =============================================================================

cat("\n========== GENERATING METRIC (SI UNIT) VERSIONS ==========\n")

# --- Unit conversion constants -------------------------------------------------
ACRE_TO_HA <- 0.40468564224           # 1 acre = 0.40468564224 hectares
AF_TO_M3   <- 1233.48183754752        # 1 acre-foot = 1233.48 cubic metres
AF_TO_KM3  <- AF_TO_M3 / 1e9          # 1 acre-foot in km^3

# --- Converters ----------------------------------------------------------------
ac_to_ha  <- function(acres) acres * ACRE_TO_HA
af_to_km3 <- function(af)    af    * AF_TO_KM3

# --- Metric output directory + filename helper --------------------------------
metric_dir <- file.path(fig_dir, "metric")

metric_path <- function(filename) {
  stem <- tools::file_path_sans_ext(filename)
  ext  <- tools::file_ext(filename)
  file.path(metric_dir, paste0(stem, "_metric.", ext))
}

cat("  Metric output directory:", metric_dir, "\n")

# Habitat target expressed in hectares (for subtitles/captions)
hab_ha_lbl <- format(round(ac_to_ha(habitat_target)), big.mark = ",")


# =============================================================================
# FIGURE 1 (metric): Overall summary + target achievement tables
# =============================================================================
# The imperial script prints these tables to the console; here we build metric
# versions and additionally save them to the metric/ folder as HTML.
cat("Creating Figure 1 tables (metric)...\n")

overall_summary_m <- summary_all %>%
  mutate(
    Quality                 = quality,
    `PAs Locked In`         = format(n_pa_locked_in, big.mark = ","),
    `PA Hectares Locked In` = format(round(ac_to_ha(total_pa_acres)), big.mark = ","),
    `Fields Selected`       = format(n_selected, big.mark = ","),
    `Hectares Retired`      = format(round(ac_to_ha(total_acres)), big.mark = ","),
    `Revenue Cost ($)`      = paste0("$", format(round(total_cost), big.mark = ",")),
    `Water Saved (km³)`     = ifelse(is.na(total_water_AW), "—",
                                     format(round(af_to_km3(total_water_AW), 4), big.mark = ",")),
    `Boundary Length`       = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(Quality, `PAs Locked In`, `PA Hectares Locked In`,
                `Fields Selected`, `Hectares Retired`,
                `Revenue Cost ($)`, `Water Saved (km³)`, `Boundary Length`)

kbl_overall_m <- overall_summary_m %>%
  kbl(caption = "Cross-Temporal Optimization Summary (with locked-in PAs)",
      align = c("l", rep("r", 7))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE)

print(kbl_overall_m)
save_kable(kbl_overall_m, metric_path("fig1_overall_summary.html"))
cat("  Saved: fig1_overall_summary_metric.html\n")

# Detailed target achievement by species and climate period (areas -> ha)
target_summary_m <- target_summary %>%
  mutate(
    target_ha   = ac_to_ha(target),
    achieved_ha = ac_to_ha(achieved),
    surplus_ha  = ac_to_ha(surplus)
  )

kbl_target_m <- target_summary_m %>%
  dplyr::select(Quality = quality, Species = species, `Climate Scenario` = climate,
                `Target (ha)` = target_ha, `Achieved (ha)` = achieved_ha,
                `Surplus (ha)` = surplus_ha, `% of Target` = pct_of_target) %>%
  mutate(
    `Target (ha)`   = format(round(`Target (ha)`),   big.mark = ","),
    `Achieved (ha)` = format(round(`Achieved (ha)`), big.mark = ","),
    `Surplus (ha)`  = format(round(`Surplus (ha)`),  big.mark = ",")
  ) %>%
  kbl(caption = "Target Achievement by Species and Climate Scenario",
      align = c("l", "l", "l", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Suitable Habitat", 1, 15) %>%
  pack_rows("High Quality Habitat", 16, 30)

print(kbl_target_m)
save_kable(kbl_target_m, metric_path("fig1_target_achievement.html"))
cat("  Saved: fig1_target_achievement_metric.html\n")


# =============================================================================
# FIGURE 2 (metric): Retirement by County
# =============================================================================
cat("Creating Figure 2 (metric)...\n")

county_long_m <- county_summary_all %>%
  mutate(total_hectares = ac_to_ha(total_acres)) %>%
  pivot_longer(
    cols      = c(total_hectares, total_rev, n_fields),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_hectares" ~ "Hectares Retired",
      metric == "total_rev"      ~ "Foregone Revenue ($)",
      metric == "n_fields"       ~ "Fields Retired"
    ),
    metric = factor(metric, levels = c("Hectares Retired", "Foregone Revenue ($)", "Fields Retired"))
  )

fig2_county_plot_m <- ggplot(county_long_m,
                             aes(x = county, y = value, fill = quality)) +
  geom_col(position = "dodge", col = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
           format(round(x), big.mark = ","))
  }) +
  scale_fill_manual(values = col_quality) +
  labs(
    title    = "Optimal retirement by county (habitat only, with locked-in PAs)",
    subtitle = paste0("Fields must meet ", hab_ha_lbl, "-ha targets for BNLL, GKR & SJKF across all climate scenarios"),
    x = NULL, y = NULL, fill = "Habitat Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 9),
    strip.text    = element_text(face = "bold", size = 10, hjust = 0),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 10),
    legend.title  = element_text(face = "bold", size = 11),
    legend.text   = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(metric_path("fig2_retirement_by_county.png"), fig2_county_plot_m,
       width = 9, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig2_retirement_by_county_metric.png\n")


# =============================================================================
# FIGURE 3 (metric): Retirement by Basin
# =============================================================================
cat("Creating Figure 3 (metric)...\n")

basin_long_m <- basin_summary_all %>%
  mutate(total_hectares = ac_to_ha(total_acres)) %>%
  pivot_longer(
    cols      = c(total_hectares, total_rev, n_fields),
    names_to  = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = case_when(
      metric == "total_hectares" ~ "Hectares Retired",
      metric == "total_rev"      ~ "Foregone Revenue ($)",
      metric == "n_fields"       ~ "Fields Retired"
    ),
    metric = factor(metric, levels = c("Hectares Retired", "Foregone Revenue ($)", "Fields Retired"))
  )

fig3_basin_plot_m <- ggplot(basin_long_m,
                            aes(x = basin, y = value, fill = quality)) +
  geom_col(position = "dodge", col = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
           format(round(x), big.mark = ","))
  }) +
  scale_fill_manual(values = col_quality) +
  labs(
    title    = "Optimal retirement by groundwater basin (habitat only, with locked-in PAs)",
    subtitle = paste0("Fields must meet ", hab_ha_lbl, "-ha targets for BNLL, GKR & SJKF across all climate scenarios"),
    x = NULL, y = NULL, fill = "Habitat Quality"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 9),
    strip.text    = element_text(face = "bold", size = 10, hjust = 0),
    axis.text.y   = element_text(size = 10),
    axis.text.x   = element_text(size = 9),
    legend.title  = element_text(face = "bold", size = 11),
    legend.text   = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave(metric_path("fig3_retirement_by_basin.png"), fig3_basin_plot_m,
       width = 10, height = 6.5, dpi = 600, bg = "white")
cat("  Saved: fig3_retirement_by_basin_metric.png\n")


# =============================================================================
# FIGURE 4 (metric): Cost and Hectares Comparison — Suitable vs. High Quality
# =============================================================================
cat("Creating Figure 4 (metric)...\n")

# Panel A: revenue cost (unchanged — reuse imperial object)
fig4a_cost_plot_m <- fig4a_cost_plot

# Panel B: hectares retired
fig4b_acres_plot_m <- ggplot(comparison_data, aes(x = quality, y = ac_to_ha(total_acres), fill = quality)) +
  geom_col(width = 0.6, col = "black", linewidth = 0.3) +
  geom_text(aes(label = format(round(ac_to_ha(total_acres)), big.mark = ",")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = col_quality, guide = "none") +
  labs(title = "B", x = NULL, y = "Hectares retired") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    axis.title.y  = element_text(margin = margin(r = 10))
  )

fig4_cost_acres_m <- fig4a_cost_plot_m + fig4b_acres_plot_m +
  plot_annotation(
    title    = "Retirement optimization: suitable vs. high quality habitat",
    subtitle = paste0("Meeting ", hab_ha_lbl, "-ha targets for BNLL, GKR & SJKF across all 5 climate scenarios"),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

ggsave(metric_path("fig4_total_revenue_acreage.png"), fig4_cost_acres_m,
       width = 7, height = 5, dpi = 600, bg = "white")
cat("  Saved: fig4_total_revenue_acreage_metric.png\n")


# =============================================================================
# FIGURE 5 (metric): Spatial maps (full SJV) — reuse maps, metric subtitle
# =============================================================================
# Field-status maps are unit-independent; only the habitat-target acreage in the
# subtitle converts to hectares. Reuses the persisted map_list panels.
if (exists("map_list")) {
  cat("Creating Figure 5 (metric)...\n")
  fig5_combined_map_m <- map_list[["Suitable"]] + map_list[["High Quality"]] +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = "Optimal retirement configuration (habitat only, with locked-in PAs)",
      subtitle = paste0("minimizing foregone revenue | ", hab_ha_lbl,
                        " ha target per species × 5 climate periods | BLM = ", chosen_blm),
      theme    = theme(
        plot.title    = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 10)
      )
    )
  ggsave(metric_path("fig5_spatial_maps.png"), fig5_combined_map_m,
         width = 11, height = 6, dpi = 600, bg = "white")
  cat("  Saved: fig5_spatial_maps_metric.png\n")
}


# =============================================================================
# FIGURE 6 (metric): Zoomed spatial maps (unit-free copy)
# =============================================================================
# Titles/subtitle carry no units, so this is identical to the imperial version.
if (exists("fig6_zoom_map")) {
  cat("Creating Figure 6 (metric)...\n")
  ggsave(metric_path("fig6_spatial_maps_zoom.png"), fig6_zoom_map,
         width = 10, height = 6, dpi = 600, bg = "white")
  cat("  Saved: fig6_spatial_maps_zoom_metric.png\n")
}


# =============================================================================
# FIGURE 7 (metric): BLM calibration (unit-free copy; cost in $M vs. boundary)
# =============================================================================
if (exists("fig7_blm_plot")) {
  cat("Creating Figure 7 (metric)...\n")
  ggsave(metric_path("fig7_blm_calibration.png"), fig7_blm_plot,
         width = 8, height = 5.5, dpi = 600, bg = "white")
  cat("  Saved: fig7_blm_calibration_metric.png\n")
}


# =============================================================================
# EXPORT SUMMARY TABLES (metric): acres -> ha, incidental AF -> km^3
# =============================================================================
cat("Exporting metric summary CSVs...\n")

county_summary_all_m <- county_summary_all %>%
  mutate(
    total_hectares  = ac_to_ha(total_acres),
    total_water_km3 = af_to_km3(total_water_AW)
  ) %>%
  dplyr::select(-total_acres, -total_water_AW)
write_csv(county_summary_all_m, metric_path("county_summaries.csv"))
cat("  Saved: county_summaries_metric.csv\n")

basin_summary_all_m <- basin_summary_all %>%
  mutate(
    total_hectares  = ac_to_ha(total_acres),
    total_water_km3 = af_to_km3(total_water_AW)
  ) %>%
  dplyr::select(-total_acres, -total_water_AW)
write_csv(basin_summary_all_m, metric_path("basin_summaries.csv"))
cat("  Saved: basin_summaries_metric.csv\n")

target_achievement_m <- target_summary %>%
  mutate(
    target   = ac_to_ha(target),
    achieved = ac_to_ha(achieved),
    surplus  = ac_to_ha(surplus)
  )
write_csv(target_achievement_m, metric_path("target_achievement.csv"))
cat("  Saved: target_achievement_metric.csv\n")


cat("\n========== METRIC (SI UNIT) VERSIONS COMPLETE (habitat-only w/ PAs) ==========\n")
cat("Metric output directory:", metric_dir, "\n")
















