# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS-TEMPORAL) — KERN-PIXLEY CORRIDOR CASE STUDY
# RESULTS VISUALIZATIONS
# =============================================================================
# Purpose: Generate figures and summary tables from the cross-temporal
#          prioritizr results produced by
#          9_1c_prioritizr_habitat_only_PAs_corridor.R.
#
# Inputs:  prioritizr_corridor_cross_temporal_PAs_results.RData
#          Contains: solutions, summary_all, target_detail, scenarios,
#                    suit_features, hq_features, pu (corridor fields + PAs),
#                    field_data, field_data_all, pa_data, corridor,
#                    corridor_union, habitat_target, chosen_blm,
#                    blm_results, bm, cost_scale_factor
#
# Outputs (in fig_dir):
#   fig1_*.csv         — Overall summary tables
#   fig2_retirement_by_basin.png   (basin breakdown — basins that intersect corridor)
#   fig3_total_revenue_acreage.png (suitable vs. high quality comparison)
#   fig4_spatial_map.png           (corridor extent, fields + PAs + corridor outline)
#   fig5_blm_calibration.png
#
# Note: County breakdown and the broader SJV zoom map have been dropped — the
#       corridor PU pool is small enough that the spatial map tells the story
#       and the county breakdown reduces to 2-3 bars.
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
fig_dir <- here("data/intermediate/misc/k2p_case_study/10_1c_prioritizr_habitat_only_PAs_k2p_corridor_figures/blm_0.0075/")


# =============================================================================
# LOAD RESULTS
# =============================================================================

load(here("data/intermediate/misc/k2p_case_study/9_1c_prioritizr_habitat_only_PAs_k2p_corridor/blm_0.0075/prioritizr_corridor_cross_temporal_PAs_results.RData"))

# Extract the cleaned PA layer from the combined planning unit object
pa_sf <- pu %>% filter(is_pa) %>% st_make_valid()

cat("Loaded corridor cross-temporal prioritizr results (with PAs):\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Field planning units (corridor):", sum(!pu$is_pa), "\n")
cat("    - Fallow:", sum(!pu$is_pa & pu$fallow == 1), "\n")
cat("    - Cultivated:", sum(!pu$is_pa & pu$fallow == 0), "\n")
cat("  PA planning units (locked in):", nrow(pa_sf), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Chosen BLM:", chosen_blm, "\n")


# =============================================================================
# LOAD BASIN LAYER & ASSIGN BASINS TO FIELDS
# =============================================================================
# The corridor optimization script doesn't carry basin assignment for fields
# Re-attach basin names here so we can produce the basin-breakdown figure.

cat("\nLoading DWR basins and assigning to corridor fields...\n")

basins_raw <- st_read(here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp"))

sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>%
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA",
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN",
                            "SAN JOAQUIN VALLEY - COSUMNES",
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY")) %>%
  st_transform(st_crs(field_data))


# Centroid-based spatial join (same logic as the basin-wide script)
field_data_with_basin <- field_data
field_centroids <- st_centroid(field_data_with_basin)
basin_join <- st_join(field_centroids,
                      sjv_basins %>% select(Basin_Su_1),
                      left = TRUE)

field_data_with_basin$basin_raw <- basin_join$Basin_Su_1

field_data_with_basin <- field_data_with_basin %>%
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

# Build an id -> basin lookup for joining onto solutions
basin_lookup <- field_data_with_basin %>%
  st_drop_geometry() %>%
  dplyr::select(id, basin)

cat("\nCorridor field basin assignment summary:\n")
print(basin_lookup %>% count(basin) %>% arrange(desc(n)) %>% as.data.frame())


# Restrict the basin layer for plotting to those that intersect the corridor —
# the spatial map then shows only the relevant basin polygons.
basins_in_corridor <- sjv_basins %>%
  filter(lengths(st_intersects(., corridor_union)) > 0)

cat("\nBasins intersecting the corridor:", nrow(basins_in_corridor), "\n")
print(sort(unique(basins_in_corridor$Basin_Su_1)))


# =============================================================================
# COLOR PALETTE
# =============================================================================

col_selected   <- "#8B2A1A"   # brick red — newly retired (selected) fields
col_unselected <- "wheat1"    # tan — non-selected fields
col_pa         <- "darkgreen" # existing protected areas (locked in)
col_retired    <- "grey70"    # fields already retired prior to optimization
col_basin_fill <- "grey95"    # basin polygons backdrop
col_basin_line <- "grey40"    # basin boundary lines
col_corridor   <- "black"     # corridor outline

# Bar-chart palette
col_quality <- c("Suitable" = "darkseagreen", "High Quality" = "darkgreen")


# =============================================================================
# FIGURE 1: Overall Summary Tables
# =============================================================================

cat("\n========== FIGURE 1: OVERALL SUMMARY ==========\n\n")

# Total PA acres locked in (constant across scenarios)
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
  kbl(caption = "Corridor Cross-Temporal Optimization Summary (with locked-in PAs)",
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
  kbl(caption = "Corridor Target Achievement by Species and Climate Scenario",
      align = c("l", "l", "l", "r", "r", "r", "r")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, position = "center") %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Suitable Habitat", 1, 15) %>%
  pack_rows("High Quality Habitat", 16, 30) %>%
  print()


# =============================================================================
# FIGURE 2: Retirement by Basin
# =============================================================================
# Basin breakdown for the corridor case study. Joins the basin assignment
# from above onto the optimization solutions.

cat("\n========== FIGURE 2: RETIREMENT BY BASIN ==========\n\n")

basin_summaries <- list()

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  
  basin_summaries[[scen]] <- sol %>%
    filter(solution_1 == 1, !is_pa) %>%
    st_drop_geometry() %>%
    left_join(basin_lookup, by = "id") %>%
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

# Zero-fill so every basin appears in both quality levels
all_combos_basin <- expand.grid(
  basin   = unique(basin_summary_all$basin),
  quality = levels(basin_summary_all$quality),
  stringsAsFactors = FALSE
)

basin_summary_all <- basin_summary_all %>%
  right_join(all_combos_basin, by = c("basin", "quality")) %>%
  mutate(across(c(n_fields, total_acres, total_rev, total_water_AW),
                ~ replace_na(.x, 0)))

# Pin a single y-axis ordering across all three facets
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

fig2_basin_plot <- ggplot(basin_long,
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
    title    = "Corridor retirement by groundwater basin (habitat only, with locked-in PAs)",
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

print(fig2_basin_plot)

ggsave(file.path(fig_dir, "fig2_retirement_by_basin.png"), fig2_basin_plot,
       width = 10, height = 6, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 3: Cost and Acres Comparison — Suitable vs. High Quality
# =============================================================================

cat("\n========== FIGURE 3: COST & ACRES COMPARISON ==========\n\n")

comparison_data <- summary_all %>%
  dplyr::select(quality, total_cost, total_acres, n_selected) %>%
  mutate(quality = factor(quality, levels = c("Suitable", "High Quality")))

# Revenue cost
fig3a_cost_plot <- ggplot(comparison_data, aes(x = quality, y = total_cost, fill = quality)) +
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
fig3b_acres_plot <- ggplot(comparison_data, aes(x = quality, y = total_acres, fill = quality)) +
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

fig3_cost_acres <- fig3a_cost_plot + fig3b_acres_plot +
  plot_annotation(
    title    = "Corridor retirement: suitable vs. high quality habitat",
    subtitle = "Meeting 25,000-acre targets for BNLL, GKR & SJKF across all 5 climate scenarios",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

print(fig3_cost_acres)

ggsave(file.path(fig_dir, "fig3_total_revenue_acreage.png"), fig3_cost_acres,
       width = 7, height = 5, dpi = 600, bg = "white")


# =============================================================================
# FIGURE 4: Spatial Map — Corridor extent, Suitable vs. High Quality
# =============================================================================
# Layer order (bottom -> top):
#   1. Basin polygons that intersect the corridor (grey95 backdrop)
#   2. Field statuses (selected = brick red, unselected = wheat, retired = grey)
#   3. Protected areas (darkgreen overlay)
#   4. Basin polygon outlines (re-drawn on top)
#   5. Corridor outline (black, on top of everything)

cat("\n========== FIGURE 4: SPATIAL MAP (CORRIDOR) ==========\n\n")

# Bounding box from the corridor field PUs (already in field CRS) — same
# pattern as the valley-wide script's field_bbox approach. Adding a small
# buffer so the corridor outline isn't flush against the panel edge.
field_bbox_raw <- st_bbox(field_data)
buffer_pct     <- 0.05
dx <- (field_bbox_raw$xmax - field_bbox_raw$xmin) * buffer_pct
dy <- (field_bbox_raw$ymax - field_bbox_raw$ymin) * buffer_pct
field_bbox <- c(
  field_bbox_raw$xmin - dx,
  field_bbox_raw$ymin - dy,
  field_bbox_raw$xmax + dx,
  field_bbox_raw$ymax + dy
)
names(field_bbox) <- c("xmin", "ymin", "xmax", "ymax")

# Restrict field_data_all to the corridor bbox extent so we're not drawing
# the full SJV. Coordinate-based filter on centroids (no spatial predicates)
# avoids the !anyNA() error we hit earlier.
ctr_all <- suppressWarnings(st_coordinates(st_centroid(field_data_all)))
in_view <- !is.na(ctr_all[, "X"]) & !is.na(ctr_all[, "Y"]) &
  ctr_all[, "X"] >= field_bbox[["xmin"]] & ctr_all[, "X"] <= field_bbox[["xmax"]] &
  ctr_all[, "Y"] >= field_bbox[["ymin"]] & ctr_all[, "Y"] <= field_bbox[["ymax"]]
field_data_view <- field_data_all[in_view, ]

cat("Fields in plot view (within corridor bbox):", nrow(field_data_view), "\n")

map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  # Build a complete field layer for plotting — same pattern as the
  # valley-wide script's sol_join, just using field_data_view (corridor
  # extent) instead of the full SJV.
  sol_join <- field_data_view %>%
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
    # Basin polygons that intersect the corridor as backdrop
    geom_sf(data = basins_in_corridor, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.3) +
    # Fields colored by status
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    # PAs overlaid in green — mapped via fill so they appear in the legend
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    # Re-draw basin outlines on top so they're not hidden by fills
    geom_sf(data = basins_in_corridor, fill = NA, color = col_basin_line, linewidth = 0.3) +
    # Corridor outline on top of everything
    geom_sf(data = corridor, fill = NA, color = col_corridor, linewidth = 0.6) +
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
      plot.title       = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 9, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.key.height = unit(1, "cm")
    ) +
    scale_y_continuous(n.breaks = 7) +
    scale_x_continuous(n.breaks = 4)
  
  map_list[[quality_label]] <- p
}

fig4_corridor_map <- map_list[["Suitable"]] + map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal corridor retirement configuration (Kern-Pixley case study)",
    subtitle = paste0(
      "minimizing foregone revenue | 25,000 ac target per species × 5 climate periods | BLM = ", chosen_blm,
      " | corridor outlined in black"
    ),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(fig4_corridor_map)

ggsave(file.path(fig_dir, "fig4_spatial_map.png"), fig4_corridor_map,
       width = 11, height = 6, dpi = 600, bg = "white")




# =============================================================================
# ADDING AN INSET TO THE PLOT ABOVE
# =============================================================================

# =============================================================================
# FIGURE 4: Spatial Map — Corridor extent, Suitable vs. High Quality
# =============================================================================
# Layer order (bottom -> top):
#   1. Basin polygons that intersect the corridor (grey95 backdrop)
#   2. Field statuses (selected = brick red, unselected = wheat, retired = grey)
#   3. Protected areas (darkgreen overlay)
#   4. Basin polygon outlines (re-drawn on top)
#   5. Corridor outline (black, on top of everything)

cat("\n========== FIGURE 4: SPATIAL MAP (CORRIDOR) ==========\n\n")

# Bounding box from the corridor field PUs (already in field CRS) — same
# pattern as the valley-wide script's field_bbox approach. Adding a buffer
# so the corridor outline isn't flush against the panel edge. The left
# side gets a larger buffer because the corridor sits to the right of its
# natural bbox center and the previous version cropped the corridor curve.
field_bbox_raw <- st_bbox(field_data)
buffer_pct_left   <- 0.20   # bigger left margin
buffer_pct_other  <- 0.05   # default for top, bottom, right
dx_left  <- (field_bbox_raw$xmax - field_bbox_raw$xmin) * buffer_pct_left
dx_right <- (field_bbox_raw$xmax - field_bbox_raw$xmin) * buffer_pct_other
dy       <- (field_bbox_raw$ymax - field_bbox_raw$ymin) * buffer_pct_other
field_bbox <- c(
  field_bbox_raw$xmin - dx_left,
  field_bbox_raw$ymin - dy,
  field_bbox_raw$xmax + dx_right,
  field_bbox_raw$ymax + dy
)
names(field_bbox) <- c("xmin", "ymin", "xmax", "ymax")

# Restrict field_data_all to the corridor bbox extent so we're not drawing
# the full SJV. Coordinate-based filter on centroids (no spatial predicates)
# avoids the !anyNA() error we hit earlier.
ctr_all <- suppressWarnings(st_coordinates(st_centroid(field_data_all)))
in_view <- !is.na(ctr_all[, "X"]) & !is.na(ctr_all[, "Y"]) &
  ctr_all[, "X"] >= field_bbox[["xmin"]] & ctr_all[, "X"] <= field_bbox[["xmax"]] &
  ctr_all[, "Y"] >= field_bbox[["ymin"]] & ctr_all[, "Y"] <= field_bbox[["ymax"]]
field_data_view <- field_data_all[in_view, ]

cat("Fields in plot view (within corridor bbox):", nrow(field_data_view), "\n")

map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  # Build a complete field layer for plotting — same pattern as the
  # valley-wide script's sol_join, just using field_data_view (corridor
  # extent) instead of the full SJV.
  sol_join <- field_data_view %>%
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
    # Basin polygons that intersect the corridor as backdrop
    geom_sf(data = basins_in_corridor, fill = col_basin_fill, color = col_basin_line,
            alpha = 0.85, linewidth = 0.5) +
    # Fields colored by status
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    # PAs overlaid in green — mapped via fill so they appear in the legend
    geom_sf(data = pa_sf %>% mutate(status = "Existing protected area"),
            aes(fill = status), color = NA, alpha = 0.85) +
    # Re-draw basin outlines on top so they're not hidden by fills
    geom_sf(data = basins_in_corridor, fill = NA, color = col_basin_line, linewidth = 0.3) +
    # Corridor outline on top of everything
    geom_sf(data = corridor, fill = NA, color = col_corridor, linewidth = 0.6) +
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
      axis.text        = element_text(size = 11, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.title     = element_text(size = 13, face = "bold"),
      legend.text      = element_text(size = 12),
      legend.key.height = unit(1.4, "cm"),
      legend.key.width  = unit(0.8, "cm")
    ) +
    scale_y_continuous(n.breaks = 7) +
    scale_x_continuous(n.breaks = 4)
  
  map_list[[quality_label]] <- p
}

# =============================================================================
# Inset map: SJV basins (all 15) + corridor highlighted in red
# =============================================================================
# Small overview map showing where the Kern-Pixley corridor sits within the
# broader SJV basin context. All 15 SJV groundwater basins drawn in grey,
# corridor filled in red to draw the eye.
#
# The inset's plotting region is constrained to the SJV basins' actual
# bounding box (with a tight 2% expansion), which removes the large
# whitespace gaps that appeared when ggplot's default expansion was used.
# The top plot.margin pushes the inset's panel region down so it aligns
# vertically with the top of the main map panels.

sjv_bbox <- st_bbox(sjv_basins)

inset_map <- ggplot() +
  geom_sf(data = sjv_basins,
          fill = "grey90", color = "grey50", linewidth = 0.25) +
  geom_sf(data = corridor,
          fill = "grey10", color = "grey10", linewidth = 0.4) +
  coord_sf(xlim = sjv_bbox[c(1, 3)], ylim = sjv_bbox[c(2, 4)],
           datum = sf::st_crs(4326), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_blank(),
    plot.margin     = margin(t = 30, r = 2, b = 2, l = 2)
  )


# =============================================================================
# Compose final figure: maps left, inset top-right, legend bottom-right
# =============================================================================
# Layout uses patchwork::area() to place four regions:
#   - Suitable map      (top-left, large)
#   - High Quality map  (top-middle, large)
#   - Inset map         (top-right, small)
#   - Legend            (bottom-right, below inset)
#
# Both maps keep their legend definitions intact; plot_layout(guides =
# "collect") harvests them and routes the merged guide to guide_area().
# Suppressing legends via theme(legend.position = "none") would leave
# guide_area() empty, so we leave the legend definitions alone.

# Define the layout grid
# Rows 1-10 span the full height; columns 1-8 host the two maps, columns
# 9-11 host the inset (rows 1-4) and the legend (rows 5-10). Giving the
# legend more rows (vs. before) shrinks the inset proportionally and
# leaves more vertical room for the four legend entries.
layout <- c(
  patchwork::area(t = 1,  l = 1,  b = 10, r = 4),   # Suitable map
  patchwork::area(t = 1,  l = 5,  b = 10, r = 8),   # High Quality map
  patchwork::area(t = 1,  l = 9,  b = 4,  r = 11),  # Inset (top-right, smaller)
  patchwork::area(t = 5,  l = 9,  b = 10, r = 11)   # Legend (bottom-right, taller)
)

fig4_corridor_map <- map_list[["Suitable"]] + map_list[["High Quality"]] + inset_map + guide_area() +
  plot_layout(design = layout, guides = "collect") +
  plot_annotation(
    title    = "Optimal corridor retirement configuration (Kern-Pixley case study)",
    subtitle = paste0(
      "minimizing foregone revenue | 25,000 ac target per species × 5 climate periods | BLM = ", chosen_blm
    ),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(fig4_corridor_map)

ggsave(file.path(fig_dir, "fig4_spatial_map_inset.png"), fig4_corridor_map,
       width = 13, height = 7, dpi = 600, bg = "white")



# =============================================================================
# FIGURE 5: BLM Calibration Plot
# =============================================================================

cat("\n========== FIGURE 5: BLM CALIBRATION ==========\n\n")

blm_results <- blm_results %>%
  mutate(total_cost_M = round(total_cost / 1e6, 3))

fig5_blm_plot <- ggplot(blm_results, aes(x = total_cost_M, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length (corridor)",
    subtitle = "Calibrated on cross-temporal suitable habitat problem with locked-in PAs",
    x        = "Foregone revenue ($M)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_classic()

print(fig5_blm_plot)

ggsave(file.path(fig_dir, "fig5_blm_calibration.png"), fig5_blm_plot,
       width = 8, height = 5.5, dpi = 600, bg = "white")



# =============================================================================
# FIGURE 6: CROP COMPOSITION OF SELECTED FIELDS — KERN-PIXLEY CORRIDOR
# Same side-by-side layout as fig4 (Suitable | High Quality), but selected
# planning units are filled by crop class (`comm`) instead of field status.
# =============================================================================

# ---- 0. Set to match your objects -------------------------------------------
id_col   <- "id"            # field ID column (PAs are negative)
comm_col <- "comm"          # crop class column on field_data
sol_col  <- "solution_1"    # selected flag in the solved prioritizr objects
n_top    <- 8               # # of crops to color individually; rest -> "Other"
# sol_suit, sol_hq : your two solved problems (sf or df with `sol_col`)
# field_data       : corridor planning units (sf) carrying `comm` + geometry
# corridor_union   : TNC corridor polygon (for the black outline)

sol_suit <- solutions[["cross_temporal_suit"]]
sol_hq   <- solutions[["cross_temporal_hq"]]

# ---- 1. Selected field IDs per solution (drop locked-in PAs via id > 0) ------
sel_ids <- function(sol) sol %>% st_drop_geometry() %>%
  filter(.data[[sol_col]] == 1, .data[[id_col]] > 0) %>% pull(all_of(id_col))

selected_all <- bind_rows(
  field_data %>% filter(.data[[id_col]] %in% sel_ids(sol_suit)) %>% mutate(quality = "Suitable"),
  field_data %>% filter(.data[[id_col]] %in% sel_ids(sol_hq))   %>% mutate(quality = "High Quality")
) %>%
  mutate(area_ha = as.numeric(st_area(geom)) / 1e4)

# ---- 2. Collapse to top crops (consistent colors across both panels) --------
top_crops <- selected_all %>% st_drop_geometry() %>%
  group_by(crop = .data[[comm_col]]) %>% summarise(ha = sum(area_ha), .groups = "drop") %>%
  slice_max(ha, n = n_top) %>% pull(crop)

# ---- 2b. Factor levels + green-free crop palette + PA layer -----------------
crop_levels <- c(top_crops, "Other", "Protected area (locked in)")

selected_all <- selected_all %>%
  mutate(crop    = factor(if_else(.data[[comm_col]] %in% top_crops, .data[[comm_col]], "Other"),
                          levels = crop_levels),
         quality = factor(quality, levels = c("Suitable", "High Quality")))

# Locked-in PAs (negative IDs) — identical across both solutions, so pull once.
# No `quality` column => facet draws them in both panels.
pa_sf <- sol_suit %>%
  filter(.data[[sol_col]] == 1, .data[[id_col]] < 0) %>%
  mutate(crop = factor("Protected area (locked in)", levels = crop_levels))

# Green-free qualitative palette (Okabe-Ito + Tol, greens/teals removed)
crop_colors <- c("cyan", "#F0E442", "#0072B2", "#E69F00", "#CC79A7",
                 "bisque3", "#882255", "#332288", "#AA4499", "#661100")
crop_pal <- c(setNames(head(crop_colors, length(top_crops)), top_crops),
              "Other"                       = "grey75",
              "Protected area (locked in)"  = "forestgreen")

# ---- 3. Framing: corridor bbox w/ asymmetric left buffer (matches fig4) ------
corridor_union <- st_transform(corridor_union, st_crs(field_data))
bb <- st_bbox(field_data); xr <- bb["xmax"] - bb["xmin"]; yr <- bb["ymax"] - bb["ymin"]
xlim <- c(bb["xmin"] - 0.20 * xr, bb["xmax"] + 0.05 * xr)
ylim <- c(bb["ymin"] - 0.05 * yr, bb["ymax"] + 0.05 * yr)

# ---- 4. Single faceted plot => one shared legend ----------------------------
fig6_crop <- ggplot() +
  geom_sf(data = field_data, fill = "grey95", color = "grey85", linewidth = 0.05) +
  geom_sf(data = pa_sf,        aes(fill = crop), color = "grey30", linewidth = 0.05) +
  geom_sf(data = selected_all, aes(fill = crop), color = "grey30", linewidth = 0.05) +
  geom_sf(data = corridor_union, fill = NA, color = "black", linewidth = 0.6) +
  facet_wrap(~ quality) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  scale_fill_manual(values = crop_pal, drop = FALSE, name = "Selected field crop class / status") +
  scale_y_continuous(n.breaks = 7) + scale_x_continuous(n.breaks = 4) +
  labs(title    = "Crop composition of selected fields (Kern–Pixley case study)",
       subtitle = "Selected planning units colored by crop class; locked-in protected areas in green") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10),
    strip.text       = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 9, color = "grey30"),
    axis.ticks       = element_line(color = "grey30"),
    panel.background = element_rect(fill = "white", color = NA),
    axis.title       = element_blank(),
    legend.title     = element_text(face = "bold"),
    legend.position  = "right"
  )

print(fig6_crop)
ggsave(file.path(fig_dir, "fig6_crop_composition.png"), fig6_crop,
       width = 11, height = 6, dpi = 600, bg = "white")






# =============================================================================
# EXPORT SUMMARY TABLES
# =============================================================================

write_csv(basin_summary_all,
          file.path(fig_dir, "corridor_basin_summaries.csv"))
write_csv(target_summary,
          file.path(fig_dir, "corridor_target_achievement.csv"))
write_csv(overall_summary,
          file.path(fig_dir, "corridor_overall_summary.csv"))


cat("\n========== ALL CORRIDOR FIGURES GENERATED ==========\n")
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
  kbl(caption = "Corridor Cross-Temporal Optimization Summary (with locked-in PAs)",
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
  kbl(caption = "Corridor Target Achievement by Species and Climate Scenario",
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
# FIGURE 2 (metric): Retirement by Basin
# =============================================================================
cat("Creating Figure 2 (metric)...\n")

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

fig2_basin_plot_m <- ggplot(basin_long_m,
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
    title    = "Corridor retirement by groundwater basin (habitat only, with locked-in PAs)",
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

ggsave(metric_path("fig2_retirement_by_basin.png"), fig2_basin_plot_m,
       width = 10, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig2_retirement_by_basin_metric.png\n")


# =============================================================================
# FIGURE 3 (metric): Cost and Hectares Comparison — Suitable vs. High Quality
# =============================================================================
cat("Creating Figure 3 (metric)...\n")

# Panel A: revenue cost (unchanged — reuse imperial object)
fig3a_cost_plot_m <- fig3a_cost_plot

# Panel B: hectares retired
fig3b_acres_plot_m <- ggplot(comparison_data, aes(x = quality, y = ac_to_ha(total_acres), fill = quality)) +
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

fig3_cost_acres_m <- fig3a_cost_plot_m + fig3b_acres_plot_m +
  plot_annotation(
    title    = "Corridor retirement: suitable vs. high quality habitat",
    subtitle = paste0("Meeting ", hab_ha_lbl, "-ha targets for BNLL, GKR & SJKF across all 5 climate scenarios"),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

ggsave(metric_path("fig3_total_revenue_acreage.png"), fig3_cost_acres_m,
       width = 7, height = 5, dpi = 600, bg = "white")
cat("  Saved: fig3_total_revenue_acreage_metric.png\n")


# =============================================================================
# FIGURE 4 (metric): Corridor spatial maps (plain + inset)
# =============================================================================
# The field-status maps are unit-independent; only the habitat-target acreage
# in the subtitle converts to hectares. Reuses the persisted corridor map
# panels (map_list), the inset overview (inset_map), and the inset layout.
if (exists("map_list")) {
  
  cat("Creating Figure 4 (metric, plain side-by-side)...\n")
  fig4_corridor_map_m <- map_list[["Suitable"]] + map_list[["High Quality"]] +
    plot_layout(guides = "collect") +
    plot_annotation(
      title    = "Optimal corridor retirement configuration (Kern-Pixley case study)",
      subtitle = paste0("minimizing foregone revenue | ", hab_ha_lbl,
                        " ha target per species × 5 climate periods | BLM = ", chosen_blm,
                        " | corridor outlined in black"),
      theme    = theme(
        plot.title    = element_text(face = "bold", hjust = 0, size = 14),
        plot.subtitle = element_text(hjust = 0, size = 10)
      )
    )
  ggsave(metric_path("fig4_spatial_map.png"), fig4_corridor_map_m,
         width = 11, height = 6, dpi = 600, bg = "white")
  cat("  Saved: fig4_spatial_map_metric.png\n")
  
  if (exists("inset_map") && exists("layout")) {
    cat("Creating Figure 4 (metric, with inset)...\n")
    fig4_corridor_map_inset_m <- map_list[["Suitable"]] + map_list[["High Quality"]] +
      inset_map + guide_area() +
      plot_layout(design = layout, guides = "collect") +
      plot_annotation(
        title    = "Optimal corridor retirement configuration (Kern-Pixley case study)",
        subtitle = paste0("minimizing foregone revenue | ", hab_ha_lbl,
                          " ha target per species × 5 climate periods | BLM = ", chosen_blm),
        theme    = theme(
          plot.title    = element_text(face = "bold", hjust = 0, size = 14),
          plot.subtitle = element_text(hjust = 0, size = 10)
        )
      )
    ggsave(metric_path("fig4_spatial_map_inset.png"), fig4_corridor_map_inset_m,
           width = 13, height = 7, dpi = 600, bg = "white")
    cat("  Saved: fig4_spatial_map_inset_metric.png\n")
  }
}


# =============================================================================
# FIGURE 5 (metric): BLM calibration (unit-free copy; cost in $M vs. boundary)
# =============================================================================
if (exists("fig5_blm_plot")) {
  cat("Creating Figure 5 (metric)...\n")
  ggsave(metric_path("fig5_blm_calibration.png"), fig5_blm_plot,
         width = 8, height = 5.5, dpi = 600, bg = "white")
  cat("  Saved: fig5_blm_calibration_metric.png\n")
}



# =============================================================================
# CROP-LEVEL SELECTION SUMMARY — suitable vs. high quality (corridor)
# Exact hectares & foregone revenue by crop class, to quantify the cost driver.
# =============================================================================
rev_col <- "revenue"   # per-field annual revenue ($) on field_data — adjust if named differently

crop_summary <- selected_all %>%
  st_drop_geometry() %>%
  mutate(q = if_else(quality == "Suitable", "suit", "hq")) %>%
  group_by(q, comm = .data[[comm_col]]) %>%
  summarise(n_fields  = n(),
            hectares  = sum(area_ha, na.rm = TRUE),
            revenue_M = sum(.data[[rev_col]], na.rm = TRUE) / 1e6,
            .groups = "drop")

# Wide comparison: deltas (High Quality − Suitable), sorted by revenue delta
crop_compare <- crop_summary %>%
  pivot_wider(id_cols = comm,
              names_from = q,
              values_from = c(n_fields, hectares, revenue_M),
              values_fill = 0) %>%
  mutate(ha_delta  = hectares_hq  - hectares_suit,
         rev_delta = revenue_M_hq - revenue_M_suit) %>%
  arrange(desc(rev_delta)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

print(crop_compare, n = Inf, width = Inf)

# Pistachios line, ready to cite
crop_compare %>% filter(comm == "Pistachios")

# QA: totals should reconcile with the overall summary (suit ~10,524 ha / $22.6M;
# hq ~11,860 ha / $88.1M)
crop_summary %>% group_by(q) %>%
  summarise(ha = sum(hectares), rev_M = sum(revenue_M), .groups = "drop")


# =============================================================================
# TABLE S[X]: CROP COMPOSITION OF SELECTED FIELDS — KERN-PIXLEY CORRIDOR
# Companion to the crop-composition maps (Figure S[X]).
# =============================================================================
library(kableExtra)

# Reorder to Suitable | High Quality | Difference, then append a totals row
crop_table_df <- crop_compare %>%
  transmute(`Crop class` = comm,
            suit_fields = n_fields_suit, suit_ha = hectares_suit, suit_rev = revenue_M_suit,
            hq_fields   = n_fields_hq,   hq_ha   = hectares_hq,   hq_rev   = revenue_M_hq,
            d_ha = ha_delta, d_rev = rev_delta)

crop_table_df <- crop_table_df %>%
  bind_rows(crop_table_df %>%
              summarise(across(where(is.numeric), sum)) %>%
              mutate(`Crop class` = "Total"))

# Display formatting: integers w/ commas, $M to 1 dp, signed deltas
crop_table_disp <- crop_table_df %>%
  mutate(suit_fields = formatC(suit_fields, format = "d", big.mark = ","),
         hq_fields   = formatC(hq_fields,   format = "d", big.mark = ","),
         suit_ha     = formatC(round(suit_ha), format = "d", big.mark = ","),
         hq_ha       = formatC(round(hq_ha),   format = "d", big.mark = ","),
         suit_rev    = formatC(suit_rev, format = "f", digits = 1),
         hq_rev      = formatC(hq_rev,   format = "f", digits = 1),
         d_ha        = formatC(round(d_ha), format = "d", big.mark = ",", flag = "+"),
         d_rev       = formatC(d_rev, format = "f", digits = 1, flag = "+"))

crop_tbl <- crop_table_disp %>%
  kbl(col.names = c("Crop class", "Fields", "Hectares", "Revenue ($M)",
                    "Fields", "Hectares", "Revenue ($M)",
                    "\u0394 Hectares", "\u0394 Revenue ($M)"),
      align = c("l", rep("r", 8)), booktabs = TRUE,
      caption = paste("Crop composition of fields selected in the Kern\u2013Pixley corridor",
                      "habitat-only optimization, under the suitable and high-quality habitat",
                      "thresholds. Differences are high-quality minus suitable; rows are sorted",
                      "by foregone-revenue difference. Locked-in protected areas are excluded.")) %>%
  add_header_above(c(" " = 1, "Suitable" = 3, "High Quality" = 3,
                     "Difference (HQ \u2212 Suitable)" = 2)) %>%
  kable_styling(full_width = FALSE, position = "center",
                bootstrap_options = c("striped", "condensed")) %>%
  row_spec(nrow(crop_table_disp), bold = TRUE) %>%   # totals row
  column_spec(1, width = "16em")

crop_tbl

save_kable(crop_tbl, metric_path("crop_composition.html"))


# =============================================================================
# EXPORT SUMMARY TABLES (metric): acres -> ha, incidental AF -> km^3
# =============================================================================
cat("Exporting metric summary CSVs...\n")

basin_summary_all_m <- basin_summary_all %>%
  mutate(
    total_hectares  = ac_to_ha(total_acres),
    total_water_km3 = af_to_km3(total_water_AW)
  ) %>%
  dplyr::select(-total_acres, -total_water_AW)
write_csv(basin_summary_all_m, metric_path("corridor_basin_summaries.csv"))
cat("  Saved: corridor_basin_summaries_metric.csv\n")

target_achievement_m <- target_summary %>%
  mutate(
    target   = ac_to_ha(target),
    achieved = ac_to_ha(achieved),
    surplus  = ac_to_ha(surplus)
  )
write_csv(target_achievement_m, metric_path("corridor_target_achievement.csv"))
cat("  Saved: corridor_target_achievement_metric.csv\n")

write_csv(overall_summary_m, metric_path("corridor_overall_summary.csv"))
cat("  Saved: corridor_overall_summary_metric.csv\n")


cat("\n========== METRIC (SI UNIT) VERSIONS COMPLETE (corridor habitat-only) ==========\n")
cat("Metric output directory:", metric_dir, "\n")

























