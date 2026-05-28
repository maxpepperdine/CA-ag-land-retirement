# =============================================================================
# PRIORITIZR HABITAT CREATION (CROSS TEMPORAL) — RESULTS VISUALIZATIONS
# =============================================================================
# Purpose: Generate figures and summary tables from the cross-temporal
#          prioritizr results produced by 9_1_prioritizr_habitat_only.R.
#
# Inputs:  prioritizr_cross_temporal_results.RData
#          Contains: solutions, summary_all, target_detail, scenarios,
#                    suit_features, hq_features, field_data, field_data_all,
#                    habitat_target, chosen_blm, blm_results, bm,
#                    cost_scale_factor
# =============================================================================


# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(prioritizr)
library(sf)
library(here)
library(scales)
library(tigris)
library(patchwork)
library(kableExtra)


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)

# figure directory
fig_dir <- here("data/intermediate/10_1_prioritizr_habitat_only_figures/")

# =============================================================================
# SECTION 1: Load Results
# =============================================================================

load(here("data/intermediate/9_1_prioritizr_habitat_only/prioritizr_cross_temporal_results.RData"))

# Load SJV county boundaries from TIGRIS
options(tigris_use_cache = TRUE)
ca_counties <- counties(state = "CA")
sjv_county_names <- c("San Joaquin", "Stanislaus", "Merced", "Madera",
                      "Fresno", "Kings", "Tulare", "Kern")
sjv_counties <- ca_counties %>%
  filter(NAME %in% sjv_county_names) %>%
  st_transform(st_crs(field_data))

cat("Loaded cross-temporal prioritizr results:\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Planning units (non-retired):", nrow(field_data), "\n")
cat("    - Fallow:", sum(field_data$fallow == 1), "\n")
cat("    - Cultivated:", sum(field_data$fallow == 0), "\n")
cat("  All fields:", nrow(field_data_all), "\n")
cat("  Retired fields (excluded):", nrow(field_data_all) - nrow(field_data), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Chosen BLM:", chosen_blm, "\n")


# =============================================================================
# SECTION 2: Overall Summary Table
# =============================================================================

cat("\n========== OVERALL SUMMARY ==========\n\n")

# Build a clean summary table with kableExtra

overall_summary <- summary_all %>%
  mutate(
    Quality            = quality,
    `Fields Selected`  = format(n_selected, big.mark = ","),
    `Acres Retired`    = format(round(total_acres), big.mark = ","),
    `Revenue Cost ($)` = paste0("$", format(round(total_cost), big.mark = ",")),
    `Water Saved (af)` = format(round(total_water_AW), big.mark = ","),
    `Boundary Length`  = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(Quality, `Fields Selected`, `Acres Retired`,
                `Revenue Cost ($)`, `Water Saved (af)`, `Boundary Length`)

overall_summary %>%
  kbl(caption = "Cross-Temporal Optimization Summary",
      align = c("l", rep("r", 5))) %>%
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
  pack_rows("Suitable Habitat", 1, 10) %>%
  pack_rows("High Quality Habitat", 11, 20) %>%
  print()


# =============================================================================
# SECTION 3: County-Level Bar Charts
# =============================================================================

# Build county summaries for both solutions
county_summaries <- list()

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  county_summaries[[scen]] <- sol %>%
    filter(solution_1 == 1) %>%
    st_drop_geometry() %>%
    group_by(county) %>%
    summarise(
      n_fields    = n(),
      total_acres = sum(acres, na.rm = TRUE),
      total_rev   = sum(revenue, na.rm = TRUE),
      total_water_AW = sum(waterAW, na.rm = TRUE),
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
all_combos <- expand.grid(
  county  = unique(county_summary_all$county),
  quality = levels(county_summary_all$quality),
  stringsAsFactors = FALSE
)

county_summary_all <- county_summary_all %>%
  right_join(all_combos, by = c("county", "quality")) %>%
  mutate(across(c(n_fields, total_acres, total_rev, total_water_AW), ~ replace_na(.x, 0)))


# Pivot to long format for faceting by metric
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

# Combined county bar chart
county_combined_plot <- ggplot(county_long,
                               aes(x = reorder(county, value),
                                   y = value, fill = quality)) +
  geom_col(position = "dodge", col = "black") +
  facet_wrap(~ metric, scales = "free_x") +
  coord_flip() +
  scale_y_continuous(labels = function(x) {
    ifelse(x >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
           format(round(x), big.mark = ","))
  }) +
  scale_fill_manual(values = c("Suitable" = "darkseagreen", "High Quality" = "darkgreen")) +
  labs(
    title    = "Optimal retirement by county (habitat only)",
    subtitle = "Fields must meet 25,000-acre targets for BNLL, GKR & SJKF across all climate scenarios",
    x        = NULL,
    y        = NULL,
    fill     = "Habitat Quality"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(hjust = 0, size = 9),
    strip.text    = element_text(face = "bold", size = 10),
    axis.text.y   = element_text(size = 9),
    axis.text.x   = element_text(size = 7),
    legend.position = "bottom"
  )

print(county_combined_plot)

# save the plot
ggsave(file.path(fig_dir, "retirement_by_county.png"), county_combined_plot,
       width = 8, height = 5, dpi = 600, bg = "white")


# =============================================================================
# SECTION 4: Cost and Acres Comparison — Suitable vs. High Quality
# =============================================================================

# Side-by-side comparison of total cost and total acres for both solutions
comparison_data <- summary_all %>%
  dplyr::select(quality, total_cost, total_acres, n_selected) %>%
  mutate(quality = factor(quality, levels = c("Suitable", "High Quality")))

# Revenue cost comparison
cost_compare_plot <- ggplot(comparison_data, aes(x = quality, y = total_cost, fill = quality)) +
  geom_col(width = 0.6, col = "black") +
  geom_text(aes(label = paste0("$", format(round(total_cost / 1e6, 1), big.mark = ","), "M")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Suitable" = "darkseagreen", "High Quality" = "darkgreen"),
                    guide = "none") +
  labs(
    title = "A",
    x     = NULL,
    y     = "Foregone revenue ($M)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0)
  )

# Acres comparison
acres_compare_plot <- ggplot(comparison_data, aes(x = quality, y = total_acres, fill = quality)) +
  geom_col(width = 0.6, col = "black") +
  geom_text(aes(label = format(round(total_acres), big.mark = ",")),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = c("Suitable" = "darkseagreen", "High Quality" = "darkgreen"),
                    guide = "none") +
  labs(
    title = "B",
    x     = NULL,
    y     = "Acres retired"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0)
  )

# Combine with patchwork
cost_acres_combined <- cost_compare_plot + acres_compare_plot +
  plot_annotation(
    title    = "Retirement optimization: suitable vs. high quality habitat",
    subtitle = "Meeting 25,000-acre targets for BNLL, GKR & SJKF across all 5 climate scenarios",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 11)
    )
  )

print(cost_acres_combined)

# save the plot
ggsave(file.path(fig_dir, "total_revenue_acreage.png"), cost_acres_combined,
       width = 7, height = 5, dpi = 600, bg = "white")


# =============================================================================
# SECTION 5: Side-by-Side Spatial Maps — Suitable vs. High Quality
# =============================================================================

# Bounding box for consistent extent
field_bbox <- st_bbox(field_data)

# Build maps for both solutions
map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  sol_join <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        # 1. prioritize existing retired status
        retired == 1 ~ "Retired",
        
        # 2. Group Selected: (Cultivated OR Short-term Fallow) AND Selected
        solution_1 == 1 ~ "Cultivated or short-term fallow (selected)",
        
        # 3. Group Not Selected: Everything else (Cultivated OR Short-term Fallow)
        TRUE ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  p <- ggplot() +
    geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", alpha = 0.75, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
    coord_sf(xlim = field_bbox[c(1,3)], ylim = field_bbox[c(2,4)], expand = FALSE,
             datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated or short-term fallow (selected)"      = "forestgreen",
        "Cultivated or short-term fallow (not selected)"  = "wheat1",
        "Retired"                                         = "grey70"
      ),
      labels = c(
        "Cultivated or short-term fallow (selected)"     = "Cultivated or short-term\nfallow (selected)",
        "Cultivated or short-term fallow (not selected)" = "Cultivated or short-term\nfallow (not selected)",
        "Retired"                                        = "Retired"
      ),
      name = "Field Status"
    ) +
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA), 
      legend.key.height = unit(1, "cm")
    )
  
  map_list[[quality_label]] <- p
}

# Combine side-by-side with patchwork
# Use a shared legend
combined_map <- map_list[["Suitable"]] + map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only)",
    subtitle = paste0("minimizing foregone revenue | 25,000 ac target per species × 5 climate periods | BLM = ", chosen_blm),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

# look at the map
print(combined_map)

# save the map
ggsave(file.path(fig_dir, "spatial_maps.png"), combined_map,
       width = 11, height = 6, dpi = 600, bg = "white")


# --- Zoomed maps: Kern, Fresno, Kings, Tulare focus area ---

# Define bounding box in WGS84, then transform to field_data CRS
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
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        # 1. prioritize existing retired status
        retired == 1 ~ "Retired",
        
        # 2. Group Selected: (Cultivated OR Short-term Fallow) AND Selected
        solution_1 == 1 ~ "Cultivated or short-term fallow (selected)",
        
        # 3. Group Not Selected: Everything else (Cultivated OR Short-term Fallow)
        TRUE ~ "Cultivated or short-term fallow (not selected)"
      )
    )
  
  p <- ggplot() +
    geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", alpha = 0.75, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
    coord_sf(xlim = zoom_bbox[c(1,3)], ylim = zoom_bbox[c(2,4)], expand = FALSE,
             datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated or short-term fallow (selected)"      = "forestgreen",
        "Cultivated or short-term fallow (not selected)"  = "wheat1",
        "Retired"                                         = "grey70"
      ),
      labels = c(
        "Cultivated or short-term fallow (selected)"     = "Cultivated or short-term\nfallow (selected)",
        "Cultivated or short-term fallow (not selected)" = "Cultivated or short-term\nfallow (not selected)",
        "Retired"                                        = "Retired"
      ),
      name = "Field Status"
    ) +
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA), 
      legend.key.height = unit(1, "cm")
    )
  
  zoom_map_list[[quality_label]] <- p
}

combined_zoom_map <- zoom_map_list[["Suitable"]] + zoom_map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only)",
    subtitle = "Kern, Fresno, Kings, and Tulare counties",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )
# look at the map
print(combined_zoom_map)

# save the map
ggsave(file.path(fig_dir, "spatial_maps_zoom.png"), combined_zoom_map,
       width = 10, height = 6, dpi = 600, bg = "white")


# =============================================================================
# SECTION 6: BLM Calibration Plot
# =============================================================================

blm_results <- blm_results %>% 
  mutate(total_cost_M = round(total_cost / 1e6, 3))

blm_plot <- ggplot(blm_results, aes(x = total_cost_M, y = boundary)) +
  geom_point(size = 3) +
  geom_line() +
  geom_text(aes(label = blm), vjust = -1, size = 3) +
  labs(
    title    = "BLM Calibration: Cost vs. Boundary Length",
    subtitle = "Calibrated on cross-temporal suitable habitat problem (10 features)",
    x        = "Foregone revenue ($M)",
    y        = "Boundary Length (lower = more cohesive)"
  ) +
  theme_classic()

print(blm_plot)

# save the plot
ggsave(file.path(fig_dir, "blm_calibration.png"), blm_plot,
       width = 7, height = 5.5, dpi = 600, bg = "white")


# =============================================================================
# SECTION 7: Export
# =============================================================================

write_csv(county_summary_all,
          here("data/intermediate/9_1_prioritizr_habitat_only/county_summaries.csv"))
write_csv(target_summary,
          here("data/intermediate/9_1_prioritizr_habitat_only/target_achievement.csv"))


cat("\n========== ALL FIGURES GENERATED ==========\n")




# =============================================================================
# ARCHIVE
# ============================================================================= 

# =============================================================================
# OLD Side-by-Side Spatial Maps — Suitable vs. High Quality
# =============================================================================

# Bounding box for consistent extent
field_bbox <- st_bbox(field_data)

# Build maps for both solutions
map_list <- list()

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  quality_label <- ifelse(grepl("suit", scen), "Suitable", "High Quality")
  
  sol_join <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1                   ~ "Retired",
        solution_1 == 1 & fallow == 1  ~ "Fallow (selected)",
        solution_1 == 1 & fallow == 0  ~ "Cultivated (selected)",
        fallow == 1                    ~ "Fallow (not selected)",
        TRUE                           ~ "Cultivated (not selected)"
      )
    )
  
  p <- ggplot() +
    geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", alpha = 0.75, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
    coord_sf(xlim = field_bbox[c(1,3)], ylim = field_bbox[c(2,4)], expand = FALSE,
             datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated (not selected)" = "grey80",
        "Cultivated (selected)"     = "forestgreen",
        "Fallow (not selected)"     = "khaki",
        "Fallow (selected)"         = "darkgreen",
        "Retired"                   = "grey60"
      ),
      name = "Field Status"
    ) +
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  map_list[[quality_label]] <- p
}

# Combine side-by-side with patchwork
# Use a shared legend
combined_map <- map_list[["Suitable"]] + map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only)",
    subtitle = paste0("minimizing foregone revenue | 25,000 ac target per species × 5 climate periods | BLM = ", chosen_blm),
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(combined_map)


# --- Zoomed maps: Kern, Fresno, Kings, Tulare focus area ---

# Define bounding box in WGS84, then transform to field_data CRS
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
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        retired == 1                   ~ "Retired",
        solution_1 == 1 & fallow == 1  ~ "Fallow (selected)",
        solution_1 == 1 & fallow == 0  ~ "Cultivated (selected)",
        fallow == 1                    ~ "Fallow (not selected)",
        TRUE                           ~ "Cultivated (not selected)"
      )
    )
  
  p <- ggplot() +
    geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", alpha = 0.75, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
    coord_sf(xlim = zoom_bbox[c(1,3)], ylim = zoom_bbox[c(2,4)], expand = FALSE,
             datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated (not selected)" = "grey80",
        "Cultivated (selected)"     = "forestgreen",
        "Fallow (not selected)"     = "khaki",
        "Fallow (selected)"         = "darkgreen",
        "Retired"                   = "grey60"
      ),
      name = "Field Status"
    ) +
    labs(title = quality_label) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  zoom_map_list[[quality_label]] <- p
}

combined_zoom_map <- zoom_map_list[["Suitable"]] + zoom_map_list[["High Quality"]] +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Optimal retirement configuration (habitat only)",
    subtitle = "Kern, Fresno, Kings, and Tulare counties",
    theme    = theme(
      plot.title    = element_text(face = "bold", hjust = 0, size = 14),
      plot.subtitle = element_text(hjust = 0, size = 10)
    )
  )

print(combined_zoom_map)


















