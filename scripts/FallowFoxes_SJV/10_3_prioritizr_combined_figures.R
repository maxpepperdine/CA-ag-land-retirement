# =============================================================================
# PRIORITIZR COMBINED OPTIMIZATION — RESULTS VISUALIZATIONS
# =============================================================================
# Purpose: Generate figures from the combined water + habitat prioritizr
#          results produced by 9_3_prioritizr_combined.R.
#
# FIGURES:
#   1. Scenario comparison grid: Acres, Revenue, Water Savings across 6 scenarios
#   2. Target achievement heatmap: 16 features × 6 scenarios
#   3. Spatial maps: Baseline+Suitable vs Baseline+HQ (full extent + zoom)
#   4. Selection frequency: fields selected across water scenarios
#   5. Cost premium of dual objectives: combined vs. water-only vs. habitat-only
#
# INPUTS:
#   - prioritizr_combined_results.RData (from 9_3_prioritizr_combined.R)
#   - prioritizr_cross_temporal_results.RData (from 9_1, for cost premium fig)
#   - valley_wide_summary.csv (from 9_2, for cost premium fig)
#
# OUTPUTS:
#   - Individual figure files (PNG) in output directory
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


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)


# Output directory
fig_dir <- here("data/intermediate/10_3_prioritizr_combined_figures/")


# =============================================================================
# SECTION 1: Load Results
# =============================================================================

# --- Combined results ---
load(here("data/intermediate/9_3_prioritizr_combined/prioritizr_combined_results.RData"))

# --- Habitat-only results (for cost premium figure) ---
habitat_results <- tryCatch({
  env <- new.env()
  load(here("data/intermediate/9_1_prioritizr_habitat_only/prioritizr_cross_temporal_results.RData"),
       envir = env)
  env
}, error = function(e) {
  cat("  NOTE: Habitat-only results not found — Figure 5 will skip habitat comparison.\n")
  NULL
})

# --- Water-only results (for cost premium figure) ---
water_summary <- tryCatch({
  read_csv(here("data/intermediate/9_2_prioritizr_water_only/valley_wide_summary.csv"),
           show_col_types = FALSE)
}, error = function(e) {
  cat("  NOTE: Water-only summary not found — Figure 5 will skip water comparison.\n")
  NULL
})


# --- Basin boundaries (for spatial maps) ---
basins_raw <- st_read(here("data/raw/i08_B118_CA_GroundwaterBasins/i08_B118_CA_GroundwaterBasins.shp"),
                      quiet = TRUE)
sjv_basins <- basins_raw %>%
  filter(grepl("^SAN JOAQUIN VALLEY", Basin_Su_1)) %>%
  filter(!Basin_Su_1 %in% c("SAN JOAQUIN VALLEY - EAST CONTRA COSTA",
                            "SAN JOAQUIN VALLEY - KETTLEMAN PLAIN",
                            "SAN JOAQUIN VALLEY - COSUMNES",
                            "SAN JOAQUIN VALLEY - PLEASANT VALLEY")) %>%
  st_transform(st_crs(field_data))


# --- Diagnostics ---
cat("Loaded combined prioritizr results:\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Scenarios:", paste(names(solutions), collapse = ", "), "\n")
cat("  Planning units:", nrow(field_data), "\n")
cat("  All fields:", nrow(field_data_all), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Water targets (TAF):", round(water_target_baseline_taf, 1),
    "/", round(water_target_rcp45_taf, 1),
    "/", round(water_target_rcp85_taf, 1), "\n")
cat("  Chosen BLM:", chosen_blm, "\n")


# --- Color palettes ---

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


# =============================================================================
# FIGURE 1: Scenario Comparison Grid
# =============================================================================
# Grouped bar chart: 3 panels (Acres, Revenue, Water Savings)
# Bars grouped by water scenario, colored by habitat quality.

cat("\nCreating Figure 1: Scenario comparison grid...\n")

fig1_data <- summary_all %>%
  mutate(
    water_scenario = factor(water_scenario,
                            levels = c("Baseline", "RCP45", "RCP85"),
                            labels = c("Baseline", "RCP 4.5\n(2020-2049)", "RCP 8.5\n(2020-2049)")),
    quality = factor(quality, levels = c("Suitable", "High Quality"))
  )

# Panel A: Acres retired
fig1a <- ggplot(fig1_data, aes(x = water_scenario, y = total_acres, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = format(round(total_acres), big.mark = ","),
                x = as.numeric(water_scenario) + ifelse(quality == "Suitable", -0.22, 0.22)),
            vjust = -0.5, size = 3, fontface = "bold") +
  labs(title = "Acres retired", x = NULL, y = "Acres") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Panel B: Foregone revenue
fig1b <- ggplot(fig1_data, aes(x = water_scenario, y = total_cost / 1e6, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("$", round(total_cost / 1e6), "M"),
                x = as.numeric(water_scenario) + ifelse(quality == "Suitable", -0.22, 0.22)),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Foregone revenue", x = NULL, y = "$ millions") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# Panel C: Water savings achieved
fig1c <- ggplot(fig1_data, aes(x = water_scenario, y = total_Snet_AF / 1000, fill = quality)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0(round(total_Snet_AF / 1000), " TAF"),
                x = as.numeric(water_scenario) + ifelse(quality == "Suitable", -0.22, 0.22)),
            vjust = -0.5, size = 3, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Water savings achieved", x = NULL, y = "TAF/yr") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# combine
fig1 <- fig1a + fig1b + fig1c +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Combined optimization results across climate scenarios",
    subtitle = paste0("Meeting 25,000-acre habitat targets (BNLL, GKR, SJKF x 5 climate periods) + ",
                      "valley-wide water savings targets | BLM = ", chosen_blm),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  ) &
  theme(legend.position = "bottom") &
  scale_fill_manual(values = quality_colors, name = "Habitat Quality")
fig1


ggsave(file.path(fig_dir, "fig1_scenario_comparison.png"), fig1,
       width = 12, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig1_scenario_comparison.png\n")


# =============================================================================
# FIGURE 2: Target Achievement Heatmap
# =============================================================================
# Tile plot: features (rows) × scenarios (columns), colored by % of target.

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
# Extract unique features in a logical order
habitat_feature_order <- c(
  "bnll_base_suit", "bnll_rcp45_2049_suit", "bnll_rcp45_2069_suit",
  "bnll_rcp85_2049_suit", "bnll_rcp85_2069_suit",
  "gkr_base_suit", "gkr_rcp45_2049_suit", "gkr_rcp45_2069_suit",
  "gkr_rcp85_2049_suit", "gkr_rcp85_2069_suit",
  "sjkf_base_suit", "sjkf_rcp45_2049_suit", "sjkf_rcp45_2069_suit",
  "sjkf_rcp85_2049_suit", "sjkf_rcp85_2069_suit"
)

# For the heatmap, use the _suit features as labels (shared across quality levels)
# since the habitat feature names differ only by _suit/_hq suffix
feature_label_order <- c(
  "Water: Baseline", "Water: RCP 4.5", "Water: RCP 8.5",
  rev(habitat_feature_order)  # reverse so BNLL is at top after coord_flip
)

# For hq scenarios, map hq feature names to suit equivalents for consistent ordering
heatmap_data <- heatmap_data %>%
  mutate(
    feature_label = case_when(
      feature_type == "water" ~ feature_label,
      TRUE ~ gsub("_hq$", "_suit", feature)  # normalize to _suit for ordering
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
    title = "Target achievement across all combined scenarios",
    subtitle = "Each cell shows % of target achieved (100% = target exactly met)",
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
  # Add horizontal lines to separate water vs habitat and species groups
  geom_hline(yintercept = 3.5, linewidth = 1, color = "gray30")

print(fig2)

ggsave(file.path(fig_dir, "fig2_target_heatmap.png"), fig2,
       width = 10, height = 9, dpi = 600, bg = "white")
cat("  Saved: fig2_target_heatmap.png\n")


# =============================================================================
# FIGURE 3: Spatial Maps — High Quality across 3 Water Scenarios
# =============================================================================
# Three maps showing Baseline, RCP 4.5, and RCP 8.5 combined solutions
# (all using High Quality habitat targets).

cat("\nCreating Figure 3: Spatial maps (HQ across water scenarios)...\n")

# Expand bounding box slightly so basins aren't clipped at edges
field_bbox <- st_bbox(field_data)
x_buffer <- (field_bbox["xmax"] - field_bbox["xmin"]) * 0.03
y_buffer <- (field_bbox["ymax"] - field_bbox["ymin"]) * 0.03
plot_bbox <- c(
  xmin = field_bbox["xmin"] - x_buffer,
  ymin = field_bbox["ymin"] - y_buffer,
  xmax = field_bbox["xmax"] + x_buffer,
  ymax = field_bbox["ymax"] + y_buffer
)

# --- Scenario definitions ---
fig3_scenarios <- tibble(
  scen  = c("combined_baseline_hq", "combined_rcp45_hq", "combined_rcp85_hq"),
  title = c("Baseline (1,849 TAF)",
            "RCP 4.5 (1,966 TAF)",
            "RCP 8.5 (2,049 TAF)")
)

# --- Helper function ---
build_scenario_map <- function(scen, title_text) {
  
  sol <- solutions[[scen]]
  
  sol_join <- field_data_all %>%
    filter(!is.na(basin)) %>%   # <-- add this line
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
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
    geom_sf(data = sjv_basins, fill = "grey90", color = "grey40",
            alpha = 0.75, linewidth = 0.3) +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    geom_sf(data = sjv_basins, fill = NA, color = "grey40", linewidth = 0.3) +
    coord_sf(xlim = plot_bbox[c("xmin", "xmax")],
             ylim = plot_bbox[c("ymin", "ymax")],
             expand = FALSE, datum = sf::st_crs(4326)) +
    scale_fill_manual(
      values = c(
        "Cultivated or short-term fallow (selected)"     = "forestgreen",
        "Cultivated or short-term fallow (not selected)" = "wheat1",
        "Retired"                                         = "grey70"
      ),
      labels = c(
        "Cultivated or short-term fallow (selected)"     = "Cultivated or short-term\nfallow (selected)",
        "Cultivated or short-term fallow (not selected)" = "Cultivated or short-term\nfallow (not selected)",
        "Retired"                                         = "Retired"
      ),
      name = "Field Status"
    ) +
    labs(title = title_text) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 11),
      panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(size = 7, color = "grey30"),
      axis.ticks       = element_line(color = "grey30"),
      panel.background = element_rect(fill = "white", color = NA),
      legend.key.height = unit(1, "cm")
    )
}

# --- Build 3 maps ---
map_base  <- build_scenario_map("combined_baseline_hq", fig3_scenarios$title[1])
map_rcp45 <- build_scenario_map("combined_rcp45_hq",    fig3_scenarios$title[2])
map_rcp85 <- build_scenario_map("combined_rcp85_hq",    fig3_scenarios$title[3])

fig3 <- map_base + map_rcp45 + map_rcp85 +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Optimal retirement configuration across water scenarios (combined, high quality habitat)",
    subtitle = paste0("Minimizing foregone revenue | 25,000 ac habitat target x 15 features + ",
                      "scenario-specific water targets | BLM = ", chosen_blm),
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
# FIGURE 4: Selection Frequency Across Water Scenarios
# =============================================================================
# For each habitat quality level, how many of the 3 water scenarios select
# each field? Fields selected in all 3 are "no-regret" retirements.

cat("\nCreating Figure 4: Selection frequency maps...\n")

# --- Build selection frequency for Suitable ---
suit_scenarios <- c("combined_baseline_suit", "combined_rcp45_suit", "combined_rcp85_suit")

freq_suit <- field_data %>%
  dplyr::select(id) %>%
  mutate(n_selected = 0L)

for (scen in suit_scenarios) {
  if (scen %in% names(solutions)) {
    sel_ids <- solutions[[scen]] %>%
      filter(solution_1 == 1) %>%
      st_drop_geometry() %>%
      pull(id)
    freq_suit <- freq_suit %>%
      mutate(n_selected = n_selected + as.integer(id %in% sel_ids))
  }
}

freq_suit <- freq_suit %>%
  mutate(n_selected_fct = factor(n_selected, levels = c("0", "1", "2", "3")))

# --- Build selection frequency for High Quality ---
hq_scenarios <- c("combined_baseline_hq", "combined_rcp45_hq", "combined_rcp85_hq")

freq_hq <- field_data %>%
  dplyr::select(id) %>%
  mutate(n_selected = 0L)

for (scen in hq_scenarios) {
  if (scen %in% names(solutions)) {
    sel_ids <- solutions[[scen]] %>%
      filter(solution_1 == 1) %>%
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

# --- Helper function for frequency maps ---
build_freq_map <- function(freq_data, title_text) {
  ggplot() +
    geom_sf(data = sjv_basins, fill = NA, color = "gray60", linewidth = 0.4) +
    geom_sf(data = freq_data, aes(fill = n_selected_fct),
            color = NA, linewidth = 0) +
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
    title = "Selection frequency across water scenarios (combined optimization)",
    subtitle = "Fields selected in all 3 water scenarios are 'no-regret' retirements meeting both habitat and water goals",
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
# Compare combined solutions against individual water-only and habitat-only
# solutions to quantify the "cost of co-optimization."

cat("\nCreating Figure 5: Cost premium of dual objectives...\n")

# --- Build comparison table ---
comparison_rows <- list()

# Combined results (6 scenarios)
for (i in 1:nrow(summary_all)) {
  row <- summary_all[i, ]
  comparison_rows[[length(comparison_rows) + 1]] <- tibble(
    analysis     = "Combined",
    water_label  = row$water_scenario,
    quality      = row$quality,
    total_cost   = row$total_cost,
    total_acres  = row$total_acres,
    total_Snet_AF = row$total_Snet_AF
  )
}

# Habitat-only results (2 scenarios)
if (!is.null(habitat_results)) {
  hab_summary <- habitat_results$summary_all
  for (i in 1:nrow(hab_summary)) {
    row <- hab_summary[i, ]
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      analysis     = "Habitat only",
      water_label  = "N/A",
      quality      = row$quality,
      total_cost   = row$total_cost,
      total_acres  = row$total_acres,
      total_Snet_AF = NA_real_
    )
  }
}

# Water-only results (3 scenarios)
if (!is.null(water_summary)) {
  for (i in 1:nrow(water_summary)) {
    row <- water_summary[i, ]
    water_lab <- case_when(
      grepl("Baseline", row$scenario) ~ "Baseline",
      grepl("RCP45", row$scenario)    ~ "RCP45",
      grepl("RCP85", row$scenario)    ~ "RCP85"
    )
    comparison_rows[[length(comparison_rows) + 1]] <- tibble(
      analysis     = "Water only",
      water_label  = water_lab,
      quality      = "N/A",
      total_cost   = row$revenue_cost_usd,
      total_acres  = row$acres_selected,
      total_Snet_AF = row$snet_achieved_af
    )
  }
}

comparison <- bind_rows(comparison_rows)

cat("\n--- Full Comparison Table ---\n")
print(comparison, width = Inf)

write_csv(comparison, file.path(fig_dir, "fig5_cost_comparison_data.csv"))


# --- Build figure: Focus on Baseline scenario comparison ---
# Compare: Water-only Baseline, Habitat-only (Suit & HQ), Combined Baseline (Suit & HQ)
fig5_data <- comparison %>%
  filter(
    (analysis == "Water only" & water_label == "Baseline") |
      (analysis == "Habitat only") |
      (analysis == "Combined" & water_label == "Baseline")
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
    )
  )

fill_values <- setNames(fig5_data$bar_fill, fig5_data$bar_label)

# Revenue comparison
fig5a <- ggplot(fig5_data, aes(x = bar_label, y = total_cost / 1e6, fill = bar_label)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("$", format(round(total_cost / 1e6), big.mark = ","), "M")),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = fill_values, guide = "none") +
  labs(title = "A. Foregone revenue", x = NULL, y = "$ millions") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8)
  )

# Acres comparison
fig5b <- ggplot(fig5_data, aes(x = bar_label, y = total_acres, fill = bar_label)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3) +
  geom_text(aes(label = format(round(total_acres), big.mark = ",")),
            vjust = -0.5, size = 3.2, fontface = "bold") +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = fill_values, guide = "none") +
  labs(title = "B. Acres retired", x = NULL, y = "Acres") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8)
  )

fig5 <- fig5a + fig5b +
  plot_annotation(
    title = "Cost of co-optimization: combined vs. individual objective solutions (Baseline scenario)",
    subtitle = "Comparing water-only, habitat-only, and combined optimization costs and land area requirements",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "gray40", size = 10)
    )
  )

print(fig5)

ggsave(file.path(fig_dir, "fig5_cost_premium.png"), fig5,
       width = 12, height = 6, dpi = 600, bg = "white")
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

fig5c <- ggplot(fig5_full_data, aes(x = water_label_clean, y = total_cost / 1e6,
                                    fill = bar_group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("$", format(round(total_cost / 1e6), big.mark = ","), "M")),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, fontface = "bold") +
  scale_y_continuous(labels = dollar_format(suffix = "M"), expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = premium_colors, name = "Analysis") +
  labs(
    title = "Cost premium of adding habitat constraints across all water scenarios",
    subtitle = "Combined optimization costs vs. water-only baseline for each climate scenario",
    x = "Water savings scenario",
    y = "Foregone revenue ($ millions)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(fig5c)

ggsave(file.path(fig_dir, "fig5b_cost_premium_all_scenarios.png"), fig5c,
       width = 10, height = 6, dpi = 600, bg = "white")
cat("  Saved: fig5b_cost_premium_all_scenarios.png\n")


# =============================================================================
# SUMMARY TABLE: Overall Comparison (kableExtra)
# =============================================================================

cat("\nCreating summary table...\n")

overall_table <- summary_all %>%
  mutate(
    `Water Scenario` = water_scenario,
    Quality = quality,
    `Fields Selected` = format(n_selected, big.mark = ","),
    `Acres Retired` = format(round(total_acres), big.mark = ","),
    `Revenue Cost` = paste0("$", format(round(total_cost / 1e6, 1), big.mark = ","), "M"),
    `Water Saved (TAF)` = round(total_Snet_AF / 1000, 1),
    `Boundary` = format(round(boundary), big.mark = ",")
  ) %>%
  dplyr::select(`Water Scenario`, Quality, `Fields Selected`, `Acres Retired`,
                `Revenue Cost`, `Water Saved (TAF)`, Boundary)

kbl_overall <- overall_table %>%
  kbl(
    caption = "Combined optimization results: water savings + habitat creation",
    align = c("l", "l", rep("r", 5))
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
                     "across 5 climate periods (15 features) plus valley-wide water savings targets. ",
                     "BLM = ", chosen_blm, "."),
    general_title = "Note: "
  )

save_kable(kbl_overall, file.path(fig_dir, "table_combined_summary.html"))
cat("  Saved: table_combined_summary.html\n")

write_csv(overall_table, file.path(fig_dir, "table_combined_summary.csv"))
cat("  Saved: table_combined_summary.csv\n")


cat("\n========== ALL FIGURES GENERATED ==========\n")
cat("Output directory:", fig_dir, "\n")

















