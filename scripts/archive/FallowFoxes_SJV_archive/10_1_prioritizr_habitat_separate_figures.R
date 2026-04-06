
# Clear environment
rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(prioritizr)
library(sf)
library(here)
library(scales)


# Load helper functions
source(here("scripts/FallowFoxes_SJV/0_startup/0_2_functions.R"))


# Print full numbers
options(scipen = 999)


# =============================================================================
# STEP 1: Load Results from 9_1_prioritizr_habitat_separate.R
# =============================================================================

load(here("data/intermediate/FallowFoxes_SJV_archive/9_1_prioritizr_habitat_separate/prioritizr_results.RData"))

# load SJV boundary (counties dissolved)
sjv_boundary <- read_sf(here("data/raw/sjv_counties/sjv_counties.shp"))
sjv_boundary <- st_transform(sjv_boundary, st_crs(field_data))

# load SJV boundary (counties delineated)
options(tigris_use_cache = TRUE)

ca_counties <- tigris::counties(state = "CA")
sjv_county_names <- c("San Joaquin", "Stanislaus", "Merced", "Madera",
                      "Fresno", "Kings", "Tulare", "Kern")
sjv_counties <- ca_counties %>%
  filter(NAME %in% sjv_county_names) %>%
  st_transform(st_crs(field_data))


cat("Loaded prioritizr results:\n")
cat("  Scenarios:", nrow(scenarios), "\n")
cat("  Solutions:", length(solutions), "\n")
cat("  Fallow fields:", nrow(field_data), "\n")
cat("  All fields:", nrow(field_data_all), "\n")
cat("  Habitat target:", format(habitat_target, big.mark = ","), "acres\n")
cat("  Chosen BLM:", chosen_blm, "\n")



# =============================================================================
# STEP 2: Cross-Scenario Comparison
# =============================================================================

cat("\n========== CROSS-SCENARIO COMPARISON ==========\n\n")
print(summary_all, n = Inf, width = Inf)


# --- Cost comparison across scenarios ---
cost_plot <- ggplot(summary_all, aes(x = climate, y = total_cost, fill = quality)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values = c("Suitable" = "#a1d99b", "High Quality" = "#00441b")) +
  labs(
    title = "Total revenue (cost) by scenario",
    x     = "Climate scenario",
    y     = "Total revenue cost ($)",
    fill  = "Habitat Quality"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(cost_plot)


# --- Acres retired comparison ---
acres_plot <- ggplot(summary_all, aes(x = climate, y = total_acres, fill = quality)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Suitable" = "#a1d99b", "High Quality" = "#00441b")) +
  labs(
    title = "Total acres retired by scenario",
    x     = "Climate scenario",
    y     = "Total acres retired",
    fill  = "Habitat Quality"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(acres_plot)


# --- Habitat achieved (should all be >= 25,000) ---
habitat_long <- summary_all %>%
  pivot_longer(cols = c(bnll_achieved, gkr_achieved),
               names_to = "species", values_to = "achieved") %>%
  mutate(species = ifelse(species == "bnll_achieved", "BNLL", "GKR"))

target_plot <- ggplot(habitat_long, aes(x = climate, y = achieved,
                                        fill = species)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = habitat_target, linetype = "dashed", color = "red") +
  facet_wrap(~ quality) +
  labs(
    title    = "Habitat Achieved vs. 25,000-Acre Target",
    x        = "Climate Scenario",
    y        = "Habitat Area (acres)",
    fill     = "Species",
    caption  = "Dashed red line = 25,000-acre target"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(target_plot)



# =============================================================================
# STEP 3: Per-Scenario Detail Summaries
# =============================================================================

# --- County-level summaries for each scenario ---
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
      total_water = sum(water, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scen) %>%
    arrange(desc(total_acres))
}

county_summary_all <- bind_rows(county_summaries)


# --- County-level visualizations ---

# Add scenario labels for faceting
county_plot_data <- county_summary_all %>%
  left_join(scenarios %>% dplyr::select(scenario_name, climate, quality),
            by = c("scenario" = "scenario_name")) %>%
  mutate(
    scenario_label = paste0(climate, "\n(", quality, ")"),
    scenario_label = factor(scenario_label,
                            levels = unique(paste0(
                              rep(c("Baseline", "RCP 4.5 (2020-2049)", "RCP 4.5 (2040-2069)",
                                    "RCP 8.5 (2020-2049)", "RCP 8.5 (2040-2069)"), 2),
                              "\n(",
                              rep(c("Suitable", "High Quality"), each = 5),
                              ")")))
  )

# Acres retired by county
county_acres_plot <- ggplot(county_plot_data,
                            aes(x = reorder(county, total_acres), y = total_acres)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ scenario_label, ncol = 5) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Acres retired by county across scenarios",
    x     = NULL,
    y     = "Total acres retired"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    strip.text    = element_text(size = 7, face = "bold"),
    axis.text.y   = element_text(size = 7),
    axis.text.x   = element_text(size = 6),
    panel.spacing = unit(0.3, "lines")
  )

print(county_acres_plot)

# Revenue cost by county
county_rev_plot <- ggplot(county_plot_data,
                          aes(x = reorder(county, total_rev), y = total_rev)) +
  geom_col(fill = "seagreen3") +
  facet_wrap(~ scenario_label, ncol = 5) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Revenue cost by county across scenarios",
    x     = NULL,
    y     = "Total revenue cost ($M)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    strip.text    = element_text(size = 7, face = "bold"),
    axis.text.y   = element_text(size = 7),
    axis.text.x   = element_text(size = 6),
    panel.spacing = unit(0.3, "lines")
  )

print(county_rev_plot)


# --- Crop-level summaries for each scenario ---
crop_summaries <- list()

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  crop_summaries[[scen]] <- sol %>%
    filter(solution_1 == 1) %>%
    st_drop_geometry() %>%
    group_by(comm) %>%
    summarise(
      n_fields    = n(),
      total_acres = sum(acres, na.rm = TRUE),
      total_rev   = sum(revenue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(scenario = scen) %>%
    arrange(desc(total_acres))
}

crop_summary_all <- bind_rows(crop_summaries)



# =============================================================================
# STEP 4: Selection Frequency Analysis
# =============================================================================

# how often is each field selected across all 10 scenarios?
# fields selected in all 10 scenarios are likely the highest priority for retirement

field_data$selection_freq <- 0

for (scen in names(solutions)) {
  sol <- solutions[[scen]]
  field_data$selection_freq <- field_data$selection_freq + sol$solution_1
}

cat("\n--- Selection Frequency Distribution ---\n")
print(table(field_data$selection_freq))

cat("\nFields selected in ALL 10 scenarios:", sum(field_data$selection_freq == 10), "\n")
cat("Fields selected in 0 scenarios:", sum(field_data$selection_freq == 0), "\n")


# --- Compute frequency separately for suit and hq scenarios ---
suit_scenarios <- scenarios %>% filter(quality == "Suitable") %>% pull(scenario_name)
hq_scenarios   <- scenarios %>% filter(quality == "High Quality") %>% pull(scenario_name)

field_data$freq_suit <- 0
field_data$freq_hq   <- 0

for (scen in suit_scenarios) {
  if (scen %in% names(solutions)) {
    field_data$freq_suit <- field_data$freq_suit + solutions[[scen]]$solution_1
  }
}

for (scen in hq_scenarios) {
  if (scen %in% names(solutions)) {
    field_data$freq_hq <- field_data$freq_hq + solutions[[scen]]$solution_1
  }
}


# --- Color palettes ---

# White for 0, light green → dark green for 1-10
freq_colors_10 <- c("white", colorRampPalette(c("#e5f5e0", "#a1d99b", "#41ab5d", "#006d2c", "#00441b"))(10))
names(freq_colors_10) <- as.character(0:10)

# White for 0, light green → dark green for 1-5
freq_colors_5 <- c("white", colorRampPalette(c("#e5f5e0", "#a1d99b", "#41ab5d", "#006d2c", "#00441b"))(5))
names(freq_colors_5) <- as.character(0:5)

# Convert to factors
field_data$selection_freq_f <- factor(field_data$selection_freq, levels = 0:10)
field_data$freq_suit_f      <- factor(field_data$freq_suit, levels = 0:5)
field_data$freq_hq_f        <- factor(field_data$freq_hq, levels = 0:5)

# Bounding box of fallow fields for consistent extent across plots
field_bbox <- st_bbox(field_data)

# Common theme for frequency maps (with lat/lon grid lines)
freq_map_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey80", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 7, color = "grey30"),
    axis.ticks       = element_line(color = "grey30"),
    panel.background = element_rect(fill = "white", color = NA)
  )


# --- Selection frequency map — all 10 scenarios ---
freq_plot <- ggplot() +
  geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", linewidth = 0.3) +
  geom_sf(data = field_data, aes(fill = selection_freq_f), color = NA) +
  geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
  coord_sf(xlim = field_bbox[c(1,3)], ylim = field_bbox[c(2,4)], expand = FALSE,
           datum = sf::st_crs(4326)) +
  scale_fill_manual(
    values = freq_colors_10,
    name   = "Times selected\n(out of 10)",
    drop   = FALSE
  ) +
  labs(
    title    = "Selection frequency across all 10 scenarios"
  ) +
  freq_map_theme

print(freq_plot)


# --- Selection frequency map — suitable habitat scenarios ---
freq_suit_plot <- ggplot() +
  geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", linewidth = 0.3) +
  geom_sf(data = field_data, aes(fill = freq_suit_f), color = NA) +
  geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
  coord_sf(xlim = field_bbox[c(1,3)], ylim = field_bbox[c(2,4)], expand = FALSE,
           datum = sf::st_crs(4326)) +
  scale_fill_manual(
    values = freq_colors_5,
    name   = "Times selected\n(out of 5)",
    drop   = FALSE
  ) +
  labs(
    title    = "Selection frequency — suitable habitat scenarios",
    subtitle = "Frequency across 5 suitable habitat climate scenarios"
  ) +
  freq_map_theme

print(freq_suit_plot)


# --- Selection frequency map — high quality habitat scenarios ---
freq_hq_plot <- ggplot() +
  geom_sf(data = sjv_counties, fill = "grey90", color = "grey40", linewidth = 0.3) +
  geom_sf(data = field_data, aes(fill = freq_hq_f), color = NA) +
  geom_sf(data = sjv_counties, fill = NA, color = "grey40", linewidth = 0.3) +
  coord_sf(xlim = field_bbox[c(1,3)], ylim = field_bbox[c(2,4)], expand = FALSE,
           datum = sf::st_crs(4326)) +
  scale_fill_manual(
    values = freq_colors_5,
    name   = "Times selected\n(out of 5)",
    drop   = FALSE
  ) +
  labs(
    title    = "Selection frequency — high quality habitat scenarios",
    subtitle = "Frequency across 5 high quality habitat climate scenarios"
  ) +
  freq_map_theme

print(freq_hq_plot)



# =============================================================================
# STEP 5: Spatial Maps for Each Scenario
# =============================================================================

# join solutions back to all fields for complete maps

for (scen in names(solutions)) {
  
  sol <- solutions[[scen]]
  
  sol_join <- field_data_all %>%
    dplyr::select(-any_of("solution_1")) %>%
    left_join(
      sol %>% st_drop_geometry() %>% dplyr::select(id, solution_1),
      by = "id"
    ) %>%
    mutate(
      solution_1 = replace_na(solution_1, 0),
      status = case_when(
        solution_1 == 1 & retired == 1 ~ "Retired (selected)",
        solution_1 == 1 & retired == 0 ~ "Newly fallowed (selected)",
        fallow == 1                     ~ "Fallow (not selected)",
        TRUE                            ~ "Cultivated"
      )
    )
  
  scen_info <- scenarios %>% filter(scenario_name == scen)
  
  p <- ggplot() +
    geom_sf(data = sol_join, aes(fill = status), color = NA, linewidth = 0) +
    scale_fill_manual(
      values = c(
        "Cultivated"                = "grey85",
        "Fallow (not selected)"     = "khaki",
        "Retired (selected)"        = "darkgreen",
        "Newly fallowed (selected)" = "forestgreen"
      ),
      name = "Field Status"
    ) +
    labs(
      title    = paste0("Optimal retirement — ", scen_info$climate,
                        " (", scen_info$quality, ")"),
      subtitle = "Minimizing revenue cost | 25,000 ac target per species",
      caption  = paste("BLM =", chosen_blm)
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  print(p)
}



































