# clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(here)

################################################################################

# load the USDA NASS data for 2018-2021
usda_2018 <- read_csv(here("data/raw/USDA_NASS/2018cactbsErrata.csv"))
usda_2019 <- read_csv(here("data/raw/USDA_NASS/201908cropyear.csv"))
usda_2020 <- read_csv(here("data/raw/USDA_NASS/2020_main_data_table.csv"))
usda_2021 <- read_csv(here("data/raw/USDA_NASS/CAC_2021_data_by_commodity_20240417.csv"))

################################################################################

# the USDA crop name and commodity codes changed after 2020
# attach the old names and codes to the new ones

#### USDA 2021 ####
usda_2021_clean <- usda_2021 %>% 
  clean_names() %>%
  select(year, current_item_name, current_item_code, legacy_item_name, legacy_commodity_code, 
         county, harvested_acres, production, unit, value, price_per_unit) %>% 
  rename(crop = current_item_name) %>% 
  # add the legacy commodity code for berries, strawberries, all
  mutate(legacy_commodity_code = if_else(crop == "Berries, Strawberries, All", 
                                         237999, 
                                         legacy_commodity_code))

# pull out the legacy item names and codes to use for merging with 2020 data
usda_2021_matching <- usda_2021_clean %>%
  distinct(legacy_item_name, legacy_commodity_code, .keep_all = TRUE) %>%
  select(legacy_item_name, legacy_commodity_code, crop, current_item_code) %>% 
  # drop row 34 (duplicate berries observation)
  slice(-34)

# filter to only the crops we have 2021 LandIQ field data for in Kern, but not in USDA NASS
usda_2021_filter <- usda_2021_clean %>%
  filter(legacy_item_name %in% c("Dates", "Avocados, All", "Olives", "Apples, All", 
                                 "Pomegranates", "Pecans", "Apricots, All", 
                                 "Peaches, Clingstone", "Plums", "Corn, Silage", 
                                 "Safflower", "Ryegrass, Perennial, All", 
                                 "Lettuce, Head", "Brussels Sprouts", "Carrots, Unspecified", 
                                 "Cucumbers", "Ryegrass, Perennial, All") 
         | crop == "Berries, Strawberries, All")

# for all observations where crop == "Berries, Strawberries, All", assign the "legacy_item_name" values as "Berries, Strawberries, Fresh Market"
usda_2021_filter <- usda_2021_filter %>%
  mutate(legacy_item_name = if_else(crop == "Berries, Strawberries, All", 
                                    "Berries, Strawberries, Fresh Market", 
                                    legacy_item_name))

# QC: count number of distinct obsv in legacy item name (should be 17)
n_distinct(usda_2021_filter$legacy_item_name)

# replace all NAs in harvested_acres, production, value with 0
usda_2021_filter <- usda_2021_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))

# subtract "Sum of Others" values from "State Total" values
# this leaves only State Total values for the counties recorded for each crop

# compute "Sum of Others" totals per crop
sum_others_2021 <- usda_2021_filter %>%
  filter(county == "Sum of Others") %>%
  group_by(legacy_item_name) %>%
  summarise(
    sum_others_acres = sum(harvested_acres, na.rm = TRUE),
    sum_others_prod  = sum(production, na.rm = TRUE),
    sum_others_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# join back to full dataset and subtract from State Total rows
usda_2021_adjusted <- usda_2021_filter %>%
  left_join(sum_others_2021, by = "legacy_item_name") %>%
  mutate(
    harvested_acres = if_else(county == "State Total",
                              harvested_acres - coalesce(sum_others_acres, 0),
                              harvested_acres),
    production = if_else(county == "State Total",
                         production - coalesce(sum_others_prod, 0),
                         production),
    value = if_else(county == "State Total",
                    value - coalesce(sum_others_value, 0),
                    value)
  ) %>%
  select(-starts_with("sum_others_"))


# now that we subtracted the "Sum of Others" values, we can drop those rows
usda_2021_estimate <- usda_2021_adjusted %>%
  filter(county != "Sum of Others")


### calculate the average county-level values per crop for 2019 ###

# (1) count how many counties reported data for each crop
county_counts_2021 <- usda_2021_estimate %>%
  filter(!county %in% c("State Total")) %>%  # exclude state totals
  group_by(legacy_item_name) %>%
  summarise(
    counties_reporting = n(),  # count all county records (even if 0)
    .groups = "drop"
  )

# (2) extract adjusted State Total values
state_totals_2021 <- usda_2021_estimate %>%
  filter(county == "State Total") %>%
  select(crop, 
         legacy_item_name, 
         state_harvested_acres = harvested_acres, 
         state_production = production, 
         state_value = value, 
         price_per_unit_stateTotal = price_per_unit, 
         unit)

# (3) join county counts to state totals and calculate averages
crop_averages_2021 <- state_totals_2021 %>%
  left_join(county_counts_2021, by = "legacy_item_name") %>%
  mutate(
    avg_harvested_acres = if_else(counties_reporting > 0, 
                                  state_harvested_acres / counties_reporting, 
                                  0),
    avg_production = if_else(counties_reporting > 0, 
                             state_production / counties_reporting, 
                             0),
    avg_value = if_else(counties_reporting > 0, 
                        state_value / counties_reporting, 
                        0)
  )

# (4) keep clean summary table
usda_2021_averages <- crop_averages_2021 %>%
  select(crop, 
         legacy_item_name, 
         avg_harvested_acres, 
         avg_production, 
         avg_value, 
         price_per_unit_stateTotal)

  
## calculations
# value_1 = production * value (value is $)
# prod_per_acre = production / harvested_acres
# price_per_acre = prod_per_acre * price_per_unit (price_per_unit is $ / unit)


################################################################################


### USDA 2020 ####

# join the new crop and item codes to the 2020 data 
usda_2020_clean <- usda_2020 %>% 
  clean_names() %>% 
  left_join(usda_2021_matching,
            by = c("commodity_code" = "legacy_commodity_code")) %>% 
  select(crop, year, current_item_code, county, harvested_acres, production, 
         unit, value = value_dollars, legacy_item_name, 
         price_per_unit = price_dollars_unit)


# filter to only the crops we need to estimate data for 
# these are the crops we have 2021 LandIQ field data for in Kern, but not in USDA NASS
usda_2020_filter <- usda_2020_clean %>%
  filter(legacy_item_name %in% c("Dates", "Avocados, All", "Olives", "Apples, All", 
                                 "Pomegranates", "Pecans", "Apricots, All", 
                                 "Peaches, Clingstone", "Plums", 
                                 "Corn, Silage", "Safflower", "Ryegrass, Perennial, All", 
                                 "Lettuce, Head", "Brussels Sprouts", "Carrots, Unspecified", 
                                 "Cucumbers", "Ryegrass, Perennial, All") 
         | crop == "Berries, Strawberries, All")

# for all observations where crop == "Berries, Strawberries, All", assign the "legacy_item_name" values as "Berries, Strawberries, Fresh Market"
usda_2020_filter <- usda_2020_filter %>%
  mutate(legacy_item_name = if_else(crop == "Berries, Strawberries, All", 
                                    "Berries, Strawberries, Fresh Market", 
                                    legacy_item_name))

# QC: count number of distinct obsv in legacy item name (should be 17)
n_distinct(usda_2020_filter$legacy_item_name)

# replace all NAs in harvested_acres, production, value with 0
usda_2020_filter <- usda_2020_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))


# there are no "Sum of Others" rows in the 2020 data, so we can skip that step


### calculate the average county-level values per crop for 2020 ###


# (1) count how many counties reported data for each crop
county_counts_2020 <- usda_2020_filter %>%
  filter(!county %in% c("State Totals")) %>%  # exclude state totals
  group_by(legacy_item_name) %>%
  summarise(
    counties_reporting = n(),  # count all county records (even if 0)
    .groups = "drop"
  )

# (2) extract adjusted State Total values
state_totals_2020 <- usda_2020_filter %>%
  filter(county == "State Totals") %>%
  select(crop, 
         legacy_item_name, 
         state_harvested_acres = harvested_acres, 
         state_production = production, 
         state_value = value, 
         price_per_unit_stateTotal = price_per_unit)

# (3) join county counts to state totals and calculate averages
crop_averages_2020 <- state_totals_2020 %>%
  left_join(county_counts_2020, by = "legacy_item_name") %>%
  mutate(
    avg_harvested_acres = if_else(counties_reporting > 0, 
                                  state_harvested_acres / counties_reporting, 
                                  0),
    avg_production = if_else(counties_reporting > 0, 
                             state_production / counties_reporting, 
                             0),
    avg_value = if_else(counties_reporting > 0, 
                        state_value / counties_reporting, 
                        0)
  )

# (4) keep clean summary table
usda_2020_averages <- crop_averages_2020 %>%
  select(crop, 
         legacy_item_name, 
         avg_harvested_acres, 
         avg_production, 
         avg_value, 
         price_per_unit_stateTotal)


################################################################################


### USDA 2019 ####

# join the new crop and item codes to the 2019 data
usda_2019_clean <- usda_2019 %>% 
  clean_names() %>% 
  left_join(usda_2021_matching,
            by = c("commodity_code" = "legacy_commodity_code")) %>% 
  select(crop, year, current_item_code, county, harvested_acres, production, 
         unit, value, legacy_item_name, price_per_unit = price_p_u)

# filter to only the crops we need to estimate data for
usda_2019_filter <- usda_2019_clean %>%
  filter(legacy_item_name %in% c("Dates", "Avocados, All", "Olives", "Apples, All", 
                                 "Pomegranates", "Pecans", "Apricots, All", 
                                 "Peaches, Clingstone", "Plums", 
                                 "Corn, Silage", "Safflower", "Ryegrass, Perennial, All", 
                                 "Berries, Strawberries, Fresh Market", "Lettuce, Head", 
                                 "Brussels Sprouts", "Carrots, Unspecified", 
                                 "Cucumbers", "Ryegrass, Perennial, All"))

# QC: count number of distinct obsv in legacy item name (should be 17)
n_distinct(usda_2019_filter$legacy_item_name)

# replace all NAs in harvested_acres, production, value with 0
usda_2019_filter <- usda_2019_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))

# subtract "Sum of Others" values from "State Total" values
# this leaves only State Total values for the counties recorded for each crop

# compute "Sum of Others" totals per crop
sum_others <- usda_2019_filter %>%
  filter(county == "Sum of Others") %>%
  group_by(legacy_item_name) %>%
  summarise(
    sum_others_acres = sum(harvested_acres, na.rm = TRUE),
    sum_others_prod  = sum(production, na.rm = TRUE),
    sum_others_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# join back to full dataset and subtract from State Total rows
usda_2019_adjusted <- usda_2019_filter %>%
  left_join(sum_others, by = "legacy_item_name") %>%
  mutate(
    harvested_acres = if_else(county == "State Totals",
                              harvested_acres - coalesce(sum_others_acres, 0),
                              harvested_acres),
    production = if_else(county == "State Totals",
                              production - coalesce(sum_others_prod, 0),
                              production),
    value = if_else(county == "State Totals",
                              value - coalesce(sum_others_value, 0),
                              value)
  ) %>%
  select(-starts_with("sum_others_"))


# now that we subtracted the "Sum of Others" values, we can drop those rows
usda_2019_estimate <- usda_2019_adjusted %>%
  filter(county != "Sum of Others") %>% 
  select(-unit)


### calculate the average county-level values per crop for 2019 ###

# (1) count how many counties reported data for each crop
county_counts_2019 <- usda_2019_estimate %>%
  filter(!county %in% c("State Totals")) %>%  # exclude state totals
  group_by(legacy_item_name) %>%
  summarise(
    counties_reporting = n(),  # count all county records (even if 0)
    .groups = "drop"
  )

# (2) extract adjusted State Total values
state_totals_2019 <- usda_2019_estimate %>%
  filter(county == "State Totals") %>%
  select(crop, 
         legacy_item_name, 
         state_harvested_acres = harvested_acres, 
         state_production = production, 
         state_value = value, 
         price_per_unit_stateTotal = price_per_unit)

# (3) join county counts to state totals and calculate averages
crop_averages_2019 <- state_totals_2019 %>%
  left_join(county_counts_2019, by = "legacy_item_name") %>%
  mutate(
    avg_harvested_acres = if_else(counties_reporting > 0, 
                                  state_harvested_acres / counties_reporting, 
                                  0),
    avg_production = if_else(counties_reporting > 0, 
                             state_production / counties_reporting, 
                             0),
    avg_value = if_else(counties_reporting > 0, 
                        state_value / counties_reporting, 
                        0)
  )

# (4) keep clean summary table
usda_2019_averages <- crop_averages_2019 %>%
  select(crop, 
         legacy_item_name, 
         avg_harvested_acres, 
         avg_production, 
         avg_value, 
         price_per_unit_stateTotal)


################################################################################


### USDA 2018 ####

# join the new crop and item codes to the 2018 data
usda_2018_clean <- usda_2018 %>% 
  clean_names() %>% 
  left_join(usda_2021_matching,
            by = c("commodity_code" = "legacy_commodity_code")) %>% 
  select(crop, year, current_item_code, county, harvested_acres, production, 
         unit, value, legacy_item_name, price_per_unit = price_p_u)

# filter to only the crops we need to estimate data for
usda_2018_filter <- usda_2018_clean %>%
  filter(legacy_item_name %in% c("Dates", "Avocados, All", "Olives", "Apples, All", 
                                 "Pomegranates", "Pecans", "Apricots, All", 
                                 "Peaches, Clingstone", "Plums", 
                                 "Corn, Silage", "Safflower", "Ryegrass, Perennial, All", 
                                 "Berries, Strawberries, Fresh Market", "Lettuce, Head", 
                                 "Brussels Sprouts", "Carrots, Unspecified", 
                                 "Cucumbers"))

# QC: count number of distinct obsv in legacy item name (should be 17)
n_distinct(usda_2018_filter$legacy_item_name)

# replace all NAs in harvested_acres, production, value with 0
usda_2018_filter <- usda_2018_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))

# subtract "Sum of Others" values from "State Totals" values
# this leaves only State Totals values for the counties recorded for each crop

# compute "Sum of Others" totals per crop
sum_others_2018 <- usda_2018_filter %>%
  filter(county == "Sum of Others") %>%
  group_by(legacy_item_name) %>%
  summarise(
    sum_others_acres = sum(harvested_acres, na.rm = TRUE),
    sum_others_prod  = sum(production, na.rm = TRUE),
    sum_others_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# join back to full dataset and subtract from State Totals rows
usda_2018_adjusted <- usda_2018_filter %>%
  left_join(sum_others_2018, by = "legacy_item_name") %>%
  mutate(
    harvested_acres = if_else(county == "State Totals",
                              harvested_acres - coalesce(sum_others_acres, 0),
                              harvested_acres),
    production = if_else(county == "State Totals",
                         production - coalesce(sum_others_prod, 0),
                         production),
    value = if_else(county == "State Totals",
                    value - coalesce(sum_others_value, 0),
                    value)
  ) %>%
  select(-starts_with("sum_others_"))


# now that we subtracted the "Sum of Others" values, we can drop those rows
usda_2018_estimate <- usda_2018_adjusted %>%
  filter(county != "Sum of Others") %>% 
  select(-unit)


### calculate the average county-level values per crop for 2018 ###

# (1) count how many counties reported data for each crop
county_counts_2018 <- usda_2018_estimate %>%
  filter(!county %in% c("State Totals")) %>%  # exclude state totals
  group_by(legacy_item_name) %>%
  summarise(
    counties_reporting = n(),  # count all county records (even if 0)
    .groups = "drop"
  )

# (2) extract adjusted State Total values
state_totals_2018 <- usda_2018_estimate %>%
  filter(county == "State Totals") %>%
  select(crop, 
         legacy_item_name, 
         state_harvested_acres = harvested_acres, 
         state_production = production, 
         state_value = value, 
         price_per_unit_stateTotal = price_per_unit)

# (3) join county counts to state totals and calculate averages
crop_averages_2018 <- state_totals_2018 %>%
  left_join(county_counts_2018, by = "legacy_item_name") %>%
  mutate(
    avg_harvested_acres = if_else(counties_reporting > 0, 
                                  state_harvested_acres / counties_reporting, 
                                  0),
    avg_production = if_else(counties_reporting > 0, 
                             state_production / counties_reporting, 
                             0),
    avg_value = if_else(counties_reporting > 0, 
                        state_value / counties_reporting, 
                        0)
  )

# (4) keep clean summary table
usda_2018_averages <- crop_averages_2018 %>%
  select(crop, 
         legacy_item_name, 
         avg_harvested_acres, 
         avg_production, 
         avg_value, 
         price_per_unit_stateTotal)


################################################################################

# now that we have averages for all four years, we can combine them into one table

# add year column to each of the average tables
usda_2018_averages <- usda_2018_averages %>% 
  mutate(year = 2018)
usda_2019_averages <- usda_2019_averages %>%
  mutate(year = 2019)
usda_2020_averages <- usda_2020_averages %>%
  mutate(year = 2020)
usda_2021_averages <- usda_2021_averages %>%
  mutate(year = 2021)

# bind them all together
usda_all_years <- bind_rows(usda_2018_averages, 
                            usda_2019_averages, 
                            usda_2020_averages, 
                            usda_2021_averages) %>% 
  mutate(price_per_unit_calc = if_else(avg_production > 0, 
                                  avg_value / avg_production, 
                                  0), .after = avg_value)
# note: price_per_unit_stateTotal is the price_per_unit value reported in the USDA NASS data


#### adjust for consumer price index (CPI) to 2021 dollars ####

# CPI 2021 = 270.970
# CPI 2020 = 258.811
# CPI 2019 = 255.657
# CPI 2018 = 251.107
# Adjusted value = Original value * (CPI in 2021 / CPI in Original Year)

usda_all_years_CPI_adj <- usda_all_years %>% 
  mutate(
    cpi_adjustment_factor = case_when(
      year == 2018 ~ 270.970 / 251.107,
      year == 2019 ~ 270.970 / 255.657,
      year == 2020 ~ 270.970 / 258.811,
      year == 2021 ~ 1
    ),
    adj_avg_value = avg_value * cpi_adjustment_factor,
    adj_price_per_unit_calc = price_per_unit_calc * cpi_adjustment_factor,
    adj_price_per_unit_stateTotal = price_per_unit_stateTotal * cpi_adjustment_factor
  ) %>%
  select(-cpi_adjustment_factor) %>% 
  # reorder columns
  select(year, crop, legacy_item_name, avg_harvested_acres, avg_production, avg_value, 
         adj_avg_value, price_per_unit_calc, adj_price_per_unit_calc, 
         price_per_unit_stateTotal, adj_price_per_unit_stateTotal)


# compute mean across all 4 years for each crop
usda_final_avgerages_CPI <- usda_all_years_CPI_adj %>% 
  group_by(legacy_item_name) %>%
  summarise(
    mean_harvested_acres = mean(avg_harvested_acres, na.rm = TRUE),
    mean_production = mean(avg_production, na.rm = TRUE),
    mean_value = mean(adj_avg_value, na.rm = TRUE),
    mean_price_per_unit_calc = mean(adj_price_per_unit_calc, na.rm = TRUE),
    mean_price_per_unit_stateTotal = mean(adj_price_per_unit_stateTotal, na.rm = TRUE),
    n_years = n_distinct(year),  # should be 4 for all crops
    .groups = "drop"
  ) %>% 
  mutate(prod_per_acre = mean_production / mean_harvested_acres, 
         price_per_acre_calc = prod_per_acre * mean_price_per_unit_calc, 
         price_per_acre_stateTotal = prod_per_acre * mean_price_per_unit_stateTotal)


# add the current NASS crop names back
usda_final_averages <- usda_final_avgerages_CPI %>% 
  left_join(usda_2021_matching %>% select(crop, legacy_item_name) %>% distinct(), 
            by = "legacy_item_name") %>% 
  # keep only crop and price_per_acre_stateTotal
  select(crop, legacy_item_name, price_per_acre = price_per_acre_stateTotal) 

# update the crop name for strawberies to the current NASS name
usda_final_averages[4, "crop"] <- "Berries, Strawberries, All"
# update the crop name for peaches to the current NASS name
usda_final_averages[12, "crop"] <- "Peaches, All"
# USDA NASS data has pomegranates spelled wrong
usda_final_averages[15, "crop"] <- "Pomegranates"

# export the final revenue estimation table
write_csv(usda_final_averages, 
          here("data/intermediate/2_1_revenueEstimation/nass_cropRevenueEstimates_2021.csv"))






