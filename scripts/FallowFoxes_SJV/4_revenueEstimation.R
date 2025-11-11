# clear environment
rm(list = ls())

library(tidyverse)
library(janitor)
library(here)

################################################################################

# load the USDA NASS data for 2021-2022
usda_2021 <- read_csv(here("data/raw/USDA_NASS/CAC_2021_data_by_commodity_20240417.csv"))
usda_2022 <- read_csv(here("data/raw/USDA_NASS/County_Ag_Commissioner_Report_2022_data_by_commodity.csv"))

################################################################################

# the USDA crop name and commodity codes changed after 2020
# attach the old names and codes to the new ones

#### USDA 2022 ####
usda_2022_clean <- usda_2022 %>% 
  clean_names() %>%
  select(year, current_item_name, current_item_code, legacy_item_name, legacy_commodity_code, 
         county, harvested_acres, production, unit, value, price_per_unit) %>% 
  rename(crop = current_item_name) %>% 
  # USDA NASS data has pomegranates spelled wrong
  mutate(crop = if_else(crop == "Pomegrantates", 
                        "Pomegranates", 
                        crop))

# pull out the legacy item names and codes to use for merging with 2020 data
usda_2022_matching <- usda_2022_clean %>%
  distinct(legacy_item_name, legacy_commodity_code, .keep_all = TRUE) %>%
  select(legacy_item_name, legacy_commodity_code, crop, current_item_code)

# filter to only the crops we have 2022 LandIQ field data for in SJV, but not in USDA NASS
usda_2022_filter <- usda_2022_clean %>%
  filter(crop %in% c("Apples", "Apricots", "Avocados", "Beans, All", "Berries, Blueberries", 
                     "Berries, Strawberries, All", "Brussels Sprouts", "Carrots", 
                     "Cherries", "Corn, Silage", "Cotton, Lint, All", "Cucumbers", 
                     "Dates", "Grapefruit", "Grapes, All", "Hay, Grain, Misc", "Kiwifruit", 
                     "Lettuce, Head", "Olives", "Onions, Dry", "Oranges, All", 
                     "Peaches, All", "Pears, All", "Pecans", "Peppers, Bell", 
                     "Plums", "Pomegranates", "Potatoes", "Rice, Excluding Wild", 
                     "Ryegrass", "Safflower", "Sugar Beets", "Sweet Potatoes", 
                     "Tomatoes, Processing", "Wheat, Grain"))

# QC: count number of distinct obsv in legacy item name 
# should be the # of crops you want to estimate revenue for (i.e., 35 crops here)
n_distinct(usda_2022_filter$crop)

# replace all NAs in harvested_acres, production, value with 0
usda_2022_filter <- usda_2022_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))

# subtract "Sum of Others" values from "State Total" values
# this leaves only State Total values for the counties recorded for each crop

# compute "Sum of Others" totals per crop
sum_others_2022 <- usda_2022_filter %>%
  filter(county == "Sum of Others") %>%
  group_by(crop) %>%
  summarise(
    sum_others_acres = sum(harvested_acres, na.rm = TRUE),
    sum_others_prod  = sum(production, na.rm = TRUE),
    sum_others_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# join back to full dataset and subtract from State Total rows
usda_2022_adjusted <- usda_2022_filter %>%
  left_join(sum_others_2022, by = "crop") %>%
  mutate(
    # if county is "State Total", subtract the "Sum of Others" values
    # otherwise, keep the original value
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
usda_2022_estimate <- usda_2022_adjusted %>%
  filter(county != "Sum of Others")


### calculate the average county-level values per crop for 2022 ###

# (1) count how many counties reported data for each crop
county_counts_2022 <- usda_2022_estimate %>%
  filter(!county %in% c("State Total")) %>%  # exclude state totals
  group_by(crop) %>%
  summarise(
    counties_reporting = n(),  # count all county records (even if 0)
    .groups = "drop"
  )

# (2) extract adjusted State Total values
state_totals_2022 <- usda_2022_estimate %>%
  filter(county == "State Total") %>%
  select(crop, 
         legacy_item_name, 
         state_harvested_acres = harvested_acres, 
         state_production = production, 
         state_value = value, 
         price_per_unit_stateTotal = price_per_unit, 
         unit)

# (3) join county counts to state totals and calculate averages
crop_averages_2022 <- state_totals_2022 %>%
  left_join(county_counts_2022, by = "crop") %>%
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
usda_2022_averages <- crop_averages_2022 %>%
  select(crop, 
         legacy_item_name, 
         avg_harvested_acres, 
         avg_production, 
         avg_value, 
         price_per_unit_stateTotal)


################################################################################


### USDA 2021 ####
usda_2021_clean <- usda_2021 %>% 
  clean_names() %>%
  select(year, current_item_name, current_item_code, legacy_item_name, legacy_commodity_code, 
         county, harvested_acres, production, unit, value, price_per_unit) %>% 
  rename(crop = current_item_name) %>% 
  # USDA NASS data has pomegranates spelled wrong
  mutate(crop = if_else(crop == "Pomegrantates", 
                        "Pomegranates", 
                        crop))

# filter to only the crops we have 2022 LandIQ field data for in Kern, but not in USDA NASS
usda_2021_filter <- usda_2021_clean %>%
  filter(crop %in% c("Apples", "Apricots", "Avocados", "Beans, All", "Berries, Blueberries", 
                     "Berries, Strawberries, All", "Brussels Sprouts", "Carrots", 
                     "Cherries", "Corn, Silage", "Cotton, Lint, All", "Cucumbers", 
                     "Dates", "Grapefruit", "Grapes, All", "Hay, Grain, Misc", "Kiwifruit", 
                     "Lettuce, Head", "Olives", "Onions, Dry", "Oranges, All", 
                     "Peaches, All", "Pears, All", "Pecans", "Peppers, Bell", 
                     "Plums", "Pomegranates", "Potatoes", "Rice, Excluding Wild", 
                     "Ryegrass", "Safflower", "Sugar Beets", "Sweet Potatoes", 
                     "Tomatoes, Processing", "Wheat, Grain"))

# # for all observations where crop == "Berries, Strawberries, All", assign the "legacy_item_name" values as "Berries, Strawberries, Fresh Market"
# # we only need an extra step for this crop type because the NASS data changed the naming convention
# usda_2021_filter <- usda_2021_filter %>%
#   mutate(legacy_item_name = if_else(crop == "Berries, Strawberries, All", 
#                                     "Berries, Strawberries, Fresh Market", 
#                                     legacy_item_name))

# QC: count number of distinct obsv in legacy item name 
# should be the # of crops you want to estimate revenue for (i.e., 35 crops here)
n_distinct(usda_2021_filter$crop)

# replace all NAs in harvested_acres, production, value with 0
usda_2021_filter <- usda_2021_filter %>%
  mutate(across(c(harvested_acres, production, value), ~replace_na(., 0)))

# subtract "Sum of Others" values from "State Total" values
# this leaves only State Total values for the counties recorded for each crop

# compute "Sum of Others" totals per crop
sum_others_2021 <- usda_2021_filter %>%
  filter(county == "Sum of Others") %>%
  group_by(crop) %>%
  summarise(
    sum_others_acres = sum(harvested_acres, na.rm = TRUE),
    sum_others_prod  = sum(production, na.rm = TRUE),
    sum_others_value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# join back to full dataset and subtract from State Total rows
usda_2021_adjusted <- usda_2021_filter %>%
  left_join(sum_others_2021, by = "crop") %>%
  mutate(
    # if county is "State Total", subtract the "Sum of Others" values
    # otherwise, keep the original value
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
  group_by(crop) %>%
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
  left_join(county_counts_2021, by = "crop") %>%
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


################################################################################


################################################################################

# now that we have averages for 2021 & 2022 we can combine them into one table

# add year column to each of the average tables
usda_2021_averages <- usda_2021_averages %>% 
  mutate(year = 2021)
usda_2022_averages <- usda_2022_averages %>%
  mutate(year = 2022)

# bind them together
usda_all_years <- bind_rows(usda_2021_averages, 
                            usda_2022_averages) %>% 
  mutate(price_per_unit_calc = if_else(avg_production > 0, 
                                  avg_value / avg_production, 
                                  0), .after = avg_value)
# note: price_per_unit_stateTotal is the price_per_unit value reported in the USDA NASS data


#### adjust for consumer price index (CPI) to 2022 dollars ####

# CPI 2022 = 292.655
# CPI 2021 = 270.970
# Adjusted value = Original value * (CPI in 2022 / CPI in Original Year)
# source: https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexannualandsemiannual_table.htm

usda_all_years_CPI_adj <- usda_all_years %>% 
  mutate(
    cpi_adjustment_factor = case_when(
      year == 2021 ~ 292.655 / 270.970,
      year == 2022 ~ 1  # no adjustment needed for 2022
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
usda_final_averages_CPI <- usda_all_years_CPI_adj %>% 
  group_by(crop) %>%
  summarise(
    mean_harvested_acres = mean(avg_harvested_acres, na.rm = TRUE),
    mean_production = mean(avg_production, na.rm = TRUE),
    mean_value = mean(adj_avg_value, na.rm = TRUE),
    mean_price_per_unit_calc = mean(adj_price_per_unit_calc, na.rm = TRUE),
    mean_price_per_unit_stateTotal = mean(adj_price_per_unit_stateTotal, na.rm = TRUE),
    n_years = n_distinct(year),  # should be 2 for all crops
    .groups = "drop"
  ) %>% 
  mutate(prod_per_acre = mean_production / mean_harvested_acres, 
         price_per_acre_calc = prod_per_acre * mean_price_per_unit_calc, 
         price_per_acre_stateTotal = prod_per_acre * mean_price_per_unit_stateTotal) %>% 
  # keep only crop and the more reliable price_per_acre_stateTotal
  select(current_NASS_crop = crop, price_per_acre = price_per_acre_stateTotal)


# export the final revenue estimation table
write_csv(usda_final_averages_CPI, 
          here("data/intermediate/4_revenueEstimation/nass_cropRevenueEstimates_2022.csv"))



################################################################################


################################################################################


# Add estimated values to the LandIQ SJV data ---------------------------------

# now that we have the revenue estimates per crop, we can join them to the LandIQ SJV data
# this is the sjv_crosswalkPlots_na.shp data from the 3_masterCrosswalk.R script

# load the LandIQ SJV data with crop crosswalk info
SJV_landIQ_na_crosswalk <- read_sf(here("data/intermediate/3_masterCrosswalk/sjv_crosswalkPlots_na/sjv_crosswalkPlots_na.shp")) %>% 
  rename(price_per_acre = prc_pr_)

# join the estimated data to the LandIQ plot crosswalk
SJV_landIQ_full_crosswalk <- SJV_landIQ_na_crosswalk %>%
  left_join(usda_final_averages_CPI, 
            by = c("NASS" = "current_NASS_crop")) %>% 
  mutate(price_per_acre = if_else(
    is.na(price_per_acre.x) & !is.na(price_per_acre.y),
    price_per_acre.y,
    price_per_acre.x)) %>%
  select(-price_per_acre.x, -price_per_acre.y)



# QC: make sure this worked
# the only LandIQ crop classes with NA values for price_per_acre should be idle and unclassified fallow
# we'll estimate values for those later
missing_crops_test <- SJV_landIQ_full_crosswalk %>%
  filter(is.na(price_per_acre)) %>%
  distinct(crp_ty_)



# export the LandIQ plots with all crosswalk info and revenue estimates
write_sf(SJV_landIQ_full_crosswalk, 
         here("data/intermediate/4_revenueEstimation/sjv_landIQ_fullCrosswalk/sjv_landIQ_fullCrosswalk.shp"))
























