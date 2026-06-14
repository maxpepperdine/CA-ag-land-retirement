# =============================================================================
# CLEANING USDA NASS CROP REVENUE DATA
# =============================================================================
# Purpose: Filter USDA NASS crop rev data to SJV counties, select relevant 
#          columns and calculate per/acre rev values.
# =============================================================================

# clear history
rm(list = ls())

# load packages
library(tidyverse)
library(readxl)
library(janitor)
library(here)

# final table should have: 
#     crop, county, year, harvested_acres, production, value, prodPerAcre, 
#     pricePerUnit, pricePerAcre

# SJV counties: 
#     San Joaquin, Stanislaus, Merced, Madera, Fresno, Kings, Tulare, Kern

# read in the USDA NASS data
rev_raw_2022 <- read_csv(here("data/raw/USDA_NASS/County_Ag_Commissioner_Report_2022_data_by_commodity.csv"))

# filter to counties in the SJV
SJV_rev_clean_2022 <- rev_raw_2022 %>% 
  clean_names() %>% 
  filter(county %in% c("San Joaquin", "Stanislaus", "Merced", "Madera", 
                       "Fresno", "Kings", "Tulare", "Kern")) %>% 
  select(crop = current_item_name, county, year, harvested_acres, production, 
         unit, value, price_per_unit) %>%
  # calculate price- and prod- per acre
  mutate(
    prod_per_acre = production / harvested_acres,
    price_per_acre = prod_per_acre * price_per_unit
  )

# save final table
write_csv(SJV_rev_clean_2022, here("data/intermediate/1_crop_revenue_crosswalk/final_revenue_sjv.csv"))




