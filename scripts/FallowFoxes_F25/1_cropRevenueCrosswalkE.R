library(tidyverse)
library(readxl)
library(janitor)

# Final table in David's has: Crop, Year, Acres, Production, Unit, Value, Value_1, PerAcre, prodPerAcre, pircePerUnit, 
# PricePerAcre, hrsAcre, jobsPer100Acre 

# Read in the data and pull out just the Kern data
rev_raw_2021 <- read_csv(here::here("data/raw/USDA_NASS/CAC_2021_data_by_commodity_20240417.csv"))
kernrev_clean_2021 <- rev_raw_2021  %>%  
  clean_names() %>%  
  filter(county == "Kern") %>% 
  select(current_item_name, year, harvested_acres, production, unit, value, price_per_unit) %>% 
  rename(crop = current_item_name) %>% 
  # if value is already total production value in $, what is value_1?
  mutate(value_1 = production * value) %>%  
  mutate(prod_per_acre = production/harvested_acres) %>% 
  mutate(price_per_acre = prod_per_acre * price_per_unit)

# final table with just the columns we want to export
final_rev_e <- kernrev_clean_2021[, c("crop", "year", "harvested_acres", "production", 
                                      "value", "value_1", "prod_per_acre", 
                                      "price_per_unit", "price_per_acre")] 

write_csv(final_rev_e, here::here("data/intermediate/1_cropRevenueCrosswalkE/final_revenue_e.csv"))





