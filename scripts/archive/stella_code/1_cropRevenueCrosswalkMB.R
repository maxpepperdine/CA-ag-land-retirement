library(tidyverse)
library(readxl)
library(janitor)

# Final table in David's has: Crop, Year, Acres, Production, Unit, Value, Value_1, PerAcre, prodPerAcre, pircePerUnit, 
# PricePerAcre, hrsAcre, jobsPer100Acre 

# Read in the data and pull out just the Kern data
rev_raw_2021 <- read_csv(here::here("data/raw/USDA_NASS/CAC_2021_data_by_commodity_20240417.csv"))
kernrev_clean_2021 <- rev_raw_2021 |> 
  clean_names() |> 
  filter(county == "Kern") |> 
  select(current_item_name, year, harvested_acres, production, unit, value, price_per_unit) |> 
  rename(Crop = current_item_name) |> 
  mutate(value_1 = production * value) |> 
  mutate(prodPerAcre = production/harvested_acres) |> 
  mutate(pricePerAcre = prodPerAcre * price_per_unit) |> 
  clean_names()

final_pre_mb <- kernrev_clean_2021 |> 
  select(-unit)

final_rev_mb <- kernrev_clean_2021[, c("crop", "year", "harvested_acres", "production", "value", "value_1", "prod_per_acre", "price_per_unit", "price_per_acre")] |> 
  drop_na()

write_csv(final_rev_mb, here::here("data/intermediate/final_revenue_mb.csv"))

