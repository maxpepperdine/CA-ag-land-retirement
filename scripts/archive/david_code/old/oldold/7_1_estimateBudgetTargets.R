
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)    # for static and interactive maps
library(janitor)


options(scipen=999)


# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")



# Read in Data ------------------------------------------------------------


kern <- read_sf("Data/6_foxExtraction/kernExtractions.shp")

# Find Targets, values ----------------------------------------------------



kernFallow <- kern %>%
  filter(fallow == 1)

# # Value of non permitted fields
# kernFallow <- kern %>%
#   filter(nonPrmt == 1)


water <- metrics(kernFallow$water) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "water (acre-ft)")

acres <- metrics(kernFallow$acres) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "acres")

fox <- metrics(kernFallow$habitat) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "fox (acre)")

rev <- metrics(kernFallow$revenue) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "revenue ($)")

jobs <- metrics(kernFallow$jobs) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "jobs (x100)")


sumTable <- bind_rows(rev, water, fox, jobs, acres) %>% 
  dplyr::select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:6, as.numeric)) %>% 
  mutate(across(2:6, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:5, names_to = "Target", values_to = "After") %>% 
  dplyr::select(-stat, -acres) %>% 
  bind_cols(c(398080000, 248531, 11203, NA)) %>% 
  mutate(
    Before = `...3`,
    .keep = "unused"
  ) %>% 
  dplyr::select(1,3,2)

  
write_csv(sumTable, "./Data/7_estimateBudgetTargets/targetTable.csv")
