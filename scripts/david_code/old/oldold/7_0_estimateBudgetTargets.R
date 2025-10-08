
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(tmap)    # for static and interactive maps
library(janitor)


#Visualization
tmap_mode("view")
tmap_options(check.and.fix = TRUE)


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


sumTable <- bind_rows(rev, water, fox, acres) %>% 
  select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:5, as.numeric)) %>% 
  mutate(across(2:5, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:4, names_to = "Target", values_to = "After") %>% 
  select(-stat, -acres) %>% 
  bind_cols(c(398080000, 248531, 11203)) %>% 
  mutate(
    Before = `...3`,
    .keep = "unused"
  ) %>% 
  select(1,3,2)

  
write_csv(sumTable, "./Data/7_estimateBudgetTargets/targetTable.csv")
