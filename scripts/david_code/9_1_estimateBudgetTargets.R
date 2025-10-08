
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)    # for static and interactive maps
library(janitor)


options(scipen = 999)


# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")



# Read in Data ------------------------------------------------------------


kern <- read_sf("Data/8_blm/cleanShapes.shp") 

# Find Targets, values ----------------------------------------------------



kernFallow <- kern %>%
  filter(fallow == 1)


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

jobsQ <- metrics(kernFallow$jobsQ) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "jobsQ (x100)")

jobsB <- metrics(kernFallow$jobsB) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "jobsB (x100)")


# If calculating area -----------------------------------------------------



ret <- kernFallow %>%
  filter(nonPrmt == 1) %>%
  {{sum(.$acres)}}

idl <- kernFallow %>%
  filter(nonPrmt == 0) %>%
  {{sum(.$acres)}}


rat <- ret / (idl + ret)


# Create table ------------------------------------------------------------




sumTable <- bind_rows(rev, water, fox, jobsQ, jobsB, acres) %>% 
  dplyr::select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:7, as.numeric)) %>% 
  mutate(across(2:7, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:6, names_to = "Target", values_to = "Values") %>% 
  dplyr::select(-stat, -acres) %>% 
  add_row(Target = "retiredRatio", Values = rat)


# Export ------------------------------------------------------------------


  
write_csv(sumTable, "./Data/9_estimateBudgetTargets/targetTable.csv")
