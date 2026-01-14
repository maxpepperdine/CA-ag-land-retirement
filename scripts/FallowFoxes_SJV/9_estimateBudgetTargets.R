# =============================================================================
# Estimating Budget Targets for prioritizr based on Idle/Fallow Fields
# =============================================================================
# Purpose: We'll use fields classified as idle or fallow in the SJV dataset to 
#          estimate budget targets for prioritizr. These targets will be used 
#          to inform the optimization process in later scripts and build our
#          prioritization scenarios with the prioritizr package.
# =============================================================================





# clear environment
rm(list = ls())

# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)    # for static and interactive maps
library(janitor)
library(here)


# don't use scientific notation; print full numbers
options(scipen = 999)


# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_SJV/0_startup/functions.R"))



# Read in Data ------------------------------------------------------------

# load spatial data with estimation for fallow/idle fields
# for now, we'll use the outputs from 6_estimateFallowing
# later, replace with the output from 8_blm

# read shapefile with fallowing estimates derived from median method
sjv_med <- read_sf(here("data/intermediate/6_estimateFallowing_median/sjvAddFallowMedian/sjvAddFallowMedian.shp"))

# read shapefile with fallowing estimated derived from KNN method
sjv_knn <- read_sf(here("data/intermediate/6_estimateFallowing_knn/sjvAddFallowKNN/sjvAddFallowKNN.shp"))



# Find Targets, values for median estimation fields -----------------------

sjvFallow_med <- sjv_med %>%
  filter(fallow == 1) %>% 
  filter(county == "Kern")

water_med <- metrics(sjvFallow_med$water) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "water (acre-ft)")

acres_med <- metrics(sjvFallow_med$acres) %>%
  as_tibble_row() %>% 
  mutate(Cat = "acres")

rev_med <- metrics(sjvFallow_med$revenue) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "revenue ($)")


# create clean summary table
sumTable_med <- bind_rows(rev_med, water_med, acres_med) %>% 
  dplyr::select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:4, as.numeric)) %>% 
  mutate(across(2:4, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:3, names_to = "Target", values_to = "Values") %>% 
  dplyr::select(-stat, -acres)



# Find Targets, values for KNN estimation fields --------------------------

sjvFallow_knn <- sjv_knn %>%
  filter(fallow == 1) %>% 
  filter(county == "Kern")

water_knn <- metrics(sjvFallow_knn$water) %>%
  as_tibble_row() %>% 
  mutate(Cat = "water (acre-ft)")

acres_knn <- metrics(sjvFallow_knn$acres) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "acres")

rev_knn <- metrics(sjvFallow_knn$revenue) %>% 
  as_tibble_row() %>% 
  mutate(Cat = "revenue ($)")

# create clean summary table
sumTable_knn <- bind_rows(rev_knn, water_knn, acres_knn) %>% 
  dplyr::select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:4, as.numeric)) %>% 
  mutate(across(2:4, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:3, names_to = "Target", values_to = "Values") %>% 
  dplyr::select(-stat, -acres)



# ==============================================================================
# FROM KERN WORKFLOW
# ==============================================================================


# Read in Data ------------------------------------------------------------

kern <- read_sf(here("data/intermediate/8_blm/cleanShapes/cleanShapes.shp")) 



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


# If calculating area -----------------------------------------------------



ret <- kernFallow %>%
  filter(retired == 1) %>%
  {{sum(.$acres)}}

idl <- kernFallow %>%
  filter(retired == 0) %>%
  {{sum(.$acres)}}


rat <- ret / (idl + ret)


# Create table ------------------------------------------------------------




sumTable <- bind_rows(rev, water, fox, acres) %>% 
  dplyr::select(Cat, everything()) %>% 
  t() %>% 
  row_to_names(1) %>% 
  as.data.frame() %>% 
  rownames_to_column("stat") %>% 
  as_tibble() %>% 
  mutate(across(2:5, as.numeric)) %>% 
  mutate(across(2:5, round)) %>% 
  filter(stat == "sum") %>% 
  pivot_longer(2:4, names_to = "Target", values_to = "Values") %>% 
  dplyr::select(-stat, -acres) %>% 
  add_row(Target = "retiredRatio", Values = rat)


# Export ------------------------------------------------------------------



write_csv(sumTable, here("data/intermediate/9_estimateBudgetTargets/targetTable.csv"))




