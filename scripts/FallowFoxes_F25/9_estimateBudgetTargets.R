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

source(here("scripts/FallowFoxes_F25/0_startup/functions.R"))



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




