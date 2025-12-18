# =============================================================================
# Assign Per-Acre Revenue and Water Use to Idle/Fallow Fields
# =============================================================================
# Purpose: For idle/fallow fields with a known last cultivated crop (last_comm),
#          assign the appropriate per-acre revenue and water use values based on
#          the county and last_comm crop type
# =============================================================================

# clear environment
rm(list = ls())

# Load required libraries
library(sf)
library(tidyverse)
library(here)


# =============================================================================
# STEP 1: load data and join last cultivated crop info to sjv year data
# =============================================================================

# sjv plots processed through crop rotation script (5_cropRotation.R)
sjv <- read_sf(here("data/intermediate/5_cropRotation/sjvYearRotation/sjvYearRotation.shp"))

# LandIQ plots with last cultivated crop info (from 1_lastCultivatedLandIQ_2022.R)
sjv_lastCult <- read_sf(here("data/intermediate/misc/lastCultivatedLandIQ_2022/landiq_2022_lastCultivated.shp")) %>% 
  select(uniqu_d, comm, last_comm, everything())

# join last cultivated crop info to sjv year data
sjv_joined <- sjv %>%
  left_join(sjv_lastCult %>% 
              st_drop_geometry() %>% 
              select(uniqu_d, last_comm),
            by = "uniqu_d") %>% 
  select(uniqu_d, comm, last_comm, everything())






