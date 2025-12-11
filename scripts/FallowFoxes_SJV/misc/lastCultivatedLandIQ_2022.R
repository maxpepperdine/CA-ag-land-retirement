# =============================================================================
# LandIQ Crop Mapping Analysis: Determining Last Cultivated Crop
# =============================================================================
# Purpose: For fields classified as Idle or Fallow in 2022, determine the most
#          recent cultivated crop from 2021, 2020, 2019, or 2018 datasets
# =============================================================================

# clear workspace
rm(list = ls())

# Load required libraries
library(sf)
library(tidyverse)
library(here)

# =============================================================================
# STEP 1: Load LandIQ plots and define idle/fallow classes
# =============================================================================


# define file paths to cleaned LandIQ plots
path_2022 <- here("data/intermediate/5_cropRotation/sjvYearRotation/sjvYearRotation.shp")
path_2021 <- here("data/intermediate/misc/2021/2_cleanPlotsLandIQ_2021/SJVID_2021/SJVID_2021.shp")
path_2020 <- here("data/intermediate/misc/2020/2_cleanPlotsLandIQ_2020/SJVID_2020/SJVID_2020.shp")
path_2019 <- here("data/intermediate/misc/2019/2_cleanPlotsLandIQ_2019/SJVID_2019/SJVID_2019.shp")
path_2018 <- here("data/intermediate/misc/2018/2_cleanPlotsLandIQ_2018/SJVID_2018/SJVID_2018.shp")

# read in all cleaned LandIQ plots
landiq_2022 <- read_sf(path_2022)
landiq_2021 <- read_sf(path_2021) %>% 
  rename(comm = crp_ty_)
landiq_2020 <- read_sf(path_2020) %>% 
  rename(comm = crp_ty_)
landiq_2019 <- read_sf(path_2019) %>% 
  rename(comm = crp_ty_)
landiq_2018 <- read_sf(path_2018) %>% 
  rename(comm = crp_ty_)

# define the comm classes that indicate fallow/idle land
idle_comms <- c("Unclassified Fallow", "Idle - Long Term", "Idle - Short Term")

# =============================================================================
# STEP 2: create lookup tables from historical years (just UniqueID and comm)
# =============================================================================

# extract just UniqueID and comm from each year 
lookup_2021 <- landiq_2021 %>%
  st_drop_geometry %>%
  select(uniqu_d, comm) %>%
  rename(comm_2021 = comm)

lookup_2020 <- landiq_2020 %>%
  st_drop_geometry %>%
  select(uniqu_d, comm) %>%
  rename(comm_2020 = comm)

lookup_2019 <- landiq_2019 %>%
  st_drop_geometry %>%
  select(uniqu_d, comm) %>%
  rename(comm_2019 = comm)

lookup_2018 <- landiq_2018 %>%
  st_drop_geometry %>%
  select(uniqu_d, comm) %>%
  rename(comm_2018 = comm)


# =============================================================================
# STEP 3: join 2018-2021 data to 2022 data
# =============================================================================

landiq_2022_joined <- landiq_2022 %>%
  left_join(lookup_2021, by = c("uniqu_d")) %>%
  left_join(lookup_2020, by = c("uniqu_d")) %>%
  left_join(lookup_2019, by = c("uniqu_d")) %>%
  left_join(lookup_2018, by = c("uniqu_d"))


# =============================================================================
# STEP 4: for idle/fallow fields in 2022, determine last cultivated crop
# =============================================================================

# create a function to find the last cultivated crop for each idle/fallow field

find_last_crop <- function(comm_2021, comm_2020, comm_2019, comm_2018, 
                           idle_comms) {
  # Check 2021 first
  if (!is.na(comm_2021) && !(comm_2021 %in% idle_comms)) {
    return(comm_2021)
  }
  
  # Check 2020
  if (!is.na(comm_2020) && !(comm_2020 %in% idle_comms)) {
    return(comm_2020)
  }
  
  # Check 2019
  if (!is.na(comm_2019) && !(comm_2019 %in% idle_comms)) {
    return(comm_2019)
  }
  
  # Check 2018
  if (!is.na(comm_2018) && !(comm_2018 %in% idle_comms)) {
    return(comm_2018)
  }
  
  # No cultivated crop found
  return(NA_character_)
}


# Apply the function to determine last cultivated crop for idle/fallow fields

landiq_2022_final <- landiq_2022_joined %>%
  mutate(
    # Initialize last_comm as NA for all records
    last_comm = NA_character_,
    
    # Only calculate last_comm for idle/fallow fields
    last_comm = case_when(
      # If current comm is NOT in idle categories, set last_comm to NA
      !(comm %in% idle_categories) ~ NA_character_,
      
      # If current comm IS in idle categories, find the last cultivated crop
      TRUE ~ mapply(
        find_last_crop,
        comm_2021,
        comm_2020,
        comm_2019,
        comm_2018,
        MoreArgs = list(idle_cats = idle_comms)
      )
    )
  )


# =============================================================================
# STEP 5: clean up and generate some summary statistics
# =============================================================================





















