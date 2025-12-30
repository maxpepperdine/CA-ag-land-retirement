# =============================================================================
# LandIQ Crop Mapping Analysis: Determining Last Cultivated Crop
# =============================================================================
# Purpose: For fields classified as Idle or Fallow in 2022, determine the most
#          recent cultivated crop from 2021, 2020, 2019, 2018, 2016, or 2014 
#          datasets
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
path_2021 <- here("data/intermediate/misc/LandIQ_processing/2021/5_cropRotation_2021/sjvYearRotation/sjvYearRotation_2021.shp")
path_2020 <- here("data/intermediate/misc/LandIQ_processing/2020/5_cropRotation_2020/sjvYearRotation/sjvYearRotation_2020.shp")
path_2019 <- here("data/intermediate/misc/LandIQ_processing/2019/5_cropRotation_2019/sjvYearRotation/sjvYearRotation_2019.shp")
path_2018 <- here("data/intermediate/misc/LandIQ_processing/2018/5_cropRotation_2018/sjvYearRotation/sjvYearRotation_2018.shp")
path_2016 <- here("data/intermediate/misc/LandIQ_processing/2016/5_cropRotation_2016/sjvYearRotation/sjvYearRotation_2016.shp")
path_2014 <- here("data/intermediate/misc/LandIQ_processing/2014/5_cropRotation_2014/sjvYearRotation/sjvYearRotation_2014.shp")

# read in all cleaned LandIQ plots
landiq_2022 <- read_sf(path_2022)
landiq_2021 <- read_sf(path_2021)
landiq_2020 <- read_sf(path_2020)
landiq_2019 <- read_sf(path_2019)
landiq_2018 <- read_sf(path_2018)
landiq_2016 <- read_sf(path_2016)
landiq_2014 <- read_sf(path_2014)


# define the comm classes that indicate fallow/idle land
idle_comms <- c("Unclassified Fallow", "Idle - Long Term", "Idle - Short Term", 
                "Idle")


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
# STEP 3: filter to idle/fallow fields in 2022, then join 2014, 2016, 2018-2021 data to 2022 data
# =============================================================================

# separate idle/fallow fields from cultivated fields
landiq_2022_idle <- landiq_2022 %>%
  filter(comm %in% idle_comms)

landiq_2022_cultivated <- landiq_2022 %>%
  filter(!(comm %in% idle_comms))

# join historical data to 2022 idle/fallow fields
landiq_2022_idle_joined <- landiq_2022_idle %>%
  left_join(lookup_2021, by = c("uniqu_d")) %>%
  left_join(lookup_2020, by = c("uniqu_d")) %>%
  left_join(lookup_2019, by = c("uniqu_d")) %>%
  left_join(lookup_2018, by = c("uniqu_d"))


# 2014 and 2016 don't have a Unique ID column like 2018-2021 data
# we need to do a spatial join based on geometry to get those years

# Ensure all layers have the same CRS
if (st_crs(landiq_2022_idle_joined) != st_crs(landiq_2016)) {
  cat("Transforming 2016 data to match 2022 CRS...\n")
  landiq_2016 <- st_transform(landiq_2016, st_crs(landiq_2022_idle_joined))
}

if (st_crs(landiq_2022_idle_joined) != st_crs(landiq_2014)) {
  cat("Transforming 2014 data to match 2022 CRS...\n")
  landiq_2014 <- st_transform(landiq_2014, st_crs(landiq_2022_idle_joined))
}


# prepare 2014 and 2016 data for join -- keep just the crop variable
landiq_2016_slim <- landiq_2016 %>%
  select(comm_2016 = crp2016)
landiq_2014_slim <- landiq_2014 %>%
  select(comm_2014 = crp2014)


# spatial join 2016 data to idle fields
# using st_join with largest = TRUE to get the field with the most overlap
landiq_2022_idle_joined <- landiq_2022_idle_joined %>%
  st_join(landiq_2016_slim, join = st_intersects, largest = TRUE)

# Spatial join 2014 data to idle fields
landiq_2022_idle_joined <- landiq_2022_idle_joined %>%
  st_join(landiq_2014_slim, join = st_intersects, largest = TRUE)


# =============================================================================
# STEP 4: for idle/fallow fields in 2022, determine last cultivated crop
# =============================================================================

# create a function to find the last cultivated crop for each idle/fallow field

find_last_crop <- function(comm_2021, comm_2020, comm_2019, comm_2018, comm_2016, 
                           comm_2014, idle_comms) {
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
  
  # Check 2016
  if (!is.na(comm_2016) && !(comm_2016 %in% idle_comms)) {
    return(comm_2016)
  }
  
  # Check 2014
  if (!is.na(comm_2014) && !(comm_2014 %in% idle_comms)) {
    return(comm_2014)
  }
  
  # No cultivated crop found
  return(NA_character_)
}


# apply the function to determine last cultivated crop for idle/fallow fields
landiq_2022_idle_final <- landiq_2022_idle_joined %>%
  mutate(
    last_comm = mapply(
      find_last_crop,
      comm_2021,
      comm_2020,
      comm_2019,
      comm_2018,
      comm_2016,
      comm_2014,
      MoreArgs = list(idle_comms = idle_comms)
    )
  )


# clean up the output to check
landiq_2022_idle_final_check <- landiq_2022_idle_final %>%
  select(uniqu_d, county, comm, comm_2021, comm_2020, comm_2019, comm_2018, 
         comm_2016, comm_2014, last_comm, 
         everything())


# =============================================================================
# STEP 5: clean up and generate some summary statistics
# =============================================================================

# remove comm columns from historical years
landiq_2022_idle_final_clean <- landiq_2022_idle_final %>%
  select(-comm_2021, -comm_2020, -comm_2019, -comm_2018, -comm_2016, -comm_2014)

# add last_comm column (as NA) to cultivated fields for consistency
landiq_2022_cultivated <- landiq_2022_cultivated %>%
  mutate(last_comm = NA_character_)

# combine back with cultivated fields from 2022
landiq_2022_final <- bind_rows(landiq_2022_cultivated, landiq_2022_idle_final_clean)
# check the crs
st_crs(landiq_2022_final)


# Count of idle/fallow fields in 2022 (using already-filtered idle dataset)
idle_summary <- landiq_2022_idle_final_clean %>%
  st_drop_geometry() %>%
  group_by(comm) %>%
  summarise(
    total_fields = n(),
    fields_with_last_crop = sum(!is.na(last_comm)),
    fields_without_last_crop = sum(is.na(last_comm)),
    .groups = "drop"
  )

print(idle_summary)

# make a prettier table of the idle_summary information
idle_summary_pretty <- idle_summary %>%
  mutate(
    percent_with_last_crop = round((fields_with_last_crop / total_fields) * 100, 2),
    percent_without_last_crop = round((fields_without_last_crop / total_fields) * 100, 2)
  ) %>%
  select(
    comm,
    total_fields,
    fields_with_last_crop,
    percent_with_last_crop,
    fields_without_last_crop,
    percent_without_last_crop
  )


# =============================================================================
# STEP 6: save the output
# =============================================================================

write_sf(landiq_2022_final, 
         here("data/intermediate/misc/LandIQ_processing/1_lastCultivatedLandIQ_2022/landiq_2022_lastCultivated.shp"), 
         append = FALSE)
















