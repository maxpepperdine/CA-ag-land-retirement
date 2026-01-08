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
sjv_lastCult <- read_sf(here("data/intermediate/misc/LandIQ_processing/1_lastCultivatedLandIQ_2022/landiq_2022_lastCultivated.shp")) %>% 
  select(uniqu_d, comm, last_comm, everything())

# join last cultivated crop info to sjv year data
sjv_joined <- sjv %>%
  left_join(sjv_lastCult %>% 
              st_drop_geometry() %>% 
              select(uniqu_d, last_comm),
            by = "uniqu_d") %>% 
  select(uniqu_d, comm, last_comm, everything())


# =============================================================================
# STEP 2: create lookup table of revenue and water use by county and comm
# =============================================================================

# define idle/fallow categories
idle_categories <- c("Unclassified Fallow", "Idle - Long Term", "Idle - Short Term")


# extract unique county-comm rev and water combinations from NON-idle fields
# these are the values we will assign to idle/fallow fields based on last_comm
revenue_water_lookup <- sjv_joined %>%
  st_drop_geometry() %>%
  filter(!(comm %in% idle_categories)) %>%
  group_by(county, comm) %>%
  # take the mean revenue and water use for each county-comm combination
  # this deals with floating point issues for county-comm combos
  summarise(
    lookup_rvPrAcr = mean(rvPrAcr, na.rm = TRUE),
    lookup_wtrPrAc = mean(wtrPrAc, na.rm = TRUE),
    .groups = "drop"
  )


# =============================================================================
# STEP 5: join lookup values to idle/fallow fields based on county and last_comm
# =============================================================================

# join lookup values to sjv data
sjv_joined_revWat <- sjv_joined %>% 
  left_join(
    revenue_water_lookup,
    by = c("county" = "county", "last_comm" = "comm")
  )


# replace zero values with appropriate lookup values for idle/fallow fields
sjv_final <- sjv_joined_revWat %>%
  mutate(
    # replace rvPrAcr: use lookup value if field is idle AND has a last_comm
    rvPrAcr = case_when(
      comm %in% idle_categories & 
        !is.na(last_comm) & 
        !is.na(lookup_rvPrAcr) ~ lookup_rvPrAcr,
      TRUE ~ rvPrAcr
    ),
    # replace wtrPrAc: use lookup value if field is idle AND has a last_comm
    wtrPrAc = case_when(
      comm %in% idle_categories & 
        !is.na(last_comm) & 
        !is.na(lookup_wtrPrAc) ~ lookup_wtrPrAc,
      TRUE ~ wtrPrAc
    ),
  ) %>% 
  mutate(
    # reclaculate total revenue and water use for idle categories only
    revYear = case_when(
      comm %in% idle_categories & 
        !is.na(last_comm) & 
        !is.na(rvPrAcr) ~ rvPrAcr * acres,
      TRUE ~ revYear
    ),
    waterYr = case_when(
      comm %in% idle_categories & 
        !is.na(last_comm) & 
        !is.na(wtrPrAc) ~ wtrPrAc * acres,
      TRUE ~ waterYr
    )
  ) %>%
  # remove the temporary lookup columns
  select(-lookup_rvPrAcr, -lookup_wtrPrAc)



# =============================================================================
# STEP 6: generate some summary stats and export
# =============================================================================

# count how many idle fields were updated
update_summary <- sjv_joined %>%
  st_drop_geometry() %>%
  filter(comm %in% idle_categories) %>%
  left_join(
    revenue_water_lookup,
    by = c("county" = "county", "last_comm" = "comm")
  ) %>%
  summarise(
    total_idle_fields = n(),
    fields_with_last_comm = sum(!is.na(last_comm)),
    fields_updated = sum(!is.na(last_comm) & !is.na(lookup_rvPrAcr)),
    fields_not_updated_no_last_comm = sum(is.na(last_comm)),
    fields_not_updated_no_match = sum(!is.na(last_comm) & is.na(lookup_rvPrAcr))
  )

# filter to fields not updated no match for review
fields_not_updated_no_match <- sjv_joined %>%
  st_drop_geometry() %>%
  filter(comm %in% idle_categories) %>%
  left_join(
    revenue_water_lookup,
    by = c("county" = "county", "last_comm" = "comm")
  ) %>%
  filter(!is.na(last_comm) & is.na(lookup_rvPrAcr))


# some fields with a last comm weren't updated because there was no match 
# in the lookup table (i.e., no revenue/water use data for that county-comm combo)
# we'll assign these last comms as NAs and estimate their values in the next script
sjv_final <- sjv_final %>%
  mutate(
    last_comm = case_when(
      comm %in% idle_categories & 
        !is.na(last_comm) & 
        rvPrAcr == 0 ~ NA_character_,
      TRUE ~ last_comm
    )
  )

# update summary test
update_summary_test <- sjv_final %>%
  st_drop_geometry() %>%
  filter(comm %in% idle_categories) %>%
  summarise(
    total_idle_fields = n(),
    fields_with_last_comm = sum(!is.na(last_comm)),
    fields_updated = sum(!is.na(last_comm) & !is.na(rvPrAcr)),
    fields_not_updated_no_last_comm = sum(is.na(last_comm)),
    # this should now be zero
    fields_not_updated_no_match = sum(!is.na(last_comm) & is.na(rvPrAcr))
  )


# export data
write_sf(sjv_final, 
         here("data/intermediate/misc/LandIQ_processing/2_lastCultivatedRevWater/sjvLastCultivatedRevWater.shp"), 
         append = FALSE)

















