# clear environment
rm(list=ls())

# Load Packages -----------------------------------------------------------

library(sf)
library(lubridate)
library(tidyverse)
library(terra)
library(spData)
library(tigris)
library(readxl)
library(here)
library(janitor)


# Metrics Function --------------------------------------------------------

source(here("scripts/FallowFoxes_SJV/0_startup/functions.R"))


# read in the .shp created in 2_cleanPlotsLandIQ_2016.R 
sjv <- read_sf(here("data/intermediate/misc/LandIQ_processing/2016/2_cleanPlotsLandIQ_2016/SJVID_2016/SJVID_2016.shp")) %>% 
  clean_names()


# rename columns; sf and dplyr don't always like the rename() function
names(sjv)[names(sjv) == "crp_ty"] <- "comm"


# count the number of duplicated observations in the unique id field
dupCounts <- sjv %>% 
  group_by(geo_grp) %>% 
  summarize(count = n()) %>% 
  filter(count > 1) %>% 
  summarize(total_dups = sum(count - 1))


# keep only the most recently cultivated crop for each unique plot
# these are all crptyp == croptyp2 observations
# LandIQ metadata indicates that crptyp2 is the main season crop for 2016 data
sjv_lastCrop <- sjv %>% 
  filter(crptyp == "croptyp2")


# Quick QC (1) -- the # observations in dupCounts + the # of unique plots should equal the total # of observations in the original sjv dataset
dupCounts$total_dups + nrow(sjv_lastCrop) == nrow(sjv)
# Quick QC (2) -- confirm there's one row per unique field ID
nrow(sjv_lastCrop) == n_distinct(sjv$geo_grp)


# examine all NA observations in comm variable
comm_na <- sjv_lastCrop %>% 
  filter(is.na(comm)) %>% 
  distinct(crp2016, .keep_all = TRUE)


# clean up the dataset to fix inconsistencies in comm codes
sjv_lastCrop <- sjv_lastCrop %>%
  mutate(
    comm = if_else(
      crp2016 %in% c(
        "Tomatoes",
        "Lettuce/Leafy Greens",
        "Miscellaneous Grain and Hay",
        "Rice",
        "Wheat"
      ),
      crp2016,
      comm
    )
  ) %>% 
  # old LandIQ lumped plums, prunes, and apricots into one category
  # we'll assign these to apricots for our purposes 
  mutate(
    comm = case_when(
      crp2016 == "Plums, Prunes and Apricots" ~ "Apricots",
      TRUE ~ comm
    )
  ) %>% 
  # old LandIQ lumped potatoes and sweet potatoes into one category
  # we'll assign these to sweet potatoes for our purposes
  mutate(
    comm = case_when(
      crp2016 == "Potatoes and Sweet Potatoes" ~ "Sweet Potatoes",
      TRUE ~ comm
    )
  ) %>%
  mutate(
    comm = case_when(
      crp2016 == "Idle" ~ "Unclassified Fallow",
      TRUE ~ comm
    )
  ) %>%
  # drop all rows where crop2016 is Sunflowesr
  # these should've been removed in clean plot step
  filter(crp2016 != "Sunflowers")

# re-examine all NA observations in comm variable
comm_na <- sjv_lastCrop %>% 
  filter(is.na(comm)) %>% 
  distinct(crp2016, .keep_all = TRUE)


# Export ----------------------------------------------------------------

write_sf(sjv_lastCrop, 
         here("data/intermediate/misc/LandIQ_processing/2016/5_cropRotation_2016/sjvYearRotation/sjvYearRotation_2016.shp"), 
         append = FALSE)




























