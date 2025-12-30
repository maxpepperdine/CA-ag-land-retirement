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


# read in the .shp created in 2_cleanPlotsLandIQ_2014.R 
sjv <- read_sf(here("data/intermediate/misc/LandIQ_processing/2014/2_cleanPlotsLandIQ_2014/SJVID_2014/SJVID_2014.shp")) %>% 
  clean_names()


# rename columns; sf and dplyr don't always like the rename() function
names(sjv)[names(sjv) == "crp_ty"] <- "comm"



# examine all NA observations in comm variable
comm_na <- sjv %>% 
  filter(is.na(comm)) %>% 
  distinct(crp2014, .keep_all = TRUE)


# clean up the dataset to fix inconsistencies in comm codes
sjv_lastCrop <- sjv %>%
  mutate(
    comm = case_when(
      crp2014 == "Alfalfa and Alfalfa Mixtures" ~ "Alfalfa & Alfalfa Mixtures",
      crp2014 == "Plums, Prunes and Apricots" ~ "Apricots",
      crp2014 == "Potatoes and Sweet Potatoes" ~ "Sweet Potatoes",
      crp2014 == "Wild Rice" ~ "Rice",
      crp2014 == "Idle" ~ "Unclassified Fallow",
      TRUE ~ comm
    )
  ) %>%
  # drop all rows where crop2014 is Sunflowesr
  # these should've been removed in clean plot step
  filter(crp2014 != "Sunflowers")

# re-examine all NA observations in comm variable
comm_na <- sjv_lastCrop %>% 
  filter(is.na(comm)) %>% 
  distinct(crp2014, .keep_all = TRUE)


# Export ----------------------------------------------------------------

write_sf(sjv_lastCrop, 
         here("data/intermediate/misc/LandIQ_processing/2014/5_cropRotation_2014/sjvYearRotation/sjvYearRotation_2014.shp"), 
         append = FALSE)




























