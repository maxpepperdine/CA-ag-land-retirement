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


# read in the .shp created in 2_cleanPlotsLandIQ_2020.R 
sjv <- read_sf(here("data/intermediate/misc/2020/2_cleanPlotsLandIQ_2020/SJVID_2020/SJVID_2020.shp")) %>% 
  clean_names()


# rename columns; sf and dplyr don't always like the rename() function
names(sjv)[names(sjv) == "crp_ty"] <- "comm"


# count the number of duplicated observations in the unique id field
dupCounts <- sjv %>% 
  group_by(uniqu_d) %>% 
  summarize(count = n()) %>% 
  filter(count > 1) %>% 
  summarize(total_dups = sum(count - 1))


# keep only the most recently cultivated crop for each unique plot
# we'll use the dt_nctv field to determine this
sjv_lastCrop <- sjv %>% 
  group_by(uniqu_d) %>% 
  arrange(uniqu_d, desc(dt_nctv)) %>% 
  filter(duplicated(uniqu_d) == FALSE)


# Quick QC (1) -- the # observations in dupCounts + the # of unique plots should equal the total # of observations in the original sjv dataset
dupCounts$total_dups + nrow(sjv_lastCrop) == nrow(sjv)
# Quick QC (2) -- confirm there's one row per unique field ID
nrow(sjv_lastCrop) == n_distinct(sjv$uniqu_d)



# Export ----------------------------------------------------------------

write_sf(sjv_lastCrop, 
         here("data/intermediate/misc/2020/5_cropRotation_2020/sjvYearRotation/sjvYearRotation_2020.shp"), 
         append = FALSE)




























