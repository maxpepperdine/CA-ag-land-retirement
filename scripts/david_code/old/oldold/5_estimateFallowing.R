
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(tigris)
library(readxl)


#Visualization
tmap_mode("view")
tmap_options(check.and.fix = TRUE)





# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")



# Load in Data ------------------------------------------------------------

# Kern Year Data
kern <- read_sf("Data/3_cropRotation/kernYearRotation.shp")

# Updated Annual Tables
annualRate <- read_csv("Data/3_cropRotation/annualKey.csv")

# FID to GEo key

fidGeoKey <- read_csv("Data/4_geoGroupFIDKey/geoGroupFIDKey.csv")

# Iris Fields
fid <- read_sf("Data/4_geoGroupFIDKey/irisFieldsYear.shp") 


# Split Kern into fallow not fallow ---------------------------------------


kernNoFallow <- kern %>% 
  filter(fallow == 0) %>% 
  mutate(
    geoGroup = geoGrop,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    .keep = "unused"
  )

kernFallow <- kern %>% 
  filter(fallow == 1) 


# Get List of 77000 fields that don't intersect Kern Data -----------------

noPermitFallow <- fid %>% 
  filter(cmmcdFl == 77000)

# # Test to find 77000 fields that touch kern. I ran analysis earlier that said
# # there were some fields with this behavior (which didn't make much sense) but
# # apparently not
# noPermitFallow %>%
#   st_filter(kern, .predicate = st_disjoint)


# Join 77000 and 66000 ----------------------------------------------------

fallowMid <- kernFallow %>% 
  mutate(
    geoGroup = geoGrop,
    .keep = "unused"
  ) %>% 
  select(
    geoGroup, acres, fallow
  ) %>% 
  left_join(fidGeoKey, by = "geoGroup") %>% 
  left_join(annualRate, by = "COMM") %>% 
  select(geoGroup, acres:annual) %>% 
  select(-fidIris)



noPermitMid <- noPermitFallow %>% 
  left_join(annualRate, by = c("cmmcdLA" = "comm_code")) %>% 
  arrange(fidIris, desc(pricePerAcre)) %>% 
  filter(duplicated(fidIris) == FALSE)  %>% 
  select(geometry:COMM) %>% 
  mutate(
    acres = st_area(.) %>% as.numeric() / 4047,
    fallow = 1
  ) 



kernFArea <- sum(kernFallow$acres)

noPermitFArea <- sum(noPermitMid$acres)

fallow <- fallowMid %>% 
  bind_rows(noPermitMid) %>% 
  mutate(
    revenue = pricePerAcre * acres,
    water = waterUse * acres
  ) 

fallow %>% 
  filter(pricePerAcre %>% is.na())

summary(fallow$revenue)


metrics(fallow$revenue)

metrics(fallow$pricePerAcre)

medianFallow <- median(fallow$pricePerAcre, na.rm = TRUE)

metrics(fallow$waterUse)

metrics(fallow$water)


medianWater <- median(fallow$waterUse, na.rm = TRUE)

fallowFinal <- fallow %>% 
  mutate(
    pricePerAcre = ifelse(is.na(pricePerAcre),
                 medianFallow,
                 pricePerAcre),
    waterUse = ifelse(is.na(waterUse),
                   medianWater,
                   waterUse),
    revenue = pricePerAcre * acres,
    water = waterUse * acres
  ) %>% 
  select(-c(annual))


joinFields <- fallowFinal %>% 
  bind_rows(kernNoFallow) %>% 
  mutate(
    id = row_number()
  )


# %>% 
#   arrange(geoGroup, desc(revenue)) %>% 
#   filter(dup)
# 
# 
# joinFields %>% 
#   filter(
#     !is.na(geoGroup)
#   ) %>% 
#   pull(geoGroup) %>% 
#   unique() %>% 
#   length()




write_sf(joinFields, "Data/5_estimateFallowing/kernAddFallow.shp")


# 
# fallowFinal %>% 
#   filter(is.na(revenue))


# 
# 
# fidGeoKey %>% 
#   filter(is.na(COMM))
# 
# 
# fallowMid %>% 
#   filter(is.na(COMM))

