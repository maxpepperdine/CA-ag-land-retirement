
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(tmap)    # for static and interactive maps
library(lwgeom)


#Visualization
tmap_mode("view")
tmap_options(check.and.fix = TRUE)





# Metrics Function --------------------------------------------------------

source("./Scripts/0_startup/functions.R")



# Load in Data ------------------------------------------------------------

# Kern Year Data
kern <- read_sf("Data/4_cropRotation/kernYearRotation.shp")

# Updated Annual Tables
annualRate <- read_csv("Data/4_cropRotation/annualKey.csv")

# FID to GEo key

fidGeoKey <- read_csv("Data/5_geoGroupFIDKey/geoGroupFIDKey.csv")

# Iris Fields
fid <- read_sf("Data/5_geoGroupFIDKey/irisFieldsYear.shp") 


fidRetired <-  fid %>% 
  filter(YrsFllw > 3)


# Remove Slivers ----------------------------------------------------------


# Find perimeter and area of fields
kernPA <- kern %>% 
  mutate(
    perim = st_perimeter(.),
    area = st_area(.),
    ap = area / perim
  )


# Organize by descending area / perimeter
kernAP <- kernPA %>% 
  select(ap) %>% 
  st_drop_geometry() %>% 
  arrange(ap) %>% 
  mutate(
    ap = as.vector(ap)
  ) %>% 
  mutate(
    rank = row_number(),
    prop = ap / max(ap),
    round = rank / nrow(kernPA)
  ) 


# Value of bottom 2.5% 
cutoff <- kernAP %>% 
  filter(round < 0.025) %>% 
  {{max(.$ap)}}


# Filter Fid by ap value
apFid <- fid %>% 
  filter(duplicated(fidIris) == FALSE) %>% 
  mutate(
    perim = st_perimeter(.),
    area = st_area(.),
    ap = (area / perim ) %>% as.vector()
  ) 


fidFilter <- apFid %>% 
  filter(ap > cutoff)


fidLost <- apFid %>% 
  filter(ap <= cutoff)

fidLost %>% 
  mutate(
    area = area %>% as.numeric() / 10000
  ) %>% 
  {{sum(.$area)}}

noFilter77 <- apFid %>%
  filter(cmmcdFl == 77000)

filter77 <- fidFilter %>% 
  filter(cmmcdFl == 77000)



# Split Kern into fallow not fallow ---------------------------------------


kernNoFallow <- kern %>% 
  filter(fallow == 0) %>% 
  mutate(
    geoGroup = geoGrop,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    hrs = hrsYear,
    jobsPer100Acre = jbP100A,
    jobs = jobsYer,
    .keep = "unused"
  )

kernFallow <- kern %>% 
  filter(fallow == 1) 



kernNoFallow %>% 
  filter(COMM == "UNCULTIVATED AG")

# Get List of 77000 fields that don't intersect Kern Data -----------------

noPermitFallow <- filter77


# Join 77000 and 66000 ----------------------------------------------------

fallowMid <- kernFallow %>% 
  mutate(
    geoGroup = geoGrop,
    .keep = "unused"
  ) %>% 
  left_join(fidGeoKey, by = "geoGroup") %>%  
  left_join(annualRate, by = c("COMM.y" = "COMM")) %>% 
  #select(geoGroup, acres, geometry:annual.y) %>% 
  #select(-fidIris) %>% 
  mutate(
    #COMM.x = COMM,
    COMM = COMM.y,
    #annual = annual.y,
    hrsAcre = hrsAcre.y,
    .keep = "unused"
  ) %>% 
  select(-hrsAcre.x)


# Assign last cultivated crop to non permitted fields
noPermitMid <- noPermitFallow %>% 
  left_join(annualRate, by = c("cmmcdLA" = "comm_code")) %>% 
  arrange(fidIris, desc(pricePerAcre)) %>% 
  filter(duplicated(fidIris) == FALSE)  %>% 
  mutate(
    acres = st_area(.) %>% as.numeric() / 4047,
    fallow = 1,
    nonPermit = 1
  ) 


noPermitMid %>% 
  select(COMM, everything()) %>% 
  filter(COMM == "PASTURELAND" | COMM == "RANGELAND")




kernFArea <- sum(kernFallow$acres)

noPermitFArea <- sum(noPermitMid$acres)



# Combine 2015 uncultivated with non permitted fields
fallow <- fallowMid %>% 
  bind_rows(noPermitMid) %>% 
  mutate(
    revenue = pricePerAcre * acres,
    water = waterUse * acres,
    hrs = hrsAcre * acres,
    jobs = jobsPer100Acre * acres
  ) 



# Summary Statistics of Fallowing and Median Value ------------------------



summary(fallow$revenue)

# Median value of fallowed land (may want to adjust)

metrics(fallow$revenue)

metrics(fallow$pricePerAcre)


medianFallow <- median(fallow$pricePerAcre, na.rm = TRUE)


# Median water use of fallowed land (may want to adjust)
metrics(fallow$waterUse)

metrics(fallow$water)


medianWater <- median(fallow$waterUse, na.rm = TRUE)





# Median job of fallowed land per 100 acres
metrics(fallow$hrsAcre)
metrics(fallow$hrs)

medianHrs <- median(fallow$hrsAcre, na.rm = TRUE)




# Median job of fallowed land per 100 acres
metrics(fallow$jobsPer100Acre)
metrics(fallow$jobs)

medianJobs <- median(fallow$jobsPer100Acre, na.rm = TRUE)



# Clean Fallow table ------------------------------------------------------


fallowFinal <- fallow %>% 
  mutate(
    pricePerAcre = ifelse(is.na(pricePerAcre),
                          medianFallow,
                          pricePerAcre),
    waterUse = ifelse(is.na(waterUse),
                      medianWater,
                      waterUse),
    hrsAcre = ifelse(is.na(hrsAcre),
                          medianHrs,
                          hrsAcre),
    jobsPer100Acre = ifelse(is.na(jobsPer100Acre),
                            medianJobs,
                            jobsPer100Acre),
    revenue = pricePerAcre * acres,
    water = waterUse * acres,
    hrs = hrsAcre * acres,
    jobs = jobsPer100Acre * acres
  ) %>% 
  select(-c(annual))


fallowFinal %>% 
  filter(fallow != 1)



metrics(fallowFinal$revenue)

metrics(fallowFinal$hrs)

fallowArea <- fallowFinal %>% 
  select(fallow, nonPermit, COMM) %>% 
  mutate(
    area = st_area(.) / 10000,
    ha = as.numeric(area)
  )



allArea <- sum(fallowArea$ha)

pArea <- fallowArea %>% 
  filter(is.na(nonPermit)) %>% 
  {{sum(.$ha)}}

npArea <- fallowArea %>% 
  filter(nonPermit == 1) %>% 
  {{sum(.$ha)}}

npAllProp <- npArea / allArea



# Join Fallowed table to non fallowed table -------------------------------


joinFields <- fallowFinal %>% 
  bind_rows(kernNoFallow) %>% 
  mutate(
    # Give new id to include non permitted fields
    id = row_number(),
    # Give land cover type to uncultivated
    retired = if_else(is.na(COMM), 1, 0),
    COMM = if_else(is.na(COMM), "UNCULTIVATED", COMM),
    nonPermit = replace_na(nonPermit, 0)
  ) %>%
  select(id, geoGroup, acres, waterUse:COMM, 
         COMM.x, revenue, water, hrs, jobs, fallow, nonPermit, retired)



joinFields2 <- fallowFinal %>% 
  bind_rows(kernNoFallow) %>% 
  mutate(
    # Give new id to include non permitted fields
    id = row_number(),
    # Give land cover type to uncultivated
    retired = if_else(YrsFllw > 3, 1, 0),
    COMM = if_else(is.na(COMM), "UNCULTIVATED", COMM),
    nonPermit = replace_na(nonPermit, 0)
  ) %>%
  select(id, geoGroup, acres, waterUse:COMM, 
         COMM.x, revenue, water, hrs, jobs, fallow, nonPermit, retired)

# Export ------------------------------------------------------------------



write_sf(joinFields2, "Data/6_estimateFallowing/kernAddFallow.shp")

