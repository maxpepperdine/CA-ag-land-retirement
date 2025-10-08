
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
kern <- read_sf("Data/3_cropRotation/kernYearRotation.shp")

# Updated Annual Tables
annualRate <- read_csv("Data/3_cropRotation/annualKey.csv")

# FID to GEo key

fidGeoKey <- read_csv("Data/4_geoGroupFIDKey/geoGroupFIDKey.csv")

# Iris Fields
fid <- read_sf("Data/4_geoGroupFIDKey/irisFieldsYear.shp") 


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

# Map
kernPA %>% 
  arrange(ap) %>% 
  head(1) %>% 
  #st_buffer(10) %>% 
  tm_shape() + 
  tm_fill(col = "red") #+
tm_shape(kern) + 
  tm_borders()

# maxAP <- kernPA %>% {{max(.$ap)}}

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


# Mean value of bottom 5% 
# cutoff <- kernAP %>% 
#   filter(round < 0.05) %>% 
#   {{median(.$ap)}}

cutoff <- kernAP %>% 
  filter(round < 0.025) %>% 
  {{max(.$ap)}}

# Filter Fid by ap value

apFid <- fid %>% 
  filter(duplicated(fidIris) == FALSE) %>% 
  #select(fidIris) %>% 
  mutate(
    perim = st_perimeter(.),
    area = st_area(.),
    ap = (area / perim ) %>% as.vector()
  ) 


fidFilter <- apFid %>% 
  filter(ap > cutoff)


noFilter77 <- apFid %>%
  filter(cmmcdFl == 77000)

filter77 <- fidFilter %>% 
  filter(cmmcdFl == 77000) # %>% 
#filter(!is.na(cmmcdLA))


# Split Kern into fallow not fallow ---------------------------------------


kernNoFallow <- kern %>% 
  filter(fallow == 0) %>% 
  mutate(
    geoGroup = geoGrop,
    pricePerAcre = rvPrAcr,
    waterUse = wtrPrAc,
    revenue = revYear,
    water = waterYr,
    jobsPer100Acre = jbP100A,
    jobs = jobsYer,
    .keep = "unused"
  )

kernFallow <- kern %>% 
  filter(fallow == 1) 


# kernNoFallow %>% 
#   st_drop_geometry() %>% 
#   distinct(COMM) %>% 
#   view()


kernNoFallow %>% 
  filter(COMM == "UNCULTIVATED AG")

# Get List of 77000 fields that don't intersect Kern Data -----------------

noPermitFallow <- filter77

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
  left_join(fidGeoKey, by = "geoGroup") %>%  
  left_join(annualRate, by = c("COMM.y" = "COMM")) %>% 
  #select(geoGroup, acres, geometry:annual.y) %>% 
  #select(-fidIris) %>% 
  mutate(
    #COMM.x = COMM,
    COMM = COMM.y,
    #annual = annual.y,
    .keep = "unused"
  ) 



# 
# fallowMid %>% 
#   filter(annual.x != annual.y) %>% 
#   view()

# # Test to find uncultivated fields not matching a last cultivated crop
# fallowMid %>% colnames()
# fallowMid %>% filter(is.na(COMM))
# fallowMid %>% 
#   st_drop_geometry() %>% 
#   distinct(COMM) %>% 
#   view()

#Assign last cultivated crop to non permitted fields
noPermitMid <- noPermitFallow %>% 
  left_join(annualRate, by = c("cmmcdLA" = "comm_code")) %>% 
  arrange(fidIris, desc(pricePerAcre)) %>% 
  filter(duplicated(fidIris) == FALSE)  %>% 
  #select(geometry:COMM) %>% 
  mutate(
    acres = st_area(.) %>% as.numeric() / 4047,
    fallow = 1,
    nonPermit = 1
  ) 


noPermitMid %>% 
  #distinct(COMM) %>% 
  filter(COMM == "PASTURELAND" | COMM == "RANGELAND")


fallowMid %>% 
  distinct(COMM) %>% 
  view()
  
  
  
noPermitMid %>% filter(is.na(COMM))

kernFArea <- sum(kernFallow$acres)

noPermitFArea <- sum(noPermitMid$acres)



# Combine 2015 uncultivated with non permitted fields
fallow <- fallowMid %>% 
  bind_rows(noPermitMid) %>% 
  mutate(
    revenue = pricePerAcre * acres,
    water = waterUse * acres,
    jobs = jobsPer100Acre * acres
  ) 

fallow %>% 
  filter(pricePerAcre %>% is.na())



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
    jobsPer100Acre = ifelse(is.na(jobsPer100Acre),
                          medianJobs,
                          jobsPer100Acre),
    revenue = pricePerAcre * acres,
    water = waterUse * acres,
    jobs = jobsPer100Acre * acres
  ) %>% 
  select(-c(annual))


fallowFinal %>% 
  filter(fallow != 1)



metrics(fallowFinal$revenue)



tm_shape(fallowFinal) +
  tm_fill(col = "red") +
  tm_shape(kernNoFallow) +
  tm_borders()


kernNoFallow %>% 
  filter(fallow == 1)



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

# test <- fallowFinal %>% 
#   bind_rows(kernNoFallow)
# 
# test %>% 
#   filter(!is.na(YrsFllw)) 
# 
# 
# fallowFinal %>% 
#   filter(!is.na(YrsFllw))

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
         COMM.x, revenue, water, jobs, fallow, nonPermit, retired)



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
         COMM.x, revenue, water, jobs, fallow, nonPermit, retired)



joinFields %>% 
  filter(retired == 1) %>% 
  tm_shape() +
  tm_fill(col = "red")


joinFields2 %>% 
  filter(retired == 1) %>% 
  tm_shape() +
  tm_fill(col = "red")


# 
# fallowMid %>% 
#   filter(is.na(COMM))
# 
# noPermitMid %>% 
#   filter(is.na(COMM)) %>% 
#   glimpse()
# 
# 
# joinFields %>% 
#   filter(
#     testPermit == 1
#   ) %>% 
#   view()
# %>% 
#   filter(
#     is.na(COMM.x)
#   ) %>% 
#   view()
# # Check Data for NA's -----------------------------------------------------
# 
# 
# 
# 
# kernFallow %>%
#   filter(is.na(COMM)) %>%
#   view()
# 
# fallowMid %>%
#   filter(is.na(COMM)) %>%
#   view()
# 
# noPermitMid %>%
#   filter(is.na(COMM)) %>%
#   view()
# 
# Fields Missing COMM
# joinFields %>%
#   filter(is.na(COMM)) %>%
#   glimpse()
# 
# # Fields Missing Water
# joinFields %>%
#   filter(is.na(water)) %>%
#   glimpse()
# 
# # Fields Missing Revenue
# joinFields %>%
#   filter(is.na(revenue)) %>%
#   glimpse()
# 
# # Fields Missing Fallow
# joinFields %>%
#   filter(is.na(fallow))
# 
# # Fields missing Jobs
# joinFields %>% 
#   filter(is.na(jobs))

# Export ------------------------------------------------------------------

joinFields %>% 
  st_drop_geometry() %>% 
  distinct(COMM) %>% 
  view()


write_sf(joinFields2, "Data/5_estimateFallowing/kernAddFallow.shp")


# Appendix ----------------------------------------------------------------


# tm_shape(noFilter77) + 
#   tm_borders() + 
#   tm_shape(filter77) + 
#   tm_fill(col = "red")
# 
#
# 
# 
# sliverInfo <- fid %>% 
#   left_join(sliver, by = "fidIris") %>% 
#   filter(duplicated(fidIris) == FALSE)
# 
# 
# 
# sliverInfo %>% pull(fidIris) %>% length()
# 
# 
# sliverInfo %>% pull(fidIris) %>% unique() %>% length()
# 
# bufferDrop <- sliverInfo %>% 
#   filter(BufferDropped == 0) 
# 
# 
# 
# bufferDropMap <- bufferDrop %>% 
#   #filter(fidIris < 10000)
#   st_union()
# 
# bufferKeep <- sliverInfo %>% 
#   filter(BufferDropped == 1) 
# 
# 
# bufferKeepMap <- bufferKeep %>% 
#   #filter(fidIris < 10000)
#   st_union()
# 
# 
# bufferKeepMap %>% st_area()
# 
# 
# 
# tm_shape(bufferKeepMap) + tm_fill(col = "red") +
#   tm_shape(bufferDropMap) + tm_borders() + tm_legend()
# 
# 
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

