
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)
library(spData)
#library(spDataLarge)
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


# Import Kern Year Data ---------------------------------------------------

kern <- read_sf("./Data/3_cleanPlots/kernID.shp")


# Read Master Crosswalk ---------------------------------------------------



masterCrosswalk <- read_csv("Data/2_masterCrosswalk/masterCrosswalk.csv")


# Join Year and Crosswalk -------------------------------------------------

kernPrice <- kern %>% 
  left_join(masterCrosswalk, by = "COMM")



# End of Year Geometry ----------------------------------------------------


# Filter plots present at the end of year, arrange by the most valuable 
endDate <- kernPrice %>% 
  filter(DT_INACT == "2015-12-31") %>% 
  mutate(
    # Assign fallowed plots annual status for ease of processing
    annual = if_else(is.na(annual), 1, annual)
  ) %>% 
  group_by(geoGroup, annual) %>% 
  arrange(desc(pricePerAcre)) %>% 
  filter(duplicated(geoGroup) == FALSE) 



# Get Most Valuable End of Year Crops -------------------------------------

# Add ACRES Column to each plot
kern2015Group <- endDate %>% 
  mutate(
    #Fix acres data column
    ACRES = round(as.numeric(Shape_Area) / 4047 , digits = 2)
  ) %>% 
  st_drop_geometry()


# Create Annual Table

annual <- kern2015Group %>% 
  select(geoGroup, everything()) %>% 
  group_by(geoGroup) %>% 
  filter(annual == 1) %>% 
  arrange(geoGroup, desc(DT_INACT)) 


# Add endCrop column
# Create separate table

endCropKey <- kern2015Group %>% 
  select(geoGroup, everything()) %>% 
  group_by(geoGroup) %>% 
  filter(annual == 1) %>% 
  arrange(geoGroup, desc(DT_INACT)) %>% 
  filter(duplicated(geoGroup) == FALSE) %>% 
  select(geoGroup, COMM) %>% 
  mutate(
    endCrop = COMM,
    .keep = 'unused'
  )


# Drop Duplicate Annual Crops ---------------------------------------------


# Create double cultivated vector 
# These are crops that can be cultivated twice within one year according 
# to Kern Ag Reports

doubleCult <- kern2015Group %>% 
  pull(COMM) %>% 
  unique() %>% 
  sort() %>% 
  str_subset("LETTUCE") %>% 
  c("POTATO", "CARROT", "BROCCOLI", "BEAN SUCCULENT")



# Drop records of more than 3 crops, and then drop any duplicated
# COMMs except for those in the double Cult list. 

dropDuplicates <- annual %>% 
  left_join(endCropKey, by = "geoGroup") %>% 
  group_by(geoGroup, endCrop, COMM) %>%
  mutate(
    count = row_number()
  ) %>% 
  filter(count < 3) %>% 
  filter(duplicated(COMM) == FALSE | COMM %in% doubleCult)





# Summarize Annual Rotations by Plot --------------------------------------


# Find Annual Value for each plot


# Summarize to get key of average rev and water use rates by ending crops
joinAnnualKey <- dropDuplicates %>% 
  group_by(endCrop, geoGroup) %>% 
  # For each plot sum revenue rates
  summarize(
    sumRev = sum(pricePerAcre, na.rm = TRUE),
    sumWater = sum(waterUse, na.rm = TRUE),
    sumHrs = sum(hrsAcre, na.rm = TRUE),
    sumJobs = sum(jobsPer100Acre),
    area = mean(area)
  ) %>%
  # Leave each plot unique rates
  mutate(
    # names are inaccurate to facilitate ease of code
    # These are not means, they are sums of different rotations
    meanRev = sumRev,
    meanWater = sumWater,
    meanHrs = sumHrs,
    meanJobs = sumJobs,
    .keep = "unused"
  )



# Get average values for all crops to estimate fallowed value


joinAnnualKeyMean <- dropDuplicates %>% 
  group_by(endCrop, geoGroup) %>% 
  # For each plot sum revenue rates
  summarize(
    sumRev = sum(pricePerAcre),
    sumWater = sum(waterUse),
    sumHrs = sum(hrsAcre),
    sumJobs = sum(jobsPer100Acre),
    area = mean(area)
  ) %>%
  # For each ending crop, find area-weighted average of all plots
  summarize(
    meanRev = weighted.mean(sumRev, area),
    meanWater = weighted.mean(sumWater, area),
    meanHrs = weighted.mean(sumHrs, area),
    meanJobs = weighted.mean(sumJobs, area)
  )

# Update master crosswalk with rotated annual rates
revenueRotationKey <- masterCrosswalk %>% 
  left_join(joinAnnualKeyMean, by = c("COMM" = "endCrop")) %>% 
  mutate(
    pricePerAcre = ifelse(is.na(meanRev), 
                          pricePerAcre,
                          meanRev),
    waterUse = ifelse(is.na(meanWater),
                      waterUse,
                      meanWater),
    hrsAcre = ifelse(is.na(meanHrs),
                            hrsAcre,
                            meanHrs),
    jobsPer100Acre = ifelse(is.na(meanJobs),
                            jobsPer100Acre,
                            meanJobs),
  ) %>% 
  select(-c(meanRev, meanWater, meanHrs, meanJobs))


# Export 
write_csv(revenueRotationKey, "./Data/4_cropRotation/annualKey.csv")


# Join Crop rotation table and clean --------------------------------------



# Join annual rates with endDate table and update rates of annuals
cropRotationRaw <- endDate %>% 
  left_join(joinAnnualKey, by = c("geoGroup",
                                  "COMM" = "endCrop"))  %>% 
  mutate(
    revPart = if_else(is.na(meanRev), pricePerAcre * ACRES, meanRev * ACRES),
    waterPart = if_else(is.na(meanWater), waterUse * ACRES, meanWater * ACRES),
    hrsPart = if_else(is.na(meanHrs), hrsAcre * ACRES, meanHrs * ACRES),
    jobsPart = if_else(is.na(meanJobs), jobsPer100Acre * ACRES, meanJobs * ACRES),
    #add fallow marker for estimating fallowed field values
    fallow = if_else(COMM == "UNCULTIVATED AG", TRUE, FALSE),
  ) %>% 
  select(-c(pricePerAcre, meanRev, waterUse, meanWater, 
            hrsAcre, meanHrs, jobsPer100Acre, meanJobs, area.x, area.y)) %>% 
  select(COMM, ACRES, geoGroup:fallow) %>% 
  group_by(geoGroup) 


# Summarize for each field ------------------------------------------------


# Kern Geometry with ID

kernGeo <- kern %>% 
  select(geoGroup) %>% 
  filter(duplicated(geoGroup) == FALSE)



#Add annual and perennial revenue / water data
cropRotation <- cropRotationRaw %>% 
  st_drop_geometry() %>% 
  ungroup() %>%
  group_by(geoGroup) %>% 
  # Choose either annual or perennial (most valuable)
  arrange(geoGroup, desc(revPart)) %>% 
  filter(duplicated(geoGroup) == FALSE) %>% 
  # Clean columns and update per acre values
  mutate(
    revYear = revPart,
    waterYear = waterPart,
    hrsYear = hrsPart,
    jobsYear = jobsPart,
    acres = ACRES,
    revPerAcre = revYear / acres,
    waterPerAcre = waterYear / acres,
    hrsAcre = hrsYear / acres,
    jobsPer100Acre = jobsYear / acres,
    .keep = "unused"
  ) %>%
  left_join(kernGeo, by = "geoGroup") %>%
  st_as_sf()

new_df <- cropRotation %>% select(-hrsYear, -jobsYear, hrsAcre)


# Check values across all plots
metrics(cropRotation$revPerAcre)

metrics(cropRotation$revYear)

metrics(cropRotation$waterYear)

metrics(cropRotation$waterPerAcre)

metrics(cropRotation$hrsYear)

metrics(cropRotation$hrsAcre)

metrics(cropRotation$jobsYear)

metrics(cropRotation$jobsPer100Acre)

# Export
write_sf(cropRotation, "./Data/4_cropRotation/kernYearRotation.shp")


