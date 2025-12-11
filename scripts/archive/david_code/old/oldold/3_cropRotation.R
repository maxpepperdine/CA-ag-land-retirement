
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


# Import Kern Year Data ---------------------------------------------------

kern <- read_sf("./Data/2_yearClean/kernID.shp")


# Read Water Crosswalk ----------------------------------------------------


revWaterCrosswalk <- read_csv("Data/1_masterCrosswalk/masterCrosswalk.csv")


# Join Year and Crosswalk -------------------------------------------------

kernPrice <- kern %>% 
  left_join(revWaterCrosswalk, by = "COMM")



# End of Year Geometry ----------------------------------------------------


#Filter sites present at the end of year, arrange by the most valuable 
endDate <- kernPrice %>% 
  filter(DT_INACT == "2015-12-31") %>% 
  group_by(geoGroup, annual) %>% 
  arrange(desc(pricePerAcre)) %>% 
  filter(duplicated(geoGroup) == FALSE) 




# Get Most Valuable End of Year Crops -------------------------------------

#Add ACRES Column to each field
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





# Summarize Annual Rotations by Field -------------------------------------




#Summarize to get key of average rev and water use rates by ending crops
joinAnnualKey <- dropDuplicates %>% 
  group_by(endCrop, geoGroup) %>% 
  #For each field sum revenue rates
  summarize(
    sumRev = sum(pricePerAcre, na.rm = TRUE),
    sumWater = sum(waterUse),
    area = mean(area)
  ) %>%
  # Leave each field unique rates
  mutate(
    #names are inaccurate to facilitate ease of code
    #These are not means, they are sums of different rotations
    meanRev = sumRev,
    meanWater = sumWater,
    .keep = "unused"
  )






joinAnnualKeyMean <- dropDuplicates %>% 
  group_by(endCrop, geoGroup) %>% 
  #For each field sum revenue rates
  summarize(
    sumRev = sum(pricePerAcre),
    sumWater = sum(waterUse),
    area = mean(area)
  ) %>%
  #For each ending crop, find area-weighted average of all fields
  summarize(
    meanRev = weighted.mean(sumRev, area),
    meanWater = weighted.mean(sumWater, area)
  )


revenueRotationKey <- revWaterCrosswalk %>% 
  left_join(joinAnnualKeyMean, by = c("COMM" = "endCrop")) %>% 
  mutate(
    pricePerAcre = ifelse(is.na(meanRev), 
                          pricePerAcre,
                          meanRev),
    waterUse = ifelse(is.na(meanWater),
                      waterUse,
                      meanWater)
  ) %>% 
  select(-c(meanRev, meanWater))



ggplot(revenueRotationKey, mapping = aes(x = pricePerAcre, y = waterUse)) +
  geom_point(mapping = aes(color = as.logical(annual)))





# Check to see if there are any uncultivated crops left 
revenueRotationKey %>% colnames()

revenueRotationKey %>% 
  filter(COMM == "UNCULTIVATED AG")




write_csv(revenueRotationKey, "./Data/3_cropRotation/annualKey.csv")


# Join Crop rotation table and clean --------------------------------------



#Join annual rates with endDate table and update rates of annuals
cropRotationRaw <- endDate %>% 
  #If switching between endCrop summary and each field comment "geoGroup"
  left_join(joinAnnualKey, by = c("geoGroup",
                                  "COMM" = "endCrop"))  %>% 
  mutate(
    revPart = if_else(is.na(meanRev), pricePerAcre * ACRES, meanRev * ACRES),
    waterPart = if_else(is.na(meanWater), waterUse * ACRES, meanWater * ACRES),
    #add fallow marker for estimating fallowed field values
    fallow = if_else(COMM == "UNCULTIVATED AG", TRUE, FALSE)
  ) %>% 
  group_by(geoGroup) 


cropRotationRaw %>% 
  select(geoGroup, COMM, pricePerAcre:meanWater) %>% 
  view("Crop Rotation Check")


trouble <- cropRotationRaw %>% 
  filter(geoGroup == 6965)


# Summarize for each field ------------------------------------------------


# Kern Geometry with ID

kernGeo <- kern %>% 
  select(geoGroup) %>% 
  filter(duplicated(geoGroup) == FALSE)



#Add annual and perennial revenue / water data
cropRotation <- cropRotationRaw %>% 
  # trouble %>% 
  st_drop_geometry() %>% 
  summarise(
    revYear = max(revPart, na.rm = TRUE),
    waterYear = max(waterPart, na.rm = TRUE),
    acres = mean(ACRES),
    revPerAcre = revYear / acres,
    waterPerAcre = waterYear / acres,
    fallow = all(fallow),
    COMM = COMM
  ) %>% 
  left_join(kernGeo, by = "geoGroup") %>% 
  st_as_sf()

# metrics(cropRotation$revPerAcre)
# 
# metrics(cropRotation$revYear)
# 
missing <- cropRotation %>% 
  filter(is.na(revPerAcre)) %>% 
  pull(geoGroup)


cropRotation %>% 
  filter(geoGroup %in% missing) %>% 
  view()

cropRotation %>% 
  filter(COMM == ("WATERMELON") | COMM == ("ASPARAGUS")) %>% 
  view()



# Try to find out why 6965 is being such a little bitch
cropRotation %>% 
  filter(geoGroup == 6965) %>% 
  view()

uncultCodes <- cropRotation %>% 
  filter(COMM == "UNCULTIVATED AG") %>% 
  pull(geoGroup) %>% 
  unique()

cropRotation %>% 
  filter(geoGroup %in% uncultCodes) %>% 
  view()



write_sf(cropRotation, "./Data/3_cropRotation/kernYearRotation.shp")


