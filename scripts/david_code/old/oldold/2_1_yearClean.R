
# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(sf)
library(terra)

# Import Kern -------------------------------------------------------------

#Import Kern Data from shapefiles for 2015. We are filtering out livestock
#land (pasture and range), clipping to a USGS San Joaquin Valley shapefile
#and then deleting a few observations of fields that are inactive before
#1/1/15. 


#Kern Fields for 2015
kern2015Raw <- read_sf("./Data/0_input/kernReproject2015/kernReproject.shp") %>% 
  #Per conversation had with ashley 8/11 filter these out
  filter(COMM != "PASTURELAND" & COMM != "RANGELAND")

#SJV Shapefile
sjv <- read_sf("Data/0_input/pp1766_cvhm_texture_regions/cvhm_texture_regions.shp") %>% 
  st_transform(crs(kern2015Raw))


#Clip kern fields to SJV
kernNoID <- kern2015Raw %>% 
  st_filter(y = sjv, .predicate = st_intersects) %>% 
  #Convert to DT
  mutate(
    DT_ACT = date(DT_ACT),
    DT_INACT = date(DT_INACT)
  )  %>% 
  #5 rows have an inactive date before 1-1-15
  filter(DT_INACT > ymd("2014-12-31")) %>% 
  st_make_valid()



# Create unique ID based on field geometry
kernGeo <- kernNoID %>% 
  select(geometry) %>% 
  #This removes duplicate geometries
  distinct() 



kernGeo2%>% 
  #Create group number based on geometry
  mutate(geoGroup = row_number()) %>% 
  mutate(
    area = as.vector(st_area(.))
  )


kern2015 <- kernNoID %>% 
  st_join(kernGeo, join = st_equals)


write_sf(kern2015, "./Data/2_yearClean/kernID.shp")




