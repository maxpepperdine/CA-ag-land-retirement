
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

# read in the df just created in cleanPlots 
kern <- read_sf("/Users/stellawing/Desktop/AgAnalysis/data/kernID.shp") |> 
  rename(comm = crp_ty_)



masterCrosswalk <- read_csv("/Users/stellawing/Desktop/AgAnalysis/data/masterCrosswalkMB.csv") # read in MB masterCrosswalk 


# Join Year and Crosswalk -------------------------------------------------

kernPrice <- kern %>% 
  left_join(masterCrosswalk, by = "comm")
