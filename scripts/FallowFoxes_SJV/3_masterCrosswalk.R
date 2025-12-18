# clear environment
rm(list=ls())

# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(sf)


# Water crosswalk -------------------------------------------------------


# Join 2022 SJV Plot crop types (LandIQ) to CADWR water use crop types

# SJV Plots from LandIQ
commW <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", 
           "Kiwis", "Apples", "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", 
           "Pomegranates", "Pecans", "Apricots", "Cherries", "Peaches/Nectarines", 
           "Pears", "Plums", "Prunes", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
           "Corn, Sorghum and Sudan", "Safflower", "Sugar beets", "Wheat", 
           "Miscellaneous Grain and Hay", "Idle – Short Term", "Idle – Long Term", 
           "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", 
           "Turf Farms", "Rice", "Onions and Garlic", "Potatoes", "Sweet Potatoes", 
           "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", 
           "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
           "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", 
           "Grapes", "Unclassified Fallow", "Young Perennials")

# CADWR Crops
cropW <- c("Citrus & Subtropical", NA, "Citrus & Subtropical", "Citrus & Subtropical", 
           "Citrus & Subtropical", "Citrus & Subtropical", "Other Deciduous", "Other Deciduous", 
           "Other Deciduous", "Almonds & Pistachios", "Other Deciduous", "Almonds & Pistachios", 
           "Other Deciduous", "Other Deciduous", "Other Deciduous", "Other Deciduous", 
           "Other Deciduous", "Other Deciduous", "Other Deciduous", "Other Deciduous", 
           "Cotton", "Dry Beans", "Other Field Crops", "Corn", "Safflower", 
           "Sugar Beet", "Grain", "Grain", NA, NA, "Alfalfa", "Pasture", 
           "Pasture", "Pasture", "Rice", "Onions & Garlic", "Potatoes", "Potatoes", 
           "Other Field Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           NA, "Truck Crops", "Tomato Fresh", "Truck Crops", "Truck Crops", "Cucurbits", 
           "Vineyard", NA, "Truck Crops")

# enframe to data table
waterCross <- enframe(commW, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(cropW, name = NULL, value = "Crop"))



# Revenue Crosswalk -------------------------------------------------------


# Join 2022 SJV Plot crop types (LandIQ) to USDA NASS crop revenue categories

# SJV Plots from LandIQ
commR <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", 
           "Kiwis", "Apples", "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", 
           "Pomegranates", "Pecans", "Apricots", "Cherries", "Peaches/Nectarines", 
           "Pears", "Plums", "Prunes", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
           "Corn, Sorghum and Sudan", "Safflower", "Sugar beets", "Wheat", 
           "Miscellaneous Grain and Hay", "Idle – Short Term", "Idle – Long Term", 
           "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", 
           "Turf Farms", "Rice", "Onions and Garlic", "Potatoes", "Sweet Potatoes", 
           "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", 
           "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
           "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", 
           "Grapes", "Unclassified Fallow", "Young Perennials")

# Adjusted CAC Crops (v2) (using all USDA NASS current crop names, not legacy)
cropR <- c("Oranges, All", "Forest Products, Misc", "Dates", "Avocados", "Olives", 
           "Grapefruit", "Kiwifruit", "Apples", NA, "Almonds, Nuts", 
           "Walnuts", "Pistachios", "Pomegranates", "Pecans", "Apricots", 
           "Cherries", "Peaches, All", "Pears, All", "Plums", "Prunes", "Cotton, Lint, All", 
           "Beans, All", "Hay, Grain, Misc", "Corn, Silage", "Safflower", "Sugar Beets", 
           "Wheat, Grain", "Hay, Grain, Misc", NA, NA, "Alfalfa, Hay", "Pasture, All", "Ryegrass", 
           "Horticulture, Sod/Turf", "Rice, Excluding Wild", "Onions, Dry", "Potatoes", "Sweet Potatoes", 
           "Horticulture, All", "Vegetables, Misc", "Berries, Blueberries", "Berries, Strawberries, All", 
           "Peppers, Bell", "Horticulture, All", "Lettuce, Head", "Tomatoes, Processing", 
           "Brussels Sprouts", "Carrots", "Cucumbers", "Grapes, All", NA, "Ryegrass")


# Enframe to data frame
revCross <- enframe(commR, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(cropR, name = NULL, value = "NASS"))



# Annual Perennial Crosswalk ----------------------------------------------

# Join 2022 SJV Plot crop types (LandIQ) to Annual / Perennial Info Table

# SJV Plots from LandIQ
commAP <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", 
            "Kiwis", "Apples", "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", 
            "Pomegranates", "Pecans", "Apricots", "Cherries", "Peaches/Nectarines", 
            "Pears", "Plums", "Prunes", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
            "Corn, Sorghum and Sudan", "Safflower", "Sugar beets", "Wheat", 
            "Miscellaneous Grain and Hay", "Idle – Short Term", "Idle – Long Term", 
            "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", 
            "Turf Farms", "Rice", "Onions and Garlic", "Potatoes", "Sweet Potatoes", 
            "Flowers, Nursery and Christmas Tree Farms", "Miscellaneous Truck Crops", 
            "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
            "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", 
            "Grapes",  "Unclassified Fallow", "Young Perennials")

# AP crop values (perennial = 0, annual = 1)
ap <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
        NA, NA, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, NA, 1)


# Enframe to data frame
annualCross <- enframe(commAP, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(ap, name = NULL, value = "annual"))



# Revenue Crosswalk + Values ----------------------------------------------


# Read in table of crops with revenue $ / acre created in the cropRevenueCrosswalk.R script. 
# The crosswalk joins revenue crop categories to LandIQ field crop categories


#Read in revenue table 
revenueRaw <- read_csv(here("data/intermediate/1_cropRevenueCrosswalk/final_revenue_sjv.csv"))

revenue <- revenueRaw %>%
  dplyr::select(crop, county, price_per_acre) %>% 
  mutate(
    NASS = crop,
    .keep = "unused"
  )




# Water Crosswalk + Values ------------------------------------------------


# Read in table of crops with water use from CADWR
# Counties of interest were selected from the full water use dataset
# The crosswalk joins water use crop categories to LandIQ field crop categories

# Read in water use data
# units are acre-ft/acre
waterRaw <- read_xlsx(here("data/intermediate/0_input/wateruse.xlsx"))

# pivot longer
water <- waterRaw %>% 
  pivot_longer(5:25, names_to = "Crop", values_to = "waterUse") %>%
  select(Crop, waterUse, County) %>% 
  # filter out "Avg. AW" observations in Crop column
  filter(Crop != "Avg. AW") %>%
  # remove the first 3 characters from observations in the County column
  mutate(
    County = str_sub(County, 4),
    .keep = "unused"
  )





# Join All Crosswalks -----------------------------------------------------

masterPre <- annualCross %>% 
  left_join(revCross, by = "COMM") %>% 
  left_join(waterCross, by = "COMM")

# # save the masterPre crosswalk
# write_csv(masterPre, 
#           here("data/intermediate/3_masterCrosswalk/sjv_masterPre_crosswalk.csv"))



# Join crosswalk data with LandIQ plots ---------------------------------------

# LandIQ spatial data
SJV_plots <- read_sf(here("data/intermediate/2_cleanPlotsLandIQ/SJVID/SJVID.shp"))

# join to master crosswalk and add revenue and water use values
master_SJV_plots <- SJV_plots %>% 
  left_join(masterPre, by = c("crp_ty_" = "COMM")) %>% 
  left_join(revenue, by = c("NASS", "county")) %>% 
  left_join(water, by = c("Crop" = "Crop", "county" = "County"))

# save the SJV plots with crosswalk info
# we'll combine this with estimated revenue values is 4_revenueEstimation.R
write_sf(master_SJV_plots, 
         here("data/intermediate/3_masterCrosswalk/sjv_crosswalkPlots_na/sjv_crosswalkPlots_na.shp"))



# Find missing LandIQ/NASS crop revenue values ---------------------------------

# examine which LandIQ and NASS crops in each county are missing price_per_acre values

# LandIQ crop types missing revenue values
missing_crops_landIQ <- master_SJV_plots %>%
  filter(is.na(price_per_acre)) %>%
  distinct(crp_ty_, county) %>%
  arrange(county, crp_ty_)


# NASS crop types missing revenue values
# NA values in this df are idle or unclassified fallow that we'll estimate later
missing_crops_nass_county <- master_SJV_plots %>%
  filter(is.na(price_per_acre)) %>%
  distinct(NASS, county) %>%
  arrange(county, NASS)
# these are the crops we need to estimate revenue for in 4_revenueEstimation.R
missing_crops_nass_sjv <- master_SJV_plots %>%
  filter(is.na(price_per_acre)) %>%
  distinct(NASS) %>%
  arrange(NASS)



















