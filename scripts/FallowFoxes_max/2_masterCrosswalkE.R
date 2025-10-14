# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)

# Full list of Crop types for 2021 (including non permitted) --------------

# Create crosswalk to join 2021 Kern Plot crop types (LandIQ) to Iris Fields data (cdpr comm codes)


# Kern Plots from LandIQ
COMMi <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", "Apples", 
           "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", "Pomegranates", "Pecans", "Apricots", 
           "Cherries", "Peaches/Nectarines", "Plums", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
           "Corn, Sorghum and Sudan", "Safflower", "Wheat", "Miscellaneous Grain and Hay", "Idle – Short Term", 
           "Idle – Long Term", "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", "Turf Farms", 
           "Onions and Garlic", "Potatoes", "Sweet Potatoes", "Flowers, Nursery and Christmas Tree Farms", 
           "Miscellaneous Truck Crops", "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
           "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", "Grapes", "Unclassified Fallow", 
           "Young Perennials") 

# Iris Codes from CDPR Report 
comm_codei <- c(2000, 30000, 6004, 28000, 28014, 6000, 4001, 152.38, 3001, 3009, 3011, 6015, 3008, 5001, 5002, 5003, 
                5005, 29121, 15001, 28024, 24002,29129, 29139, 28078, 66000, 66000, 23001, 28035, 28069, 33008, 14011, 14013, 
                14018, 151.34, 28024, 1023, 1016, 11003, 61006, 13031, 11005, 13004, 29111, 10010, 29141, 66000, 154.42)


irisCross <- enframe(COMMi, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(comm_codei, name = NULL, value = "comm_code"))


# Water Use Crosswalk -----------------------------------------------------


# Join Kern Plot Crop Types to CADWR Water Use Crop Types


# Kern Plots from LandIQ
commW <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", "Apples", 
           "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", "Pomegranates", "Pecans", "Apricots", 
           "Cherries", "Peaches/Nectarines", "Plums", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
           "Corn, Sorghum and Sudan", "Safflower", "Wheat", "Miscellaneous Grain and Hay", "Idle – Short Term", 
           "Idle – Long Term", "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", "Turf Farms", 
           "Onions and Garlic", "Potatoes", "Sweet Potatoes", "Flowers, Nursery and Christmas Tree Farms", 
           "Miscellaneous Truck Crops", "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
           "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", "Grapes", "Unclassified Fallow", 
           "Young Perennials") 

# CADWR Crops
cropW <- c("Citrus and Subtropical", "Truck Crops", "Other Deciduous", "Citrus and Subtropical", "Citrus and Subtropical", "Citrus and Subtropical", 
           "Other Deciduous", "Other Deciduous", "Almonds & Pistachios", "Other Deciduous", "Almonds & Pistachios", "Other Deciduous", 
           "Other Deciduous", "Other Deciduous", "Other Deciduous", "Other Deciduous", "Other Deciduous", "Cotton", "Dry Beans", 
           "Other Field Crops", "Corn", "Safflower", "Grain", "Grain", NA, NA, "Alfalfa", "Pasture", "Other Field Crops", 
           "Pasture", "Onions & Garlic", "Potatoes", "Potatoes", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops","Tomato Fresh", "Truck Crops", "Truck Crops", "Cucurbits", "Vineyard", NA, "Truck Crops")


# Enframe to data table
waterCross <- enframe(commW, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(cropW, name = NULL, value = "Crop"))


# Revenue Crosswalk -------------------------------------------------------


# Create USDA NASS Revenue to Kern 2021 Plot Crosswalk

# Kern Plot Crops
commR <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", "Apples", 
           "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", "Pomegranates", "Pecans", "Apricots", 
           "Cherries", "Peaches/Nectarines", "Plums", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
           "Corn, Sorghum and Sudan", "Safflower", "Wheat", "Miscellaneous Grain and Hay", "Idle – Short Term", 
           "Idle – Long Term", "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", "Turf Farms", 
           "Onions and Garlic", "Potatoes", "Sweet Potatoes", "Flowers, Nursery and Christmas Tree Farms", 
           "Miscellaneous Truck Crops", "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
           "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", "Grapes", "Unclassified Fallow", 
           "Young Perennials")



# Adjusted CAC Crops
cropR <- c("Oranges, All", "Forest Products, Unspecified", "Dates", "Avocados, All", "Olives", "Grapefruit", "Stone Fruits, All", "Forest Products, Unspecified", 
           "Almonds, Nuts", "Walnuts", "Pistachios", "Pomegrantates", "Horticulture, Fruit/Vine/Nut Plants", "Stone Fruits, All", "Cherries",  
           "Stone Fruits, All", "Stone Fruits, All", "Cotton, Lint, All", "Beans, All", "Hay, Grain, Misc", "Silage, All", "Safflower", "Wheat, Grain", 
           "Hay, Grain, Misc", NA, NA, "Alfalfa, Hay", "Pasture, All", "Pasture, All", "Horticulute, Sod/Turf", 
           "Onions, Dry", "Potatoes", "Potatoes", "Horticulture, Woody Ornamentals", "Vegetables, Misc", "Berries, Blueberries",
           "Berries, All", "Peppers, Bell", "Horticulture, All", "Horticulture, All", "Tomatoes, Processing", "Vegetables, Misc", "Vegetables, Misc", 
           "Horticulture, Fruit/Vine/Nut Plants", "Grapes, All", NA, "Horticulure, All")


# Enframe to data frame
revCross <- enframe(commR, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(cropR, name = NULL, value = "NASS"))


# Annual Perennial Crosswalk ----------------------------------------------

# Join Kern Plot Crops to Annual / Perennial Info Table

COMMap <- c("Citrus", "Eucalyptus", "Dates", "Avocados", "Olives", "Miscellaneous Subtropical Fruits", "Apples", 
            "Miscellaneous Deciduous", "Almonds", "Walnuts", "Pistachios", "Pomegranates", "Pecans", "Apricots", 
            "Cherries", "Peaches/Nectarines", "Plums", "Cotton", "Beans (Dry)", "Miscellaneous Field Crops", 
            "Corn, Sorghum and Sudan", "Safflower", "Wheat", "Miscellaneous Grain and Hay", "Idle – Short Term", 
            "Idle – Long Term", "Alfalfa & Alfalfa Mixtures", "Mixed Pasture", "Miscellaneous Grasses", "Turf Farms", 
            "Onions and Garlic", "Potatoes", "Sweet Potatoes", "Flowers, Nursery and Christmas Tree Farms", 
            "Miscellaneous Truck Crops", "Bush Berries", "Strawberries", "Peppers", "Greenhouse", "Lettuce/Leafy Greens", 
            "Tomatoes", "Cole Crops", "Carrots", "Melons, Squash and Cucumbers", "Grapes", "Unclassified Fallow", 
            "Young Perennials")

# AP crop values
ap <- c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, NA, NA, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 
        1, 1, 0, 1, 1, 1, 1, 1, 0, NA, 1)


# Enframe to data frame
annualCross <- enframe(COMMap, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(ap, name = NULL, value = "annual"))




# Revenue Crosswalk + Values ----------------------------------------------


# Read in table of crops with revenue $ / acre created in the 
# cropRevenueJobsCrosswalk.R script. 
# This is matched to a crosswalk matrix I created based on KCACR data and 
# intuition. 
# The crosswalk joins revenue crop categories to kern field crop categories


#Read in revenue table - TO DO ************
revenueRaw <- read_csv(here::here("data/intermediate/1_cropRevenueCrosswalkE/final_revenue_e.csv"))

revenue <- revenueRaw %>%
  dplyr::select(crop, price_per_acre) %>% 
  mutate(
    NASS = crop,
    .keep = "unused"
  )


# # Water Crosswalk + Values ------------------------------------------------



# #Read in table of crops with water use / acre created in the cropValueClean.R
# #script. This is matched to a crosswalk matrix I created based on CADWR
# #data and intuition. The crosswalk joins water use crop categories to kern
# #field crop categories
# 
# 
#Read in water use data
waterRaw <- read_xlsx(here::here("data/intermediate/0_input/wateruse.xlsx"))


water <- waterRaw %>%
  filter(County == '15_Kern') %>%
  pivot_longer(5:25, names_to = "Crop", values_to = "waterUse") %>% 
  dplyr::select(Crop, waterUse)




# Join all Crosswalks -----------------------------------------------------


masterPre <- irisCross %>% 
  left_join(annualCross, by = "COMM") %>% 
  left_join(revCross, by = "COMM") %>% 
  left_join(waterCross, by = "COMM") %>% 
  left_join(water, by = "Crop") %>% 
  left_join(revenue, by = "NASS") |> 
  clean_names() 


# Load in the estimated csv 

estimated <-read_csv(here::here("data/intermediate/0_input/estimated_updated.csv"))


# Stack the two dfs 

combined_df <- bind_rows(masterPre, estimated) |> 
  filter(!is.na(price_per_acre) | comm %in% c("Idle – Short Term", "Idle – Long Term", "Unclassified Fallow")) |> 
  distinct()

write_csv(combined_df, here::here("data/intermediate/masterCrosswalkE.csv"))














