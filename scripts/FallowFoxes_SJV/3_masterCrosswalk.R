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

# Regional median imputation ------
# Replaces zeros with the median of non-zero values from the same hydrologic
# region (HR). Falls back to statewide median if the entire HR is zero, or to
# the overall median across all crops if every county is zero for that crop.

impute_regional_median <- function(df, crop_cols) {
  
  df_imputed <- df
  
  # Overall fallback: median of all non-zero values across all crop columns
  all_nonzero <- unlist(df[crop_cols])
  all_nonzero <- all_nonzero[all_nonzero != 0]
  overall_median <- median(all_nonzero, na.rm = TRUE)
  
  for (crop in crop_cols) {
    
    all_vals <- df_imputed[[crop]]
    n_zeros  <- sum(all_vals == 0)
    if (n_zeros == 0) next
    
    # If entire column is zero, fill with overall median
    if (all(all_vals == 0)) {
      message(sprintf("  %-25s | all counties zero — overall median (%.2f)", crop, overall_median))
      df_imputed[[crop]] <- overall_median
      next
    }
    
    message(sprintf("  %-25s | %d zero(s) to impute", crop, n_zeros))
    
    # Median by HR (excluding zeros)
    hr_medians <- df_imputed %>%
      filter(.data[[crop]] != 0) %>%
      group_by(HR) %>%
      summarise(median_val = median(.data[[crop]], na.rm = TRUE), .groups = "drop")
    
    # Statewide fallback
    fallback_median <- median(all_vals[all_vals != 0], na.rm = TRUE)
    
    for (i in seq_len(nrow(df_imputed))) {
      if (df_imputed[[crop]][i] == 0) {
        region   <- df_imputed$HR[i]
        regional <- hr_medians$median_val[hr_medians$HR == region]
        
        if (length(regional) > 0 && !is.na(regional)) {
          df_imputed[[crop]][i] <- regional
        } else {
          df_imputed[[crop]][i] <- fallback_median
        }
      }
    }
  }
  
  return(df_imputed)
}


# --- Read and impute Applied Water (AW) ---

awRaw <- read_xlsx(here("data/raw/ca_dwr/wateruse_AW.xlsx"))
names(awRaw) <- trimws(names(awRaw))

meta_cols    <- c("Year", "RO", "HR", "County")
crop_cols_aw <- setdiff(names(awRaw), c(meta_cols, "Avg. AW"))

message("\n========== Imputing Applied Water (AW) ==========")
awFill <- impute_regional_median(awRaw, crop_cols_aw)
awFill$`Avg. AW` <- rowMeans(awFill[crop_cols_aw], na.rm = TRUE)

# Pivot to long format
waterAW <- awFill %>%
  pivot_longer(all_of(crop_cols_aw), names_to = "Crop", values_to = "waterUse_AW") %>%
  select(Crop, waterUse_AW, County) %>%
  mutate(County = str_sub(County, 4))


# --- Read and impute ETc ---

etcRaw <- read_xlsx(here("data/raw/ca_dwr/wateruse_ETc.xlsx"))
names(etcRaw) <- trimws(names(etcRaw))

crop_cols_etc <- setdiff(names(etcRaw), c(meta_cols, "Total ETc"))

message("\n========== Imputing ETc ==========")
etcFill <- impute_regional_median(etcRaw, crop_cols_etc)
etcFill$`Total ETc` <- rowSums(etcFill[crop_cols_etc], na.rm = TRUE)

# Pivot to long format
waterETc <- etcFill %>%
  pivot_longer(all_of(crop_cols_etc), names_to = "Crop", values_to = "waterUse_ETc") %>%
  select(Crop, waterUse_ETc, County) %>%
  mutate(County = str_sub(County, 4))


# --- Combine AW and ETc into a single water use table ---

water <- waterAW %>%
  left_join(waterETc, by = c("Crop", "County"))





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
         here("data/intermediate/3_masterCrosswalk/sjv_crosswalkPlots_na/sjv_crosswalkPlots_na.gpkg"), 
         append = FALSE)



# Find missing LandIQ/NASS crop revenue values ---------------------------------

# examine which LandIQ and NASS crops in each county are missing price_per_acre values

# LandIQ crop types missing revenue values
missing_crops_landIQ <- master_SJV_plots %>%
  filter(is.na(price_per_acre)) %>%
  distinct(crp_ty_, county) %>%
  arrange(county, crp_ty_)


# # find all unqiue crop type/county combos
# crops_county <- master_SJV_plots %>%
#   distinct(crp_ty_, county) %>%
#   arrange(county, crp_ty_) %>% 
#   rename(comm = crp_ty_)
# 
# # save this for later reference
# write_csv(crops_county, here("data/intermediate/3_masterCrosswalk/sjv_cropCountyCombos.csv"))


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



















