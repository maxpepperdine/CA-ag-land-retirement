# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)
library(sf)
library(tmap)

# Revenue Crosswalk + Values ----------------------------------------------


# Read in table of crops with revenue $ / acre created in the cropValueClean.R
# script. This is matched to a crosswalk matrix I created based on KCACR
# data and intuition. The crosswalk joins revenue crop categories to kern
# field crop categories


#Read in revenue table
revenueRaw <- read_csv("Data/0_input/cropValue3.csv")

revenue <- revenueRaw %>%
  select(CROP, pricePerAcre, jobsPer100Acre)




# Create KCACR to Fields Crosswalk ----------------------------------------

# Crosswalk created by hand in excel. dput Function is used to get columns as 
# vector for reproducibility / robustness.

#############################################################################
# Uncomment this if more modifications are made in excel
#
# #Match Field COMM crop codes to KCACR data
# revenueCrosswalkRaw <- read_csv("./Data/0_input/kernFieldCropTypes.csv")
# 
# # Format uncertainty crosswalk
# revenueCrosswalk <- revenueCrosswalkRaw %>%
#   mutate(
#     CROP = case_when(
#       !is.na(Yes) ~ Yes,
#       !is.na(Maybe) ~ Maybe,
#       !is.na(Guess) ~ Guess
#     ),
#     .keep = "unused"
#   ) 
# 
# dput(revenueCrosswalk$COMM)
# dput(revenueCrosswalk$CROP)
##############################################################################

COMM <- c("ALFALFA", "ALFALFA SEED", "ALMOND", "APPLE", "APRICOT", "ARTICHOKE", 
          "ARUGULA", "ASPARAGUS", "AVOCADO", "BARLEY", "BARLEY FOR/FOD", 
          "BEAN DRIED", "BEAN SUCCULENT", "BEET", "BEETS, RED", "BLACKBERRY", 
          "BLUEBERRY", "BOK CHOY LSE LF", "BOYSENBERRY", "BROCCOLI", "BROCCOLI SEED", 
          "CABBAGE", "CABBAGE SEED", "CACTUS LEAF", "CACTUS PEAR", "CANTALOUPE", 
          "CARROT", "CAULIFLOWER", "CELERY", "CHERRY", "CHESTNUT", "CHIVE", 
          "CILANTRO", "COLLARD", "CORN FOR/FOD", "CORN, GRAIN", "CORN, HUMAN CON", 
          "COTTON", "CUCUMBER", "DAIKON", "DANDELION GREEN", "DILL", "EGGPLANT", 
          "FENNEL", "FIG", "FRUIT, BERRY", "GAI CHOY LSE LF", "GAI LON TGHT HD", 
          "GARBANZO BEAN", "GARLIC", "GOURD", "GRAPE", "GRAPE, RAISIN", 
          "GRAPE, WINE", "GRAPEFRUIT", "HONEYDEW MELON", "JUJUBE", "KALE", 
          "KIWI", "KOHLRABI", "LEEK", "LEMON", "LETTUCE HEAD", "LETTUCE HEAD SD", 
          "LETTUCE LEAF", "LETTUCE LEAF SD", "LETTUCE ROMAINE", "LOVEGRASS (FORA", 
          "MELON", "MUSK MELON", "MUSTARD", "MUSTARD GREENS", "N-GRNHS PLANT", 
          "N-OUTDR PLANTS", "NAPA CBG TGHT H", "NECTARINE", "OAT", "OAT FOR/FOD", 
          "OF-BULB", "OLIVE", "ONION DRY ETC", "ONION GREEN", "ONION SEED", 
          "OP-CHRSTMS TREE", "OP-DEC. SHRUB", "OP-DEC. TREE", "OP-FLWRNG PLANT", 
          "OP-PALM", "OP-ROSE", "OP-VINE", "ORANGE", "OT-CONIFER", "OT-DEC. TREE", 
          "PAPAYA", "PARSLEY", "PARSNIP", "PASTURELAND", "PEACH", "PEAR", 
          "PEAS", "PECAN", "PEPPER FRUITNG", "PEPPER SPICE", "PERSIMMON", 
          "PISTACHIO", "PLUM", "POMEGRANATE", "POMELO", "POTATO", "PUMPKIN", 
          "QUINCE", "RADISH", "RANGELAND", "RAPE", "RUTABAGA", "RYE", "RYEGRAS FOR/FOD", 
          "SAFFLOWER", "SORGHUM FOR/FOD", "SORGHUM MILO", "SPINACH", "SQUASH", 
          "SQUASH, WINTER", "STRAWBERRY", "SUDANGRASS", "SUGARBEET", "SWEET POTATO", 
          "SWISS CHARD", "TANGELO", "TANGERINE", "TANGERINE/SDLS", "TOMATO", 
          "TOMATO PROCESS", "TOMATO SEED", "TRITICALE", "TURF/SOD", "TURNIP", 
          "UNCULTIVATED AG", "VETCH", "WALNUT", "WATERMELON", "WHEAT", 
          "WHEAT FOR/FOD", "YAM", "ZUCCHINI")

CROP <- c("Hay,Alfalfa", "Field", "Almonds", "MiscellaneousFruitNut", 
          "Apricots", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "Barley", 
          "Barley", "Beans,DryEdible", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "Blueberries", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "MiscellaneousVegetable", 
          "Vegetable", "MiscellaneousVegetable", "Vegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "Cherries", 
          "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousFieldCrops", 
          "MiscellaneousVegetable", "CottonLint,Pima", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
          "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "Beans,DryEdible", "Garlic,Fresh", "MiscellaneousVegetable", 
          "TableVariety", "RaisinVariety", "WineVariety", "Grapefruit", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "MiscellaneousVegetable", 
          "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "Lemons", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "HerbaceousPlants", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "HerbaceousPlants", 
          "HerbaceousPlants", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
          "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", "None", 
          "MiscellaneousFruitNut", "Onions,Fresh", "MiscellaneousVegetable", 
          "Vegetable", "ChristmasTrees", "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", 
          "HerbaceousPlants", "OrnamentalTrees&Shrubs", "Roses", "FruitandNutTrees&Vines", 
          "Oranges,Navels", "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", 
          "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "Pasture, Range", "MiscellaneousFruitNut", "MiscellaneousFruitNut", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "Peppers,BellFresh", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "Pistachios", 
          "Plums", "MiscellaneousFruitNut", "Citrus,All", "Potatoes,All", 
          "MiscellaneousVegetable", "MiscellaneousFruitNut", "MiscellaneousVegetable", 
          "Pasture, Range", "MiscellaneousFieldCrops", "MiscellaneousVegetable", 
          "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", 
          "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
          "Hay,Other", "MiscellaneousVegetable", "MiscellaneousVegetable", 
          "MiscellaneousVegetable", "Tangerine&Tangelo", "Tangerine&Tangelo", 
          "Tangerine&Tangelo", "Tomatoes,Fresh", "Tomatoes,Processed", 
          "Vegetable", "MiscellaneousFieldCrops", "Turf", "MiscellaneousVegetable", 
          NA, "Cover Crop", "Walnuts", "Watermelons,Seeded/Seedless", "Wheat", 
          "Wheat", "MiscellaneousVegetable", "MiscellaneousVegetable")


revenueCrossWalk <- enframe(COMM, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(CROP, name = NULL, value = "CROP")) %>%
  left_join(revenue, by = "CROP") %>%
  mutate(
    pricePerAcre = ifelse((COMM == "UNCULTIVATED AG"), 0, pricePerAcre)
  )



# Join Crop COMM to COMMCODES ---------------------------------------------



# Crosswalk between crop COMM Codes (string) and iris COMM Codes (numeric)
commWalk <- read_csv("./Data/0_input/comm_codes.csv")



# Exploration of multiple COMM's per CODE

#
commKeyRaw <- revenueCrosswalk %>%
  select(COMM) %>%
  distinct(COMM) %>%
  left_join(commWalk, by = c("COMM" = "comm")) %>%
  group_by(comm_code) %>%
  mutate(
    n = n()
  ) %>%
  # filter(n > 1) %>%
  left_join(revenueCrosswalk, by = "COMM") %>%
  select(COMM, comm_code, pricePerAcre, jobsPer100Acre) %>%
  arrange(comm_code, desc(pricePerAcre)) %>%
  filter(duplicated(comm_code) == FALSE)


# Iris Fields
irisFields <- read_csv("./Data/0_input/KernFieldsThruTime_David_121922.csv")

# Check iris comm codes against mine

fieldCodes <- commKeyRaw %>%
  pull(comm_code) %>%
  unique()

irisCodes <- irisFields %>%
  filter(commcodeFields == 77000) %>%
  pull(commcodeLastActive) %>%
  unique() %>%
  sort()


missingCrops <- setdiff(irisCodes, fieldCodes)

# # Weird that my shit has codes that aren't in ashley's data,
# # may be due to end of year mumbo jumbo
# missingCrops2 <- setdiff(fieldCodes, irisCodes)





missing <- commWalk %>%
  filter(comm_code %in% missingCrops)

# missingCommFields <- irisFields %>%
#   filter(commcodeFields == 77000) %>%
#   filter(commcodeLastActive %in% missingCrops)
#
# 
# missingCommFieldsMap <- missingCommFields %>%
#   tm_shape() + tm_borders(col = "red")
# 


# Fill in Missing codes ---------------------------------------------------



emptyTable <- revenueCrosswalk %>% bind_rows(missing) %>%
  unite(col = "COMM1", c(COMM, comm), na.rm = TRUE) %>%
  arrange(COMM1)




emptyTable %>%
  filter(is.na(CROP))



write_csv(emptyTable, "revCrosswalkIntermediate.csv")




# # Water Crosswalk + Values ------------------------------------------------
# 
# 
# 
# #Read in table of crops with water use / acre created in the cropValueClean.R
# #script. This is matched to a crosswalk matrix I created based on CADWR
# #data and intuition. The crosswalk joins water use crop categories to kern
# #field crop categories
# 
# 
#Read in water use data
waterRaw <- read_xlsx("Data/0_input/waterUse.xlsx")


water <- waterRaw %>%
  filter(County == '15_Kern') %>%
  pivot_longer(5:25, names_to = "Crop", values_to = "waterUse")

#Join to crosswalk
waterCrosswalkRaw <- read_csv("fieldWaterUseCrosswalk.csv")

waterCrosswalk <- waterCrosswalkRaw %>%
  unite("Crop", Yes, Maybe, Guess, na.rm = TRUE) %>%
  mutate(
    #Do some string cleaning
    Crop = na_if(Crop, ""),
    Crop = str_replace(Crop, "Vineyards", "Vineyard")
  ) %>%
  left_join(water, by = "Crop") %>%
  mutate(
    waterUse = replace_na(waterUse, 0)
  ) %>%
  select(-c("Year", "RO", "HR", "County"))


emptyWater <- waterCrosswalk %>% bind_rows(missing) %>%
  unite(col = "COMM1", c(COMM, comm), na.rm = TRUE) %>%
  arrange(COMM1)

write_csv(emptyWater, "waterCrosswalkIntermediate.csv")




# Read in modified crosswalks ---------------------------------------------

waterFinal <- read_csv("Data/0_input/waterCrosswalkFinal.csv") %>% 
  left_join(water, by = "Crop") %>% 
  select(COMM1, waterUse.y)


revFinal <- read_csv("Data/0_input/revCrosswalkFinal.csv") %>% 
  left_join(revenue, by = "CROP") %>% 
  select(COMM1, pricePerAcre.y)

# Join Revenue and Water Tables -------------------------------------------


revWaterCrosswalk <- revFinal %>% 
  full_join(waterFinal, by = "COMM1") %>% 
  mutate(
    pricePerAcre = pricePerAcre.y,
    waterUse = waterUse.y,
    .keep = "unused"
  )

write_csv(revWaterCrosswalk, "crosswalkFinal.csv")



# Load Crosswalk ----------------------------------------------------------

revWater <- read_csv("Data/0_input/crosswalkFinal.csv")



# Load Annual / Perennial Data --------------------------------------------


ap <- read_csv("Data/0_input/Crops0020FamiliesImport.csv")



#Reduce AP to columns I need
smallAP <- ap %>% 
  select(commodity, annual, commcodeFill)


cropAP <- revWater %>%
  left_join(smallAP, by = c("COMM1" = "commodity")) %>% 
  mutate(
    COMM = COMM1,
    .keep = "unused"
  )



#Create vector of uncultivated and rangeland to filter out when estimate
#Value
fallowRange <- c("UNCULTIVATED AG")

#Create list of COMM codes to be changed to annual
guessAnnuals <- cropAP %>%
  #filter(yrRat < 0.2) %>%
  filter(is.na(annual)) %>%
  distinct(COMM) %>%
  filter(!(COMM %in% fallowRange)) %>%
  pull(COMM)


#Change missing annual codes
cropRmNA <- cropAP %>%
  #Change NA's in annual perennial
  mutate(
    annual = if_else((COMM %in% guessAnnuals), 1, annual)
  )


commWalk <- read_csv("Data/0_input/comm_codes.csv")


comparison <- cropRmNA %>% 
  left_join(commWalk, by = c(#"comm_code" = "commcodeFill", 
    "COMM" = "comm"), 
    #keep = TRUE
  ) %>% 
  select(-commcodeFill)



cropRmNA %>% 
  filter(!(COMM %in% fallowRange)) %>% 
  filter(is.na(pricePerAcre))

write_csv(comparison, "./Data/1_masterCrosswalk/masterCrosswalk.csv")



