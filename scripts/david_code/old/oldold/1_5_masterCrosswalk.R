# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)




# Full list of Crop types for 2015 (including non permitted) --------------


# Making crosswalks is easier by hand in excel, however not very reproducible, 
# difficult to keep track of. Making crosswalks in R is very tedious, but 
# easier to keep track of / modify. This code is to take the jobCrossWalkRaw
# file modifications made in excel, and output the code required to make the 
# vectors by hand. For future use / reference. 
# 
# 
# # # Made changes by hand
# master <- read_csv("./Data/1_masterCrosswalk/masterCrosswalk.csv")
# 
# 
# irisCOMM <- master %>% 
#   select(COMM, comm_code)
# 
# 
# 
# dput(irisCOMM$COMM)
# dput(irisCOMM$comm_code)


COMMi <- c("ALFALFA", "ALFALFA SEED", "ALMOND", "APPLE", "APRICOT", "ARTICHOKE", 
          "ARUGULA", "ASPARAGUS", "AVOCADO", "BARLEY", "BARLEY FOR/FOD", 
          "BEAN DRIED", "BEAN LIMA", "BEAN SUCCULENT", "BEET", "BEETS, RED", 
          "BLACKBERRY", "BLUEBERRY", "BOK CHOY LSE LF", "BOYSENBERRY", 
          "BROCCOLI", "BROCCOLI SEED", "CABBAGE", "CABBAGE SEED", "CACTUS LEAF", 
          "CACTUS PEAR", "CANTALOUPE", "CARROT", "CAULIFLOWER", "CELERY", 
          "CHERRY", "CHESTNUT", "CHINESE GREEN", "CHIVE", "CILANTRO", "CLOVER", 
          "COLLARD", "CORN FOR/FOD", "CORN, GRAIN", "CORN, HUMAN CON", 
          "COTTON", "CUCUMBER", "DAIKON", "DANDELION GREEN", "DILL", "EGGPLANT", 
          "FENNEL", "FIG", "FLAX", "FORAGE HAY/SLGE", "FRUIT, BERRY", "GAI CHOY LSE LF", 
          "GAI LON TGHT HD", "GARBANZO BEAN", "GARLIC", "GF-VINE", "GOURD", 
          "GRAIN", "GRAPE", "GRAPE, RAISIN", "GRAPE, WINE", "GRAPEFRUIT", 
          "HEMP/CANNABIS", "HERB, SPICE", "HERB, SPICE-ORG", "HERB, SPICE-ORGANIC", 
          "HONEYDEW MELON", "INDUSTRIAL HEMP", "INDUSTRIAL SITE", "JOJOBA BEAN", 
          "JUJUBE", "KALE", "KIWI", "KOHLRABI", "LANDSCAPE MAIN", "LEEK", 
          "LEMON", "LETTUCE HEAD", "LETTUCE HEAD SD", "LETTUCE LEAF", "LETTUCE LEAF SD", 
          "LETTUCE ROMAINE", "LOVEGRASS (FORA", "MELON", "MUSK MELON", 
          "MUSTARD", "MUSTARD GREENS", "N-GRNHS PLANT", "N-OUTDR PLANTS", 
          "NAPA CBG TGHT H", "NECTARINE", "OAT", "OAT FOR/FOD", "OF-BULB", 
          "OILSEED", "OKRA", "OKRA-ORGANIC", "OLIVE", "ONION DRY ETC", 
          "ONION GREEN", "ONION SEED", "OP-CHRSTMS TREE", "OP-DEC. SHRUB", 
          "OP-DEC. TREE", "OP-FLWRNG PLANT", "OP-PALM", "OP-ROSE", "OP-VINE", 
          "ORANGE", "OT-CONIFER", "OT-DEC. TREE", "PAPAYA", "PARSLEY", 
          "PARSNIP", "PASTURELAND", "PEACH", "PEAR", "PEAS", "PECAN", "PEPPER FRUITNG", 
          "PEPPER SPICE", "PERSIMMON", "PISTACHIO", "PLUM", "POMEGRANATE", 
          "POMELO", "POTATO", "PRUNE", "PUMPKIN", "QUINCE", "RADISH", "RANGELAND", 
          "RAPE", "RECREATION AREA", "RESEARCH COMMOD", "RUTABAGA", "RYE", 
          "RYEGRAS FOR/FOD", "SAFFLOWER", "SORGHUM FOR/FOD", "SORGHUM MILO", 
          "SPINACH", "SQUASH", "SQUASH, SUMMER", "SQUASH, SUMMER-", "SQUASH, SUMMER-ORGANIC", 
          "SQUASH, WINTER", "STRAWBERRY", "SUDANGRASS", "SUGARBEET", "SWEET POTATO", 
          "SWISS CHARD", "TANGELO", "TANGERINE", "TANGERINE/SDLS", "TOMATO", 
          "TOMATO PROCESS", "TOMATO SEED", "TRITICALE", "TURF/SOD", "TURNIP", 
          "UNCUL NON-AG", "UNCULTIVATED AG", "VEGETABLE", "VEGETABLE LEAF", 
          "VEGETABLE ROOT", "VEGETABLE SEED", "VETCH", "WALNUT", "WATERMELON", 
          "WHEAT", "WHEAT FOR/FOD", "YAM", "ZUCCHINI")


comm_codei <- c(23001, 23001, 3001, 4001, 5001, 13018, 13056, 16002, 28000, 
                29103, 22008, 15001, 9987, 15003, 29109, 13001, 1002, 1009, 13502, 
                9941, 13005, 13005, 13007, 13007, 13048, 6028, 10002, 29111, 
                13008, 29113, 5002, 3004, 9956, 14005, 13501, 9986, 13009, 22005, 
                24002, 29119, 29121, 10010, 14023, 13014, 8015, 11001, 28008, 
                6005, 9976, 22000, 1000, 13509, 13903, 15032, 14007, 9969, 10026, 
                9992, 29141, 29141, 29143, 2002, 26010, 8000, 8000, 8000, 29122, 
                26010, 9915, 9928, 6504, 13011, 9901, 13012, 9960, 14010, 2004, 
                13045, 13045, 13031, 13031, 13031, 22023, 29122, 29122, 29123, 
                13021, 153, 154, 13010, 5003, 29125, 22006, 152, 9919, 15015, 
                15015, 28014, 14011, 16004, 14011, 154, 154, 154, 154, 154, 154, 
                154, 2006, 156, 156, 9916, 13022, 14012, 28035, 5004, 4003, 29127, 
                3008, 11003, 8050, 6012, 3011, 5005, 6015, 9974, 14013, 9971, 
                10011, 4004, 14014, 28045, 28051, 9925, 9927, 14015, 28064, 22035, 
                29129, 22004, 29131, 13024, 10012, 10013, 10013, 10013, 10014, 
                1016, 22011, 9937, 14018, 13025, 2007, 2008, 2008, 11005, 29136, 
                11005, 24011, 33008, 29137, 9911, 66000, 13000, 13000, 13000, 
                13000, 23010, 3009, 10008, 29139, 22007, 14021, 10015)




irisCross <- enframe(COMMi, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(comm_codei, name = NULL, value = "comm_code"))

#setdiff(irisCOMM2, irisCOMM)




# Water Use Crosswalk -----------------------------------------------------


# waterFinal <- read_csv("Data/0_input/waterCrosswalkFinal.csv")
# 
# 
# 
# dput(waterFinal$COMM1)
# dput(waterFinal$Crop)

commW <- c("ALFALFA", "ALFALFA SEED", "ALMOND", "APPLE", "APRICOT", "ARTICHOKE", 
           "ARUGULA", "ASPARAGUS", "AVOCADO", "BARLEY", "BARLEY FOR/FOD", 
           "BEAN DRIED", "BEAN LIMA", "BEAN SUCCULENT", "BEET", "BEETS, RED", 
           "BLACKBERRY", "BLUEBERRY", "BOK CHOY LSE LF", "BOYSENBERRY", 
           "BROCCOLI", "BROCCOLI SEED", "CABBAGE", "CABBAGE SEED", "CACTUS LEAF", 
           "CACTUS PEAR", "CANTALOUPE", "CARROT", "CAULIFLOWER", "CELERY", 
           "CHERRY", "CHESTNUT", "CHINESE GREEN", "CHIVE", "CILANTRO", "CLOVER", 
           "COLLARD", "CORN FOR/FOD", "CORN, GRAIN", "CORN, HUMAN CON", 
           "COTTON", "CUCUMBER", "DAIKON", "DANDELION GREEN", "DILL", "EGGPLANT", 
           "FENNEL", "FIG", "FLAX", "FORAGE HAY/SLGE", "FRUIT, BERRY", "GAI CHOY LSE LF", 
           "GAI LON TGHT HD", "GARBANZO BEAN", "GARLIC", "GF-VINE", "GOURD", 
           "GRAIN", "GRAPE", "GRAPE, RAISIN", "GRAPE, WINE", "GRAPEFRUIT", 
           "HEMP/CANNABIS", "HERB, SPICE", "HERB, SPICE-ORG", "HERB, SPICE-ORGANIC", 
           "HONEYDEW MELON", "INDUSTRIAL HEMP", "INDUSTRIAL SITE", "JOJOBA BEAN", 
           "JUJUBE", "KALE", "KIWI", "KOHLRABI", "LANDSCAPE MAIN", "LEEK", 
           "LEMON", "LETTUCE HEAD", "LETTUCE HEAD SD", "LETTUCE LEAF", "LETTUCE LEAF SD", 
           "LETTUCE ROMAINE", "LOVEGRASS (FORA", "MELON", "MUSK MELON", 
           "MUSTARD", "MUSTARD GREENS", "N-GRNHS PLANT", "N-OUTDR PLANTS", 
           "NAPA CBG TGHT H", "NECTARINE", "OAT", "OAT FOR/FOD", "OF-BULB", 
           "OILSEED", "OKRA", "OKRA-ORGANIC", "OLIVE", "ONION DRY ETC", 
           "ONION GREEN", "ONION SEED", "OP-CHRSTMS TREE", "OP-DEC. SHRUB", 
           "OP-DEC. TREE", "OP-FLWRNG PLANT", "OP-PALM", "OP-ROSE", "OP-VINE", 
           "ORANGE", "OT-CONIFER", "OT-DEC. TREE", "PAPAYA", "PARSLEY", 
           "PARSNIP", "PASTURELAND", "PEACH", "PEAR", "PEAS", "PECAN", "PEPPER FRUITNG", 
           "PEPPER SPICE", "PERSIMMON", "PISTACHIO", "PLUM", "POMEGRANATE", 
           "POMELO", "POTATO", "PRUNE", "PUMPKIN", "QUINCE", "RADISH", "RANGELAND", 
           "RAPE", "RECREATION AREA", "RESEARCH COMMOD", "RUTABAGA", "RYE", 
           "RYEGRAS FOR/FOD", "SAFFLOWER", "SORGHUM FOR/FOD", "SORGHUM MILO", 
           "SPINACH", "SQUASH", "SQUASH, SUMMER", "SQUASH, SUMMER-", "SQUASH, SUMMER-ORGANIC", 
           "SQUASH, WINTER", "STRAWBERRY", "SUDANGRASS", "SUGARBEET", "SWEET POTATO", 
           "SWISS CHARD", "TANGELO", "TANGERINE", "TANGERINE/SDLS", "TOMATO", 
           "TOMATO PROCESS", "TOMATO SEED", "TRITICALE", "TURF/SOD", "TURNIP", 
           "UNCUL NON-AG", "UNCULTIVATED AG", "VEGETABLE", "VEGETABLE LEAF", 
           "VEGETABLE ROOT", "VEGETABLE SEED", "VETCH", "WALNUT", "WATERMELON", 
           "WHEAT", "WHEAT FOR/FOD", "YAM", "ZUCCHINI")


cropW <- c("Alfalfa", "Alfalfa", "Almonds & Pistachios", "Other Deciduous", 
           "Other Deciduous", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Citrus & Subtropical", "Grain", "Grain", "Dry Beans", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Citrus & Subtropical", "Citrus & Subtropical", 
           "Cucurbits", "Truck Crops", "Truck Crops", "Truck Crops", "Other Deciduous", 
           "Other Deciduous", "Truck Crops", "Onions & Garlic", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Corn", "Corn", "Corn", "Cotton", 
           "Cucurbits", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Other Deciduous", "Grain", "Grain", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Dry Beans", "Onions & Garlic", 
           "Truck Crops", "Cucurbits", "Grain", "Vineyard", "Vineyard", 
           "Vineyard", "Citrus & Subtropical", NA, "Truck Crops", "Truck Crops", 
           "Truck Crops", "Cucurbits", NA, NA, "Truck Crops", "Truck Crops", 
           "Truck Crops", "Other Deciduous", "Truck Crops", NA, "Onions & Garlic", 
           "Citrus & Subtropical", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Other Field Crops", "Cucurbits", 
           "Cucurbits", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Other Deciduous", "Grain", "Grain", "Truck Crops", 
           "Grain", "Truck Crops", "Truck Crops", "Citrus & Subtropical", 
           "Onions & Garlic", "Onions & Garlic", "Onions & Garlic", "Truck Crops", 
           "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Truck Crops", "Citrus & Subtropical", "Truck Crops", "Truck Crops", 
           "Other Deciduous", "Truck Crops", "Truck Crops", "Pasture", "Other Deciduous", 
           "Other Deciduous", "Truck Crops", "Other Deciduous", "Truck Crops", 
           "Truck Crops", "Other Deciduous", "Almonds & Pistachios", "Other Deciduous", 
           "Other Deciduous", "Citrus & Subtropical", "Potatoes", "Other Deciduous", 
           "Cucurbits", "Truck Crops", "Truck Crops", NA, "Truck Crops", 
           NA, NA, "Truck Crops", "Grain", "Grain", "Safflower", "Other Field Crops", 
           "Other Field Crops", "Truck Crops", "Cucurbits", "Cucurbits", 
           "Cucurbits", "Cucurbits", "Cucurbits", "Truck Crops", "Other Field Crops", 
           "Sugar Beets", "Potatoes", "Truck Crops", "Citrus & Subtropical", 
           "Citrus & Subtropical", "Citrus & Subtropical", "Tomato Fresh", 
           "Tomato Processing", "Tomato Fresh", "Grain", "Pasture", "Truck Crops", 
           NA, NA, "Truck Crops", "Truck Crops", "Truck Crops", "Truck Crops", 
           "Grain", "Other Deciduous", "Cucurbits", "Grain", "Grain", "Potatoes", 
           "Cucurbits")


waterCross <- enframe(commW, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(cropW, name = NULL, value = "Crop"))





# Revenue Crosswalk -------------------------------------------------------



# 
# revFinal <- read_csv("Data/0_input/revCrosswalkFinal.csv") 
# 
# dput(revFinal$COMM1)
# dput(revFinal$CROP)


COMMr <- c("ALFALFA", "ALFALFA SEED", "ALMOND", "APPLE", "APRICOT", "ARTICHOKE", 
           "ARUGULA", "ASPARAGUS", "AVOCADO", "BARLEY", "BARLEY FOR/FOD", 
           "BEAN DRIED", "BEAN LIMA", "BEAN SUCCULENT", "BEET", "BEETS, RED", 
           "BLACKBERRY", "BLUEBERRY", "BOK CHOY LSE LF", "BOYSENBERRY", 
           "BROCCOLI", "BROCCOLI SEED", "CABBAGE", "CABBAGE SEED", "CACTUS LEAF", 
           "CACTUS PEAR", "CANTALOUPE", "CARROT", "CAULIFLOWER", "CELERY", 
           "CHERRY", "CHESTNUT", "CHINESE GREEN", "CHIVE", "CILANTRO", "CLOVER", 
           "COLLARD", "CORN FOR/FOD", "CORN, GRAIN", "CORN, HUMAN CON", 
           "COTTON", "CUCUMBER", "DAIKON", "DANDELION GREEN", "DILL", "EGGPLANT", 
           "FENNEL", "FIG", "FLAX", "FORAGE HAY/SLGE", "FRUIT, BERRY", "GAI CHOY LSE LF", 
           "GAI LON TGHT HD", "GARBANZO BEAN", "GARLIC", "GF-VINE", "GOURD", 
           "GRAIN", "GRAPE", "GRAPE, RAISIN", "GRAPE, WINE", "GRAPEFRUIT", 
           "HEMP/CANNABIS", "HERB, SPICE", "HERB, SPICE-ORG", "HERB, SPICE-ORGANIC", 
           "HONEYDEW MELON", "INDUSTRIAL HEMP", "INDUSTRIAL SITE", "JOJOBA BEAN", 
           "JUJUBE", "KALE", "KIWI", "KOHLRABI", "LANDSCAPE MAIN", "LEEK", 
           "LEMON", "LETTUCE HEAD", "LETTUCE HEAD SD", "LETTUCE LEAF", "LETTUCE LEAF SD", 
           "LETTUCE ROMAINE", "LOVEGRASS (FORA", "MELON", "MUSK MELON", 
           "MUSTARD", "MUSTARD GREENS", "N-GRNHS PLANT", "N-OUTDR PLANTS", 
           "NAPA CBG TGHT H", "NECTARINE", "OAT", "OAT FOR/FOD", "OF-BULB", 
           "OILSEED", "OKRA", "OKRA-ORGANIC", "OLIVE", "ONION DRY ETC", 
           "ONION GREEN", "ONION SEED", "OP-CHRSTMS TREE", "OP-DEC. SHRUB", 
           "OP-DEC. TREE", "OP-FLWRNG PLANT", "OP-PALM", "OP-ROSE", "OP-VINE", 
           "ORANGE", "OT-CONIFER", "OT-DEC. TREE", "PAPAYA", "PARSLEY", 
           "PARSNIP", "PASTURELAND", "PEACH", "PEAR", "PEAS", "PECAN", "PEPPER FRUITNG", 
           "PEPPER SPICE", "PERSIMMON", "PISTACHIO", "PLUM", "POMEGRANATE", 
           "POMELO", "POTATO", "PRUNE", "PUMPKIN", "QUINCE", "RADISH", "RANGELAND", 
           "RAPE", "RECREATION AREA", "RESEARCH COMMOD", "RUTABAGA", "RYE", 
           "RYEGRAS FOR/FOD", "SAFFLOWER", "SORGHUM FOR/FOD", "SORGHUM MILO", 
           "SPINACH", "SQUASH", "SQUASH, SUMMER", "SQUASH, SUMMER-", "SQUASH, SUMMER-ORGANIC", 
           "SQUASH, WINTER", "STRAWBERRY", "SUDANGRASS", "SUGARBEET", "SWEET POTATO", 
           "SWISS CHARD", "TANGELO", "TANGERINE", "TANGERINE/SDLS", "TOMATO", 
           "TOMATO PROCESS", "TOMATO SEED", "TRITICALE", "TURF/SOD", "TURNIP", 
           "UNCUL NON-AG", "UNCULTIVATED AG", "VEGETABLE", "VEGETABLE LEAF", 
           "VEGETABLE ROOT", "VEGETABLE SEED", "VETCH", "WALNUT", "WATERMELON", 
           "WHEAT", "WHEAT FOR/FOD", "YAM", "ZUCCHINI")




CROPr <- c("Hay,Alfalfa", "Field", "Almonds", "MiscellaneousFruitNut", 
           "Apricots", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousFruitNut", "Barley", 
           "Barley", "Beans,DryEdible", "Beans,DryEdible", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
           "Blueberries", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", "Vegetable", "MiscellaneousVegetable", 
           "Vegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "Cherries", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousFieldCrops", "MiscellaneousVegetable", "CottonLint,Pima", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousFruitNut", "MiscellaneousFieldCrops", "SilageandForage", 
           "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "Beans,DryEdible", "Garlic,Fresh", "OrnamentalTrees&Shrubs", 
           "MiscellaneousVegetable", "Hay,Grain", "TableVariety", "RaisinVariety", 
           "WineVariety", "Grapefruit", NA, "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", NA, NA, "MiscellaneousFruitNut", 
           "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", NA, "MiscellaneousVegetable", "Lemons", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "OrnamentalTrees&Shrubs", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", 
           "MiscellaneousVegetable", "MiscellaneousFruitNut", "MiscellaneousFieldCrops", 
           "MiscellaneousFieldCrops", "OrnamentalTrees&Shrubs", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
           "Onions,Fresh", "MiscellaneousVegetable", "Vegetable", "OrnamentalTrees&Shrubs", 
           "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", 
           "OrnamentalTrees&Shrubs", "Roses", "FruitandNutTrees&Vines", 
           "Oranges,Navels", "OrnamentalTrees&Shrubs", "OrnamentalTrees&Shrubs", 
           "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "Pasture, Range", "MiscellaneousFruitNut", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", "MiscellaneousFruitNut", "Peppers,BellFresh", 
           "MiscellaneousVegetable", "MiscellaneousFruitNut", "Pistachios", 
           "Plums", "MiscellaneousFruitNut", "Citrus,All", "Potatoes,All", 
           "MiscellaneousFruitNut", "MiscellaneousVegetable", "MiscellaneousFruitNut", 
           "MiscellaneousVegetable", "Pasture, Range", "MiscellaneousFieldCrops", 
           NA, NA, "MiscellaneousVegetable", "MiscellaneousFieldCrops", 
           "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", "MiscellaneousFieldCrops", 
           "MiscellaneousFieldCrops", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousFruitNut", "Hay,Other", 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "Tangerine&Tangelo", "Tangerine&Tangelo", "Tangerine&Tangelo", 
           "Tomatoes,Fresh", "Tomatoes,Processed", "Vegetable", "MiscellaneousFieldCrops", 
           "MiscellaneousFieldCrops", "MiscellaneousVegetable", NA, NA, 
           "MiscellaneousVegetable", "MiscellaneousVegetable", "MiscellaneousVegetable", 
           "MiscellaneousVegetable", "MiscellaneousFieldCrops", "Walnuts", 
           "Watermelons,Seeded/Seedless", "Wheat", "Wheat", "MiscellaneousVegetable", 
           "MiscellaneousVegetable")

revCross <- enframe(COMMr, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(CROPr, name = NULL, value = "KCACR"))





# Annual Perennial Crosswalk ----------------------------------------------


# master <- read_csv("./Data/1_masterCrosswalk/masterCrosswalk.csv")
# 
# 
# apCross <- master %>% 
#   select(COMM, annual)
# 
# dput(apCross$COMM)
# dput(apCross$annual)


COMMap <- c("ALFALFA", "ALFALFA SEED", "ALMOND", "APPLE", "APRICOT", "ARTICHOKE", 
            "ARUGULA", "ASPARAGUS", "AVOCADO", "BARLEY", "BARLEY FOR/FOD", 
            "BEAN DRIED", "BEAN LIMA", "BEAN SUCCULENT", "BEET", "BEETS, RED", 
            "BLACKBERRY", "BLUEBERRY", "BOK CHOY LSE LF", "BOYSENBERRY", 
            "BROCCOLI", "BROCCOLI SEED", "CABBAGE", "CABBAGE SEED", "CACTUS LEAF", 
            "CACTUS PEAR", "CANTALOUPE", "CARROT", "CAULIFLOWER", "CELERY", 
            "CHERRY", "CHESTNUT", "CHINESE GREEN", "CHIVE", "CILANTRO", "CLOVER", 
            "COLLARD", "CORN FOR/FOD", "CORN, GRAIN", "CORN, HUMAN CON", 
            "COTTON", "CUCUMBER", "DAIKON", "DANDELION GREEN", "DILL", "EGGPLANT", 
            "FENNEL", "FIG", "FLAX", "FORAGE HAY/SLGE", "FRUIT, BERRY", "GAI CHOY LSE LF", 
            "GAI LON TGHT HD", "GARBANZO BEAN", "GARLIC", "GF-VINE", "GOURD", 
            "GRAIN", "GRAPE", "GRAPE, RAISIN", "GRAPE, WINE", "GRAPEFRUIT", 
            "HEMP/CANNABIS", "HERB, SPICE", "HERB, SPICE-ORG", "HERB, SPICE-ORGANIC", 
            "HONEYDEW MELON", "INDUSTRIAL HEMP", "INDUSTRIAL SITE", "JOJOBA BEAN", 
            "JUJUBE", "KALE", "KIWI", "KOHLRABI", "LANDSCAPE MAIN", "LEEK", 
            "LEMON", "LETTUCE HEAD", "LETTUCE HEAD SD", "LETTUCE LEAF", "LETTUCE LEAF SD", 
            "LETTUCE ROMAINE", "LOVEGRASS (FORA", "MELON", "MUSK MELON", 
            "MUSTARD", "MUSTARD GREENS", "N-GRNHS PLANT", "N-OUTDR PLANTS", 
            "NAPA CBG TGHT H", "NECTARINE", "OAT", "OAT FOR/FOD", "OF-BULB", 
            "OILSEED", "OKRA", "OKRA-ORGANIC", "OLIVE", "ONION DRY ETC", 
            "ONION GREEN", "ONION SEED", "OP-CHRSTMS TREE", "OP-DEC. SHRUB", 
            "OP-DEC. TREE", "OP-FLWRNG PLANT", "OP-PALM", "OP-ROSE", "OP-VINE", 
            "ORANGE", "OT-CONIFER", "OT-DEC. TREE", "PAPAYA", "PARSLEY", 
            "PARSNIP", "PASTURELAND", "PEACH", "PEAR", "PEAS", "PECAN", "PEPPER FRUITNG", 
            "PEPPER SPICE", "PERSIMMON", "PISTACHIO", "PLUM", "POMEGRANATE", 
            "POMELO", "POTATO", "PRUNE", "PUMPKIN", "QUINCE", "RADISH", "RANGELAND", 
            "RAPE", "RECREATION AREA", "RESEARCH COMMOD", "RUTABAGA", "RYE", 
            "RYEGRAS FOR/FOD", "SAFFLOWER", "SORGHUM FOR/FOD", "SORGHUM MILO", 
            "SPINACH", "SQUASH", "SQUASH, SUMMER", "SQUASH, SUMMER-", "SQUASH, SUMMER-ORGANIC", 
            "SQUASH, WINTER", "STRAWBERRY", "SUDANGRASS", "SUGARBEET", "SWEET POTATO", 
            "SWISS CHARD", "TANGELO", "TANGERINE", "TANGERINE/SDLS", "TOMATO", 
            "TOMATO PROCESS", "TOMATO SEED", "TRITICALE", "TURF/SOD", "TURNIP", 
            "UNCUL NON-AG", "UNCULTIVATED AG", "VEGETABLE", "VEGETABLE LEAF", 
            "VEGETABLE ROOT", "VEGETABLE SEED", "VETCH", "WALNUT", "WATERMELON", 
            "WHEAT", "WHEAT FOR/FOD", "YAM", "ZUCCHINI")


ap <- c(0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 
        1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 
        1, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
        1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 
        1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 
        0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 
        1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, NA, 1, 1, 1, 
        1, 0, 0, 1, 1, 1, 1, 1)



annualCross <- enframe(COMMap, name = NULL, value = "COMM") %>% 
  bind_cols(enframe(ap, name = NULL, value = "annual"))




# Revenue Crosswalk + Values ----------------------------------------------


# Read in table of crops with revenue $ / acre created in the cropValueClean.R
# script. This is matched to a crosswalk matrix I created based on KCACR
# data and intuition. The crosswalk joins revenue crop categories to kern
# field crop categories


#Read in revenue table
revenueRaw <- read_csv("Data/0_input/cropValue3.csv")

revenue <- revenueRaw %>%
  dplyr::select(CROP, pricePerAcre, hrsAcre) %>% 
  mutate(
    KCACR = CROP,
    .keep = "unused"
  )





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
  pivot_longer(5:25, names_to = "Crop", values_to = "waterUse") %>% 
  dplyr::select(Crop, waterUse)



# Join all Crosswalks -----------------------------------------------------


masterPre <- irisCross %>% 
  left_join(annualCross, by = "COMM") %>% 
  left_join(revCross, by = "COMM") %>% 
  left_join(waterCross, by = "COMM") %>% 
  left_join(water, by = "Crop") %>% 
  left_join(revenue, by = "KCACR")


# Check for missing Values
glimpse(masterPre)

masterPre %>% 
  filter(is.na(COMM))

masterPre %>% 
  filter(is.na(comm_code))

masterPre %>% 
  filter(is.na(annual))

masterPre %>% 
  filter(is.na(KCACR))

masterPre %>% 
  filter(is.na(Crop))

masterPre %>% 
  filter(is.na(waterUse))

masterPre %>% 
  filter(is.na(pricePerAcre))

masterPre %>% 
  filter(is.na(hrsAcre))



# Drop extra columns

masterCrosswalk <- masterPre %>% 
  select(-KCACR, -Crop)


# Export

write_csv(masterCrosswalk, "./Data/1_masterCrosswalk/masterCrosswalk.csv")

