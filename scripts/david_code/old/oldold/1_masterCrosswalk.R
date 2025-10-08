# Load Packages -----------------------------------------------------------

library(lubridate)
library(tidyverse)
library(readxl)


# Revenue Crosswalk + Values ----------------------------------------------


#Read in table of crops with revenue $ / acre created in the cropValueClean.R
#script. This is matched to a crosswalk matrix I created based on KCACR
#data and intuition. The crosswalk joins revenue crop categories to kern
#field crop categories


#Read in revenue table
revenueRaw <- read_csv("cropValue.csv")

revenue <- revenueRaw %>%
  select(CROP, pricePerAcre)


# #Match Field COMM crop codes to KCACR data
# revenueCrosswalkRaw <- read_csv("kernFieldCropTypes.csv") 
# 
# 
# 
# 
# # Final Crosswalk ---------------------------------------------------------
# 
# 
# 
# revenueCrosswalk <- revenueCrosswalkRaw %>%
#   mutate(
#     CROP = case_when(
#       !is.na(Yes) ~ Yes,
#       !is.na(Maybe) ~ Maybe,
#       !is.na(Guess) ~ Guess
#     ),
#     .keep = "unused"
#   ) %>%
#   left_join(revenue, by = "CROP") %>% 
#   mutate(
#     pricePerAcre = ifelse((COMM == "UNCULTIVATED AG"), 0, pricePerAcre) 
#   )
# 
# 
# 
# # Join Crop COMM to COMMCODES ---------------------------------------------
# 
# 
# 
# # Crosswalk between crop COMM Codes (string) and iris COMM Codes (numeric)
# commWalk <- read_csv("./comm_codes.csv")
# 
# 
# 
# # Exploration of multiple COMM's per CODE
# 
# # 
# commKeyRaw <- revenueCrosswalk %>%
#   select(COMM) %>% 
#   distinct(COMM) %>% 
#   left_join(commWalk, by = c("COMM" = "comm")) %>% 
#   group_by(comm_code) %>% 
#   mutate(
#     n = n()
#   ) %>% 
#   # filter(n > 1) %>% 
#   left_join(revenueCrosswalk, by = "COMM") %>% 
#   select(COMM, comm_code, pricePerAcre) %>% 
#   arrange(comm_code, desc(pricePerAcre)) %>% 
#   filter(duplicated(comm_code) == FALSE)
# 
# 
# 
# # Check iris comm codes against mine 
# 
# fieldCodes <- commKeyRaw %>% 
#   pull(comm_code) %>% 
#   unique()
# 
# irisCodes <- irisFields %>% 
#   filter(commcodeFields == 77000) %>% 
#   pull(commcodeLastActive) %>% 
#   unique() %>% 
#   sort()
# 
# 
# missingCrops <- setdiff(irisCodes, fieldCodes)
# 
# 
# 
# missing <- commWalk %>% 
#   filter(comm_code %in% missingCrops) 
# 
# missingCommFields <- irisFields %>% 
#   filter(commcodeFields == 77000) %>% 
#   filter(commcodeLastActive %in% missingCrops)
# 
# 
# missingCommFieldsMap <- missingCommFields %>% 
#   tm_shape() + tm_borders(col = "red")
# 
# 
# 
# # Fill in Missing codes ---------------------------------------------------
# 
# emptyTable <- revenueCrosswalk %>% bind_rows(missing) %>% 
#   unite(col = "COMM1", c(COMM, comm), na.rm = TRUE) %>% 
#   arrange(COMM1)
# 
# 
# 
# 
# emptyTable %>% 
#   filter(is.na(CROP))
# 
# 
# 
# write_csv(emptyTable, "revCrosswalkIntermediate.csv")
# 
# 
# 
# 
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
waterRaw <- read_xlsx("waterUse.xlsx")


water <- waterRaw %>%
  filter(County == '15_Kern') %>%
  pivot_longer(5:25, names_to = "Crop", values_to = "waterUse")

# #Join to crosswalk
# waterCrosswalkRaw <- read_csv("fieldWaterUseCrosswalk.csv")
# 
# waterCrosswalk <- waterCrosswalkRaw %>% 
#   unite("Crop", Yes, Maybe, Guess, na.rm = TRUE) %>% 
#   mutate(
#     #Do some string cleaning
#     Crop = na_if(Crop, ""),
#     Crop = str_replace(Crop, "Vineyards", "Vineyard")
#   ) %>% 
#   left_join(water, by = "Crop") %>% 
#   mutate(
#     waterUse = replace_na(waterUse, 0)
#   ) %>% 
#   select(-c("Year", "RO", "HR", "County"))
# 
# 
# emptyWater <- waterCrosswalk %>% bind_rows(missing) %>% 
#   unite(col = "COMM1", c(COMM, comm), na.rm = TRUE) %>% 
#   arrange(COMM1)
# 
# write_csv(emptyWater, "waterCrosswalkIntermediate.csv")
# 



# Read in modified crosswalks ---------------------------------------------

waterFinal <- read_csv("waterCrosswalkFinal.csv") %>% 
  left_join(water, by = "Crop") %>% 
  select(COMM1, waterUse.y)


revFinal <- read_csv("revCrosswalkFinal.csv") %>% 
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


commWalk <- read_csv("comm_codes.csv")


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



