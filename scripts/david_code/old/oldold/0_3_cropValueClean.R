library(tidyverse)
library(readxl)


# Read in NAWSPAD ---------------------------------------------------------

nawsRaw1 <- read_csv("Data/0_input/NAWS_A2E197.csv") %>% 
  select(FWID, CROP, FY)

nawsRaw2 <- read_csv("Data/0_input/NAWS_F2Y197.csv") %>% 
  select(FWID, FLC)

naws <- nawsRaw1 %>% 
  left_join(nawsRaw2, by = "FWID") %>% 
  filter(FY == 2015) %>% 
  #filter(FY > 2013, FY < 2017) %>% 
  count(CROP, FLC) %>% 
  group_by(CROP) %>% 
  mutate(
    SUM = sum(n),
    prop = (n / SUM) + 1
  ) %>% 
  filter(FLC == 1) %>% 
  
  mutate(
    match = case_when(CROP == 1 ~ "Field Crops",
                      CROP == 2 ~ "Fruits & Nuts",
                      CROP == 3 ~ "HORTICULTURE", 
                      CROP == 4 ~ "VEGETABLES", 
                      CROP == 5 ~ "MISC/MULT")
  )

# naws %>% 
#   filter(#FLC == 1, 
#          FY == 2015) %>% 
#   count(CROP) %>% 
#   mutate(
#     SUM = sum(n),
#     prop = n / SUM
#   )



# Read in Sector Key ------------------------------------------------------

# Information about employment per ag sector

# Read in QCEW industry sector codes, copied into text file from 
# QCEW website. 
codes <- read_lines("./Data/0_input/employmentCodes.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("Code", "Industry Title"), sep = "\\t") %>% 
  slice(-1)






# Clean Job Number information Data ---------------------------------------


# Using employment data from 2015, get the number of jobs per ag industry that 
# year. Kern has a very high number of farm contractors, that are not specific
# to industry, so I added them to the industry specific workers while mainting 
# the same proportion of labor between industries. 



# QCEW employment data from 2015
singleFileRaw <- read_csv("./Data/0_input/2015_annual_singlefile/2015.annual.singlefile.csv") 
#singleFileRaw <- read_csv("./Data/0_input/2016.annual.singlefile.csv")

singleFile <- singleFileRaw %>% 
  # Filter to kern county
  filter(area_fips == "06029") %>% 
  # Isolate # jobs and industry
  left_join(codes, by = c("industry_code" = "Code")) %>% 
  select(`Industry Title`,
         industry_code,
         #annual_avg_estabs, 
         annual_avg_emplvl, 
         #total_annual_wages, taxable_annual_wages, 
         #annual_avg_wkly_wage, avg_annual_pay
  ) %>% 
  # Filter to ag industries (111) and ag support (115) 
  filter(str_detect(industry_code, "^111..$") | str_detect(industry_code, "^115...$")) %>% 
  # Clean columns
  mutate(
    industry = str_replace(`Industry Title`, "NAICS", ""),
    industry = str_replace_all(industry, "[:digit:]", ""),
    jobs = annual_avg_emplvl,
    .keep = "unused"
  ) %>% 
  select(industry, everything()) #%>% 
  # Filter out industries with no workers
  #filter(jobs != 0) 


singleFile



# Find California Workers per crop area -----------------------------------
# 
# 
# singleFileRaw %>% 
#   mutate(
#     area_fips2 = as.numeric(area_fips)
#   ) %>%
#   #filter(area_fips2 %>% is.na()) %>% 
#   #distinct(area_fips2, area_fips)
#   
#   
#   
#   # Filter to kern county
#   filter(area_fips2 >= 06000, area_fips2 < 7000) %>% 
#   
#   # filter(area_fips2 > 6200) %>% 
#   # distinct(area_fips2)
#   # Isolate # jobs and industry
#   left_join(codes, by = c("industry_code" = "Code")) %>% 
#   select(area_fips,
#          area_fips2,
#          `Industry Title`,
#          industry_code,
#          #annual_avg_estabs, 
#          annual_avg_emplvl, 
#          #total_annual_wages, taxable_annual_wages, 
#          #annual_avg_wkly_wage, avg_annual_pay
#   ) %>% 
#   # Filter to ag industries (111) and ag support (115) 
#   filter(str_detect(industry_code, "^111..$") | str_detect(industry_code, "^115...$")) %>% 
#   # Clean columns
#   mutate(
#     industry = str_replace(`Industry Title`, "NAICS", ""),
#     industry = str_replace_all(industry, "[:digit:]", ""),
#     jobs = annual_avg_emplvl,
#     .keep = "unused"
#   ) %>% 
#   group_by(industry, industry_code) %>% 
#   summarise(jobs = sum(jobs)) %>% 
#   view()
#   select(industry, everything()) #%>% 
# # Filter out industries with no workers
# #filter(jobs != 0) 
# 



# Join QCEW and NAWSPAD ---------------------------------------------------

# 
# # This code is to create a csv that can easily be modified in excel to make
# # a crop / industry crosswalk by hand. It's written out, modified in excel,
# # and written back in
# 
# 
# 
# padCrop <- naws %>% 
#   #filter(!is.na(pricePerAcre)) %>% 
#   pull(match)
# 
# 
# indVect <- singleFile %>%
#   mutate(industry = str_trim(industry)) %>% 
#   #arrange(industry) %>% 
#   pull(industry) %>% 
#   unique()
# 
# 
# 
# newVect2 <- c(padCrop, rep(NA, (length(indVect) - length(padCrop))))
# 
# 
# overIt <- enframe(newVect2, name = NULL, value = 'crop') %>% 
#   bind_cols(enframe(indVect, name = NULL, value = "industry")) %>% 
#   mutate(
#     match = NA
#   ) %>% 
#   select(industry, match, crop)
# 
# write_csv(overIt, "./Data/0_input/jobCrosswalkRaw2.csv")
# 
# 
# 
# # Made changes by hand
# crossWalkhand <- read_csv("./Data/0_input/jobCrosswalkRaw2.csv") %>%
#   select(industry, match)
# 
# 
# # Get columns as vector output to console
# 
# 
# dput(crossWalkhand$industry)
# dput(crossWalkhand$match)


# Copy back into script for easier modifications

industry <- c("Soil preparation, planting, and cultivating", "Wheat farming", 
           "Rice farming", "Other grain farming", "Vegetable and melon farming", 
           "Orange groves", "Citrus (except orange) groves", "Noncitrus fruit and tree nut farming", 
           "Food crops grown under cover", "Nursery and floriculture production", 
           "Cotton farming", "Hay farming", "All other crop farming", "Cotton ginning", 
           "Crop harvesting, primarily by machine", "Postharvest crop activities (except cotton ginning)", 
           "Farm labor contractors and crew leaders", "Farm management services", 
           "Support activities for animal production")



match <- c(NA, "Field Crops", "Field Crops", "Field Crops", "VEGETABLES", 
           "Fruits & Nuts", "Fruits & Nuts", "Fruits & Nuts", "HORTICULTURE", 
           "HORTICULTURE", "Field Crops", "Field Crops", "MISC/MULT", NA, 
           NA, NA, NA, NA, NA)

# Turn into tibble
jobCross2 <- enframe(industry, name = NULL, value = "industry") %>% 
  bind_cols(enframe(match, name = NULL, value = "Crop"))







# Distribute FLC ----------------------------------------------------------


FLC <- singleFile %>% 
  filter(industry_code == "115115") %>% 
  {{sum(.$jobs)}}

jobWeighted <- singleFile %>% 
  mutate(industry = str_trim(industry)) %>% 
  left_join(jobCross2, by = "industry") %>% 
  left_join(naws, by = c("Crop" = "match")) %>% 
  #group_by(Crop) %>% 
  # mutate(
  #   n = n(),
  #   prop = prop / n,
  #   jobsAdjusted = round(prop * FLC + jobs)
  # ) %>% 
  mutate(
    jobsAdjusted = round(jobs * prop)
  ) %>% 
  filter(!is.na(jobsAdjusted))


jobWeighted %>% 
  select(industry, Crop,jobs, jobsAdjusted, prop) %>% 
  view()





write_csv(jobWeighted, "./Data/0_input/jobsPerSector.csv")


jA <- sum(jobWeighted$jobsAdjusted)
j <- sum(jobWeighted$jobs)


# Crop Value Estimate -----------------------------------------------------


# Text Files copied from KCACR pdf's. Some formatting was done in text editor
# to be able to read in. Potential improvements are doing a read by line and 
# using stringr package so more reproducible. 

# Read in .txt files copied from PDF KCACR
try <- read_table("Data/0_input/cropYield.txt", na = "---", comment = "*")
try2 <- read_table("Data/0_input/cropYield2.txt", na = "---", comment = "*")


# Format Table ------------------------------------------------------------


# Join and format crop value tables, to get revenue per acre for major crop
# types. 


#Join two tables and clean
cropValue <- try2 %>%
  mutate(
    PRODUCTION = as.character(PRODUCTION)
  ) %>% 
  bind_rows(try)  %>% 
  #Clean values and set column datatypes
  mutate(
    VALUE = parse_number(str_replace(VALUE, "^[:alpha:]/", "")),
    PRODUCTION = parse_number(str_replace(PRODUCTION, "^[:alpha:]/", "")),
    YEAR = str_match(YEAR, "[:digit:]+"),
    YEAR = YEAR[,1] %>% as.numeric()
  ) %>% 
  #Add row numbers, 
  mutate(marker = row_number() %% 2) %>% 
  #modulo divide by 2 to get even odd, 
  #get rid of odd (2015) row designation (NA) 
  mutate(
    marker = na_if(marker, 1)
  ) %>% 
  #copy crop name to row marker and fill up
  mutate(
    marker = ifelse(marker == 0, CROP, marker)
  ) %>% 
  fill(marker, .direction = "up") %>% 
  #then simple drop 2014 data, str_c mutate
  filter(YEAR == '2015') %>% 
  mutate(
    #Replaces "d" filler value with blank
    marker = str_replace(marker, "^d$", ""),
    #Join two columns of names together
    CROP = str_c(CROP, marker)
  ) %>% 
  select(-marker) %>% 
  #arrange(CROP) %>% 
  #Find crop revenue per acre
  mutate(
    prodPerAcre = PRODUCTION / ACRES,
    pricePerUnit = VALUE_1 / PRODUCTION,
    pricePerAcre = prodPerAcre * pricePerUnit
  )


#write_csv(cropValue, "Data/0_input/cropValue2.csv")



# # Join job and revenue ----------------------------------------------------
# 
# 
# # This code is to create a csv that can easily be modified in excel to make
# # a crop / industry crosswalk by hand. It's written out, modified in excel,
# # and written back in
# 
# 
# 
# cropValueVec <- cropValue %>% 
#   filter(!is.na(pricePerAcre)) %>% 
#   pull(CROP) %>% 
#   unique()
# 
# 
# jobVect <- jobWeighted %>%
#   mutate(industry = str_trim(industry)) %>% 
#   #arrange(industry) %>% 
#   pull(industry) %>% 
#   unique()
# 
# 
# 
# newVect <- c(jobVect, rep(NA, (length(cropValueVec) - length(jobVect))))
# 
# 
# overIt <- enframe(cropValueVec, name = NULL, value = 'crop') %>% 
#   bind_cols(enframe(newVect, name = NULL, value = "industry")) %>% 
#   mutate(
#     match = NA
#   ) %>% 
#   select(crop, match, industry)
# 
# write_csv(overIt, "./Data/0_input/jobCrosswalkRaw.csv")
# 
# 
# 
# 
# 
# # Read in crosswalk -------------------------------------------------------
# 
# 
# # Making crosswalks is easier by hand in excel, however not very reproducible, 
# # difficult to keep track of. Making crosswalks in R is very tedious, but 
# # easier to keep track of / modify. This code is to take the jobCrossWalkRaw
# # file modifications made in excel, and output the code required to make the 
# # vectors by hand. For future use / reference. 
# 
# 
# # # Made changes by hand
# # crossWalkhand <- read_csv("./Data/0_input/jobCrosswalkRaw2.csv") %>% 
# #   select(crop, match)
# # 
# # 
# # # Get columns as vector output to console
# # 
# # 
# # dput(crossWalkhand$crop)
# # dput(crossWalkhand$match)
# 
# 
# # Copy back into script for easier modifications
# 
# crop <- c("Cotton", "Field", "Vegetable", "FruitandNutTrees&Vines", "OrnamentalTrees&Shrubs", 
#           "Roses", "Almonds", "Apricots", "Blueberries", "Cherries", "Citrus,All", 
#           "Grapefruit", "Lemons", "Oranges,Navels", "Oranges,Valencia", 
#           "Tangerine&Tangelo", "Grapes,All", "RaisinVariety", "TableVariety", 
#           "WineVariety", "Pistachios", "Plums", "Tomatoes,Fresh", "Tomatoes,Processed", 
#           "Walnuts", "MiscellaneousFruitNut", "Barley", "Beans,DryEdible", 
#           "CottonLint,Upland&Acala", "CottonLint,Pima", "Hay,Alfalfa", 
#           "Hay,Grain", "Hay,Other", "SilageandForage", "Wheat", "MiscellaneousFieldCrops", 
#           "Garlic,Fresh", "Garlic,Processed", "Lettuce,Head", "Onions,Fresh", 
#           "Onions,Dehydrator", "Peppers,BellFresh", "Potatoes,All", "Potatoes,Spring", 
#           "Potatoes,Winter", "Watermelons,Seeded/Seedless", "MiscellaneousVegetable"
#           )
# 
# 
# match <- c("Cotton farming", "Hay farming", "Vegetable and melon farming", 
#            "Noncitrus fruit and tree nut farming", "Nursery and floriculture production", 
#            "Nursery and floriculture production", "Noncitrus fruit and tree nut farming", 
#            "Noncitrus fruit and tree nut farming", "Food crops grown under cover", 
#            "Noncitrus fruit and tree nut farming", "Citrus (except orange) groves", 
#            "Citrus (except orange) groves", "Citrus (except orange) groves", 
#            "Orange groves", "Orange groves", "Citrus (except orange) groves", 
#            "Citrus (except orange) groves", "Citrus (except orange) groves", 
#            "Citrus (except orange) groves", "Citrus (except orange) groves", 
#            "Noncitrus fruit and tree nut farming", "Noncitrus fruit and tree nut farming", 
#            "Food crops grown under cover", "Food crops grown under cover", 
#            "Noncitrus fruit and tree nut farming", "Noncitrus fruit and tree nut farming", 
#            "Hay farming", "All other crop farming", "Cotton farming", "Cotton farming", 
#            "Hay farming", "Hay farming", "Hay farming", "Hay farming", "All other crop farming", 
#            "All other crop farming", "Vegetable and melon farming", "Vegetable and melon farming", 
#            "Vegetable and melon farming", "Vegetable and melon farming", 
#            "Vegetable and melon farming", "Vegetable and melon farming", 
#            "Vegetable and melon farming", "Vegetable and melon farming", 
#            "Vegetable and melon farming", "Vegetable and melon farming", 
#            "Vegetable and melon farming"
#            )
# 
# # Turn into tibble
# jobCross <- enframe(crop, name = NULL, value = "crop") %>% 
#   bind_cols(enframe(match, name = NULL, value = "industry"))
# 
# 
# 
# export <- jobCross %>% 
#   bind_cols(select(overIt, industry) %>% mutate(ind = industry), )
# 
# 
# 
# write_csv(export, "Data/0_input/jobCrosswalkRaw3.csv")
# 
# 
# 
# # Making crosswalks is easier by hand in excel, however not very reproducible, 
# # difficult to keep track of. Making crosswalks in R is very tedious, but 
# # easier to keep track of / modify. This code is to take the jobCrossWalkRaw
# # file modifications made in excel, and output the code required to make the 
# # vectors by hand. For future use / reference. 
# 
# 
# # Made changes by hand
# crossWalkhand <- read_csv("./Data/0_input/jobCrosswalkRaw3.csv") %>%
#   select(1, 2) %>% 
#   mutate(
#     match = industry...2,
#     .keep = "unused"
#   )
# 
# 
# # Get columns as vector output to console
# 
# 
# dput(crossWalkhand$crop)
# dput(crossWalkhand$match)


crop <- c("Cotton", "Field", "Vegetable", "FruitandNutTrees&Vines", "OrnamentalTrees&Shrubs", 
          "Roses", "Almonds", "Apricots", "Blueberries", "Cherries", "Citrus,All", 
          "Grapefruit", "Lemons", "Oranges,Navels", "Oranges,Valencia", 
          "Tangerine&Tangelo", "Grapes,All", "RaisinVariety", "TableVariety", 
          "WineVariety", "Pistachios", "Plums", "Tomatoes,Fresh", "Tomatoes,Processed", 
          "Walnuts", "MiscellaneousFruitNut", "Barley", "Beans,DryEdible", 
          "CottonLint,Upland&Acala", "CottonLint,Pima", "Hay,Alfalfa", 
          "Hay,Grain", "Hay,Other", "SilageandForage", "Wheat", "MiscellaneousFieldCrops", 
          "Garlic,Fresh", "Garlic,Processed", "Lettuce,Head", "Onions,Fresh", 
          "Onions,Dehydrator", "Peppers,BellFresh", "Potatoes,All", "Potatoes,Spring", 
          "Potatoes,Winter", "Watermelons,Seeded/Seedless", "MiscellaneousVegetable"
)

match <- c("Cotton farming", "Other grain farming", "Vegetable and melon farming", 
           "Noncitrus fruit and tree nut farming", "Nursery and floriculture production", 
           "Nursery and floriculture production", "Noncitrus fruit and tree nut farming", 
           "Noncitrus fruit and tree nut farming", "Food crops grown under cover", 
           "Noncitrus fruit and tree nut farming", "Citrus (except orange) groves", 
           "Citrus (except orange) groves", "Citrus (except orange) groves", 
           "Orange groves", "Orange groves", "Citrus (except orange) groves", 
           "Citrus (except orange) groves", "Citrus (except orange) groves", 
           "Citrus (except orange) groves", "Citrus (except orange) groves", 
           "Noncitrus fruit and tree nut farming", "Noncitrus fruit and tree nut farming", 
           "Food crops grown under cover", "Food crops grown under cover", 
           "Noncitrus fruit and tree nut farming", "Noncitrus fruit and tree nut farming", 
           "Hay farming", "All other crop farming", "Cotton farming", "Cotton farming", 
           "Hay farming", "Hay farming", "Hay farming", "Other grain farming", 
           "Wheat farming", "All other crop farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming"
           )


# Turn into tibble
jobCross <- enframe(crop, name = NULL, value = "crop") %>% 
  bind_cols(enframe(match, name = NULL, value = "industry"))




# Join to crop value table ------------------------------------------------


# Join job industry to crop value, find jobs per acre per industry type,
# and join to final crop value table. 


cropValue2 <- cropValue %>% 
  filter(!is.na(pricePerAcre)) %>% 
  left_join(jobCross, by = c("CROP" = "crop"))


jobsPerAcreTable <- cropValue2 %>% 
  group_by(industry) %>% 
  summarise(
    acres = sum(ACRES)
  ) %>% 
  left_join(jobWeighted, by = "industry") %>% 
  mutate(
    jobsPer100Acre = (jobsAdjusted / acres) * 100
  ) %>% 
  select(industry, jobsPer100Acre)

finalTable <- cropValue2 %>% 
  left_join(jobsPerAcreTable, by = "industry") %>% 
  select(-industry)


write_csv(finalTable, "./Data/0_input/cropValue3.csv")

# Appendix ----------------------------------------------------------------




# 
# oldCropValue <- read_csv("Data/0_input/cropValue.csv")
# 
# total <- cropValue %>% 
#   bind_rows(oldCropValue) %>% 
#   select(-PERACRE:-pricePerAcre)
#   
# 
# 
# total[!duplicated(total), ] %>% 
#   view()
# 
# cropValueVect <- pull(cropValue, CROP) %>% 
#   unique()
# 
# # Test to see if string matching could be a viable option 
# test <- expand_grid(cropValueVec, jobVect) 
# 
# test %>% 
#   mutate(
#     crop = str_to_lower(cropValueVec) %>% str_trim(),
#     industry = str_to_lower(jobVect) %>% str_trim(),
#     .keep = "unused"
#   ) %>% 
#   mutate(
#     detect = str_detect(industry, str_sub(crop, 1, 4))
#   ) %>% 
#   filter(detect) %>% 
#   view()
