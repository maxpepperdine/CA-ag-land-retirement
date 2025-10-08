library(tidyverse)
library(readxl)





# Build Ag Budget Table ---------------------------------------------------


Crop <- c("Field Corn", NA, "Silage Corn", NA, "Alfalfa", NA, NA, NA, 
          NA, NA, "Raisins", NA, NA, NA, NA, NA, "Processing Tomatoes", 
          NA, NA, "Wine Grapes", NA, NA, NA, "Wine Grapes", NA, NA, NA, 
          "Wine Grapes", NA, NA, NA, "Wine Grapes", NA, NA, NA, "Pistachios", 
          NA, "Almonds", NA, NA, "Baby Lima Beans", NA, "Raisins", NA, 
          NA, NA, NA, "Wine Grapes", NA, NA, "Large Lima Beans", NA, "Plums", 
          NA, NA, "Grain Sorghum", NA, "Sorghum Silage", NA, "Processing Peaches", 
          NA, "Walnuts", NA, "Garbanzo Beans", NA, "Garbanzo Beans", NA, 
          "Table Grapes", NA, NA, NA, NA, NA, NA, NA, "Table Grapes", NA, 
          NA, NA, NA, NA, NA, NA, "Table Grapes", NA, NA, NA, NA, NA, NA, 
          NA, "Table Grapes", NA, NA, NA, NA, NA, NA, NA, "Asparagus", 
          NA, "Blackeye Beans", NA, "Blackeye Beans", NA, "Cherries", NA, 
          "Corn Silage", NA, "Field Corn", NA, "Cotton", NA, NA, "Cotton", 
          NA, NA, "Cotton", NA, NA, "Wine Grapes", NA, "Lemons", NA, NA, 
          "Mandarins", NA, "Oranges", NA, NA, "Small Grain Silage", NA, 
          "Wheat", NA)

Year <- c(2015, NA, 2015, NA, 2016, NA, NA, NA, NA, NA, 2016, NA, NA, 
          NA, NA, NA, 2018, NA, NA, 2019, NA, NA, NA, 2019, NA, NA, NA, 
          2019, NA, NA, NA, 2019, NA, NA, NA, 2015, NA, 2016, NA, NA, 2016, 
          NA, 2016, NA, NA, NA, NA, 2016, NA, NA, 2016, NA, 2016, NA, NA, 
          2016, NA, 2016, NA, 2017, NA, 2017, NA, 2018, NA, 2018, NA, 2018, 
          NA, NA, NA, NA, NA, NA, NA, 2018, NA, NA, NA, NA, NA, NA, NA, 
          2018, NA, NA, NA, NA, NA, NA, NA, 2018, NA, NA, NA, NA, NA, NA, 
          NA, 2013, NA, 2013, NA, 2013, NA, 2017, NA, 2012, NA, 2015, NA, 
          2012, NA, NA, 2012, NA, NA, 2012, NA, NA, 2012, NA, 2015, NA, 
          NA, 2011, NA, 2015, NA, NA, 2013, NA, 2013, NA)


Labor <- c(3.89, 7.75, 0.86, 7.5, 1.6, 1, 1.17, 0.02, 1.8, 3.34, 12.93, 
           24, 36, 2.82, 15, 0.75, 7.39, 3.69, 11.55, 8.41, 6.5, 5.5, 0.75, 
           8.53, 9, 5.5, 0.75, 7.67, 7.5, 5.5, 0.75, 8.41, 8.55, 5.5, 0.75, 
           13.05, 4.27, 9.16, 8.07, 1, 2.66, 3, 10.83, 24, 36, 2.82, 13, 
           7.88, 60.5, 6.4, 2.66, 3, 7.07, 84, 13.5, 2.53, 0.4, 2.53, 0.4, 
           17.99, 156.06, 5.96, 2.17, 4.21, 6, 3.95, 7.15, 17.13, 95, 3, 
           154, 2, 8, 219, 480, 15.45, 3, 85, 2, 8, 85, 140, 347, 17.28, 
           112, 3, 1.75, 8.5, 95, 170, 427, 14.9, 3, 85, 2, 8, 85, 140, 
           410, 20.34, 12.14, 2.77, 1.41, 2.77, 1.81, 13.28, 54.45, 2, 0.9, 
           2.93, 7.5, 4.73, 4, 2.8, 4.61, 4, 2.8, 4.99, 4, 0.3, 8.55, 59.5, 
           9.5, 5, 5.65, 9.2, 16.74, 9.5, 7.74, 9.1, 0.91, 0.6, 0.82, 0.75
)

newDat <- enframe(Crop, name = NULL, value = "Crop") %>% 
  bind_cols(enframe(Year, name = NULL, value = "Year")) %>% 
  bind_cols(enframe(Labor, name = NULL, value = "Labor"))




# Clean Ag Budget Table ---------------------------------------------------



budget <- newDat %>% 
  #group_by(Crop) %>% 
  mutate(
    id = if_else(!is.na(Crop), row_number(), NA)
  ) %>% 
  fill(id) %>% 
  group_by(id) %>% 
  summarise(
    Crop = first(Crop),
    Year = first(Year),
    Labor = sum(Labor)
  ) %>% 
  group_by(Crop, Year) %>% 
  mutate(
    Labor2 = mean(Labor) %>% round(2)
  ) %>%
  group_by(Crop) %>% 
  mutate(
    Labor3 = mean(Labor2) %>% round(2)
  ) %>% 
  select(Crop, Labor3) %>% 
  filter(duplicated(Crop) == F)

write_csv(budget, "./Data/0_input/jobsBudget.csv")




# Build QCEW Table --------------------------------------------------------





# Information about employment per ag sector

# Read in QCEW industry sector codes, copied into text file from 
# QCEW website. 
codes <- read_lines("./Data/0_input/employmentCodes.txt") %>% 
  as_tibble() %>% 
  separate(value, into = c("Code", "Industry Title"), sep = "\\t") %>% 
  slice(-1)





# Clean QCEW --------------------------------------------------------------


# Using employment data from 2015, get the number of jobs per ag industry that 
# year. Kern has a very high number of farm contractors, that are not specific
# to industry, so I added them to the industry specific workers while mainting 
# the same proportion of labor between industries. 



# QCEW employment data from 2015
singleFileRaw <- read_csv("./Data/0_input/2015_annual_singlefile/2015.annual.singlefile.csv") 

singleFile <- singleFileRaw %>% 
  # Filter to kern county
  filter(area_fips == "06029") %>% 
  # Isolate # jobs and industry
  left_join(codes, by = c("industry_code" = "Code")) %>% 
  select(`Industry Title`,
         industry_code,
         annual_avg_emplvl, 
  ) %>% 
  # Filter to ag industries (111)
  filter(str_detect(industry_code, "^111..$")) %>% 
  # Clean columns
  mutate(
    industry = str_replace(`Industry Title`, "NAICS", ""),
    industry = str_replace_all(industry, "[:digit:]", ""),
    jobs = annual_avg_emplvl,
    .keep = "unused"
  ) %>% 
  select(industry, everything()) %>% 
# Filter out industries with no workers
  filter(jobs != 0) %>% 
  mutate(
    industry = trimws(industry)
  )


singleFile



# Crop Value Estimate -----------------------------------------------------


# Text Files copied from KCACR pdf's. Some formatting was done in text editor
# to be able to read in. Potential improvements are doing a read by line and 
# using stringr package so more reproducible. 

# Read in .txt files copied from PDF KCACR
try <- read_table("Data/0_input/cropYield.txt", na = "---", comment = "*")
try2 <- read_table("Data/0_input/cropYield2.txt", na = "---", comment = "*")


# Clean KCACR Crop Value Table --------------------------------------------



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




# Build Budget / KCACR Crosswalk ------------------------------------------




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

match <- c("Cotton", "Alfalfa", "Asparagus", "Walnuts", "Raisins", "Raisins", 
           "Almonds", "Processing Peaches", "Cherries", "Cherries", "Oranges", 
           "Mandarins", "Lemons", "Oranges", "Oranges", "Mandarins", "Raisins", 
           "Raisins", "Table Grapes", "Wine Grapes", "Pistachios", "Plums", 
           "Processing Tomatoes", "Processing Tomatoes", "Walnuts", "Cherries", 
           "Alfalfa", "Garbanzo Beans", "Cotton", "Cotton", "Alfalfa", "Alfalfa", 
           "Alfalfa", "Small Grain Silage", "Wheat", "Wheat", "Raisins", 
           "Raisins", "Asparagus", "Asparagus", "Asparagus", "Asparagus", 
           "Asparagus", "Asparagus", "Asparagus", "Mandarins", "Field Corn"
)


# Turn into tibble
budgetCross <- enframe(crop, name = NULL, value = "crop") %>% 
  bind_cols(enframe(match, name = NULL, value = "Crop"))




# Join to crop value table ------------------------------------------------


# Join job industry to crop value, find jobs per acre per industry type,
# and join to final crop value table. 


cropValue2 <- cropValue %>% 
  filter(!is.na(pricePerAcre)) %>% 
  left_join(budgetCross, by = c("CROP" = "crop"))


addBudget <- cropValue2 %>% 
  left_join(budget, by = "Crop") %>% 
  mutate(
    hrsAcre = Labor3,
    .keep = "unused"
  ) 



# Build QCEW / KCACR Crosswalk --------------------------------------------



crop1 <- c("Cotton", "Field", "Vegetable", "FruitandNutTrees&Vines", "OrnamentalTrees&Shrubs", 
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

match1 <- c("Cotton farming", "Hay farming", "Vegetable and melon farming", 
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
           "Hay farming", "Hay farming", "Hay farming", "All other crop farming", 
           "Wheat farming", "All other crop farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming", 
           "Vegetable and melon farming", "Vegetable and melon farming"
)


# Turn into tibble
qCEWCross <- enframe(crop1, name = NULL, value = "crop") %>% 
  bind_cols(enframe(match1, name = NULL, value = "industry"))




# Join to crop value table ------------------------------------------------


# Join job industry to crop value, find jobs per acre per industry type,
# and join to final crop value table. 


addQCross <- addBudget %>% 
  filter(!is.na(pricePerAcre)) %>% 
  left_join(qCEWCross, by = c("CROP" = "crop"))


jobsPerAcreTable <- addQCross %>% 
  group_by(industry) %>% 
  summarise(
    acres = sum(ACRES)
  ) %>% 
  left_join(singleFile, by = "industry") %>% 
  mutate(
    jobsPer100Acre = (jobs / acres) * 100
  ) %>% 
  select(industry, jobsPer100Acre)

finalTable <- addQCross %>% 
  left_join(jobsPerAcreTable, by = "industry") %>% 
  select(-industry)

# finalTable <- cropValue2 %>% 
#   left_join(jobsPerAcreTable, by = "industry") %>% 
#   select(-industry)


write_csv(finalTable, "./Data/0_input/cropValue3.csv")

# Appendix ----------------------------------------------------------------



