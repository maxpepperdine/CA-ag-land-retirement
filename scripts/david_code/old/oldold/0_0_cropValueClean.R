library(tidyverse)

# Crop Value Estimate -----------------------------------------------------


#Read in .txt files copied from PDF KCACR
try <- read_table("Data/0_input/cropYield.txt", na = "---", comment = "*")
try2 <- read_table("Data/0_input/cropYield2.txt", na = "---", comment = "*")


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
    YEAR = YEAR[,1]
  ) %>% 
  #Add row numbers, 
  mutate(marker = row_number() %% 2) %>% 
  #modulo divide by 2 to get even odd, 
  #get rid of odd (2015) row designation (NA) 
  na_if(1) %>% 
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
  arrange(CROP) %>% 
  #Find crop revenue per acre
  mutate(
    prodPerAcre = PRODUCTION / ACRES,
    pricePerUnit = VALUE_1 / PRODUCTION,
    pricePerAcre = prodPerAcre * pricePerUnit
  ) 


write_csv(cropValue, "Data/0_input/cropValue.csv")
