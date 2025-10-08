library(tidyverse)



# Read in shapefiles and write out to one folder --------------------------


# Check final crosswalk ---------------------------------------------------

cross1 <- read_csv("./Data/8_prioritizr/finalCropKey.csv")


cross2 <- read_csv("./Data/9_prioritizrPost/finalCropKey.csv")


cross3 <- read_csv("./Data/10_prioritizrPostRetired/finalCropKeyR.csv")


setdiff(cross2, cross3)


cross2 %>% distinct(value)


full_join(cross2, cross3, by = c("value")) %>% 
  filter(is.na(name.y))



cross1 %>% 
  full_join(cross1, cross2, by = "value") %>% 
  filter(is.na(name.y))

veg1 <- cross1 %>% pull(value) 

veg2 <- pull(cross2, value)


setdiff(veg2, veg1)


sumTable <- read_csv("./Data/summaryTable.csv")


sumTableC <- read_csv("./Data/summaryCropTable.csv")
