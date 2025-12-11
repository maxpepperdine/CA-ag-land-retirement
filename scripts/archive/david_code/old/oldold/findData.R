library(tidyverse)

dat <- read_csv("availableCropData.csv")

datN <- read_csv("availableCropNorth.csv") %>% 
  anti_join(dat, by = c("Commodity", "Region", "Year"))

dat3 <- read_csv("dat3.csv")

dat4 <- read_csv("dat4.csv") %>% 
  anti_join(dat3, by = c("Commodity", "Region", "Year"))



data <- dat %>% 
  bind_rows(datN, dat3, dat4)



glimpse(dat)


recent <- dat %>% 
  filter(Year > 2010) 

recentN <- datN %>% 
  filter(Year > 2010)


recentAll <- data %>% 
  filter(Year > 2010, !str_detect(Commodity, "Sheep"), !str_detect(Commodity, "Beef")) %>%
  arrange(Commodity, Year, desc(Region)) 

recentAll %>% 
  slice(c(4, 10, 15:21, 23:48, 52, 53, 54, 56:60, 62)) %>% 
  view()
