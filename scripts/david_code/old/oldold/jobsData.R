library(tidyverse)

# 
# dat <- read_csv("./Data/jobs.csv")
# 
# 
# dput(dat$Crop)
# dput(dat$Year)
# dput(dat$Labor)


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



newDat2 <- newDat %>% 
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

