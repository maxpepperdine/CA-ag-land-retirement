library(tidyverse)
library(rmapshaper)
library(sf)
library(smoothr)
library(tmap)
library(terra)


source("Scripts/0_startup/0_2_functions.R")


#tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Outline -----------------------------------------------------------------

outlineRaw <- maxFW <- read_sf("Data/10_prioritizr/maxFW.shp") 

outline <- outlineRaw %>% 
  tm_shape() +
  tm_fill()


# Template Raster ---------------------------------------------------------


rast_template  <-  rast(ext = ext(outlineRaw), 
                        resolution = 100,
                        crs = st_crs(outlineRaw)$wkt)





# Read In Data ------------------------------------------------------------


# Max FW

maxFW <- read_sf("Data/10_prioritizr/maxFW.shp")

maxFWB <- read_sf("Data/10_prioritizr/maxFWB.shp") %>% 
  select(id, solution_1) %>% 
  st_drop_geometry()



maxFWMap <- maxFW %>% 
  select(id, solution_1) %>% 
  left_join(maxFWB, by = "id", suffix = c("", "B")) %>% 
  mutate(
    solution_1B = if_else(solution_1B == 1, -2, 0),
    #fieldsChosen = rowSums(across(starts_with("solution")))
    fc = solution_1 + solution_1B,
  ) %>% 
  filter(fc != 0) %>%  
  rasterize(rast_template, field = "fc", touches = TRUE, fun = min) %>% 
  as.polygons() %>% 
  st_as_sf() %>% 
  mutate(
    fieldsChosen = case_when(fc == -2 ~ "BLM scenario",
                             fc == -1 ~ "Both", 
                             #fc == 0 ~ "Neither", 
                             fc == 1 ~ "Non BLM scenario")
  ) %>% 
  #st_simplify(dTolerance = 500) %>% 
  ms_simplify() %>% 
  smooth()  


maxFWMap2 <- maxFWMap %>% 
  tm_shape() + 
  tm_fill(col = "fieldsChosen",
          style = "cat",
          palette = "viridis"
          )


outline +
  maxFWMap2 +
  tm_layout(legend.position = c("right", "top"), 
              title= 'Maximum Fox Water Scenario', 
              title.position = c('left', 'bottom')
              )



# Max F

maxF <- read_sf("Data/10_prioritizr/maxF.shp")

maxFB <- read_sf("Data/10_prioritizr/maxFB.shp") %>% 
  select(id, solution_1) %>% 
  st_drop_geometry()



maxFMap <- maxF %>% 
  select(id, solution_1) %>% 
  left_join(maxFB, by = "id", suffix = c("", "B")) %>% 
  mutate(
    solution_1B = if_else(solution_1B == 1, -2, 0),
    #fieldsChosen = rowSums(across(starts_with("solution")))
    fc = solution_1 + solution_1B,
    fieldsChosen = case_when(fc == -2 ~ "BLM scenario",
                             fc == -1 ~ "Both", 
                             fc == 0 ~ "Neither", 
                             fc == 1 ~ "Non BLM scenario")
  ) %>% 
  filter(fieldsChosen != "Neither") %>% 
  tm_shape() +
  tm_fill(col = "fieldsChosen", style = "cat", palette = "viridis") 




outline +
  maxFMap +
  tm_layout(legend.position = c("right", "top"), 
            title= 'Maximum Fox Scenario', 
            title.position = c('left', 'bottom')
  )




# Min FW


minFW <- read_sf("Data/10_prioritizr/minFW.shp")

minFWB <- read_sf("Data/10_prioritizr/minFWB.shp") %>% 
  select(id, solution_1) %>% 
  st_drop_geometry()



minFWMap <- minFW %>% 
  select(id, solution_1) %>% 
  left_join(minFWB, by = "id", suffix = c("", "B"))

tmap_mode("plot")



minFWMap <- minFW %>% 
  select(id, solution_1) %>% 
  left_join(minFWB, by = "id", suffix = c("", "B")) %>% 
  mutate(
    solution_1B = if_else(solution_1B == 1, -2, 0),
    #fieldsChosen = rowSums(across(starts_with("solution")))
    fc = solution_1 + solution_1B,
    fieldsChosen = case_when(fc == -2 ~ "BLM scenario",
                             fc == -1 ~ "Both", 
                             fc == 0 ~ "Neither", 
                             fc == 1 ~ "Non BLM scenario")
  ) %>% 
  filter(fieldsChosen != "Neither") %>% 
  tm_shape() +
  tm_fill(col = "fieldsChosen", style = "cat", palette = "viridis") 




outline +
  minFWMap +
  tm_layout(legend.position = c("right", "top"), 
            title= 'Minimum Fox Water Scenario', 
            title.position = c('left', 'bottom')
  )


# Min F


minF <- read_sf("Data/10_prioritizr/minF.shp")

minFB <- read_sf("Data/10_prioritizr/minFB.shp") %>% 
  select(id, solution_1) %>% 
  st_drop_geometry()



minFMap <- minF %>% 
  select(id, solution_1) %>% 
  left_join(minFB, by = "id", suffix = c("", "B"))

tmap_mode("plot")



minFMap <- minF %>% 
  select(id, solution_1) %>% 
  left_join(minFB, by = "id", suffix = c("", "B")) %>% 
  mutate(
    solution_1B = if_else(solution_1B == 1, -2, 0),
    #fieldsChosen = rowSums(across(starts_with("solution")))
    fc = solution_1 + solution_1B,
    fieldsChosen = case_when(fc == -2 ~ "BLM scenario",
                             fc == -1 ~ "Both", 
                             fc == 0 ~ "Neither", 
                             fc == 1 ~ "Non BLM scenario")
  ) %>% 
  filter(fieldsChosen != "Neither") %>% 
  tm_shape() +
  tm_fill(col = "fieldsChosen", style = "cat", palette = "viridis") 




outline +
  minFMap +
  tm_layout(legend.position = c("right", "top"), 
            title= 'Minimum Fox Scenario', 
            title.position = c('left', 'bottom')
  )


