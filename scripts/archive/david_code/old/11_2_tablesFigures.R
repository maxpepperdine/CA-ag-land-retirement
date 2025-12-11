library(tidyverse)
library(rmapshaper)
library(sf)
library(smoothr)
library(tmap)
library(terra)


source("Scripts/0_startup/0_2_functions.R")


#tmap_mode("view")
tmap_options(check.and.fix = TRUE)


# Read in Priortizr Runs --------------------------------------------------

runsRaw <- list.files(
  path = "./Data/10_prioritizr/",
  pattern = "shp$",
  full.names = T
)

# Remove retired, base, and BLM runs
runs <- runsRaw %>% 
  str_subset("B|R|b", negate = T)



# Read in Shapefiles ------------------------------------------------------


kern <- read_sf("./Data/10_prioritizr/baseR.shp")


runShps <- map(runs, read_sf)





# Acres selected in every scenario -------------------------------------------

findAll <- function(tibble) {
  
  tibble %>% 
    st_drop_geometry() %>% 
    dplyr::select(id, acres, solution_1)#%>% 
  #filter(solution_1 == 1)
  
  
  
}


every <- map(runShps, findAll) 


every2 <- every %>% 
  reduce(left_join, by = c("id", "acres")) %>% 
  pivot_longer(cols = starts_with("solution"), names_to = "solution", values_to = "selected") %>% 
  group_by(id, acres) %>% 
  summarise(
    selected = sum(selected)
  )

allScen <- every2 %>% 
  filter(selected == 6) %>% 
  {{sum(.$acres)}}

halfScen <- every2 %>% 
  filter(selected < 3, selected > 0) %>%
  {{sum(.$acres)}}



# Join to Kern Data -------------------------------------------------------

kernNew <- kern %>% 
  left_join(every2, by = "id")



# Outline -----------------------------------------------------------------

outlineRaw <- kernNew

outline <- outlineRaw %>% 
  tm_shape() +
  tm_fill()


# Template Raster ---------------------------------------------------------


rast_template  <-  rast(ext = ext(outlineRaw), 
                        resolution = 100,
                        crs = st_crs(outlineRaw)$wkt)





# Map of Frequency --------------------------------------------------------



freqMapRaw <- kernNew %>% 
  select(id, selected) %>%   
  rasterize(rast_template, field = "selected", touches = TRUE, fun = min) %>% 
  as.polygons() %>% 
  st_as_sf() %>% 
  filter(selected != 0) %>% 
  mutate(
    `Fallow Frequency` = case_when(#selected == 0 ~ "0 None",
                             selected < 3 ~ "1-2 Low", 
                             selected < 4 ~ "3-4 Medium",
                             T ~ '5-6 High')
  ) %>% 
  #st_simplify(dTolerance = 500) %>% 
  ms_simplify() %>% 
  smooth()  


freqMap <- freqMapRaw %>% 
  tm_shape() + 
  tm_fill(col = "Fallow Frequency",
          style = "cat",
          palette = "viridis"
  )


outline +
  freqMap +
  tm_layout(legend.position = c("right", "top"), 
            title= 'Fields Chosen', 
            title.position = c('left', 'bottom')
  )







# Plotted Maps  -----------------------------------------------------------



allMap <- habitatProportion %>% 
  semi_join(every2 %>%  filter(selected == 6), by = "id") %>% 
  tm_shape() + 
  tm_fill(col = "red", alpha = 0.7)


halfMap <- habitatProportion %>% 
  semi_join(every2 %>%  filter(selected < 3, selected > 0), by = "id") %>% 
  tm_shape() + 
  tm_fill(col = "blue", alpha = 0.7)


habitatProportion %>% 
  tm_shape() + 
  tm_borders() + 
  allMap + halfMap






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


