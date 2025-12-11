library(tidyverse)
library(rmapshaper)
library(sf)
library(smoothr)
library(tmap)
library(terra)
library(tigris)


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

# Template Raster ---------------------------------------------------------


rast_template  <-  rast(ext = ext(outlineRaw), 
                        resolution = 150,
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
  ms_simplify(keep = 0.05) %>% 
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
  ) + tm_basemap(server = "OpenTopoMap")






# Fallowed Map ------------------------------------------------------------





fallowMap <- kernNew %>% 
  mutate(
    mapCol = case_when(Crop == "FALLOW" ~ "Idled", 
                       Crop == "UNCULTIVATED" ~ "Retired", 
                       T ~ "Active")
  ) %>% 
  tm_shape() +
  #tm_fill(col = "mapCol", palette = c("lightgreen", "red", "darkred"))
  tm_fill(
    col = "fallow", 
    style = "cat", 
    palette = c("green", "red"),
    title = "",
    labels = c("Active Agriculture", "Fallowed")
  ) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_compass() +
  tm_layout(
    bg.color = "grey95",
    legend.text.size = 1,
    legend.position = c("right", "top")
  )


baseM + fallowMap

# California Map inset ----------------------------------------------------

###################################
# Shapefiles / Data
###################################


# SJV Shapefile
sjv <- read_sf("Data/0_input/pp1766_cvhm_texture_regions/cvhm_texture_regions.shp") %>% 
  st_transform(crs(kern))


caRaw <- counties(state = "CA")



kernCounty <- caRaw %>% 
  filter(NAME == "Kern") %>% 
  st_transform(crs(kern)) 


greatValley <- sjv %>% 
  st_intersection(kernCounty)



ca <- caRaw %>% 
  #st_union() %>% 
  st_transform(crs(kern))


valleyReg <- st_bbox(greatValley) %>% 
  st_as_sfc() %>% 
  st_buffer(10000)




########################################
# Maps
#########################################


countyMap <- kernCounty %>% 
  tm_shape() +
  tm_fill(col = "skyblue", alpha = .8)
  #tm_borders(col = "black", lwd = 2)


caMap <- ca %>% 
  tm_shape() + 
  tm_borders(lwd = 1) +
  tm_fill(col = "beige", alpha = .5)


redValley <- greatValley %>% 
  tm_shape() +
  tm_fill(col = "red", title = "Great Valley Region")


####################
# Final
####################


caInset <- caMap + 
  countyMap +
  redValley +
  tm_shape(kernCounty) +
  tm_borders(col = "blue") +
  tm_shape(valleyReg) +
  tm_borders(lwd = 5) +
  tm_add_legend(
    type = "fill", 
    col = c("skyblue", "red"),
    border.col = "white",
    #alpha = c("1", )
    #lwd = c(0.1, 3, 2),
    #lty = c(1, 4, 1),
    labels = c("Kern County", "Great Valley Region")
  ) +
  tm_legend(
    #legend.text.size = 1, 
    legend.width = 2
  )

caInset





# Final Fallowed Map ------------------------------------------------------

library(tmaptools)
library(OpenStreetMap)
#data(NLD_muni)
osm_NLD <- read_osm(greatValley, zoom = 10, ext=1.1)
tm_shape(osm_NLD) + tm_rgb()

baseM <- tm_shape(osm_NLD) + tm_rgb()
 


library(grid)
new <- baseM + fallowMap
print(caInset, vp = viewport(0.20, 0.27, width = 0.5, height = 0.5))

tmap_save(tm = new,
          "./Data/11_tablesFigures/fallowMap.tiff",
          insets_tm = caInset,
          insets_vp = viewport(0.20, 0.27, width = 0.5, height = 0.5), 
          height = 2080,
          width = 1600)
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


