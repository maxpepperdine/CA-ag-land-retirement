library(tidyverse)
library(rmapshaper)
library(sf)
library(smoothr)
library(tmap)
library(terra)
library(tigris)
library(tmaptools)

library(grid)


source("Scripts/0_startup/0_2_functions.R")


#tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Find Plot Fallowing Frequency -------------------------------------------



# Read in PrioritizR Runs
runsRaw <- list.files(
  path = "./Data/10_prioritizr/",
  pattern = "shp$",
  full.names = T
)

# Remove retired, base, and BLM runs
runs <- runsRaw %>% 
  str_subset("B|R|b", negate = T)


runShps <- map(runs, read_sf)


# Read in Base Kern Fallowing Map
kern <- read_sf("./Data/10_prioritizr/baseR.shp")



# Function to select id and whether fallowed or not from prioritizR runs
findAll <- function(tibble) {
  
  tibble %>% 
    st_drop_geometry() %>% 
    dplyr::select(id, acres, solution_1)
  
}

# Map over all selected runs
every <- map(runShps, findAll) 

# Join Runs and summarize to columns by plot ID
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


# Join to Base Kern data as column
kernNew <- kern %>% 
  left_join(every2, by = "id")



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
    legend.text.size = .59, 
    legend.width = .5
  )

caInset



# Basemap -----------------------------------------------------------------


# Create Basemap of Greater valley region of kern from OSM


osm_NLD <- read_osm(greatValley, zoom = 10, ext=1.1)
tm_shape(osm_NLD) + tm_rgb()

baseM <- tm_shape(osm_NLD) + tm_rgb()



baseM

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
    palette = c("seagreen3", "red"),
    title = "",
    labels = c("Active Agriculture", "Fallowed")
  ) +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_compass() +
  tm_layout(
    #bg.color = "grey95",
    #legend.text.size = 1,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.frame = T,
    main.title = "Fallowing in Kern Great Valley Region, 2015"
  )


baseM + fallowMap


# Final Fallowed Map ------------------------------------------------------


new <- baseM + fallowMap
# new
# 
# print(caInset, vp = viewport(0.22, 0.27, width = 0.6, height = 0.5))

tmap_save(tm = new,
          "./Data/11_tablesFigures/fallowMap.tiff",
          insets_tm = caInset,
          insets_vp = viewport(0.20, 0.27, width = 0.6, height = 0.5),
          #height = 2080,
          #width = 1600
)


# Create Map of Fallowing Frequency ---------------------------------------

# Rasterize then vectorize plots so as to obscure which fields are being 
# selected



# Raster Template
rast_template  <-  rast(ext = ext(kernNew), 
                        resolution = 150,
                        crs = st_crs(kernNew)$wkt)


# Clean
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
                             T ~ '5-6 High'),
    `Fallow Frequency` = factor(`Fallow Frequency`, levels = c("1-2 Low", "3-4 Medium", "5-6 High"))
  ) %>% 
  #st_simplify(dTolerance = 500) %>% 
  ms_simplify(keep = 0.10) #%>% 
  #smooth()  



pall <- get_brewer_pal("YlOrRd", n = 3, contrast = c(0.25, 0.75))

#pall <- get_brewer_pal("OrRd", n = 3, contrast = c(0.34, 0.75))

# Create Map object
freqMap <- freqMapRaw %>% 
  tm_shape() + 
  tm_fill(col = "Fallow Frequency",
          #style = "cat",
          palette = pall
  )  +
  tm_scale_bar(position = c("center", "bottom")) +
  tm_compass() +
  tm_layout(
    #bg.color = "grey95",
    #legend.text.size = 1,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.frame = T,
    main.title = "Fallowing Selection Frequency"
  )


# Create Basemap of Greater valley region of kern from OSM


osm_freq <- read_osm(greatValley, 
                     zoom = 10, 
                     ext=1.1, 
                     #type = "osm-public-transport"
            )
#tm_shape(osm_NLD) + tm_rgb()

baseF <- tm_shape(osm_freq) + tm_rgb()

baseF + freqMap




new2 <- baseF + freqMap
new2


tmap_save(tm = new2,
          "./Data/11_tablesFigures/freqMap.tiff",
          insets_tm = caInset,
          insets_vp = viewport(0.20, 0.27, width = 0.6, height = 0.5),
          #height = 2080,
          #width = 1600
)



#palette_explorer()


