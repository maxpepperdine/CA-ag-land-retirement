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


# Expand CA Bounding Box to add legend at top

bbox_new <- st_bbox(ca) # current bounding box

#xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
#bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top

bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon





caMap <- ca %>% 
  tm_shape(bbox = bbox_new) + 
  tm_borders(lwd = 1) +
  tm_fill(col = "beige", alpha = .5)


redValley <- greatValley %>% 
  tm_shape() +
  tm_fill(col = "orange", title = "Great Valley Region")


####################
# Final
####################


caInset <- caMap + 
  countyMap +
  redValley +
  tm_shape(kernCounty) +
  tm_borders(col = "blue") +
  tm_shape(valleyReg) +
  tm_borders(col = "black", lwd = 2.5) +
  tm_add_legend(
    type = "fill", 
    col = c("skyblue", "orange"),
    border.col = "white",
    #alpha = c("1", )
    #lwd = c(0.1, 3, 2),
    #lty = c(1, 4, 1),
    labels = c("Kern County", "Great Valley Region")
  ) +
  tm_legend(
    legend.text.size = 1, 
    legend.width = 1,
    #legend.outside = T,
    legend.position = c("left", "top")
    #legend.bg.color = "white"
  )





caInset



# Basemap -----------------------------------------------------------------


# Create Basemap of Greater valley region of kern from OSM

bmaps <- c("osm", "opencyclemap",
           "osm-transport", "osm-public-transport", 
           "bing", "stamen-toner", "stamen-terrain",
  "stamen-watercolor", "apple-iphoto" )



mapFunc <- function(baseMap) {
  
  
  osm_base <- read_osm(greatValley, 
                       zoom = 10, 
                       #type = "apple-iphoto", 
                       #type = "stamen-terrain",
                       type = baseMap,
                       ext = 1.1)
  baseM <- tm_shape(osm_base,
                    raster.downsample = F
  ) + tm_rgb()
  
  
  
  baseM
  
  
  
  
  
}


baseMCol <- map(bmaps, mapFunc)




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
  tm_scale_bar(
    position = c("center", "bottom"),
    text.size =  1,
    bg.color = "white"
  ) +
  tm_compass(
    type = "arrow",
    text.size = 1,
    size = 4,
    text.color = "white",
    
  ) +
  tm_layout(
    #bg.color = "grey95",
    legend.text.size = 1,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.frame = T,
    #main.title = "Fallowing in Kern Great Valley Region, 2015"
  )


baseMCol[[5]] + fallowMap


# Final Fallowed Map ------------------------------------------------------


new <- baseMCol[[5]] + fallowMap
# new
# 
# print(caInset, vp = viewport(0.22, 0.27, width = 0.6, height = 0.5))

tmap_save(tm = new,
          "./Data/11_tablesFigures/fallowMap.tiff",
          insets_tm = caInset,
          insets_vp = viewport(0.163, 0.307, width = 0.6, height = 0.6),
          #height = 2080,
          #width = 1600
)


# Create Map of Fallowing Frequency ---------------------------------------

# Rasterize then vectorize plots so as to obscure which fields are being 
# selected



# Raster Template
rast_template  <-  rast(ext = ext(kernNew), 
                        resolution = 2500,
                        crs = st_crs(kernNew)$wkt)


# Clean
freqMapRawRast <- kernNew %>% 
  select(id, selected) %>%   
  mutate(`Selection Frequency` = selected / 6) %>% 
  rasterize(rast_template, field = "Selection Frequency", 
            #touches = TRUE, 
            fun = mean) 


freqMap2 <- tm_shape(freqMapRawRast) + tm_raster(palette = "YlOrRd")  +
  tm_scale_bar(
    position = c("center", "bottom"),
    text.size =  1,
    bg.color = "white"
  ) +
  tm_compass(
    type = "arrow",
    text.size = 1,
    size = 4,
    text.color = "white",
    
  ) +
  tm_layout(
    #bg.color = "grey95",
    legend.text.size = 1,
    legend.position = c("right", "top"),
    legend.bg.color = "white",
    legend.frame = T,
    #main.title = "Fallowing Selection Frequency"
  )


freqMap2


# Export ------------------------------------------------------------------


new2 <- baseMCol[[5]] + freqMap2
new2


tmap_save(tm = new2,
          "./Data/11_tablesFigures/freqMap.tiff",
          insets_tm = caInset,
          insets_vp = viewport(0.163, 0.307, width = 0.6, height = 0.6),
          #height = 2080,
          #width = 1600
)

