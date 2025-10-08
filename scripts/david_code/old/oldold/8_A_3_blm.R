library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(fasterize)

source("Scripts/0_startup/0_2_functions.R")


tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf("Data/6_foxExtraction/kernExtractions.shp") %>% 
  st_make_valid() %>% 
  # There is one outlier due to processing issues. 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM)
  )




habitatExtract %>% 
  filter(water == 0) %>% 
  distinct(COMM)


# 
# 
# habitatExtract %>% 
#   lwgeom::st_snap_to_grid(100) %>% 
#   select(geometry, everything()) %>% 
#   st_make_valid() %>% 
#   tm_shape() + 
#   tm_borders(col = "blue")
# 
# 
# # Using map shaper doesn't really make a difference
# habitatExtract %>% 
#   ms_simplify(keep = .5, keep_shapes = T, snap_interval = 50)  %>% 
#   st_make_valid() %>% 
#   tm_shape() + 
#   tm_borders(col = "blue")
# 
# 

# 
# # Create raster templates
# 
# rast_template  <-  rast(ext = ext(habitatExtract), 
#                        resolution = 30,
#                        crs = st_crs(habitatExtract)$wkt)
# 
# 
# raster_template <- raster(rast_template)
# 
# 
# 
# 
# # Rasterize
# 
# fieldRast <- habitatExtract %>% 
#   rasterize(rast_template, field = "id", touches = TRUE, fun = min)
# 
# 
# fieldRaster <- habitatExtract %>% 
#   fasterize(raster_template, field = "id")
# 
# 
# # Vectorize
# 
# reVector <- fieldRast %>% 
#   as.polygons() %>% 
#   st_as_sf()
# 
# 
# reVector2 <- fieldRaster %>% 
#   rast() %>% 
#   as.polygons() %>% 
#   st_as_sf() %>% 
#   mutate(
#     id = layer,
#     .keep = "unused"
#   )
  
  
  
  
  #raster::rasterToPolygons(fun = max, dissolve = T)


missingFunc <- function(res) {
  
  
  
  rast_template  <-  rast(ext = ext(habitatExtract), 
                          resolution = res,
                          crs = st_crs(habitatExtract)$wkt)
  
  
  
  
  # Rasterize
  
  fieldRast <- habitatExtract %>% 
    rasterize(rast_template, field = "id", touches = TRUE, fun = min)
  
  
  
  
  reVector <- fieldRast %>% 
    as.polygons() %>% 
    st_as_sf()
  
  
  
  #vars <- enquo(var)
  
  missing <- habitatExtract %>% dplyr::pull(id) %>% 
    setdiff(reVector %>% dplyr::pull(id))
  
  
  missingPolys <- habitatExtract %>%
    filter(id %in% missing)


  print(missingPolys)


  tm_shape(reVector) + tm_polygons(col = "green", alpha = 0.1) +
    tm_shape(missingPolys) + tm_borders(col = "red", lwd = 1.5)


}



missingFunc(30)



bad1 <- habitatExtract %>% 
  filter(id == 13952)

bad2 <- habitatExtract %>% 
  filter(id == 4096)



bad1 %>% tm_shape() +
  tm_fill(col = "green") +
  tm_shape(bad2) + 
  tm_borders(col = "red")


df <- habitatExtract

indicator <- st_contains(df, sparse = FALSE) 




indicator[1:10, 1:10]

ind2 <- indicator %>% 
  as_tibble(rownames = "Row") %>% 
  #rowwise() %>% 
  mutate(
    across(starts_with("V"), ~ if_else(.x == T, 1, 0))
  )


ind3 <- ind2 %>% 
  slice_head(n = 10) %>% 
  rowwise() %>% 
  mutate(
    Sum = sum(c_across(starts_with("V")))
  ) %>% 
  select(Row, Sum)


df %>% 
  select(indicator) %>% 
  filter(!indicator)

# 
# # 
# # 
# # tm_shape(fieldRaster) + tm_raster() + 
# #   tm_shape(habitatExtract) + tm_borders(col = "red")
# # 
# # 
# # tm_shape(reVector) + tm_fill(col = "green", alpha = 0.3) + 
# #   tm_shape(habitatExtract) + tm_borders(col = "red")
# # 
# 
# 
# 
# missing <- habitatExtract %>% pull(id) %>% 
#   setdiff(reVector %>% pull(id))
# 
# 
# missingPolys <- habitatExtract %>% 
#   filter(id %in% missing)
# 
# summary(fieldRaster)
# 
# tm_shape(reVector) + tm_fill(col = "green", alpha = 0.3) + 
# tm_shape(missingPolys %>% filter(id == 10062)) + tm_fill(col = "red")
# 
# tmap_mode("plot")
# 
# 
# missingPolys %>% 
#   filter(id == 10062) %>% 
#   mutate(
#     area = st_area(.)
#   ) %>% tm_shape() + tm_polygons()
#   
#   
# # 
# # write_sf(reVector, "./Data/8_prioritizr/blmShapes.shp")
# # 
# # bm <- boundary_matrix(reVector)
