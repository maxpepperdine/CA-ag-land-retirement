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


habitatExtract <- read_sf("Data/7_foxExtraction/kernExtractions.shp") %>% 
  st_make_valid() %>% 
  # There is one outlier due to processing issues. 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM),
    jobsB = hrs / 20.8,
    jobsQ = jobs,
    .keep = "unused"
  )



rast_template  <-  rast(ext = ext(habitatExtract), 
                        resolution = 30,
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


missingPolys

# 
#   tm_shape(reVector) + tm_polygons(col = "green", alpha = 0.1) +
#     tm_shape(missingPolys) + tm_borders(col = "red", lwd = 1.5)




badList <- missingPolys %>% 
  pull(id)

habitatExtract2 <- habitatExtract %>% 
  filter(!(id %in% badList))


write_sf(habitatExtract2, "./Data/8_blm/cleanShapes.shp")

write_sf(reVector, "./Data/8_blm/blmShapes.shp")


# 
# 
#   
# getContains <- function(df) {
#   
#   
#   indicator <- st_contains(df, sparse = FALSE) %>% 
#     as_tibble(rownames = "Row") %>% 
#     #rowwise() %>% 
#     mutate(
#       Row = as.numeric(Row),
#       across(starts_with("V"), ~ if_else(.x == T, 1, 0))
#     ) 
#   
#   
#   findFunction <- function(rowNumber) {
#     
#     
#     test <- indicator %>% 
#       filter(Row > rowNumber, Row <= (rowNumber + 1000)) %>% 
#       mutate(
#         Sum = rowSums(across(starts_with("V")))
#       ) %>% 
#       #select(Row, Sum) %>% 
#       filter(Sum > 1)
#     
#     # if (nrow(test) > 0) {
#     return(test)
#     # } else {
#     #   return(NULL)
#     # }
#     
#     
#   }
#   
#   
#   rowNums <- seq(0, 14000, 1000)
#   
#   
#   test3 <- map(rowNums, findFunction) %>% 
#     bind_rows() %>% 
#     #select(Row, Sum) %>% 
#     filter(Sum > 1) %>% 
#     pivot_longer(cols = starts_with("V")) %>% 
#     filter(value > 0)
#  
#   return(test3) 
# 
# }
# 
# 
# duples <- getContains(habitatExtract)
# 
# 
# badList <- duples %>% 
#   mutate(
#     name = str_extract(name, "[:digit:]+") 
#   ) %>% 
#   filter(Row != name) %>% 
#   pull(name) %>% 
#   unique() %>% 
#   as.numeric()

# Appendix ----------------------------------------------------------------
# 
# 
# bad1 <- habitatExtract %>% 
#   filter(id == 13952)
# 
# bad2 <- habitatExtract %>% 
#   filter(id == 4096)
# 
# 
# 
# bad1 %>% tm_shape() +
#   tm_fill(col = "green") +
#   tm_shape(bad2) + 
#   tm_borders(col = "red")
# 
# 

# 
# 
# 
# 
#  findFunction <- function(rowNumber) {
#   
#   
#   test <- ind2 %>% 
#     filter(Row > rowNumber, Row <= (rowNumber + 1000)) %>% 
#     mutate(
#       Sum = rowSums(across(starts_with("V")))
#     ) %>% 
#     select(Row, Sum) %>% 
#     filter(Sum > 1)
#     
#   # if (nrow(test) > 0) {
#     return(test)
#   # } else {
#   #   return(NULL)
#   # }
#   
#   
# }
# 
# 
# rowNums <- seq(0, 14000, 1000)
# 
# 
# test3 <- map(rowNums, findFunction)
# 
# 
# test3 %>% bind_rows()
# 
# test2 <- ind2 %>% 
#   #select(-Row) %>% 
#   slice_head(n = 1000) %>% 
# #   rowSums()
# # 
# # test2[which(test2 > 1)]
# 
# # typeof()
#   #$rowwise() %>% 
#   mutate(
#     Sum = rowSums(across(starts_with("V")))
#   ) %>% 
#   select(Row, Sum) #%>% 
#   
# 
# nrow(test2)
# 
# filter(Sum > 1)
# 
# 
# df %>% 
#   select(indicator) %>% 
#   filter(!indicator)
# 
# 
# 
# habitatExtract %>% 
#   filter(water == 0) %>% 
#   distinct(COMM)
# 
# 
# # 
# # 
# # habitatExtract %>% 
# #   lwgeom::st_snap_to_grid(100) %>% 
# #   select(geometry, everything()) %>% 
# #   st_make_valid() %>% 
# #   tm_shape() + 
# #   tm_borders(col = "blue")
# # 
# # 
# # # Using map shaper doesn't really make a difference
# # habitatExtract %>% 
# #   ms_simplify(keep = .5, keep_shapes = T, snap_interval = 50)  %>% 
# #   st_make_valid() %>% 
# #   tm_shape() + 
# #   tm_borders(col = "blue")
# # 
# # 
# 
# # 
# # # Create raster templates
# # 
# # rast_template  <-  rast(ext = ext(habitatExtract), 
# #                        resolution = 30,
# #                        crs = st_crs(habitatExtract)$wkt)
# # 
# # 
# # raster_template <- raster(rast_template)
# # 
# # 
# # 
# # 
# # # Rasterize
# # 
# # fieldRast <- habitatExtract %>% 
# #   rasterize(rast_template, field = "id", touches = TRUE, fun = min)
# # 
# # 
# # fieldRaster <- habitatExtract %>% 
# #   fasterize(raster_template, field = "id")
# # 
# # 
# # # Vectorize
# # 
# # reVector <- fieldRast %>% 
# #   as.polygons() %>% 
# #   st_as_sf()
# # 
# # 
# # reVector2 <- fieldRaster %>% 
# #   rast() %>% 
# #   as.polygons() %>% 
# #   st_as_sf() %>% 
# #   mutate(
# #     id = layer,
# #     .keep = "unused"
# #   )
#   
#   
#   
#   
#   #raster::rasterToPolygons(fun = max, dissolve = T)
# 


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
