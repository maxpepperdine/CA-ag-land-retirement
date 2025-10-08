library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)

source("Scripts/0_startup/functions.R")


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
  filter(is.na(COMM))

habitatExtract %>% 
  arrange(COMM) %>% 
  pull(COMM) %>% 
  unique()

# Find Budget and Targets -------------------------------------------------

targets <- read_csv("./Data/7_estimateBudgetTargets/targetTable.csv")

oldCost <- 398080
oldWater <- 248531
oldFox <- 11203

cost <- targets[[1, 3]] / 1000
waterTarget <- targets[[2, 3]] / 10
foxTarget <- targets[[3, 3]]
jobTarget <- targets[[4, 3]] / 100 # Jobs are still in x100 


#Filter small hab / water values, and divide revenue by 1000, water by 10
habitatProportion <- habitatExtract %>%
  mutate(
    fox = if_else(habitat < 0.001, 0, habitat), #(habitat / habitatSum) * 100,
    water = if_else(water < 0.0001, 0, water),
    water = water / 10,
    jobs = jobs / 100,
    revenue = revenue / 1000#(water / waterSum) * 100
  )


# habProp2 <- habitatProportion %>% 
#   st_transform(crs(try))



#Get total values for habitat and water use
habitatSum <- sum(habitatProportion$habitat)
waterSum <- sum(habitatProportion$water)
jobSum <- sum(habitatProportion$jobs)



# Vector of Crop types for ending comm key 
commCodeVect <- c()


# Create and Solve Problesm -----------------------------------------------




# Min Fox Water
minSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) 

minSet

minSetSolve <- solve(minSet) %>% 
  convertComm()

# Max Fox Water


maxSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.07) 

maxSet

maxSetSolve <- solve(maxSet) %>% 
  convertComm()

# Min Fox

minFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) 

minFox

minFoxSolve <- solve(minFox) %>% 
  convertComm()

# Max Fox



maxFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.48) 

maxFox

maxFoxSolve <- solve(maxFox) %>% 
  convertComm()


# Min Water


minWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 

minWater

minWaterSolve <- solve(minWater) %>% 
  convertComm()



# Max Water


maxWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3 ) 

maxWater

maxWaterSolve <- solve(maxWater) %>% 
  convertComm()





# Evaluatate Solutions ----------------------------------------------------


# Min Fox Water

minFWCost <- eval_cost_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

eval_n_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

minFWSum <- eval_feature_representation_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

print(attr(minSetSolve, "objective"))



# Max Fox Water



maxFWCost <- eval_cost_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_cost_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_n_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

maxFWSum <- eval_feature_representation_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))



# Min Fox


minFoxCost <- eval_cost_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

eval_n_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

minFoxSum <- eval_feature_representation_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))



# Max Fox



maxFoxCost <- eval_cost_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

eval_n_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

maxFoxSum <- eval_feature_representation_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))


# Min Water

minWaterCost <- eval_cost_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

eval_n_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

minWaterSum <- eval_feature_representation_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))


# Max Water

maxWaterCost <- eval_cost_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

eval_n_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

maxWaterSum <- eval_feature_representation_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))






# Table of results --------------------------------------------------------

# Get jobs lost for all scenarios
findLostJobs <- function(prioritizrRun) {
  
  prioritizrRun %>% 
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    summarize(jobsLost = sum(jobs))
  
}


runs <- list(maxSetSolve, minSetSolve, maxFoxSolve, minFoxSolve, maxWaterSolve, minWaterSolve)

jobsCostList <- map(runs, findLostJobs) %>% 
  bind_rows()



tableNames <- enframe(c("Max Fox Water", "Min Fox Water", "Max Fox", 
                        "Min Fox", "Max Water", "Min Water"), 
                      name = NULL, value = "Scenario")


summaryTable <- bind_rows(maxFWCost, minFWCost, maxFoxCost, minFoxCost, maxWaterCost, minWaterCost) %>% 
  bind_cols(tableNames) %>% 
  dplyr::select(-summary) %>% 
  dplyr::select(Scenario, everything()) %>% 
  mutate(count = c(2, 2, 1, 1, 1, 1)) %>% 
  uncount(count) %>% 
  bind_cols(bind_rows(maxFWSum, minFWSum, maxFoxSum, minFoxSum, maxWaterSum, minWaterSum)) %>% 
  dplyr::select(-summary) %>% 
  mutate(
    `Cost ($)` = format(as.integer(round(cost * 1000, -4)), nsmall=1, big.mark=","),
    Resource = feature,
    .keep = "unused"
  ) 


foxy <- summaryTable %>% 
  filter(Resource == "fox") %>% 
  mutate(
    `Total Fox Habitat (ha)` = round((total_amount / 2.471), -1),
    `Fox Area Fallowed (ha)` = round((absolute_held / 2.471), -1),
    `Relative Area Fallowed` = round((relative_held), 2),
    .keep = "unused"
  )

wet <- summaryTable %>% 
  filter(Resource == "water") %>% 
  mutate(
    `Total Water Use (ac-ft)` = round((total_amount * 10), -1),
    `Water Savings (ac-ft)` = round((absolute_held * 10), -1),
    `Relative Water Savings` = round((relative_held), 2),
    .keep = "unused"
  )


sumTableFinal <- foxy %>% 
  full_join(wet, by = c("Scenario", "Cost ($)")) %>% 
  fill(`Total Fox Habitat (ha)`) %>% 
  fill(`Total Water Use (ac-ft)`) %>% 
  mutate(across(everything(), as.character)) %>% 
  replace(is.na(.), " - ") %>% 
  dplyr::select(-starts_with("Resou")) %>% 
  as_tibble() %>% 
  bind_cols(jobsCostList) %>% 
  mutate(
    `Jobs Lost` = round(jobsLost),
    .keep = "unused"
  ) %>% 
  dplyr::select(Scenario, `Cost ($)`, `Jobs Lost`, everything()) %>% glimpse




write_csv(sumTableFinal, "./Data/summaryTable.csv")










# Create Ideal / Base Scenarios -------------------------------------------






# Set 
base <- habitatProportion %>% 
  mutate(
    solution_1 = case_when(fallow == 1 ~ 1, 
                           .default = 0),
  ) %>% 
  convertComm()



baseRetired <- habitatProportion %>% 
  mutate(
    solution_1 = case_when(fallow == 1 ~ 1, 
                           .default = 0),
  ) %>% 
  convertCommRetired()



base %>% 
  st_drop_geometry() %>% 
  filter(Crop == "FALLOW") %>% glimpse()

testFu <- base %>% 
  st_drop_geometry() %>% 
  filter(Crop == "FALLOW") 
testFu2 <- baseRetired %>% 
  st_drop_geometry() %>% 
  filter(Crop == "FALLOW") 


setdiff(testFu, testFu2)


base %>% 
  arrange(Crop) %>% 
  pull(Crop) %>% 
  unique()


idealPre <- habitatProportion %>% 
  mutate(
    solution_1 = 0
  ) %>% 
  convertComm()

idealPre %>% 
  arrange(Crop) %>% 
  pull(Crop) %>% 
  unique()




habitatProportion %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  #filter(solution_1 == 1) %>% 
  filter(COMM == "UNCULTIVATED") %>% 
  view()


write_sf(idealPre, "./Data/output/idealPre.shp")

write_sf(base, "./Data/output/base.shp")

write_sf(baseRetired, "./Data/output/baseRetired.shp")


# Write Shapes ------------------------------------------------------------

# Min Fox Water
write_sf(minSetSolve, "./Data/output/minFoxWater.shp")

# Max Fox Water
write_sf(maxSetSolve, "./Data/output/maxFoxWater.shp")

# Min Fox
write_sf(minFoxSolve, "./Data/output/minFox.shp")

# Max Fox
write_sf(maxFoxSolve, "./Data/output/maxFox.shp")

# Min Water
write_sf(minWaterSolve, "./Data/output/minWater.shp")

# Max Water
write_sf(maxWaterSolve, "./Data/output/maxWater.shp")

# Make Crop Key and Export ------------------------------------------------




cropKey <- commCodeVect %>% unique() %>% 
  enframe() %>% 
  arrange(value)


write_csv(cropKey, "./Data/output/finalCropKey.csv")





# Appendix ----------------------------------------------------------------




# # Visualize results -------------------------------------------------------
# 
# plotFiles <- list.files(
#   path = "./Data/output/",
#   pattern = ".shp$",
#   full.names = TRUE
# )
# 
# tmap_mode("plot")
# plotResults <- function(shapefile) {
#   shp <- read_sf(shapefile)
#   tm_shape(shp) + tm_fill(col = "solution_1")
# }
# 
# map(plotFiles, plotResults)
# 
# plotResults(plotFiles[1])
#
#
# 
# # Find NA in Solutions ----------------------------------------------------
# 
# 
# 
# maxWaterSolve %>% 
#   filter(is.na(solution_1))
# 
# 
# minFoxSolve %>% 
#   filter(is.na(solution_1))
# 
# 
# 
# # There is a geoGroup with NA permit data
# 
# 
# 
# habitatProportion %>% 
#   pull(geoGrop) %>% unique() %>% 
#   length()
# 
# # There are also multiple 
# 
# 
# tm_shape(maxWaterSolve) + tm_polygons(col = "solution_1")
# 
# 
# 
# 
# 
# # Minimum Set Objective Gap ------------------------------------------------
# 
# 
# p1 <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>%
#   add_min_set_objective() %>%
#   add_relative_targets(0.2) %>%
#   add_gap_portfolio(10, pool_gap = 0.1)
# 
# 
# 
# p100 <-  problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>%
#   add_min_set_objective() %>%
#   add_relative_targets(0.2) %>%
#   add_gap_portfolio(1000, pool_gap = 0.2)
# 
# 
# print(p1)
# 
# # print number of planning units
# number_of_planning_units(p1)
# 
# # print number of features
# number_of_features(p1)
# 
# s1 <- solve(p1)
# 
# s100 <- solve(p100)
# 
# 
# s1 %>%
#   st_drop_geometry() %>%
#   dplyr::select(starts_with("solution")) %>%
#   vegdist(method = "jaccard")
# 
# 
# 
# # plot solution
# 
# #tmaptools::palette_explorer()
# 
# tm_shape(s1) +
#   tm_polygons(col = "solution_1", style = "cat", palette = "-Pastel1")
# 
# 
# tm_shape(s1) +
#   tm_polygons(col = "solution_10", style = "cat", palette = "-Pastel1")
# 
# 
# plot(s1, col = c("grey90", "darkgreen"), main = "solution_1")
# 
# # data
# 
# 
# # extract the objective (numerical value being minimized or maximized)
# print(attr(s1, "objective"))
# print(attr(s1, "status"))
# 
# 
# 
# #Column of how many times a plot was picked
# 
# s1Total <- s1 %>%
#   mutate(
#     sum = rowSums(across(solution_1:solution_10))
#   )
# 
# 
# s100Total <- s100 %>%
#   mutate(
#     sum = rowSums(across(starts_with("solution_")))
#   )
# 
# viridisMap <- function(sf) {
#   tm_shape(sf) +
#     tm_borders() +
#     tm_shape(sf %>% filter(sum > 0)) +
#     tm_fill(col = "sum", n = 10, palette = "viridis")
# }
# 
# viridisMap(s1Total)
# 
# viridisMap(s100Total)
# 
#
# 
# # Max Coverage Gap ---------------------------------------------------------
# 
# 
# pMax <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>%
#   add_max_features_objective(budget = 400000) %>%
#   add_relative_targets(0.1) %>%
#   add_gap_portfolio(1000, pool_gap = 0.2)
# 
# 
# 
# presolve_check(pMax)
# 
# 
# 
# sMax <- solve(pMax)
# 
# 
# 
# sMaxTotal <- s100 %>%
#   mutate(
#     sum = rowSums(across(starts_with("solution_")))
#   )
# 
# 
# 
# viridisMap(sMaxTotal)
# 
# 
# 
# # Vegan Dissimilarity Matrix ----------------------------------------------
# 
# 
# matrix <- sMax %>% 
#   st_drop_geometry() 
# 
# 
# rownames(matrix) <- matrix$geoGroup
# 
# matrix <- matrix[, -(1:5)] %>% t()
# 
# matrix <- as.matrix(matrix)
# 
# 
# d <- vegdist(matrix, method = "jaccard", binary = TRUE)
# 
# finv <- function (k, dist_obj) {
#   if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
#   n <- attr(dist_obj, "Size")
#   valid <- (k >= 1) & (k <= n * (n - 1) / 2)
#   k_valid <- k[valid]
#   j <- rep.int(NA_real_, length(k))
#   j[valid] <- floor(((2 * n + 1) - sqrt((2 * n - 1) ^ 2 - 8 * (k_valid - 1))) / 2)
#   i <- j + k - (2 * n - j) * (j - 1) / 2
#   cbind(i, j)
# }
# 
# 
# max(d)
# 
# 
# finv(which(d == max(d)), d)
# 
# sMaxTest <- sMax %>% 
#   dplyr::select(solution_437, solution_9)
# 
# 
# overlay <- sMaxTest %>%
#   filter(solution_437 == 1 | solution_9 == 1) %>% 
#   mutate(
#     sum = solution_437 - solution_9
#   )
# 
# 
# 
# tm_shape(sMaxTest) + 
#   tm_borders() +
# 
# 
# tm_shape(overlay) + 
#   tm_polygons(col = "sum", style = "cat", palette = "-Pastel1")
# 
# 
# # # Map geoGroup 6330
# # 
# # tm_shape(rescale) + tm_borders() + tm_shape(rescale %>% filter(geoGroup == 6330)) + tm_fill(col = "red") 
# # 
# # rescale %>% filter(geoGroup == 6330) %>% st_is_valid()
# # 
# # tm_shape() + tm_fill()
# 
# 
# 
# 
# 
# conduct <- raster("./Data/0_input/resistance_idealPre_eq3.tif") 
# 
# raster01 = function(r){
#   
#   # get the min max values
#   minmax_r = range(values(r), na.rm=TRUE) 
#   
#   # rescale 
#   return( (r-minmax_r[1]) / (diff(minmax_r)))
# }
# 
# bif <- raster01(conduct)
# 
# try <- 1 - raster01(conduct)
# 
# tm_shape(bif) + tm_raster()
# tm_shape(try) + tm_raster()
# 
# 
# 
# # Ideal Conductance -------------------------------------------------------
# 
# conduct <- raster("./Data/0_input/resistance_idealPre_eq3.tif") 
# 
# raster01 = function(r){
#   
#   # get the min max values
#   minmax_r = range(values(r), na.rm=TRUE) 
#   
#   # rescale 
#   return( (r-minmax_r[1]) / (diff(minmax_r)))
# }
# 
# 
# 
# 
# bif <- raster01(conduct)
# 
# 
# try <- 1 - bif
# 
# try2 <- 1002 - conduct %>% 
#   round(digits = 0)
# 
# 
# tm_shape(bif) + tm_raster()
# tm_shape(try) + tm_raster()
# 
# conn <- connectivity_matrix(habProp2, try)
# conn2 <- connectivity_matrix(habProp2, try2)
# 
# 
# conn3 <- connectivity_matrix(habProp2, bif)
# # Create Solve Problem
# maxFWIdeal <- problem(habProp2, c("fox", "water"), cost_column = "revenue") %>% 
#   add_max_features_objective(cost) %>% 
#   add_absolute_targets(c(foxTarget, waterTarget)) %>% 
#   add_connectivity_penalties(0.5, data = bif)
# 
# 
# maxFWIdeal
# 
# 
# maxFWIdealSolve <- solve(maxFWIdeal)
# 
# 
# maxFWIdealCost <- eval_cost_summary(maxFWIdeal, maxFWIdealSolve %>% dplyr::select(solution_1))
# 
# eval_n_summary(maxFWIdeal, maxFWIdealSolve %>% dplyr::select(solution_1))
# 
# maxFWIdealSum <- eval_feature_representation_summary(maxFWIdeal, maxFWIdealSolve %>% dplyr::select(solution_1))
# 
# eval_target_coverage_summary(maxFWIdeal, maxFWIdealSolve %>% dplyr::select(solution_1))
# 
# 
# tm_shape(maxFWIdealSolve) + 
#   tm_fill(col = "solution_1", style = "cat", palette = "-Pastel1")
