library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(vegan)
library(Rcpp)

source("./functions.R")


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


# Set 
base <- habitatExtract %>% 
  mutate(
    solution_1 = if_else(COMM_x == "UNCULTIVATED AG", 1, 0)
  )

base %>% 
  arrange(COMM) %>% 
  pull(COMM) %>% 
  unique()


idealPre <- habitatExtract %>% 
  mutate(
    solution_1 = 0
  )



write_sf(idealPre, "./Data/8_prioritizr/idealPre.shp")

write_sf(base, "./Data/8_prioritizr/base.shp")

# Find Budget and Targets -------------------------------------------------

targets <- read_csv("./Data/7_estimateBudgetTargets/targetTable.csv")

oldCost <- 398080
oldWater <- 248531
oldFox <- 11203

cost <- targets[[1, 3]] / 1000
waterTarget <- targets[[2, 3]] / 10
foxTarget <- targets[[3, 3]]



#Get total values for habitat and water use
habitatSum <- sum(habitatExtract$habitat)
waterSum <- sum(habitatExtract$water)

#Filter small hab / water values, and divide revenue by 1000, water by 10
habitatProportion <- habitatExtract %>%
  mutate(
    fox = if_else(habitat < 0.001, 0, habitat), #(habitat / habitatSum) * 100,
    water = if_else(water < 0.0001, 0, water),
    water = water / 10,
    revenue = revenue / 1000#(water / waterSum) * 100
  )

# habitatProportion <- habitatProportionRaw %>% 
#   #No idea why, but options won't work with this field
#   filter(geoGroup != 6330)

metrics(habitatProportion %>% filter(fox > 0) %>% .$fox)
metrics(habitatProportion %>% filter(water > 0) %>% .$water)

habitatProportion %>% 
  filter(
    fox > 0 & fox < 0.01
  )



# Convert Crop Type Function ----------------------------------------------

# Vector of Crop types for ending comm key 
commCodeVect <- c()

# Function to convert fallowed fields to crop type "fallow"
# and add crop types to vector
convertComm <- function(tibble) {
  change <- tibble %>%
    mutate(
      Crop = if_else(solution_1 == 1, "FALLOW", COMM)
    )
  
  codes <- change %>%
    st_drop_geometry() %>%
    dplyr::pull(Crop) %>% unique()

  commCodeVect <<- append(commCodeVect, codes)
  
  change
}


# Minimum Set Objective Fox Water -----------------------------------------


fallowHabitat <- habitatProportion %>% 
  filter(fallow == 1)

metrics(fallowHabitat$habitat)


minSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) 

minSet

minSetSolve <- solve(minSet) 


minSetSolve %>% 
  convertComm()

minSetSolve %>% 
  mutate(
    Crop = if_else(solution_1 == 1, "FALLOW", COMM)
  )

minFWCost <- eval_cost_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

eval_n_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

minFWSum <- eval_feature_representation_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minSet, minSetSolve %>% dplyr::select(solution_1))

print(attr(minSetSolve, "objective"))


write_sf(minSetSolve, "./Data/8_prioritizr/minFoxWater.shp")




# Max Coverage Fox Water --------------------------------------------------

maxSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.07) 

maxSet

maxSetSolve <- solve(maxSet) %>% 
  convertComm()




maxFWCost <- eval_cost_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_cost_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_n_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

maxFWSum <- eval_feature_representation_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxSet, maxSetSolve %>% dplyr::select(solution_1))




write_sf(maxSetSolve, "./Data/8_prioritizr/maxFoxWater.shp")



# Min Set Fox -------------------------------------------------------------




minFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) 

minFox

minFoxSolve <- solve(minFox) %>% 
  convertComm()

minFoxCost <- eval_cost_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

eval_n_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

minFoxSum <- eval_feature_representation_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minFox, minFoxSolve %>% dplyr::select(solution_1))




write_sf(minFoxSolve, "./Data/8_prioritizr/minFox.shp")

# Max Fox -----------------------------------------------------------------


maxFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.48) 

# maxFox
# 
# presolve_check(maxFox)

maxFoxSolve <- solve(maxFox) %>% 
  convertComm()




maxFoxCost <- eval_cost_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

eval_n_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

maxFoxSum <- eval_feature_representation_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxFox, maxFoxSolve %>% dplyr::select(solution_1))




write_sf(maxFoxSolve, "./Data/8_prioritizr/maxFox.shp")




# Min Set Water -----------------------------------------------------------




minWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 

minWater

minWaterSolve <- solve(minWater) %>% 
  convertComm()

minWaterCost <- eval_cost_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

eval_n_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

minWaterSum <- eval_feature_representation_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(minWater, minWaterSolve %>% dplyr::select(solution_1))




write_sf(minWaterSolve, "./Data/8_prioritizr/minWater.shp")



# Max Water ---------------------------------------------------------------


maxWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3 ) 

maxWater

maxWaterSolve <- solve(maxWater) %>% 
  convertComm()




maxWaterCost <- eval_cost_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

eval_n_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

maxWaterSum <- eval_feature_representation_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(maxWater, maxWaterSolve %>% dplyr::select(solution_1))




write_sf(maxWaterSolve, "./Data/8_prioritizr/maxWater.shp")






# Table of results --------------------------------------------------------

tableNames <- enframe(c("Max Fox Water", "Min Fox Water", "Max Fox", "Min Fox", "Max Water", "Min Water"), name = NULL, value = "Scenario")





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
  dplyr::select(-starts_with("Resou"))
 



write_csv(sumTableFinal, "./Data/summaryTable.csv")

# Make Crop Key and Export ------------------------------------------------




cropKey <- commCodeVect %>% unique() %>% 
  enframe() %>% 
  arrange(value)


write_csv(cropKey, "./Data/8_prioritizr/finalCropKey.csv")

# # Visualize results -------------------------------------------------------
# 
# plotFiles <- list.files(
#   path = "./Data/8_prioritizr/",
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


