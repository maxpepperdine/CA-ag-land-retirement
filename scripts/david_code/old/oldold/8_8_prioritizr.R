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












# Create Base Scenarios ---------------------------------------------------





# Set 
base <- habitatProportion %>% 
  mutate(
    solution_1 = if_else(fallow == 1, 1, 0),
  ) %>% 
  convertComm()

glimpse(habitatProportion)

baseRetired <- habitatProportion %>% 
  mutate(
    solution_1 = if_else(fallow == 1, 1, 0),
  ) %>% 
  convertCommRetired()




# Create Ideal Pre Scenarios ----------------------------------------------

commCodeVect <- c()
# Needs to be put into linkage mapper to create LCP's, which will then be
# constrained in step 8
idealPre <- habitatProportion %>% 
  mutate(
    solution_1 = 0
  ) %>% 
  convertComm()


idealPreRetired <- habitatProportion %>% 
  mutate(
    solution_1 = 0
  ) %>% 
  convertCommRetired()

write_sf(idealPre, "./Data/8_prioritizr/idealPre.shp")
write_sf(idealPreRetired, "./Data/8_prioritizr/idealPreR.shp")


# Make Crop Key and Export ------------------------------------------------




cropKey <- commCodeVect %>% unique() %>% 
  enframe() %>% 
  arrange(value)


write_csv(cropKey, "./Data/8_prioritizr/finalCropKey.csv")





# Appendix ----------------------------------------------------------------

