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

#cost <- targets[[1, 3]] / 1000
waterTarget <- targets[[2, 3]] / 10
foxTarget <- targets[[3, 3]]
cost <- targets[[4, 3]] / 100 # Jobs are still in x100 


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




# Create and Solve Problesm -----------------------------------------------




# Min Fox Water
minSet <- problem(habitatProportion, c("fox", "water"), cost_column = "jobs") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) 

minSet

minSetSolve <- solve(minSet) %>% 
  convertComm()

# Max Fox Water


maxSet <- problem(habitatProportion, c("fox", "water"), cost_column = "jobs") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.07) 

maxSet

maxSetSolve <- solve(maxSet) %>% 
  convertComm()

# Min Fox

minFox <- problem(habitatProportion, c("fox"), cost_column = "jobs") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) 

minFox

minFoxSolve <- solve(minFox) %>% 
  convertComm()

# Max Fox



maxFox <- problem(habitatProportion, c("fox"), cost_column = "jobs") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.48) 

maxFox

maxFoxSolve <- solve(maxFox) %>% 
  convertComm()


# Min Water


minWater <- problem(habitatProportion, c("water"), cost_column = "jobs") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 

minWater

minWaterSolve <- solve(minWater) %>% 
  convertComm()



# Max Water


maxWater <- problem(habitatProportion, c("water"), cost_column = "jobs") %>% 
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
findLostRev <- function(prioritizrRun) {
  
  prioritizrRun %>% 
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    summarize(revLost = sum(revenue))
  
}


runs <- list(maxSetSolve, minSetSolve, maxFoxSolve, minFoxSolve, maxWaterSolve, minWaterSolve)

revCostList <- map(runs, findLostRev) %>% 
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
    `Jobs Lost` = round(cost),
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
  full_join(wet, by = c("Scenario", "Jobs Lost")) %>% 
  fill(`Total Fox Habitat (ha)`) %>% 
  fill(`Total Water Use (ac-ft)`) %>% 
  mutate(across(everything(), as.character)) %>% 
  replace(is.na(.), " - ") %>% 
  dplyr::select(-starts_with("Resou")) %>% 
  as_tibble() %>% 
  bind_cols(revCostList) %>% 
  mutate(
    # `Cost ($)` = format(as.integer(round(revLost * 1000, -4)),
    #                     #nsmall=1,
    #                     big.mark=","),
    `Cost ($)` = format(round(revLost * 1000, -4), nsmall = 0, big.mark = ","),
    .keep = "unused"
  ) %>% 
  dplyr::select(Scenario, `Cost ($)`, `Jobs Lost`, everything()) %>% glimpse




write_csv(sumTableFinal, "./Data/summaryTableJob.csv")






# Write Shapes ------------------------------------------------------------

# Min Fox Water
write_sf(minSetSolve, "./Data/9_prioritizrJob/minFoxWaterJob.shp")

# Max Fox Water
write_sf(maxSetSolve, "./Data/9_prioritizrJob/maxFoxWaterJob.shp")

# Min Fox
write_sf(minFoxSolve, "./Data/9_prioritizrJob/minFoxJob.shp")

# Max Fox
write_sf(maxFoxSolve, "./Data/9_prioritizrJob/maxFoxJob.shp")

# Min Water
write_sf(minWaterSolve, "./Data/9_prioritizrJob/minWaterJob.shp")

# Max Water
write_sf(maxWaterSolve, "./Data/9_prioritizrJob/maxWaterJob.shp")


