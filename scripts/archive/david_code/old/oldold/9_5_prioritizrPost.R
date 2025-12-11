library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)

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



# Create and Solve Problesm -----------------------------------------------


 # Min Fox Water
minSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) 

minSet

minSetSolve <- solve(minSet)

# Max Fox Water


maxSet <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.07) 

maxSet

maxSetSolve <- solve(maxSet)

# Min Fox

minFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) 

minFox

minFoxSolve <- solve(minFox) 

# Max Fox



maxFox <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.48) 

maxFox

maxFoxSolve <- solve(maxFox)


# Min Water


minWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 

minWater

minWaterSolve <- solve(minWater)


# Max Water


maxWater <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3.05 ) 

maxWater

maxWaterSolve <- solve(maxWater) 


# Ideal Scenario

# Read in initial Corridors
idealCorridors <- rast("C:/pythonProject - Copy/resistance_idealPre_eq3_corridors_truncated_at_200k.tif")


idealCorridors2 <- idealCorridors %>% 
  as.polygons() %>% 
  st_as_sf() %>% 
  st_union() %>% 
  st_transform(st_crs(habitatProportion)) %>% 
  st_simplify(dTolerance = 270)

tm_shape(idealCorridors2) + 
  tm_fill(col = "red", alpha = 0.5)


habitatProportionIdeal <- habitatProportion %>% 
  mutate(
    locked_out = st_intersects(st_geometry(.), idealCorridors2) %>% 
      lengths > 0,
    locked_out = if_else(locked_out == TRUE, F, T),
    intersect = if_else(locked_out == T, 0, 1)
  )


sum(habitatProportionIdeal$intersect)


ideal <- problem(habitatProportionIdeal, c("fox", "intersect"), cost_column = "revenue") %>%
  add_max_features_objective(cost) %>%
  add_absolute_targets(c((foxTarget), 4040))


# ideal <- problem(habitatProportionIdeal, c("fox"), cost_column = "revenue") %>% 
#   add_max_features_objective(cost) %>% 
#   add_absolute_targets(c(foxTarget / 2)) %>% 
#   add_locked_out_constraints("locked_out") %>% 
#   add_gap_portfolio(number_solutions = 5, pool_gap = 0.2)


idealSolve <- solve(ideal) %>% 
  convertComm()



# Ideal
eval_cost_summary(ideal, idealSolve %>% dplyr::select(solution_1))

eval_n_summary(ideal, idealSolve %>% dplyr::select(solution_1))
eval_feature_representation_summary(ideal, idealSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(ideal, idealSolve %>% dplyr::select(solution_1))






# Evaluatate Solutions ----------------------------------------------------


# Ideal

idealCost <- eval_cost_summary(ideal, idealSolve %>% dplyr::select(solution_1))

eval_n_summary(ideal, idealSolve %>% dplyr::select(solution_1))

idealSum <- eval_feature_representation_summary(ideal, idealSolve %>% dplyr::select(solution_1))

eval_target_coverage_summary(ideal, idealSolve %>% dplyr::select(solution_1))




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
  )


baseOut <- base %>% 
  convertComm()


baseROut <- base %>% 
  convertCommRetired()



write_sf(baseROut, "./Data/9_prioritizrPost/baseR.shp")
write_sf(baseOut, "./Data/9_prioritizrPost/base.shp")



# Table of results --------------------------------------------------------

# Get jobs lost for all scenarios
findJobsFallowArea <- function(prioritizrRun) {
  
  prioritizrRun %>% 
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    summarize(
      jobsLost = sum(jobs),
      area = sum(acres)
    )
  
}



runs <- list(maxSetSolve, minSetSolve, maxFoxSolve, minFoxSolve, 
             maxWaterSolve, minWaterSolve, idealSolve)




findMostFallowed <- function(table) {
  
  table %>%
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    group_by(COMM) %>% 
    summarize(area = sum(acres)) %>% 
    arrange(desc(area)) %>% 
    slice_head(n = 10) %>% 
    mutate(
      row = row_number()
    ) %>% 
    pivot_wider(names_from = row, values_from = COMM:area)
  
}


mostFallowed <- map(runs, findMostFallowed) %>% 
  bind_rows()


jobsCostList <- map(runs, findLostJobs) %>% 
  bind_rows()



tableNames <- enframe(c("Max Fox Water", "Min Fox Water", "Max Fox", 
                        "Min Fox", "Max Water", "Min Water", "Ideal"), 
                      name = NULL, value = "Scenario")


summaryTable <- bind_rows(maxFWCost, minFWCost, maxFoxCost, 
                          minFoxCost, maxWaterCost, minWaterCost,
                          idealCost) %>% 
  bind_cols(tableNames) %>% 
  dplyr::select(-summary) %>% 
  dplyr::select(Scenario, everything()) %>% 
  mutate(count = c(2, 2, 1, 1, 1, 1, 1)) %>% 
  uncount(count) %>% 
  bind_cols(bind_rows(maxFWSum, minFWSum, maxFoxSum, 
                      minFoxSum, maxWaterSum, minWaterSum,
                      slice_head(idealSum, n = 1))) %>% 
  dplyr::select(-summary) %>% 
  mutate(
    `Cost ($)` = format(as.integer(round(cost * 1000, -4)), nsmall=1, big.mark=","),
    Resource = feature,
    .keep = "unused"
  ) 


foxy <- summaryTable %>% 
  filter(Resource == "fox") %>% 
  mutate(
    `Total Fox Habitat (ha)` = round((total_amount / 2.471), -2),
    `Fox Habitat Fallowed (ha)` = round((absolute_held / 2.471), -2),
    `Relative Fox Habitat Fallowed` = round((relative_held), 2),
    .keep = "unused"
  )

wet <- summaryTable %>% 
  filter(Resource == "water") %>% 
  mutate(
    `Total Water Use (ac-ft)` = round((total_amount * 10), -2),
    `Water Savings (ac-ft)` = round((absolute_held * 10), -2),
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
  dplyr::select(Scenario, `Cost ($)`, `Jobs Lost`, everything()) %>%
  bind_cols(allFallowed) %>%
  mutate(
    `Total Area Fallowed (ha)` = round((area / 2.471), -2),
    .keep = "unused"
  ) 


baseMostFallowed <- base



nonRuns <- findMostFallowed(base) %>% 
  mutate(
    Scenario = "base"
  )



cropsFallowed <- sumTableFinal %>% 
  dplyr::select(Scenario) %>% 
  bind_cols(mostFallowed) %>%
  bind_rows(nonRuns)




write_csv(sumTableFinal, "./Data/summaryTable.csv")


write_csv(cropsFallowed, "./Data/summaryCropTable.csv")


# Create Shapes -----------------------------------------------------------

# Retired

runsR <- map(runs, convertCommRetired)

# Idle

runsI <- map(runs, convertComm)

# List of Names to write
nm <- list("maxFW", "minFW", "maxF", "minF", "maxW", "minW", "ideal") %>% 
  str_c("R.shp")



namesR <- str_c("./Data/9_prioritizrPost/", nm)


namesI <- namesR %>%
  str_replace("R", "")

# Write out

map2(runsR, namesR, write_sf)
map2(runsI, namesI, write_sf)


# Make Crop Key and Export ------------------------------------------------



cropKey <- runsR %>% 
  list(runsI) %>% 
  bind_rows() %>% 
  st_drop_geometry() %>% 
  pull(Crop) %>% 
  unique() %>%
  sort() %>% 
  enframe() 



write_csv(cropKey, "./Data/9_prioritizrPost/finalCropKey.csv")





# Appendix ----------------------------------------------------------------


