library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)

source("Scripts/0_startup/0_2_functions.R")


tmap_mode("view")
tmap_options(check.and.fix = TRUE)



# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf("Data/7_estimateBudgetTargets/cleanShapes.shp") %>% 
  st_make_valid() %>% 
  # There is one outlier due to processing issues. 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM)
  )



bmShapes <- read_sf("./Data/7_estimateBudgetTargets/blmShapes.shp") 


bm <- bmShapes %>% 
  boundary_matrix()


habitatExtract %>% 
  filter(is.na(COMM))

habitatExtract %>% 
  arrange(COMM) %>% 
  pull(COMM) %>% 
  unique()

# Find Budget and Targets -------------------------------------------------

targets <- read_csv("./Data/8_prioritizr/targetTable.csv")

oldCost <- 398080
oldWater <- 248531
oldFox <- 11203

cost <- targets[[1, 3]] / 1000
waterTarget <- targets[[2, 3]] / 10
foxTarget <- targets[[3, 3]]
retTarget <- targets[[5, 3]]



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

jobTarget <- jobSum - (targets[[4, 3]] / 100) # Jobs are still in x100 




# Solve Function ----------------------------------------------------------


solveOutput <- function(problem, solved) {
  
  cost <- eval_cost_summary(problem, solved %>% dplyr::select(solution_1))
  
  runSummary <- eval_feature_representation_summary(problem, solved %>% dplyr::select(solution_1))
  
  cost %>% bind_cols(runSummary)
  
}



# Create and Solve Problesm -----------------------------------------------



# Min Fox Water

# First Pass




minFW <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets((c(foxTarget, waterTarget)) * retTarget)



solveOutput(minFW, solve(minFW))

solShp <- solve(minFW)

newProblem <- solShp %>% 
  mutate(
    locked_out = solution_1 %>% as.logical(),
    .keep = "unused"
  ) %>% 
  problem(c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets((c(foxTarget, waterTarget)) * (1 - retTarget)) %>% 
  add_locked_out_constraints("locked_out")


solShp2 <- solve(newProblem) %>% 
  mutate(
    solution_1 = case_when(locked_out ~ 1,
                           T ~ solution_1)
  )


solveOutput(newProblem, solShp2)

glimpse(solShp2)

# Min Fox Water BLM


minFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)

# Max Fox Water


maxFW <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.07) 



# Max Fox Water BLM


maxFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)


# Min Fox

minF <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) 



# Min Fox Water BLM


minFB <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)



# Max Fox



maxF <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.61) 


solveOutput(maxF, solve(maxF))



# Max Fox BLM


maxFB <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)



# Min Water


minW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 



# Max Water


maxW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3.05 ) 



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




# Evaluate Runs -----------------------------------------------------------


# List of problems
runs <- list(minFW, minFWB, maxFW, maxFWB, minF, minFB, maxF, maxFB, minW, maxW)

runNames <- c("minFW, minFWB, maxFW, maxFWB, minF, 
              minFB, maxF, maxFB, minW, maxW") %>% 
  str_split(",[:space:]*") %>% 
  unlist()

# Solve all Problems
solved <- map(runs, solve)


solved2 <- append(solved, list(base))


solved2 %>% flatten()


length(solved2)


str(solved2)

# Create Summary tables for all problems
outputList <- map2(runs, solved, solveOutput)




# Output Table ------------------------------------------------------------




# Format into final table
outputListLength <- map(outputList, nrow) %>% 
  unlist()


runNames2 <- rep(runNames, outputListLength) %>% 
  enframe(name = NULL, value = "Scenario") %>% 
  bind_cols(bind_rows(outputList))


runNames2 %>% view()



# Get jobs lost and total fallowed area for all scenarios
findJobsFallowArea <- function(prioritizrRun) {
  
  prioritizrRun %>% 
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    summarize(
      jobsLost = sum(jobs),
      area = sum(acres)
    )
  
}



jobsCostList <- map(solved2, findJobsFallowArea) %>% 
  bind_rows() %>% 
  bind_cols(enframe(runNames %>% append("base"), name = NULL, value = "Scenario"))

summaryTable <- runNames2 %>% 
  dplyr::select(-starts_with("summary")) %>% 
  dplyr::select(Scenario, everything()) %>% 
  mutate(
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
  full_join(wet, by = c("Scenario", "cost")) %>% 
  dplyr::select(-starts_with("Resou")) %>% 
  as_tibble() %>% 
  add_row(tibble_row(
    Scenario = "base",
    cost = cost,
    `Fox Habitat Fallowed (ha)` = round((foxTarget / 2.471), -2),
    `Water Savings (ac-ft)` = round((waterTarget * 10), -2),
  )) %>% 
  left_join(jobsCostList, by = "Scenario") %>% 
  mutate(
    `Jobs Lost` = round(jobsLost),
    .keep = "unused"
  ) %>% 
  mutate(
    `Cost ($)` = format(as.integer(round(cost * 1000, -4)), nsmall=1, big.mark=","),
    `Total Area Fallowed (ha)` = round((area / 2.471), -2),
    .keep = "unused"
  ) %>% 
  fill(`Total Fox Habitat (ha)`) %>%
  fill(`Total Water Use (ac-ft)`) %>%
  mutate(
    `Relative Fox Habitat Fallowed`  = round(`Fox Habitat Fallowed (ha)` / `Total Fox Habitat (ha)`, 2),
    `Relative Water Savings` = round(`Water Savings (ac-ft)` /  `Total Water Use (ac-ft)`, 2)
  ) %>% 
  mutate(across(everything(), as.character)) %>% 
  replace(is.na(.), " - ") %>% 
  dplyr::select(Scenario, `Cost ($)`, `Jobs Lost`, `Total Area Fallowed (ha)`,everything())


# Table of results --------------------------------------------------------



# Find 10 most fallowed crops
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



nonRuns <- findMostFallowed(base) %>% 
  mutate(
    Scenario = "base"
  )

mostFallowed <- map(solved, findMostFallowed) %>% 
  bind_rows() %>%
  bind_rows(nonRuns)




# List of Names to write out shpFiles
nm <- runNames %>% 
  str_c("R.shp")


cropsNameTable <- runNames %>% 
  append(list("base"))




cropsFallowed <- enframe(cropsNameTable, name = NULL, value = "Scenario") %>% 
  bind_cols(mostFallowed) 



write_csv(sumTableFinal, "./Data/summaryTable.csv")


write_csv(cropsFallowed, "./Data/summaryCropTable.csv")


# Create Shapes -----------------------------------------------------------

# Retired

runsR <- map(solved, convertCommRetired)

# Idle

runsI <- map(solved, convertComm)



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


