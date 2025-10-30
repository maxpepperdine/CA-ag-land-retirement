# clear environment
rm(list = ls())

library(tidyverse)
library(prioritizr)
library(prioritizrdata)
library(sf)
library(tmap)
library(terra)
library(rmapshaper)
library(here)

source(here("scripts/FallowFoxes_F25/0_startup/0_2_functions.R"))


# tmap_mode("view")
# tmap_options(check.and.fix = TRUE)

options(scipen = 999)

# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf(here("data/intermediate/8_blm/cleanShapes/cleanShapes.shp")) %>% 
  st_make_valid() %>% 
  select(-crop)


bmShapes <- read_sf(here("data/intermediate/8_blm/blmShapes/blmShapes.shp")) 


bm <- bmShapes %>% 
  boundary_matrix()



# Find Budget and Targets -------------------------------------------------

targets <- read_csv(here("data/intermediate/9_estimateBudgetTargets/targetTable.csv"))

## NOTE: not sure where these values come from, and they aren't used anywhere else in the script

# oldCost <- 398080
# oldWater <- 248531
# oldFox <- 11203

cost <- targets[[1, 2]] / 1e6  # convert to "millions of dollars"
waterTarget <- targets[[2, 2]] / 10
foxTarget <- targets[[3, 2]]



#Filter small hab / water values, and divide revenue by 1000, water by 10
habitatProportion <- habitatExtract %>%
  mutate(
    fox = if_else(habitat < 0.001, 0, habitat), 
    water = if_else(water < 0.0001, 0, water),
    water = water / 10,
    revenue = revenue / 1e6  # convert to "millions of dollars"
  )


#Get total values for habitat and water use
habitatSum <- sum(habitatProportion$habitat)
waterSum <- sum(habitatProportion$water)



# Solve Function ----------------------------------------------------------


solveOutput <- function(problem, solved) {
  
  cost <- eval_cost_summary(problem, solved %>% dplyr::select(solution_1))
  
  runSummary <- eval_feature_representation_summary(problem, solved %>% dplyr::select(solution_1))
  
  cost %>% bind_cols(runSummary)
  
}



# Create and Solve Problesm -----------------------------------------------


# Min Fox Water
minFW <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) %>% 
  add_gurobi_solver()


# Min Fox Water BLM
minFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm) %>% 
  add_gurobi_solver()


# Max Fox Water
maxFW <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.08) %>% 
  add_gurobi_solver() 


# Max Fox Water BLM
maxFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 1.26) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm) %>% 
  add_gurobi_solver()


# Min Fox
minF <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) %>% 
  add_gurobi_solver() 


# Min Fox Water BLM
minFB <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm) %>% 
  add_gurobi_solver()


# Max Fox
maxF <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 2.61) %>% 
  add_gurobi_solver()


# Max Fox BLM
maxFB <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 1.05) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm) %>% 
  add_gurobi_solver()


# Min Water
minW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) %>% 
  add_gurobi_solver() 


# Max Water
maxW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3.05 ) %>% 
  add_gurobi_solver() 




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



write_sf(baseROut, here("data/intermediate/10_prioritizr/baseR/baseR.shp"))
write_sf(baseOut, here("data/intermediate/10_prioritizr/base/base.shp"))




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



# Create Summary tables for all problems
outputList <- map2(runs, solved, solveOutput)






# Output Table ------------------------------------------------------------




# Format into final table
outputListLength <- map(outputList, nrow) %>% 
  unlist()


runNames2 <- rep(runNames, outputListLength) %>% 
  enframe(name = NULL, value = "Scenario") %>% 
  bind_cols(bind_rows(outputList))


# Get jobs, water savings, fox habitat and total fallowed area for all scenarios
findJobsFallowArea <- function(prioritizrRun) {
  
  prioritizrRun %>% 
    st_drop_geometry() %>% 
    filter(solution_1 == 1) %>% 
    summarize(
      `Jobs Lost (AIC)` = round(sum(jobsB)),
      `Jobs Lost (QCEW)` = round(sum(jobsQ)),
      `Jobs Lost (QCEW) + FLC ` = round(sum(jobsQ) / 0.33),
      `Fox Habitat Fallowed (kha)` = round(sum(fox / 2471), 1),
      `Water Savings (kac-ft)` = round(sum(water * .01)),
      `Total Area Fallowed (kha)` = round(sum(acres) / 2471)
    )
  
}



jobsCostList <- map(solved2, findJobsFallowArea) %>% 
  bind_rows() %>% 
  bind_cols(enframe(runNames %>% append("base"), name = NULL, value = "Scenario"))

# Reformat
summaryTable <- runNames2 %>% 
  dplyr::select(-starts_with("summary")) %>% 
  dplyr::select(Scenario, everything()) %>% 
  mutate(
    Resource = feature,
    .keep = "unused"
  ) 


# Total Fox Habitat
foxy <- summaryTable %>% 
  filter(Resource == "fox") %>% 
  mutate(
    `Total Fox Habitat (kha)` = round((total_amount / 2471), 1),
    .keep = "unused"
  ) %>% 
  select(-absolute_held, -relative_held)

# Total Water Use
wet <- summaryTable %>% 
  filter(Resource == "water") %>% 
  mutate(
    `Total Water Use (kac-ft)` = round((total_amount * 0.01), 1),
    .keep = "unused"
  ) %>% 
  select(-absolute_held, -relative_held)


# Summary Table of Prioritizr Runs
sumTableFinal <- foxy %>% 
  full_join(wet, by = c("Scenario", "cost")) %>% 
  dplyr::select(-starts_with("Resou")) %>% 
  as_tibble() %>% 
  add_row(tibble_row(
    Scenario = "base",
    cost = cost,
  )) %>% 
  left_join(jobsCostList, by = "Scenario") %>% 
  mutate(
    `Cost ($M)` = as.integer(round(cost / 1000, 0)),
    .keep = "unused"
  ) %>% 
  fill(`Total Fox Habitat (kha)`) %>%
  fill(`Total Water Use (kac-ft)`) %>%
  mutate(
    `Relative Fox Habitat Fallowed`  = round(`Fox Habitat Fallowed (kha)` / `Total Fox Habitat (kha)`, 2),
    `Relative Water Savings` = round(`Water Savings (kac-ft)` /  `Total Water Use (kac-ft)`, 2)
  ) %>% 
  mutate(across(everything(), as.character)) %>% 
  replace(is.na(.), " - ") %>% 
  mutate(
    Scenario = str_replace(Scenario, "F", " Fox"),
    Scenario = str_replace(Scenario, "W", " Water"),
    Scenario = str_replace(Scenario, "B", "-BLM"),
    Scenario = str_replace(Scenario, "min", "Min."),
    Scenario = str_replace(Scenario, "max", "Max."),
    Scenario = str_replace(Scenario, "base", "Base")
  ) %>% 
  separate(
    col = Scenario,
    into = c("Scenario", "BLM"),
    sep = "(?<=-)"
  ) %>% 
  mutate(
    Scenario = str_replace(Scenario, '-', ""),
    `Boundary Length Modifier` = if_else(is.na(BLM), "-", "X"),
    .keep = "unused"
  ) %>% 
  dplyr::select(1, 13, 10, 4, 5, 6, 9, 2, 7, 11, 3, 8, 12)



# Most Fallowed Crops Table -----------------------------------------------



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


# Base Scenario
nonRuns <- findMostFallowed(base) %>% 
  mutate(
    Scenario = "base"
  )

# Prioritizr Runs
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



write_csv(sumTableFinal, "./Data/11_tablesFigures/summaryTable.csv")


write_csv(cropsFallowed, "./Data/11_tablesFigures/summaryCropTable.csv")



# Shapefiles --------------------------------------------------------------



# Retired

runsR <- map(solved, convertCommRetired)

# Idle

runsI <- map(solved, convertComm)



namesR <- str_c("./Data/10_prioritizr/", nm)


namesI <- namesR %>%
  str_replace("R", "")

# Write out

map2(runsR, namesR, write_sf)
map2(runsI, namesI, write_sf)


# Make Crop Key and Export ------------------------------------------------

# For Connectivity Resistance script 

# Matrix of all land cover types in the runs with numeric code
cropKey <- runsR %>% 
  list(runsI) %>% 
  bind_rows() %>% 
  st_drop_geometry() %>% 
  pull(Crop) %>% 
  unique() %>%
  sort() %>% 
  enframe() 



write_csv(cropKey, "./Data/10_prioritizr/finalCropKey.csv")




