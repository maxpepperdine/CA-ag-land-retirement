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

options(scipen = 999)

# Read In Data ------------------------------------------------------------


habitatExtract <- read_sf("Data/8_blm/cleanShapes.shp") %>% 
  st_make_valid() %>% 
  # There is one outlier due to processing issues. 
  mutate(
    COMM = if_else(COMM == "UNCULTIVATED AG", "UNCULTIVATED", COMM)
  )



bmShapes <- read_sf("./Data/8_blm/blmShapes.shp") 


bm <- bmShapes %>% 
  boundary_matrix()


habitatExtract %>% 
  filter(is.na(COMM))

habitatExtract %>% 
  arrange(COMM) %>% 
  pull(COMM) %>% 
  unique()

# Find Budget and Targets -------------------------------------------------

targets <- read_csv("./Data/9_estimateBudgetTargets/targetTable.csv")

oldCost <- 398080
oldWater <- 248531
oldFox <- 11203

cost <- targets[[1, 2]] / 1000
waterTarget <- targets[[2, 2]] / 10
foxTarget <- targets[[3, 2]]



#Filter small hab / water values, and divide revenue by 1000, water by 10
habitatProportion <- habitatExtract %>%
  mutate(
    fox = if_else(habitat < 0.001, 0, habitat), #(habitat / habitatSum) * 100,
    water = if_else(water < 0.0001, 0, water),
    water = water / 10,
    jobsQ = jobsQ / 100,
    jobsB = jobsB / 100,
    revenue = revenue / 1000#(water / waterSum) * 100
  )


# habProp2 <- habitatProportion %>% 
#   st_transform(crs(try))



#Get total values for habitat and water use
habitatSum <- sum(habitatProportion$habitat)
waterSum <- sum(habitatProportion$water)
jobBSum <- sum(habitatProportion$jobsB)
jobQSum <- sum(habitatProportion$jobsQ)

 # Jobs are still in x100 
jobQTarget <- jobQSum - (targets[[5, 2]] / 100)
jobBTarget <- jobBSum - (targets[[4, 2]] / 100)


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
  add_absolute_targets(c(foxTarget, waterTarget)) 


# Min Fox Water BLM


minFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(foxTarget, waterTarget)) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)

# Max Fox Water


maxFW <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 3.08) 


# 
# solveOutput(maxFW, solve(maxFW))


# Max Fox Water BLM


maxFWB <- problem(habitatProportion, c("fox", "water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget, waterTarget) * 1.26) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)


#solveOutput(maxFWB, solve(maxFWB))

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


#solveOutput(maxF, solve(maxF))



# Max Fox BLM


maxFB <- problem(habitatProportion, c("fox"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(foxTarget) * 1.05) %>% 
  add_boundary_penalties(penalty = 0.1, edge_factor = 0, 
                         data = bm)


#solveOutput(maxFB, solve(maxFB))

# Min Water


minW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_min_set_objective() %>% 
  add_absolute_targets(c(waterTarget)) 



# Max Water


maxW <- problem(habitatProportion, c("water"), cost_column = "revenue") %>% 
  add_max_features_objective(cost) %>% 
  add_absolute_targets(c(waterTarget) * 3.05 ) 




#solveOutput(maxW, solve(maxW))

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



write_sf(baseROut, "./Data/10_prioritizr/baseR.shp")
write_sf(baseOut, "./Data/10_prioritizr/base.shp")




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




# Acres selected in every scenario -------------------------------------------

findAll <- function(tibble) {
  
  tibble %>% 
    st_drop_geometry() %>% 
    dplyr::select(id, acres, solution_1)#%>% 
    #filter(solution_1 == 1)
  
  
  
}


every <- map(solved[c(1, 3, 5, 7, 9, 10)], findAll) 


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




allMap <- habitatProportion %>% 
  semi_join(every2 %>%  filter(selected == 6), by = "id") %>% 
  tm_shape() + 
  tm_fill(col = "red", alpha = 0.7)


halfMap <- habitatProportion %>% 
  semi_join(every2 %>%  filter(selected < 3, selected > 0), by = "id") %>% 
  tm_shape() + 
  tm_fill(col = "blue", alpha = 0.7)


habitatProportion %>% 
  tm_shape() + 
  tm_borders() + 
  allMap + halfMap






# Output Table ------------------------------------------------------------




# Format into final table
outputListLength <- map(outputList, nrow) %>% 
  unlist()


runNames2 <- rep(runNames, outputListLength) %>% 
  enframe(name = NULL, value = "Scenario") %>% 
  bind_cols(bind_rows(outputList))

# 
# runNames2 %>% view()



# Get jobs lost and total fallowed area for all scenarios
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
    `Total Fox Habitat (kha)` = round((total_amount / 2471), 1),
    #`Fox Habitat Fallowed (kha)` = round((absolute_held / 2471), 1),
    #`Relative Fox Habitat Fallowed` = round((relative_held), 2),
    .keep = "unused"
  ) %>% 
  select(-absolute_held, -relative_held)

wet <- summaryTable %>% 
  filter(Resource == "water") %>% 
  mutate(
    `Total Water Use (kac-ft)` = round((total_amount * 0.01), 1),
    #`Water Savings (kac-ft)` = round((absolute_held * .01), 1),
    #`Relative Water Savings` = round((relative_held), 2),
    .keep = "unused"
  ) %>% 
  select(-absolute_held, -relative_held)


sumTableFinal <- foxy %>% 
  full_join(wet, by = c("Scenario", "cost")) %>% 
  dplyr::select(-starts_with("Resou")) %>% 
  as_tibble() %>% 
  add_row(tibble_row(
    Scenario = "base",
    cost = cost,
    #`Fox Habitat Fallowed (ha)` = round((foxTarget / 2.471), -2),
    #`Water Savings (ac-ft)` = round((waterTarget * 10), -2),
  )) %>% 
  left_join(jobsCostList, by = "Scenario") %>% 
  mutate(
    `Cost ($M)` = as.integer(round(cost / 1000, 0)),
    #`Total Area Fallowed (kha)` = round((area / 2471), 1),
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
  #glimpse()
  #dplyr::select(Scenario, `Cost ($M)`, `Jobs Lost (AIC)`, `Jobs Lost (QCEW)`, `Total Area Fallowed (kha)`,everything())
  
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
  dplyr::select(1, 13, 4, 5, 6, 9, 2, 7, 11, 3, 8, 12)

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



write_csv(sumTableFinal, "./Data/11_tablesFigures/summaryTable.csv")


write_csv(cropsFallowed, "./Data/11_tablesFigures/summaryCropTable.csv")


# Create Shapes -----------------------------------------------------------

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



cropKey <- runsR %>% 
  list(runsI) %>% 
  bind_rows() %>% 
  st_drop_geometry() %>% 
  pull(Crop) %>% 
  unique() %>%
  sort() %>% 
  enframe() 



write_csv(cropKey, "./Data/10_prioritizr/finalCropKey.csv")





# Appendix ----------------------------------------------------------------


