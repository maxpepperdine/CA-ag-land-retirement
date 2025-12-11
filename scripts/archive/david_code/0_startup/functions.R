#functions


# Metrics Function --------------------------------------------------------


#Create function to run mean, median, min, max, and sd on vectors. This 
#is to be able to get statistics of calculated columns easily. 


metrics <- function(vector) {
  #Run Summary statistics for vector 
  f <- c("mean", "median", "min", "max", "sum", "sd")
  
  param <- list(
    list(vector)
    
  )
  
  list1 <- invoke_map(f, param, na.rm = TRUE)
  
  list2 <- set_names(list1, f)
  
  list2
  
}


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


# Factor in change between retired and 
convertCommRetired <- function(tibble) {
  change <- tibble %>%
    mutate(
      Crop = case_when(((retired == 1) & (solution_1 == 1)) ~ "UNCULTIVATED", 
                       ((retired == 0) & (solution_1 == 1)) ~ "FALLOW", 
                       TRUE ~ COMM)
    )
  
  codes <- change %>%
    st_drop_geometry() %>%
    dplyr::pull(Crop) %>% unique()
  
  commCodeVect <<- append(commCodeVect, codes)
  
  change
}
