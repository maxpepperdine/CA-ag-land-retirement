library(tidyverse)
library(pdftools)



pdfs <- list.files(path = "./Data/costStudies/",
                   full.names = T)

findTable <- function(file) {
  
  
  pdf <- pdf_text(file)
  
  
  #Split pdf pages into vectors where every element is a unique line
  split <- map(pdf, ~ str_split(., "\\n")) %>% 
    flatten()
  
  
  #return(trimws(split))
  
  
  # Get page lists that have costs and returns
  pageList <- map(
    split,
    ~ str_detect(., regex("operating costs", ignore_case = T)) %>% 
                    any()
  ) %>% 
    unlist()
  
  # Subset list of pages
  split2 <- split[pageList]
  
  # Get units of labor "hrs"
  pageList2 <- map(
    split2,
    ~ str_detect(., regex("hrs[:space:]", ignore_case = T)) %>% 
                     any()
  ) %>% 
    unlist()
  
  # Subset again
  split3 <- split2[pageList2]
  
  
  
  # Get units of labor "hrs"
  pageList3 <- map(
    split3,
    ~ str_detect(., regex("machine", ignore_case = T)) %>% 
      any()
  ) %>% 
    unlist()
  
  # Subset again
  split4 <- split3[pageList3] %>% 
    map(trimws)
  
  
  
  pageList4 <- map(
    split4,
    ~ str_detect(., regex("year 4", ignore_case = T), negate = T) %>% 
      all()
  ) %>% 
    unlist()
  
  split5 <- split4[(pageList4)]
  
  
  
}


help <- findTable(pdfs[[1]]) %>% 
  #unlist() %>% 
  map(trimws)


try <- map(pdfs, findTable)


tryNot <- try %>% 
  map(length) %>% 
  unlist()


try2 <- try %>% 
  flatten()

test <- try2[[6]]



# Find labor hours and sum ------------------------------------------------


pickHrs <- function(vector) {
  
  
  # Get page lists that labor
  lineList <- map(
    vector,
    ~ str_detect(., regex("labor", ignore_case = T)) %>% 
      any()
  ) %>% 
    unlist()
  
  
  #return(lineList)
  # Subset list of pages
  sub1 <- vector[lineList == TRUE]
  
  
  sub2 <- map_chr(sub1, ~ str_replace_all(., "[:space:]", "")) %>% 
    map_chr(~ str_extract(., "(.*)(?=(hrs))")) %>% 
    map_chr(~ str_replace_all(., "[:alpha:]", "")) #%>% 
    # as.numeric() %>% 
    # abs() %>% 
    # sum(na.rm = T)
  
  
  return(sub2)
  
  
}

pickHrs(test)

map(try2, pickHrs) %>% 
  flatten()

pickHrs(try2[[8]])

# Find Crop and year ------------------------------------------------------

pickCrop <- function(vector) {
  
  
  # Get page lists that have costs and returns
  lineList <- map(
    vector,
    ~ str_detect(., regex("labor", ignore_case = T)) %>% 
      any()
  ) %>% 
    unlist()
  
  
  #return(lineList)
  # Subset list of pages
  sub1 <- vector[lineList == TRUE]
  
  
  sub2 <- map_chr(sub1, ~ str_replace_all(., "[:space:]", "")) %>% 
    map_chr(~ str_extract(., "(.*)(?=(hrs))")) %>% 
    map_chr(~ str_replace_all(., "[:alpha:]", "")) %>% 
    as.numeric() %>% 
    abs() %>% 
    sum(na.rm = T)
  
  
  return(sub2)
  
  
  
  
  
}

