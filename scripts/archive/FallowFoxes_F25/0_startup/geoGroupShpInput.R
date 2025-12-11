# Load necessary packages
library(sf)
library(here)
library(dplyr)
library(janitor)
library(nngeo)

# Step 1: Load and filter Kern data for 2021
CV2021Raw <- read_sf(here("data", "Redownload_2021"))

kern_clean_2021 <- CV2021Raw |>
  clean_names() |>
  filter(county == "Kern") |>
  select(acres, county, region, croptyp2) |>
  st_transform(crs = 3310)

# Subset: randomly sample 100 rows for testing
set.seed(123)  # for reproducibility
kern_subset <- kern_clean_2021 |>
  slice_sample(n = 100)

# Step 2: Find 9 nearest neighbors (including self), then remove self to keep 8
nearest_neighbors <- st_nn(kern_subset, kern_subset, 
                           k = 9,              # 9 to include self, will drop it later
                           returnDist = FALSE,
                           progress = TRUE)


