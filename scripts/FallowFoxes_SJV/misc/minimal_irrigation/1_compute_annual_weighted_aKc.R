# =============================================================================
# Compute ET0-Weighted Annual Average aKc for California Central Valley Crops
# =============================================================================
# 
# This script uses monthly adjusted crop coefficients (aKc) from Mhawej et al.
# (2021) and weights them by monthly reference evapotranspiration (ET0) from
# the California Irrigation Management Information System (CIMIS).
#
# ET0 Source: CIMIS Reference Evapotranspiration Zones Map (DWR & UC Davis, 1999)
#   - PDF: https://wwwcimis.water.ca.gov/Content/pdf/CimisRefEvapZones.pdf
#   - The study area (~37°40'-37°50'N, 121°0'-121°30'W) in the San Joaquin /
#     Central Valley falls within CIMIS ETo Zones 12 and 14.
#   - Zone 12: East Side Sacramento-San Joaquin Valley
#   - Zone 14: Mid-Central Valley, Southern Sierra Nevada, Tehachapi &
#              High Desert Mountains
#   - We average Zones 12 and 14 to represent the aKc study area.
#   - ET0 values are in inches/month (long-term monthly averages).
#
# aKc Source: Mhawej et al. (2021), Table 2
#   - Agricultural Water Management, 256, 107059
#   - doi: 10.1016/j.agwat.2021.107059
#
# Formula: Annual_aKc = sum(aKc_month * ET0_month) / sum(ET0_month)
# =============================================================================

library(tidyverse)
library(here)

# ---------------------------------------------------------------------------
# 1. CIMIS Monthly ET0 Data (inches/month)
# ---------------------------------------------------------------------------
# From the CIMIS ETo Zones reference PDF
# https://wwwcimis.water.ca.gov/Content/pdf/CimisRefEvapZones.pdf
# Zone 12: East Side Sacramento-San Joaquin Valley
# Zone 14: Mid-Central Valley, Southern Sierra Nevada, Tehachapi & High Desert Mountains

ET0_zone12 <- c(Jan = 1.24, Feb = 1.96, Mar = 3.41, Apr = 5.10, May = 6.82,
                Jun = 7.80, Jul = 8.06, Aug = 7.13, Sep = 5.40, Oct = 3.72,
                Nov = 1.80, Dec = 0.93)

ET0_zone14 <- c(Jan = 1.55, Feb = 2.24, Mar = 3.72, Apr = 5.10, May = 6.82,
                Jun = 7.80, Jul = 8.68, Aug = 7.75, Sep = 5.70, Oct = 4.03,
                Nov = 2.10, Dec = 1.55)

# average of Zones 12 and 14 to represent the study area
ET0_monthly <- (ET0_zone12 + ET0_zone14) / 2

cat("Monthly ET0 (inches/month) - Average of CIMIS Zones 12 & 14:\n")
print(round(ET0_monthly, 2))
cat(sprintf("\nTotal Annual ET0: %.1f inches\n\n", sum(ET0_monthly)))

# ---------------------------------------------------------------------------
# 2. Monthly aKc Data from Mhawej et al. (2021), Table 2
# ---------------------------------------------------------------------------

# load aKc data
aKc_data <- read_csv(here("data/raw/crop_coefficients/aKc_monthly_avgs_mhawej.csv"))

# ---------------------------------------------------------------------------
# 3. Compute ET0-Weighted Annual aKc
# ---------------------------------------------------------------------------
# Formula: Annual_aKc = sum(aKc_i * ET0_i) / sum(ET0_i)
#
# This is equivalent to: Total Annual ETa / Total Annual ET0
# and properly accounts for the fact that summer months with high ET0
# contribute more to annual water consumption than winter months.

month_cols <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

aKc_data$Annual_Weighted_aKc <- apply(aKc_data[, month_cols], 1, function(row) {
  sum(row * ET0_monthly) / sum(ET0_monthly)
})

# round to 2 decimal places
aKc_data$Annual_Weighted_aKc <- round(aKc_data$Annual_Weighted_aKc, 2)

# ---------------------------------------------------------------------------
# 4. Display and Export Results
# ---------------------------------------------------------------------------

# print summary
cat(sprintf("  Range: %.2f to %.2f\n",
            min(aKc_data$Annual_Weighted_aKc),
            max(aKc_data$Annual_Weighted_aKc)))
cat(sprintf("  Mean:  %.2f\n", mean(aKc_data$Annual_Weighted_aKc)))
cat(sprintf("  Median: %.2f\n\n", median(aKc_data$Annual_Weighted_aKc)))

# show top 10 highest water-consuming crops
cat("Top 10 crops by annual weighted aKc:\n")
top10 <- aKc_data %>%
  arrange(desc(Annual_Weighted_aKc)) %>%
  dplyr::select(Crop_Type, Annual_Weighted_aKc) %>%
  head(10)
print(top10, row.names = FALSE)

# keep only relevant columns for export
aKc_final <- aKc_data %>%
  dplyr::select(crop_type = Crop_Type, weighted_aKc = Annual_Weighted_aKc)

# export to CSV
output_file <- here("data/intermediate/misc/minimal_irrigation/1_compute_annual_weighted_aKc/aKc_annual_weighted.csv")
write_csv(aKc_final, output_file)










