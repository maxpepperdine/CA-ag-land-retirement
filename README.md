# Using systematic conservation prioritization to inform strategic agricultural land retirement in the San Joaquin Valley

## Description
This repository contains the data and replication code for an analysis to inform strategic agricultural land retirement in the San Joaquin Valley (SJV). The analysis utilized systematic conservation planning (`prioritizr` R package) to meet defined environmental targets (habitat creation, water savings) while minimizing foregone agricultural revenue across 4M+ acres of agricultural land.

## Repository organization description
```
CA-ag-land-retirement
├── scripts/ 
|   └── FallowFoxes_SJV/
|       └── 1_cropRevenueCrosswalk.R
|       └── 2_cleanPlotsLandIQ.R
|       └── 3_masterCrosswalk.R
|       └── 4_revenueEstimation.R
|       └── 5_cropRotation.R
|       └── 6_estimateFallowing_median.R
|       └── 7_habitatExtraction.R
|       └── 8_blm.R
|       └── 9_1_prioritizr_habitat_only.R
|       └── 9_2_prioritizr_water_only.R
|       └── 10_1_prioritizr_habitat_only_figures.R
|       └── 10_2_prioritizr_water_only_figures.R
|   └── misc/
|       └── habitat_suitability/
|           └── 1_env_pred_processing_historic.qmd
|           └── 2_env_pred_processing_future.qmd
|           └── sdm_files/
|               └── bnll_sdm/
|                   └── 0_occurrence_data_processing.qmd
|                   └── 1_extract_occ_env_data.qmd
|                   └── 2_generate_bg_points.qmd
|                   └── 3_extract_bg_env_data.qmd
|                   └── 4_maxent_predictions.qmd
|                   └── 5_prediction_plotting.qmd
|               └── gkr_sdm/
|                   └── 1_extract_occ_env_data_gkr.qmd
|                   └── 2_generate_bg_points_gkr.qmd
|                   └── 3_extract_bg_env_data_gkr.qmd
|                   └── 4_maxent_predictions_gkr.qmd
|                   └── 5_prediction_plotting_gkr.qmd
|               └── sjkf_sdm/
|                   └── 1_extract_occ_env_data_sjkf.qmd
|                   └── 2_generate_bg_points_sjkf.qmd
|                   └── 3_extract_bg_env_data_sjkf.qmd
|                   └── 4_maxent_predictions_sjkf.qmd
|                   └── 5_prediction_plotting_sjkf.qmd
|       └── LandIQ_processing/
|           └── 1_lastCultivatedLandIQ_2022.R
|           └── 2_lastCultivatedRevWater.R
|           └── 2014/
|               └── 2_cleanPlotsLandIQ_2014.R
|               └── 5_cropRotation_2014.R
|           └── 2016/
|               └── 2_cleanPlotsLandIQ_2016.R
|               └── 5_cropRotation_2016.R
|           └── 2018/
|               └── 2_cleanPlotsLandIQ_2018.R
|               └── 5_cropRotation_2018.R
|           └── 2019/
|               └── 2_cleanPlotsLandIQ_2019.R
|               └── 5_cropRotation_2019.R
|           └── 2020/
|               └── 2_cleanPlotsLandIQ_2020.R
|               └── 5_cropRotation_2020.R
|           └── 2021/
|               └── 2_cleanPlotsLandIQ_2021.R
|               └── 5_cropRotation_2021.R
|       └── minimal_irrigation/
|           └── mechanistic_Snet.R
|           └── ratio_scaling_Snet.R
├── .gitignore                           # files to ignore when pushing to GitHub 
├── README.md                            # description of repository
├── CA-ag-land-retirement.Rproj          # R project file
```

## Data access
All relevant data available here (insert link later).

The data were too large to upload to GitHub, but once downloaded, your repository should have the following structure:
```
CA-ag-land-retirement
├── *folders/files listed above*
│
├──data/
|   └── intermediate
|       └── 0_input
|       └── 1_cropRevenueCrosswalk
|       └── 2_cleanPlotsLandIQ
|       └── 3_masterCrosswalk
|       └── 4_revenueEstimation
|       └── 5_cropRotation
|       └── 6_estimateFallowing_median
|       └── 7_habitatExtraction
|       └── 8_blm
|       └── 9_1_prioritizr_habitat_only
|       └── 9_2_prioritizr_water_only
|       └── 10_1_prioritizr_habitat_only_figures
|       └── 10_2_prioritizr_water_only_figures
|       └── misc/
|           └── habitat_suitability/
|               │   all env predictor variables & SDM intermediate/output data
|           └── LandIQ_processing/
|               │   all intermediate LandIQ (2014, 2016, 2018-2021) processing 
|           └── minimal_irrigation/
|               │   S_net generation data used in prioritizr water savings
|   └── raw
```
## Acknowledgements
*add acknowledgements* 

