# This function takes in dried soil weight data and creates a
# scale that can be used to normalize starting proportional concentrations
# from qPCR data.

# Sarah Gao
# Octoer 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

norm_soil_wt <- function(stats_data, soil_wt) {
  mean_soil_wt <- mean(soil_wt$dry_soil_only)
  norm_soil_wt <- stats_data %>%
    left_join(soil_wt) %>%
    select(-c(extraction_soil_wt_mg, qubit_concentration)) %>%
    mutate(wt_norm = mean_soil_wt / dry_soil_only)
  return(norm_soil_wt)
}
