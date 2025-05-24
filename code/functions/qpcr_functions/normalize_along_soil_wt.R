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
  clean_soil_wt <- soil_wt %>%
    filter(is.na(dry_soil_only) == FALSE) %>%
    select(-notes) %>%
    rename(sample_id = sample_no) %>%
    select(c(sample_id, dry_soil_only))
  # Find the mean soil weight across all samples
  mean_soil_wt <- mean(clean_soil_wt$dry_soil_only)
  norm_soil_wt <- stats_data %>%
    left_join(clean_soil_wt)  %>%
    # Create a factor based on soil weight to multiply
    # proportional concentration values with
    mutate(soil_wt_factor = mean_soil_wt / dry_soil_only) %>%
    select(-dry_soil_only)

  return(norm_soil_wt)
}
