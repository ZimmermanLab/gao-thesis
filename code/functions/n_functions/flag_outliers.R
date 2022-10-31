# This function reads in cleaned SmartChem N data flags outliers.
# No NAs are accepted for this function.

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

# Load libraries
library("readr")
library("dplyr")
library("tidyr")

flag_outliers <- function(clean_dataset) {
  outlier_flags <- clean_dataset %>%
    mutate(outlier_flag = case_when(nh3 > (median_nh3 + 3 * iqr_nh3) |
                                      nh3 < (median_nh3 - 3 * iqr_nh3) |
                                      no2_no3 > (
                                        median_no2_no3 + 3 * iqr_no2_no3) |
                                      no2_no3 < (
                                        median_no2_no3 - 3 * iqr_no2_no3)
                                      ~ "extreme",
                                    nh3 > (median_nh3 + 1.5 * iqr_nh3) |
                                      nh3 < (median_nh3 - 1.5 * iqr_nh3) |
                                      no2_no3 > (
                                        median_no2_no3 + 1.5 * iqr_no2_no3) |
                                      no2_no3 < (
                                        median_no2_no3 - 1.5 * iqr_no2_no3)
                                    ~ "moderate"))
  return(outlier_flags)
}
