# This function reads in cleaned SmartChem N data, creates median
# and IQR summaries, and flags outliers.
# No NAs are accepted for this function.

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

# Load libraries
library("readr")
library("dplyr")
library("tidyr")

sum_samps <- function(clean_dataset) {
  outlier_flags <- clean_dataset %>%
    select(-run_time) %>%
    group_by(sample_no, sample_type) %>%
    mutate(samp_med_nh3 = median(nh3),
              samp_iqr_nh3 = IQR(nh3),
              samp_med_no2_no3 = median(no2_no3),
              samp_iqr_no2_no3 = IQR(no2_no3)) %>%
    mutate(outlier_flag = case_when(nh3 > (samp_med_nh3 + 3 * samp_iqr_nh3) |
                                      nh3 < (samp_med_nh3 - 3 * samp_iqr_nh3) |
                                      no2_no3 > (
                                        samp_med_no2_no3 + 3 *
                                          samp_iqr_no2_no3) |
                                      no2_no3 < (
                                        samp_med_no2_no3 - 3 *
                                          samp_iqr_no2_no3)
                                      ~ "extreme",
                                    nh3 > (samp_med_nh3 + 1.5 *
                                             samp_iqr_nh3) |
                                      nh3 < (samp_med_nh3 - 1.5 *
                                               samp_iqr_nh3) |
                                      no2_no3 > (
                                        samp_med_no2_no3 + 1.5 *
                                          samp_iqr_no2_no3) |
                                      no2_no3 < (
                                        samp_med_no2_no3 - 1.5 *
                                          samp_iqr_no2_no3)
                                    ~ "moderate"))
  return(outlier_flags)
}
