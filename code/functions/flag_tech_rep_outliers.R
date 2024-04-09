# This function takes in cleaned, sample-only data with technical replicates
# and creates flags for outliers.
# >1.5x IQR = moderate outlier, >3x IQR = extreme outlier

# Sarah Gao
# April 8th, 2024
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

flag_outliers <- function(samples_only) {
  outlier_flags <- clean_data %>%
    group_by(sample_no) %>%
    mutate(median_n = median(n_per),
           median_c = median(c_per),
           iqr_n = IQR(n_per),
           iqr_c = IQR(c_per)) %>%
    mutate(outlier_flags_per = case_when(n_per > (median_n + 3 * iqr_n) |
                                           n_per < (median_n - 3 * iqr_n) |
                                           c_per > (median_c + 3 * iqr_c) |
                                           c_per < (median_c - 3 * iqr_c) ~ "extreme",
                                         n_per > (median_n + 1.5 * iqr_n) |
                                           n_per < (median_n - 1.5 * iqr_n) |
                                           c_per > (median_c + 1.5 * iqr_c) |
                                           c_per < (median_c - 1.5 * iqr_c) ~ "moderate"))
}
