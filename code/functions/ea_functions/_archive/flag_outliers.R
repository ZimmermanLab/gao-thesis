# This function takes in cleaned EA data and establishes outlier flags.
# >1.5x IQR = moderate outlier, >3x IQR = extreme outlier

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

flag_outliers <- function(clean_data, type) {
  if(type == "percent"){
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
  } else if(type == "ratio"){
    outlier_flags <- clean_data %>%
      group_by(sample_no) %>%
      mutate(median_cn = median(c_n_ratio),
             iqr_cn = IQR(c_n_ratio)) %>%
      mutate(outlier_flags_ratio = case_when(c_n_ratio > (median_cn + 3 * iqr_cn) |
                                         c_n_ratio < (median_cn - 3 * iqr_cn)
                                       ~ "extreme",
                                       c_n_ratio > (median_cn + 1.5 * iqr_cn) |
                                         c_n_ratio < (median_cn - 1.5 * iqr_cn)
                                       ~ "moderate"))
  } else {print("Please enter type of data: `percent`` or `ratio`")}
  return(outlier_flags)
}
