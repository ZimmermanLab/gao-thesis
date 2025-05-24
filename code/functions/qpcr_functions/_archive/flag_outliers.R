# This script reports outliers of various degrees (< 1.5x IQR = moderate,
# < 3x IQR = extreme) from qPCR datasets.
# Datasets cannot have NA values in them.

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")

flag_outliers <- function(clean_dataset) {
  clean_stats <- clean_dataset %>%
    group_by(sample_no) %>%
    mutate(median_cq = median(cq),
           iqr = IQR(cq))
  outlier_flags <- clean_stats %>%
    mutate(outlier_flag = case_when(cq > (median_cq + 3 * iqr) |
                                      cq < (median_cq - 3 * iqr) ~ "extreme",
                                    cq > (median_cq + 1.5 * iqr) |
                                      cq < (median_cq - 1.5 * iqr) ~ "moderate"))
  return(outlier_flags)
}
