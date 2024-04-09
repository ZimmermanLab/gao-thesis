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
  # Zeroes out any negative values
  samples_only$value[samples_only$value < 0] <- 0
  outlier_flags <- samples_only %>%
    group_by(sample_id, measurement_type, subtype, subsubtype,
             run_or_plate_id, analyzed_date) %>%
    mutate(median = median(value, na.rm = TRUE),
           iqr = IQR(value, na.rm = TRUE)) %>%
    mutate(outlier_flags = case_when(value > (median + 3 * iqr) |
                                           value < (median - 3 * iqr)
                                         ~ "extreme",
                                         value > (median + 1.5 * iqr) |
                                           value < (median - 1.5 * iqr)
                                         ~ "moderate"))

  return(outlier_flags)
}
