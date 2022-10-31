# This function reads in N sample data with replicates and finds summary
# statistics, ie means and SDs, per sample.

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

# Load libraries
library("dplyr")
library("tidyr")

summarize_samples <- function(n_samples) {
  samples_stats <- n_samples %>%
    group_by(sample_no, sample_type) %>%
    summarize(samp_med_nh3 = median(nh3),
              samp_iqr_nh3 = IQR(nh3),
              samp_med_no2_no3 = median(no2_no3),
              samp_iqr_no2_no3 = IQR(no2_no3))
  return(samples_stats)
}
