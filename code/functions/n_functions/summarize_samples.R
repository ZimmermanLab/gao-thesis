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
    summarize(mean_nh3 = mean(nh3),
              sd_nh3 = sd(nh3),
              mean_no2_no3 = mean(no2_no3),
              sd_no2_no3 = sd(no2_no3))
  return(samples_stats)
}
