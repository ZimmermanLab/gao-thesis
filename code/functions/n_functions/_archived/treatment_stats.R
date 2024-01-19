# This function runs stats on the cleaned N data from the SmartChem.

# Sarah Gao
# July 7th, 2022
# hellosarahgao@gmail.com

library("dplyr")

# Summarize statistics for each treatment level
summarize_treat_stats <- function(sample_stats, sample_assignments) {
  treatment_stats <- sample_stats %>%
    group_by(sample_no, sample_type) %>%
    left_join(sample_assignments) %>%
    group_by(cc_treatment, drying_treatment, pre_post_wet, sample_type) %>%
    summarize(mean_mean_nh3 = mean(mean_nh3),
              sd_mean_nh3 = sd(mean_nh3),
              mean_mean_no2_no3 = mean(mean_no2_no3),
              sd_mean_no2_no3 = sd(mean_no2_no3))
  return(treatment_stats)
}
