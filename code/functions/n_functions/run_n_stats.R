# This function runs stats on the cleaned N data from the SmartChem.

# Sarah Gao
# July 7th, 2022
# hellosarahgao@gmail.com

library("dplyr")

# Summarize data (i.e. calculate means and SDs) and map to jar assignments
run_n_stats <- function(clean_n_data, sample_assignments) {
  n_data_stats <- clean_n_data %>%
    group_by(sample_no, sample_type) %>%
    summarize(mean_nh3 = mean(nh3),
              sd_nh3 = sd(nh3),
              mean_no2_no3 = mean(no2_no3),
              sd_no2_no3 = sd(no2_no3)) %>%
    left_join(sample_assignments)

  return(n_data_stats)
}
