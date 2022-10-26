# This function maps samples to treatment conditions and finds the
# means and SDs of proportional starting concentrations across treatments

# Sarah Gao
# Octoer 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

calc_treatment_stats <- function(norm_prop_data, all_treatments) {
  treatment_stats <- norm_prop_data %>%
    left_join(all_treatments) %>%
    group_by(pre_post_wet, drying_treatment, cc_treatment) %>%
    summarize(mean_mean_fungal = mean(fung_prop_conc_norm),
              sd_mean_fungal = sd(fung_prop_conc_norm),
              mean_mean_bacterial = mean(bact_prop_conc_norm),
              sd_mean_bacterial = sd(bact_prop_conc_norm),
              mean_ratio = mean(fung_bact_ratio),
              sd_ratio = sd(fung_bact_ratio))
  return(treatment_stats)
}
