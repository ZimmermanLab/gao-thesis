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
    summarize(med_fungal = median(fung_prop_conc_norm),
              med_bacterial = median(bact_prop_conc_norm),
              med_ratio = median(fung_bact_ratio))
  return(treatment_stats)
}
