# This function normalizes proportional concentration data from raw Cq values,
# normalizes them using weights derived from dried soil samples, and
# calculates fungal:bacterial ratios from these proportions per sample.

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

calc_ratios <- function(prop_conc_data, soil_wt){
  mean_soil_wt <- mean(soil_wt$dry_soil_only)
  ratios <- prop_conc_data %>%
    left_join(soil_wt) %>%
    select(-c(extraction_soil_wt_mg, qubit_concentration)) %>%
    mutate(wt_norm = mean_soil_wt / dry_soil_only,
           bact_prop_conc_norm = concentration_bacterial * wt_norm,
           fung_prop_conc_norm = concentration_fungal * wt_norm,
           fung_bact_ratio = fung_prop_conc_norm / bact_prop_conc_norm) %>%
    select(-c(2:9))
  return(ratios)
}
