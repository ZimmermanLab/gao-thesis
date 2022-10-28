# This function normalizes proportional concentration data from raw Cq values,
# normalizes them using weights derived from dried soil samples

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

norm_conc <- function(prop_conc_data, soil_wt){
  mean_soil_wt <- mean(soil_wt$dry_soil_only)
  norm_conc <- prop_conc_data %>%
    left_join(soil_wt) %>%
    select(-c(extraction_soil_wt_mg, qubit_concentration)) %>%
    mutate(wt_norm = mean_soil_wt / dry_soil_only,
           prop_conc_norm = prop_concentration * wt_norm) %>%
    select(-c(3:8))
  return(norm_conc)
}
