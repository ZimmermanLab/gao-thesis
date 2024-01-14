# This function takes in qPCR datasets with already calculated proportional
# concentration values from raw Cq values and normalizes them using factors
# derived from dried soil weights

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

norm_conc <- function(qpcr_prop_conc_wts){
  norm_conc <- qpcr_prop_conc_wts %>%
    mutate(value = prop_concentration * soil_wt_factor) %>%
    mutate(units = "proportional_concentration_normalized") %>%
    select(-c(soil_wt_factor, prop_concentration))

  return(norm_conc)
}
