# This function takes in qPCR data and finds proportional starting
# concentrations using raw Cq values.

# Sarah Gao
# Octoer 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")

calc_prop_conc <- function(stats_data){
  prop_conc <- stats_data %>%
    mutate(prop_concentration = 2 ^ (- median_cq))
  return(prop_conc)
}
