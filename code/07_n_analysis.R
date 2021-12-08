# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Load in libraries

library("stringr")

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
n_data_clean <- clean_n_data(
  "data/raw_data/SmartChem_N_extractions/20210212_Gao_1_export.csv")

# Source and run function to analyze stats for samples
source("code/functions/n_functions/n_calculate_sample_stats.R")
n_sample_stats <- n_calculate_sample_stats(n_data_clean)

# Read in master list of samples by cover crop treatment
cc_list <- readr::read_csv("output/jar_assignments/master_list.csv", col_names =  FALSE) %>%
  select(X2) %>%
  group_by(grp = c("jar_w_cc", "jar_no_cc", "no_soil_controls")[(X2 %in% X2) + 1]) %>%
  mutate(n = row_number()) %>%


  spread(grp, X2)

