# This function cleans up the raw N data from the SmartChem.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

# Load libraries
library("dplyr")
library("tidyr")

n_calculate_sample_stats <- function(clean_n_data) {

  # Calculate NH3 means and SDs for individual replicates
  # Note that each soil sample is run in at least triplicate
  mean_nh3 <- c()
  rsd_nh3 <- c()
  for (row in 1:length(clean_n_data$sample_no)) {
    if (clean_n_data$id_match[row] == 0) {
      sample_group_nh3 <- c(clean_n_data$nh3[row],
                            clean_n_data$nh3[row + 1])
      sample_mean_nh3 <- mean(sample_group_nh3)
      sample_rsd_nh3 <- sd(sample_group_nh3) * 100 / sample_mean_nh3
    }

    else{
      sample_mean_nh3 <- NA
      sample_rsd_nh3 <- NA
    }
    mean_nh3 <- c(mean_nh3, sample_mean_nh3)
    rsd_nh3 <- c(rsd_nh3, sample_rsd_nh3)
  }

  # Add NH3 stats to master all_samples dataframe
  all_samples <- clean_n_data %>%
    bind_cols(data.frame(mean_nh3, rsd_nh3))

  ########

  # Calculate NO2 means and SDs for individual replicates
  # Note that each soil sample is run in at least triplicate
  mean_no2 <- c()
  rsd_no2 <- c()
  for (row in 1:length(clean_n_data$sample_no)) {
    if (clean_n_data$id_match[row] == 0) {
      sample_group_no2 <- c(clean_n_data$no2[row],
                                clean_n_data$no2[row + 1])
      sample_mean_no2 <- mean(sample_group_no2)
      sample_rsd_no2 <- sd(sample_group_no2) * 100 / sample_mean_no2
    }

    else{
      sample_mean_no2 <- NA
      sample_rsd_no2 <- NA
    }

    # Calculate NO2 means and SDs for unique soil samples
    mean_no2 <- c(mean_no2, sample_mean_no2)
    rsd_no2 <- c(rsd_no2, sample_rsd_no2)
  }

  # Add stats to master all_samples dataframe
  all_samples <- all_samples %>%
    bind_cols(data.frame(mean_no2, rsd_no2)) %>%
    group_by(sample_no)

  ########

  # Calculate NO2-NO3 means and SDs for individual replicates
  # Note that each soil sample is run in at least triplicate
  mean_no2_no3 <- c()
  rsd_no2_no3 <- c()
  for (row in 1:length(clean_n_data$sample_no)) {
    if (clean_n_data$id_match[row] == 0) {
      sample_group_no2_no3 <- c(clean_n_data$no2_no3[row],
                            clean_n_data$no2_no3[row + 1])
      sample_mean_no2_no3 <- mean(sample_group_no2_no3)
      sample_rsd_no2_no3 <- sd(sample_group_no2_no3) * 100 / sample_mean_no2_no3
    }

    else{
      sample_mean_no2_no3 <- NA
      sample_rsd_no2_no3 <- NA
    }

    # Calculate NO2-NO3 means and SDs for unique soil samples
    mean_no2_no3 <- c(mean_no2_no3, sample_mean_no2_no3)
    rsd_no2_no3 <- c(rsd_no2_no3, sample_rsd_no2_no3)
  }

  # Add stats to master all_samples dataframe
  all_samples <- all_samples %>%
    bind_cols(data.frame(mean_no2_no3, rsd_no2_no3)) %>%
    group_by(sample_no)

  ########

  # Clean up all_samples list to only include relevant stats
  all_samples <- all_samples %>%
    select(sample_no, mean_nh3, rsd_nh3,
           mean_no2, rsd_no2, mean_no2_no3, rsd_no2_no3) %>%
    filter(!is.na(mean_nh3))

return(all_samples)
}
