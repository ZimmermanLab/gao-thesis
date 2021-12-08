# This function reads in EA data and finds the
# mean and RSD of each sample.

# Sarah Gao
# October 12, 2021
# hellosarahgao@gmail.com

# Note that this script assumes that each sample is run 2 times.

library("dplyr")
library("stringr")

calculate_sample_stats <- function(cleaned_ea_data) {

  # Collect all the samples into one dataframe and create a column that checks
  # for matched names
  all_samples <- cleaned_ea_data %>%
    select(pos, sample, n_per, c_per) %>%
    filter(sample != "SRM") %>%
    filter(sample != "BLANK") %>%
    filter(!str_detect(sample, "^ASP")) %>%
    mutate(name_match = as.numeric(
      sample == lag(sample, 1))) %>%
    replace(is.na(.), 0) %>%
    arrange(sample)

  # Run a for loop that calculates the mean %C and C RSD for each sample
  mean_c <- c()
  rsd_c <- c()
  for (sample_no in 1:length(all_samples$sample_no)) {
    if (all_samples$name_match[sample_no] == 0) {
      sample_group_c <- c(all_samples$c_per[sample_no],
                          all_samples$c_per[sample_no + 1])
      sample_mean_c <- mean(sample_group_c)
      sample_rsd_c <- sd(sample_group_c) * 100 / sample_mean_c
      }
    else{
      sample_mean_c <- NA
      sample_rsd_c <- NA
    }
      mean_c <- c(mean_c, sample_mean_c)
      rsd_c <- c(rsd_c, sample_rsd_c)
  }

  all_samples <- cbind(all_samples, mean_c, rsd_c)

  # Run a for loop that calculates the mean %N and N RSD for each sample
  mean_n <- c()
  rsd_n <- c()
  for (sample_no in 1:length(all_samples$sample_no)) {
    if (all_samples$name_match[sample_no] == 0) {
      sample_group_n <- c(all_samples$n_per[sample_no],
                        all_samples$n_per[sample_no + 1])
      sample_mean_n <- mean(sample_group_n)
      sample_rsd_n <- sd(sample_group_n) * 100 / sample_mean_n
      }
    else{
      sample_mean_n <- NA
      sample_rsd_n <- NA
    }
    mean_n <- c(mean_n, sample_mean_n)
    rsd_n <- c(rsd_n, sample_rsd_n)
  }

  all_samples <- cbind(all_samples, mean_n, rsd_n)

return(all_samples)
}
