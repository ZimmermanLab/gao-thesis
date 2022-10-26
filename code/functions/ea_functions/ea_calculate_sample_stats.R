# This function reads in cleaned EA data and finds the
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
    select(sample_no, c_n_ratio) %>%
    filter(!(sample_no == "SRM" |
               sample_no == "BLANK" |
               sample_no == "Blank" |
               grepl("Blank", sample_no) == TRUE |
               sample_no == "Bypass")) %>%
    filter(!str_detect(sample_no, "^ASP")) %>%
    replace(is.na(.), 0) %>%
    arrange(sample_no)

  ea_stats <- all_samples %>%
    # Filter out samples that are clearly bad
    filter(c_n_ratio > 6) %>%
    group_by(sample_no) %>%
    summarize(mean_c_n = mean(c_n_ratio),
              sd_c_n = sd(c_n_ratio)) %>%
    mutate(rsd_c_n = sd_c_n * 100 / mean_c_n)

# Flag any crazies that might need to be rerun
all_samples_clean <- ea_stats %>%
  mutate(flag = case_when(rsd_c_n > 10 | is.na(sd_c_n) ~ "yes"))

return(all_samples_clean)
}
