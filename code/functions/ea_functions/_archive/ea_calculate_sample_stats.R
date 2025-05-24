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
    select(sample_no, c_n_ratio, n_per, c_per) %>%
    filter(!(sample_no == "SRM" |
               sample_no == "BLANK" |
               sample_no == "Blank" |
               grepl("Blank", sample_no) == TRUE |
               sample_no == "Bypass")) %>%
    filter(!str_detect(sample_no, "^ASP")) %>%
    arrange(sample_no)

  ea_stats <- all_samples %>%
    group_by(sample_no) %>%
    summarize(mean_cn = mean(c_n_ratio),
              sd_cn = sd(c_n_ratio),
              mean_nper = mean(n_per),
              mean_cper = mean(c_per))

return(ea_stats)
}
