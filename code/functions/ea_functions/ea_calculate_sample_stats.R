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
    select(sample_no, n_mg, c_mg) %>%
    filter(!(sample_no == "SRM" |
               sample_no == "BLANK" |
               sample_no == "Blank")) %>%
    filter(!str_detect(sample_no, "^ASP")) %>%
    replace(is.na(.), 0) %>%
    arrange(sample_no)

ea_stats <- all_samples %>%
  group_by(sample_no) %>%
  summarize(mean_n = mean(n_mg),
            sd_n = sd(n_mg),
            mean_c = mean(c_mg),
            sd_c = sd(c_mg)) %>%
  mutate(rsd_n = sd_n * 100 / mean_n,
         rsd_c = sd_c * 100 / mean_c)

# Flag any crazies that might need to be rerun
all_samples_clean <- ea_stats %>%
  mutate(flag = case_when(rsd_c > 10 | rsd_n > 10
                          | is.na(sd_n) | is.na(sd_c) ~ "yes"))

return(all_samples_clean)
}
