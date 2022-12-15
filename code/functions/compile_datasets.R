# This function compiles all the datasets into one beefy dataframe

# Sarah Gao
# December 7, 2022
# hellosarahgao@gmail.com

library("dplyr")

compile_datasets <- function() {
  # Read in DNA data
  dna <- read_csv("data/cleaned_data/qPCR/samp_medians.csv")

  # Read in CO2 data
  co2 <- read_csv("data/cleaned_data/LICOR/all_peaks.csv") %>%
    rename(peak_co2_med = median,
           peak_co2_iqr = iqr)

  # Read in total CN data
  total_cn <- read_csv("data/cleaned_data/EA/samp_sum_all.csv")

  # Read in extractable + leachate N data
  inorg_n <- read_csv("data/cleaned_data/SmartChem_N/samp_sum_ratio.csv")

  # Merge into one
  all_data <- dna %>%
    left_join(co2) %>%
    left_join(total_cn) %>%
    left_join(inorg_n) %>%
    select(-c(date, day_elapsed)) %>%
    # Recode drying_treatment and pre_post_wet
    mutate(time = case_when(drying_treatment == "initial" ~ 0,
                            drying_treatment == "one_wk" ~ 1,
                            drying_treatment == "two_wk" ~ 2,
                            (drying_treatment == "four_wk" |
                            drying_treatment == "all_dry") ~ 4),
           drought_type = case_when((pre_post_wet == "initial" |
                                      pre_post_wet == "cw") ~ "none",
                                    (pre_post_wet == "pre" |
                                      pre_post_wet == "all_dry") ~ "continued",
                                    pre_post_wet == "post" ~ "ended")) %>%
    # Sum all inorganic N into one column
    mutate(leach_inorg_n_per = leach_nh3_per_median + leach_no2no3_per_median,
           ext_inorg_n_per = ext_nh3_per_median + ext_no2no3_per_median) %>%
    relocate(c(sample_no, cc_treatment, time, drought_type)) %>%
    select(-c(drying_treatment, pre_post_wet))

  return(all_data)
}
