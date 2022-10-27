# This function reads in EA data that's been mapped to treatment levels
# and compiles them to find summary statistics per treatment

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

library("dplyr")

summarize_treatments <- function(sample_stats) {
  compiled <- sample_stats %>%
    group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
    summarise(mean_mean_cn = mean(mean_cn),
              sd_mean_cn = sd(mean_cn),
              mean_mean_nper = mean(mean_nper),
              sd_mean_nper = sd(mean_nper),
              mean_mean_cper = mean(mean_cper),
              sd_mean_cper = sd(mean_cper))
  return(compiled)
}
