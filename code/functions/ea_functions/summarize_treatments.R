# This function reads in EA data that's been mapped to treatment levels
# and compiles them to find summary statistics per treatment

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

library("dplyr")

summarize_treatments <- function(sample_stats) {
  compiled <- sample_stats %>%
    group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
    summarise(mean_mean_cn = mean(mean_c_n),
              sd_mean_cn = sd(mean_c_n))
  return(compiled)
}
