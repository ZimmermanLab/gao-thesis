# This function requires a master list of sample treatments and a dataframe of
# nitrate, nitrite, and ammonia results.
# It then organizes the results by treatment group and maps them to the master
# list of sample treatments and results.

# Sarah Gao
# January 5, 2022
# hellosarahgao@gmail.com

# Load packages
library("dplyr")

# Aggregate statistical data by treatment group
add_n_results <- function(all_treatments, n_sample_stats) {
  n_sample_stats <- n_sample_stats %>%
    mutate(drying_treatment = NA, pre_post_wet = NA, cc_treatment = NA)

  wk4_w_cc_pre <- data.frame()
  wk4_no_cc_pre <- data.frame()
  wk4_w_cc_post <- data.frame()
  wk4_no_cc_post <- data.frame()
  wk4_w_cc_cw <- data.frame()
  wk4_no_cc_cw <- data.frame()

  for (row in 1:length(n_sample_stats$sample_no)) {
    sample_cc <- all_treatments$cc_treatment[which(
      grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]
    sample_pre_post_wet <- all_treatments$pre_post_wet[which(
      grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]
    sample_drying_treatment <- all_treatments$drying_treatment[which(
      grepl((n_sample_stats$sample_no[row]), all_treatments$sample_no))]

    if (sample_cc == "no_cc" && sample_pre_post_wet == "pre" &&
        sample_drying_treatment == "four_wk") {
      wk4_no_cc_pre <- rbind(wk4_no_cc_pre, n_sample_stats[row, ])
      wk4_no_cc_pre$drying_treatment <- sample_drying_treatment
      wk4_no_cc_pre$pre_post_wet <- sample_pre_post_wet
      wk4_no_cc_pre$cc_treatment <- sample_cc
    }

    if (sample_cc == "w_cc" && sample_pre_post_wet == "pre" &&
        sample_drying_treatment == "four_wk") {
      wk4_w_cc_pre <- rbind(wk4_w_cc_pre, n_sample_stats[row, ])
      wk4_w_cc_pre$drying_treatment <- sample_drying_treatment
      wk4_w_cc_pre$pre_post_wet <- sample_pre_post_wet
      wk4_w_cc_pre$cc_treatment <- sample_cc
    }

    if (sample_cc == "no_cc" && sample_pre_post_wet == "post"
        && sample_drying_treatment == "four_wk") {
      wk4_no_cc_post <- rbind(wk4_no_cc_post, n_sample_stats[row, ])
      wk4_no_cc_post$drying_treatment <- sample_drying_treatment
      wk4_no_cc_post$pre_post_wet <- sample_pre_post_wet
      wk4_no_cc_post$cc_treatment <- sample_cc
    }

    if (sample_cc == "w_cc" && sample_pre_post_wet == "post"
        && sample_drying_treatment == "four_wk") {
      wk4_w_cc_post <- rbind(wk4_w_cc_post, n_sample_stats[row, ])
      wk4_w_cc_post$drying_treatment <- sample_drying_treatment
      wk4_w_cc_post$pre_post_wet <- sample_pre_post_wet
      wk4_w_cc_post$cc_treatment <- sample_cc
    }

    if (sample_cc == "no_cc" && sample_pre_post_wet == "pre"
        && sample_drying_treatment == "four_wk_cw") {
      wk4_no_cc_cw <- rbind(wk4_no_cc_cw, n_sample_stats[row, ])
      wk4_no_cc_cw$drying_treatment <- sample_drying_treatment
      wk4_no_cc_cw$pre_post_wet <- sample_pre_post_wet
      wk4_no_cc_cw$cc_treatment <- sample_cc
    }

    if (sample_cc == "w_cc" && sample_pre_post_wet == "pre"
        && sample_drying_treatment == "four_wk_cw") {
      wk4_w_cc_cw <- rbind(wk4_w_cc_cw, n_sample_stats[row, ])
      wk4_w_cc_cw$drying_treatment <- sample_drying_treatment
      wk4_w_cc_cw$pre_post_wet <- sample_pre_post_wet
      wk4_w_cc_cw$cc_treatment <- sample_cc
    }
  }

  all_treatments_n <- rbind(
    wk4_no_cc_cw, wk4_no_cc_post, wk4_no_cc_pre, wk4_w_cc_cw, wk4_w_cc_post,
    wk4_w_cc_pre) %>%
    arrange(sample_no)

  return(all_treatments_n)
}
