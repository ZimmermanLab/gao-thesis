# This function calculates statistics per week for differences between
# w cc and without cc samples in the N type inputted as an argument

# The argument time_type takes in either "per_wk" or "all_wk" depending on
# whether you want to look at one week at a time or a time course across all,
# respectively.

# Sarah Gao
# November 1, 2022

library("dplyr")

wk_stats <- function(n_data, n_type) {
  all_stats <- data.frame("drying_treatment" = as.character(),
                          "leach_stat_p" = as.numeric(),
                          "leach_stat_w" = as.numeric(),
                          "ext_stat_p" = as.numeric(),
                          "ext_stat_w" = as.numeric(),
                          "ratio_stat_p" = as.numeric(),
                          "ratio_stat_w" = as.numeric())
  all_sum <- data.frame("drying_treatment" = as.character(),
                        "cc_treatment" = as.character(),
                        "leach_med" = as.numeric(),
                        "leach_iqr" = as.numeric(),
                        "ext_med" = as.numeric(),
                        "ext_iqr" = as.numeric(),
                        "ratio_med" = as.numeric(),
                        "ratio_iqr" = as.numeric())
  wk_list <- list("initial", "one_wk", "two_wk", "four_wk")

  # Set y variables depending on N species passed as argument
  y_var_leach <- paste0("leach_", n_type, "_per_median")
  y_var_ext <- paste0("ext_", n_type, "_per_median")
  y_var_ratio <- paste0("ratio_", n_type)

  for (a in wk_list) {
    # Run Wilcox test for each week
    wk_subset <- n_data %>%
      filter(drying_treatment == a)
    wk_stat_leach <- wk_subset %>%
      wilcox.test(data = ., get(y_var_leach) ~ cc_treatment)
    wk_stat_ext <- wk_subset %>%
      wilcox.test(data = ., get(y_var_ext) ~ cc_treatment)
    wk_stat_ratio <- wk_subset %>%
      wilcox.test(data = ., get(y_var_ratio) ~ cc_treatment)
    new_stats <- data.frame("drying_treatment" = a,
                            "leach_stat_p" = wk_stat_leach$p.value,
                            "leach_stat_w" = wk_stat_leach$statistic,
                            "ext_stat_p" = wk_stat_ext$p.value,
                            "ext_stat_w" = wk_stat_ext$statistic,
                            "ratio_stat_p" = wk_stat_ratio$p.value,
                            "ratio_stat_w" = wk_stat_ratio$statistic)
    all_stats <- rbind(all_stats, new_stats)

    # Create summary of medians and IQRs for each week
    wk_sum <- wk_subset %>%
      group_by(cc_treatment) %>%
      summarise(leach_med = median(get(y_var_leach)),
                leach_iqr = IQR(get(y_var_leach), na.rm = TRUE),
                ext_med = median(get(y_var_ext)),
                ext_iqr = IQR(get(y_var_ext), na.rm = TRUE))
    new_sum <- wk_sum %>%
      mutate(drying_treatment = a) %>%
      relocate(drying_treatment)
    all_sum <- rbind(all_sum, new_sum)
  }
  # Combine into one big list
  return_list <- list("all_sum" = all_sum,
                      "all_stats" = all_stats)
  return(return_list)
}

