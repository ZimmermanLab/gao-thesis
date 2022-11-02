# This function calculates statistics per week for differences between
# w cc and without cc samples in the N type inputted as an argument

# The argument time_type takes in either "per_wk" or "all_wk" depending on
# whether you want to look at one week at a time or a time course across all,
# respectively.

# Sarah Gao
# November 1, 2022

library("dplyr")

wk_stats <- function(n_data, y_var, time_type) {
  wk_list <- list("initial", "one_wk", "two_wk", "four_wk")

  if (time_type == "per_wk") {
    all_stats <- data.frame("drying_treatment" = as.character(),
                            "leach_stat_p" = as.numeric(),
                            "leach_stat_chi" = as.character(),
                            "ext_stat_p" = as.numeric(),
                            "ext_stat_chi" = as.character())
    all_sum <- data.frame("drying_treatment" = as.character(),
                          "cc_treatment" = as.character(),
                          "leach_med" = as.numeric(),
                          "leach_iqr" = as.numeric(),
                          "ext_med" = as.numeric(),
                          "ext_iqr" = as.numeric())
    for (a in wk_list) {
      # Initial subset
      wk_sub <- n_data %>%
        filter(drying_treatment == a) %>%
        pivot_wider(names_from = samp_type, values_from = all_of(y_var))
      # Initial stats
      y_var_leach <- paste0("leach_", y_var, "_median")
      y_var_ext <- paste0("ext_", y_var, "_median")
      wk_stat_leach <- wk_sub %>%
        kruskal.test(data = ., get(y_var_leach) ~ cc_treatment)
      wk_stat_ext <- wk_sub %>%
        kruskal.test(data = ., get(y_var_ext) ~ cc_treatment)

      new_stats <- data_frame("drying_treatment" = a,
                              "leach_stat_p" = wk_stat_leach$p.value,
                              "leach_stat_chi" = wk_stat_leach$statistic,
                              "ext_stat_p" = wk_stat_ext$p.value,
                              "ext_stat_chi" = wk_stat_ext$statistic)
      all_stats <- rbind(all_stats, new_stats)

      # Initial summary
      wk_sum <- wk_sub %>%
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

  } else if (time_type == "all_wk") {
      initial <- n_data %>%
        filter(drying_treatment == "initial") %>%
        kruskal.test(data = ., get(y_var) ~ cc_treatment)
      one <- n_data %>%
        filter(drying_treatment == "one_wk") %>%
        kruskal.test(data = ., get(y_var) ~ cc_treatment)
      two <- n_data %>%
        filter(drying_treatment == "two_wk") %>%
        kruskal.test(data = ., get(y_var) ~ cc_treatment)
      four <- n_data %>%
        filter(drying_treatment == "four_wk") %>%
        kruskal.test(data = ., get(y_var) ~ cc_treatment)

      return_list <- data.frame("drying_treatment" = c("initial", "one_wk", "two_wk",
                                                  "four_wk"),
                           "chi_sq" = c(initial$statistic,
                                        one$statistic,
                                        two$statistic,
                                        four$statistic),
                           "df" = c(initial$parameter,
                                    one$parameter,
                                    two$parameter,
                                    four$parameter),
                           "p_value" = c(initial$p.value,
                                         one$p.value,
                                         two$p.value,
                                         four$p.value))
  }
  return(return_list)
}
