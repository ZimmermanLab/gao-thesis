# This function runs the Kruskal-Wallis test comparing the effects of rewetting on
# bacterial DNA quantities at each separate time point

# Sarah Gao
# October 29, 2022
# hellosarahgao@gmail.com

compare_rewet_time <- function(treatment_data, cc_type) {
  if(cc_type != "all") {
    one_wk <- treatment_data %>%
      filter(cc_treatment == cc_type,
             drying_treatment == "one_wk",
             pre_post_wet != "cw")
    one_wk_stats <- one_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
    two_wk <- treatment_data %>%
      filter(cc_treatment == cc_type,
             drying_treatment == "two_wk",
             pre_post_wet != "cw")
    two_wk_stats <- two_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
    four_wk <- treatment_data %>%
      filter(cc_treatment == cc_type,
             drying_treatment == "four_wk",
             pre_post_wet != "cw")
    four_wk_stats <- four_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
    }
  if(cc_type == "all") {
    one_wk <- treatment_data %>%
      filter(drying_treatment == "one_wk",
             pre_post_wet != "cw")
    one_wk_stats <- one_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
    two_wk <- treatment_data %>%
      filter(drying_treatment == "two_wk",
             pre_post_wet != "cw")
    two_wk_stats <- two_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
    four_wk <- treatment_data %>%
      filter(drying_treatment == "four_wk",
             pre_post_wet != "cw")
    four_wk_stats <- four_wk %>%
      kruskal.test(data = ., prop_conc_norm ~ pre_post_wet)
  }

  all_time_stats <- data.frame("drying_treatment" = c("one_wk",
                                                      "two_wk",
                                                      "four_wk"),
                               "chi_sq" = c(one_wk_stats$statistic,
                                             two_wk_stats$statistic,
                                             four_wk_stats$statistic),
                               "df" = c(one_wk_stats$parameter,
                                        two_wk_stats$parameter,
                                        four_wk_stats$parameter),
                               "p_value" = c(one_wk_stats$p.value,
                                             two_wk_stats$p.value,
                                             four_wk_stats$p.value))
  return(all_time_stats)
}
