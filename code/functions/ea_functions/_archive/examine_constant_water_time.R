# This function takes in C:N ratio data summarized at the treatment level
# and looks at changes over time in constantly watered samples.

# Sarah Gao
# October 27, 2022
# hellosarahgao@gmail.com

library("tidyr")
library("dplyr")

examine_cw_time <- function(treatment_stats, type) {

  # Subset data
  constant_time <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    filter(pre_post_wet == "initial" |
             pre_post_wet == "cw") %>%
    filter(drying_treatment == "initial" |
             drying_treatment == "one_wk" |
             drying_treatment == "two_wk" |
             drying_treatment == "four_wk")

  # It type == "ratio", create ratio plot
  if(type == "ratio") {
    constant_time_ratio_plot <- constant_time  %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cn,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                        ymin = mean_mean_cn - sd_mean_cn,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "C:N Ratios of Constantly Watered Samples Over Time") +
      coord_cartesian(ylim = c(16, 23)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry")))
  # Save ratio plot
  outlier_type <-deparse(substitute(treatment_stats))
  ggsave(file = paste0("output/2022/ea_plots/02_constant_water/ratio_cw_",
                       str_sub(outlier_type, start = 12), ".png"),
         plot = constant_time_ratio_plot,
    width = 15, height = 8, units = "in")

  # Run ANOVA statistics on C:N ratio to test effect of time
  constant_time_ratio_stats <- constant_time %>%
    select(-c(pre_post_wet)) %>%
    filter(cc_treatment == "no_cc") %>%
    lm(data = ., mean_mean_cn ~ drying_treatment) %>%
    anova()

  # Create list to return multiple things
  return_list <- list(
    "ratio_plot" = constant_time_ratio_plot,
    "ratio_stats" = constant_time_ratio_stats)

  } else if(type == "nper") {
    # Create %N plot
    constant_time_nper_plot <- constant_time  %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_nper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_nper + sd_mean_nper,
                        ymin = mean_mean_nper - sd_mean_nper,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "%N of Constantly Watered Samples Over Time") +
      coord_cartesian(ylim = c(0.2, 0.55)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry")))
    # Save %N plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/02_constant_water/cw_nper_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = constant_time_nper_plot,
           width = 15, height = 8, units = "in")

    # Run ANOVA statistics on %N to test effect of time
    constant_time_nper_stats <- constant_time %>%
      lm(data = ., mean_mean_nper ~ drying_treatment) %>%
      anova()

    # Create list to return multiple things
    return_list <- list(
      "nper_plot" = constant_time_nper_plot,
      "nper_stats" = constant_time_nper_stats)

    } else if (type == "cper") {
    # Create %C plot
    constant_time_cper_plot <- constant_time  %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cper + sd_mean_cper,
                        ymin = mean_mean_cper - sd_mean_cper,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "%C of Constantly Watered Samples Over Time") +
      coord_cartesian(ylim = c(2.25, 3.25)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry")))
    # Save %C plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/02_constant_water/cw_cper_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = constant_time_cper_plot,
           width = 15, height = 8, units = "in")

    # Run ANOVA statistics to test effect of time
    constant_time_cper_stats <- constant_time %>%
      lm(data = ., mean_mean_cper ~ drying_treatment) %>%
      anova()

    # Create list to return multiple things
    return_list <- list(
      "cper_plot" = constant_time_cper_plot,
      "cper_stats" = constant_time_cper_stats)
}

  return(return_list)
}
