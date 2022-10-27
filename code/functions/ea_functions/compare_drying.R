# This function takes in C:N ratio data summarized at the treatment level
# and compares changes from drying soil across time

# Sarah Gao
# October 27, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("tidyr")

compare_drying <- function(treatment_stats, type) {

  # Subset data
  drying_diff <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    filter(pre_post_wet == "initial" |
             pre_post_wet == "pre" |
             pre_post_wet == "all_dry")

  if (type == "ratio") {

    # Plot C:N ratio
    drying_ratio_plot <- drying_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cn,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                        ymin = mean_mean_cn - sd_mean_cn,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "Effect of Drying Over Time on C:N Ratios") +
      coord_cartesian(ylim = c(16, 23)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial","one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("initial", "pre", "all_dry"),
                       labels = c("Initial", "Pre-Wet", "All-Dry"))
    # Save C:N ratio plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/04_drying/ratio_drying_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = drying_ratio_plot,
           width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and time
    drying_ratio_stats <- drying_diff %>%
      lm(data = ., mean_mean_cn ~ cc_treatment) %>%
      anova() %>%
      rbind(drying_diff %>%
              lm(data = ., mean_mean_cn ~ drying_treatment) %>%
              anova())

    # Create list to return multiple things
    return_list <- list("ratio_plot" = drying_ratio_plot,
                        "ratio_stats" = drying_ratio_stats)

  } else if (type == "nper") {

    # Plot %N
    drying_nper_plot <- drying_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_nper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_nper + sd_mean_nper,
                        ymin = mean_mean_nper - sd_mean_nper,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "Effect of Drying Over Time on %N") +
      coord_cartesian(ylim = c(0.2, 0.55)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial","one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("initial", "pre", "all_dry"),
                       labels = c("Initial", "Pre-Wet", "All-Dry"))
    # Save %N plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/04_drying/nper_drying_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = drying_nper_plot,
           width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and time
    drying_nper_stats <- drying_diff %>%
      lm(data = ., mean_mean_nper ~ cc_treatment) %>%
      anova() %>%
      rbind(drying_diff %>%
              lm(data = ., mean_mean_nper ~ drying_treatment) %>%
              anova())

    # Create list to return multiple things
    return_list <- list("nper_plot" = drying_nper_plot,
                        "nper_stats" = drying_nper_stats)

  } else if (type == "cper") {

    # Plot %C
    drying_cper_plot <- drying_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cper + sd_mean_cper,
                        ymin = mean_mean_cper - sd_mean_cper,
                        group = cc_treatment),
                    position = position_dodge()) +
      labs(title = "Effect of Drying Over Time on %C") +
      coord_cartesian(ylim = c(2.25, 3.25)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial","one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("initial", "pre", "all_dry"),
                       labels = c("Initial", "Pre-Wet", "All-Dry"))
    # Save %C plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/04_drying/cper_drying_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = drying_cper_plot,
           width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and time
    drying_cper_stats <- drying_diff %>%
      lm(data = ., mean_mean_cper ~ cc_treatment) %>%
      anova() %>%
      rbind(drying_diff %>%
              lm(data = ., mean_mean_cper ~ drying_treatment) %>%
              anova())

    # Create list to return multiple things
    return_list <- list("cper_plot" = drying_cper_plot,
                        "cper_stats" = drying_cper_stats)
    }
  return(return_list)
}
