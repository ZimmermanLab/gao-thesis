# This function takes in C and N data summarized at the treatment level
# and compares changes at one, two, and four weeks between pre and post wet
# samples

# Sarah Gao
# October 27, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("tidyr")

compare_rewetting <- function(treatment_stats, type) {

  # Subset data
  pre_post_diff <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    filter(pre_post_wet == "pre" |
             pre_post_wet == "post") %>%
    filter(drying_treatment == "one_wk" |
             drying_treatment == "two_wk" |
             drying_treatment == "four_wk")

  if (type == "ratio") {
    # Plot C:N ratio
    rewet_ratio_plot <- pre_post_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cn,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                        ymin = mean_mean_cn - sd_mean_cn,
                        group = cc_treatment),
                    position = position_dodge()) +
      theme(axis.title.x=element_blank()) +
      labs(title = "Effect of Rewetting Across Time on C:N Ratios",
           y = "C:N Ratio") +
      coord_cartesian(ylim = c(16, 23)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("pre", "post"),
                       labels = c("Pre-Wet", "Post-Wet"))
      # Save C:N ratio plot
      outlier_type <-deparse(substitute(treatment_stats))
      ggsave(file = paste0("output/2022/ea_plots/03_rewetting/ratio_rewetting_",
                           str_sub(outlier_type, start = 12), ".png"),
             plot = rewet_ratio_plot,
             width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and pre_post_wet
    rewet_ratio_stats <- pre_post_diff %>%
      lm(data = ., mean_mean_cn ~ pre_post_wet * cc_treatment) %>%
      anova()

    # Create list to return multiple things
    return_list <- list("ratio_plot" = rewet_ratio_plot,
                        "ratio_stats" = rewet_ratio_stats)

  } else if (type == "nper") {

    # Plot %N
    rewet_nper_plot <- pre_post_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_nper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_nper + sd_mean_nper,
                        ymin = mean_mean_nper - sd_mean_nper,
                        group = cc_treatment),
                    position = position_dodge()) +
      theme(axis.title.x=element_blank()) +
      labs(title = "Effect of Rewetting Across Time on %N",
           y = "%N") +
      coord_cartesian(ylim = c(0.2, 0.55)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("pre", "post"),
                       labels = c("Pre-Wet", "Post-Wet"))
    # Save %N plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/03_rewetting/nper_rewetting_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = rewet_nper_plot,
           width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and pre_post_wet
    rewet_nper_stats <- pre_post_diff %>%
      lm(data = ., mean_mean_nper ~ pre_post_wet * cc_treatment) %>%
      anova()

    # Create list to return multiple things
    return_list <- list("nper_plot" = rewet_nper_plot,
                        "nper_stats" = rewet_nper_stats)

  } else if (type == "cper") {

    # Plot %C
    rewet_cper_plot <- pre_post_diff %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet)) +
      geom_col(aes(y = mean_mean_cper,
                   fill = cc_treatment),
               position = position_dodge()) +
      geom_errorbar(aes(ymax = mean_mean_cper + sd_mean_cper,
                        ymin = mean_mean_cper - sd_mean_cper,
                        group = cc_treatment),
                    position = position_dodge()) +
      theme(axis.title.x=element_blank()) +
      labs(title = "Effect of Rewetting Across Time on %C",
           y = "%C") +
      coord_cartesian(ylim = c(2.25, 3.25)) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      scale_x_discrete(limits = c("pre", "post"),
                       labels = c("Pre-Wet", "Post-Wet"))
    # Save %C plot
    outlier_type <-deparse(substitute(treatment_stats))
    ggsave(file = paste0("output/2022/ea_plots/03_rewetting/cper_rewetting_",
                         str_sub(outlier_type, start = 12), ".png"),
           plot = rewet_cper_plot,
           width = 15, height = 8, units = "in")

    # Run statistics on effect of cc_treatment and pre_post_wet
    rewet_cper_stats <- pre_post_diff %>%
      lm(data = ., mean_mean_cper ~ pre_post_wet * cc_treatment) %>%
      anova()

    # Create list to return multiple things
    return_list <- list("cper_plot" = rewet_cper_plot,
                        "cper_stats" = rewet_cper_stats)
  }
    return(return_list)
}
