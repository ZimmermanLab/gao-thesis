# This function plots fungal:bacterial ratios

# Sarah Gao
# October 25, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

plot_dna <- function(treatment_stats, type) {

  if(type == "ratio") {
    bar_plot <- treatment_stats %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet,
                 y = mean_ratio,
                 fill = cc_treatment)) +
      geom_col(position = position_dodge()) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      geom_errorbar(aes(ymax = mean_ratio + sd_ratio,
                        ymin = mean_ratio - sd_ratio),
                    size = 0.25,
                    width = 0.2,
                    position = position_dodge(0.9)) +
        coord_cartesian(ylim = c(-0.07, 0.20)) +
      scale_x_discrete(limits = c("all_dry", "initial", "cw", "pre", "post"),
                       labels = c("All-Dry", "Initial", "Constant", "Pre-Wet",
                                  "Post-Wet"))
  } else if(type == "bacterial") {
    bar_plot <- treatment_stats %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet,
                 y = mean_mean_bacterial,
                 fill = cc_treatment)) +
        geom_col(position = position_dodge()) +
        facet_grid(. ~ factor(drying_treatment,
                              levels = c("initial", "one_wk", "two_wk",
                                         "four_wk", "all_dry"))) +
        geom_errorbar(aes(ymax = mean_mean_bacterial + sd_mean_bacterial,
                          ymin = mean_mean_bacterial - sd_mean_bacterial),
                      size = 0.25,
                      width = 0.2,
                      position = position_dodge(0.9)) +
        scale_x_discrete(limits = c("all_dry", "initial", "cw", "pre", "post"),
                         labels = c("All-Dry", "Initial", "Constant", "Pre-Wet",
                                    "Post-Wet"))
  } else if(type == "fungal") {
    bar_plot <- treatment_stats %>%
      group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
      ggplot(aes(x = pre_post_wet,
                 y = mean_mean_fungal,
                 fill = cc_treatment)) +
      geom_col(position = position_dodge()) +
      facet_grid(. ~ factor(drying_treatment,
                            levels = c("initial", "one_wk", "two_wk",
                                       "four_wk", "all_dry"))) +
      geom_errorbar(aes(ymax = mean_mean_fungal + sd_mean_fungal,
                        ymin = mean_mean_fungal - sd_mean_fungal),
                    size = 0.25,
                    width = 0.2,
                    position = position_dodge(0.9)) +
      scale_x_discrete(limits = c("all_dry", "initial", "cw", "pre", "post"),
                       labels = c("All-Dry", "Initial", "Constant", "Pre-Wet",
                                  "Post-Wet"))
  } else {
    print("Please enter the type of data you want to plot: ratio, bacterial, or fungal.")
  }
return(bar_plot)
}
