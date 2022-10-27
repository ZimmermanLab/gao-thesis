# This function creates plots from C:N ratio data

# Sarah Gao
# October 26, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")

create_ratio_plot <- function(treatment_stats) {
  ratio_plot <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
    ggplot(aes(x = pre_post_wet)) +
    geom_col(aes(y = mean_mean_cn,
                 fill = cc_treatment),
             position = position_dodge()) +
    geom_errorbar(aes(ymax = mean_mean_cn + sd_mean_cn,
                      ymin = mean_mean_cn - sd_mean_cn,
                      group = cc_treatment),
                  position = position_dodge()) +
    facet_grid(. ~ factor(drying_treatment,
                          levels = c("initial", "one_wk", "two_wk",
                                     "four_wk", "all_dry"))) +
    coord_cartesian(y = c(17, 23)) +
    scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"),
                     labels = c("All-Dry", "Initial", "Constant",
                                "Pre-Wet", "Post-Wet")) +
    labs(title = "C:N ratios of All Treatment Levels",
         x = "C:N Ratio",
         y = "Treatment")

  # Save plot
  outlier_type <-deparse(substitute(treatment_stats))
  ggsave(file = paste0("output/2022/ea_plots/01_all_data//ratio_plot_",
                       str_sub(outlier_type, start = 12), ".png"),
         plot = ratio_plot,
         width = 15, height = 8, units = "in")
  return(ratio_plot)
}

