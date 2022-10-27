# This function creates plots from C and N percentage data

# Sarah Gao
# October 27, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")

create_per_plot <- function(treatment_stats) {

  # Create N plot
  n_per_plot <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
    ggplot(aes(x = pre_post_wet)) +
    geom_col(aes(y = mean_mean_nper,
                 fill = cc_treatment),
             position = position_dodge()) +
    geom_errorbar(aes(ymax = mean_mean_nper + sd_mean_nper,
                      ymin = mean_mean_nper - sd_mean_nper,
                      group = cc_treatment),
                  position = position_dodge()) +
    facet_grid(. ~ factor(drying_treatment,
                          levels = c("initial", "one_wk", "two_wk",
                                     "four_wk", "all_dry"))) +
    coord_cartesian(y = c(0.2, 0.55)) +
    scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"),
                     labels = c("All-Dry", "Initial", "Constant",
                                "Pre-Wet", "Post-Wet")) +
    labs(title = "%N of All Treatment Levels",
         x = "Treatment",
         y = "%N")
  # Save N plot
  outlier_type <-deparse(substitute(treatment_stats))
  ggsave(file = paste0("output/2022/ea_plots/01_all_data/nper_plot_",
                       str_sub(outlier_type, start = 12), ".png"),
         plot = n_per_plot,
         width = 15, height = 8, units = "in")

  # Create C plot
  c_per_plot <- treatment_stats %>%
    filter(pre_post_wet != "no_soil") %>%
    group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
    ggplot(aes(x = pre_post_wet)) +
    geom_col(aes(y = mean_mean_cper,
                 fill = cc_treatment),
             position = position_dodge()) +
    geom_errorbar(aes(ymax = mean_mean_cper + sd_mean_cper,
                      ymin = mean_mean_cper - sd_mean_cper,
                      group = cc_treatment),
                  position = position_dodge()) +
    facet_grid(. ~ factor(drying_treatment,
                          levels = c("initial", "one_wk", "two_wk",
                                     "four_wk", "all_dry"))) +
    coord_cartesian(y = c(2.2, 3.2)) +
    scale_x_discrete(limits = c("all_dry","initial", "cw", "pre", "post"),
                     labels = c("All-Dry", "Initial", "Constant",
                                "Pre-Wet", "Post-Wet")) +
    labs(title = "%C of All Treatment Levels",
         x = "Treatment",
         y = "%C")
  # Save C plot
  outlier_type <-deparse(substitute(treatment_stats))
  ggsave(file = paste0("output/2022/ea_plots/01_all_data/cper_plot_",
                       str_sub(outlier_type, start = 12), ".png"),
         plot = c_per_plot,
         width = 15, height = 8, units = "in")

  # Create list to return multiple plots
  return_list <- list(
    "n_plot" = n_per_plot, "c_plot" = c_per_plot)
  return(return_list)
}
