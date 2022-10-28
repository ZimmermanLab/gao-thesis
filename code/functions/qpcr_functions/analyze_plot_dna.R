# This function examines the effects of rewetting on proportional DNA
# concentrations from qPCR analyses by running a Kruskal-Wallis test. It also
# creates a boxplot from this data.

# Sarah Gao
# October 27, 2022
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")

analyze_plot_dna <- function(treatment_mapped, cc_type) {
  if (cc_type == "w_cc") {
    rewet_subset <- treatment_mapped %>%
      # Select only pre-wet and post-wet data at 1, 2, and 4 weeks
      filter(cc_treatment == "w_cc" &&
               pre_post_wet == "pre" |
               pre_post_wet == "post")

  } else if (cc_type == "no_cc") {
    rewet_subset <- treatment_mapped %>%
      # Select only pre-wet and post-wet data at 1, 2, and 4 weeks
      filter(cc_treatment == "no_cc" &&
               pre_post_wet == "pre" |
               pre_post_wet == "post")
  }

  rewet_med <- rewet_subset %>%
    group_by(drying_treatment, pre_post_wet) %>%
    summarize(median = median(prop_conc_norm),
            iqr = IQR(prop_conc_norm))

  # Use Kruskal-Wallis test to see effect of rewetting
  rewet_stats <- kruskal.test(
    prop_conc_norm ~ pre_post_wet, data = rewet_subset)

  # Reorder pre/post wet for ggplot
  rewet_subset$pre_post_wet <- factor(rewet_subset$pre_post_wet,
                                           levels = c("pre", "post"),
                                           ordered = T)

  # Create a boxpot of DNA quantities pre/post rewetting at each time point
  rewet_plot <- rewet_subset %>%
    ggplot(aes(x = drying_treatment,
               y = prop_conc_norm,
               fill = pre_post_wet)) +
    geom_boxplot() +
    scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"))

  # Take name of input
  outlier_type <-deparse(substitute(treatment_mapped))
  # Save out the plot
  ggsave(file = paste0("output/2022/qpcr_plots/",
                       str_sub(outlier_type, end = 4), "_rewet_",
                       cc_type, ".png"),
         plot = rewet_plot,
         width = 15, height = 8, units = "in")

  # Create a list to return both stats and plot
  return_list <- list("stats" = rewet_stats, "plot" = rewet_plot,
                      "median" = rewet_med)
  return(return_list)
}
