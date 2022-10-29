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
      filter(cc_treatment == "w_cc",
               pre_post_wet == "pre" |
               pre_post_wet == "post")

  } else if (cc_type == "no_cc") {
    rewet_subset <- treatment_mapped %>%
      # Select only pre-wet and post-wet data at 1, 2, and 4 weeks
      filter(cc_treatment == "no_cc",
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

  # Set plot themes
  source("code/functions/set_plot_themes.R")
  set_theme()

  # Take name of input
  input_name <- deparse(substitute(treatment_mapped))
  # Get "bacterial" or "fungal" from input name
  micro_type = ifelse(
    str_detect(input_name, "bact") == TRUE, "Bacterial", "Fungal")
  # Get title descriptors from argument
  cc_title = ifelse(cc_type == "w_cc", "With Cover Crop", "Without Cover Crop")

  # Create a boxpot of DNA quantities pre/post rewetting at each time point
  rewet_plot <- rewet_subset %>%
    ggplot(aes(x = drying_treatment,
               y = prop_conc_norm,
               # Reorder to have pre first then post
               fill = factor(pre_post_wet, level = c("pre", "post")))) +
    geom_boxplot() +
    # scale_fill_discrete(breaks= c("pre", "post")) +
    scale_x_discrete(limits = c("one_wk", "two_wk", "four_wk"),
                     labels = c("One Week", "Two Weeks", "Four Weeks")) +
    scale_fill_manual(name = NULL, limits = c("pre", "post"),
                      values = c("#16B4FF", "#34980D"),
                      labels = c("Pre-Wet", "Post-Wet")) +
    scale_color_manual(name = NULL, values = c("#097CB2", "#195004"),
                       labels = c("Pre-Wet", "Post-Wet")) +
    labs(x = "Drying Time",
         y = "Proportional Concentration",
         title = paste(micro_type, "DNA Changes From Rewetting\n", cc_title))

  # Save out the plot
  ggsave(file = paste0("output/2022/qpcr_plots/",
                       str_sub(input_name, end = 4), "_rewet_",
                       cc_type, ".png"),
         plot = rewet_plot,
         width = 10, height = 8, units = "in")

  # Create a list to return both stats and plot
  return_list <- list("stats" = rewet_stats, "plot" = rewet_plot,
                      "median" = rewet_med)
  return(return_list)
}
