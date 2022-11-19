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
    cc_title = "With Cover Crop"

  } else if (cc_type == "no_cc") {
    rewet_subset <- treatment_mapped %>%
      # Select only pre-wet and post-wet data at 1, 2, and 4 weeks
      filter(cc_treatment == "no_cc",
               pre_post_wet == "pre" |
               pre_post_wet == "post")
    cc_title = "Without Cover Crop"

  } else if (cc_type == "all") {
      rewet_subset <- treatment_mapped %>%
        filter(pre_post_wet == "pre" |
                 pre_post_wet == "post")
      cc_title = "In All Samples"
  }

  # Take name of input
  input_name <- deparse(substitute(treatment_mapped))

  # Determine microbial type
  if (str_detect(input_name, "bact")) {
    micro_type = "conc_norm_bact"
    micro_title = "Bacterial"
  } else if (str_detect(input_name, "fung"))  {
    micro_type = "conc_norm_fung"
    micro_title = "Fungal"
  }

  # Find medians per sample first across all tech reps
  samp_sum <- rewet_subset %>%
    group_by(sample_no, drying_treatment, pre_post_wet) %>%
    summarize(samp_median = median(get(micro_type)))
  # Find medians + IQRs across all treatment levels
  treat_sum <- samp_sum %>%
    group_by(drying_treatment, pre_post_wet) %>%
    # Find medians per treatment level
    summarize(treat_median = median(samp_median),
            treat_iqr = IQR(samp_median)) %>%
    arrange(drying_treatment == "one_wk")

  # Use Kruskal-Wallis test to see effect of rewetting
  stats_rewet <- kruskal.test(samp_median ~ pre_post_wet,
                              data = samp_sum)
  # Use Kruskal-Wallis test to see effect of drying in pre only
  stats_drying <- samp_sum %>%
    filter(pre_post_wet == "pre") %>%
    kruskal.test(samp_median ~ drying_treatment,
                              data = .,)

  # Create a boxplot of DNA quantities pre/post rewetting at each time point
  # Set facet labels
  facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                       "two_wk" = "Two Weeks",
                                       "four_wk" = "Four Weeks"))
  rewet_plot <- samp_sum %>%
    ggplot(aes(x = factor(pre_post_wet, levels = c("pre", "post")),
               y = samp_median)) +
    # Reorder to have pre first then post
    geom_boxplot(aes(fill = pre_post_wet,
      color = pre_post_wet)) +
    facet_wrap(~ factor(drying_treatment,
                        levels = c("one_wk", "two_wk", "four_wk")),
               labeller = facet_drying_labels)  +
    scale_x_discrete(labels = c("Pre-Wet", "Post-Wet")) +
    scale_fill_manual(name = NULL, limits = c("pre", "post"),
                      values = c("#16B4FF", "#34980D"),
                      labels = c("Pre-Wet", "Post-Wet")) +
    scale_color_manual(name = NULL, limits = c("pre", "post"),
                       values = c("#097CB2", "#195004"),
                       labels = c("Pre-Wet", "Post-Wet")) +
    theme(legend.position = "none",
          strip.text = element_text(size = 12)) +
    labs(x = element_blank(),
         y = "Proportional Concentration",
         title = paste(micro_title, "DNA Changes From Rewetting\n", cc_title))

  # Save out the plot
  ggsave(file = paste0("output/2022/qpcr_plots/",
                       # Account for microbial and outlier type in file name
                       str_sub(input_name, end = 4), "_rewet_",
                       cc_type, "_", str_sub(input_name, start = 16),".png"),
         plot = rewet_plot,
         width = 10, height = 8, units = "in")

  # Create a list to return both stats and plot
  return_list <- list("rewet_stats" = stats_rewet,
                      "drying_stats" = stats_drying,
                      "plot" = rewet_plot,
                      "median" = treat_sum)
  return(return_list)
}
