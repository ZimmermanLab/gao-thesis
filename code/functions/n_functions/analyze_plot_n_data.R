# This function analyzes and plots N data along drying
# treatments (ie over the course of the one week, two
# week, and four week experiment)

# Sarah Gao
# October 31, 2022
# hellosarahgao@gmail.com

library("ggplot2")
library("dplyr")

analyze_plot_n <- function(n_data, y_var, n_type) {
  # Create facet labels
  facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                       "two_wk" = "Two Weeks",
                                       "four_wk" = "Four Weeks",
                                       "initial" = "Initial"))
  var_name <- deparse(substitute(y_var))
  if (str_detect(var_name, "leach")) {
    title_samp_type = "Leachate"
  } else if (str_detect(var_name, "ext")) {
    title_samp_type = "Total N Extract"
  }
  # Plot data
  n_plot <- n_data %>%
    filter(!is.na(!! sym(y_var))) %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (!! sym(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(drying_treatment,
                        levels = c("initial", "one_wk", "two_wk", "four_wk")),
               labeller = facet_drying_labels)  +
    scale_fill_manual(name = NULL, limits = c("no_cc", "w_cc"),
                      values = c("#16B4FF", "#34980D"),
                      labels = c("No Cover Crop", "With Cover Crop")) +
    scale_color_manual(name = NULL, limits = c("no_cc", "w_cc"),
                       values = c("#097CB2", "#195004"),
                       labels = c("No Cover Crop", "With Cover Crop")) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom") +
    labs(y = paste(toupper(n_type), "Nitrogen (mg)"),
         title = paste(toupper(n_type), "Nitrogen Per Gram of Fresh Soil in",
                       title_samp_type))

  ggsave(n_plot, filename = paste0("output/2022/n_plots/", input_name, ".png"),
         width = 14, height = 8, units = "in")

  return(n_plot)
}
