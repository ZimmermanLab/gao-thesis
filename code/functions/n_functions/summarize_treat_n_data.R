# This function analyzes N data along drying
# treatments (ie over the course of the initial, one week, two
# week, and four week experiment) and produces a summary with medians and IQRs.

# Sarah Gao
# October 31, 2022
# hellosarahgao@gmail.com

library("ggplot2")
library("dplyr")

sum_plot_n <- function(n_data, y_var, n_type) {
  # Create facet labels
  facet_drying_labels <- as_labeller(c("one_wk" = "One Week",
                                       "two_wk" = "Two Weeks",
                                       "four_wk" = "Four Weeks",
                                       "initial" = "Initial"))
  var_name <- deparse(substitute(y_var))
  if (str_detect(var_name, "leach")) {
    title = paste("Leachate", toupper(n_type), "Nitrogen in Post-Wet Soils")
    y_title = paste0("\u03bcg ", toupper(n_type), " / g of Fresh Soil")
    var_name <- str_sub(var_name, end = -6)
  } else if (str_detect(var_name, "ext")) {
    title = paste("Total", toupper(n_type), "in Post-Wet Soils")
    y_title = paste0("\u03bcg ", toupper(n_type), " / g of Fresh Soil")
    var_name <- str_sub(var_name, end = -6)
  } else if (str_detect(var_name, "ratio")) {
    title = paste("Ratio of Leachate to Total", toupper(n_type),
                  "in Post-Wet Soils")
    y_title = paste("Leachate : Total",
                    toupper(n_type))
    }

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
    labs(y = y_title,
         title = title)

  ggsave(n_plot, filename = paste0("output/2022/n_plots/", y_var, ".png"),
         width = 14, height = 8, units = "in")

# Create median and IQR summary of data
treat_sum <- n_data %>%
  group_by(cc_treatment, drying_treatment) %>%
  summarize(med = median(!! sym(y_var), na.rm = TRUE),
            iqr = IQR(!! sym(y_var), na.rm = TRUE)) %>%
  arrange(factor(drying_treatment,
                 levels = c("initial", "one_wk", "two_wk", "four_wk")))

# Compile results into a list
return_list <- list("treat_sum" = treat_sum,
                 "plot" = n_plot)

  return(return_list)
}
