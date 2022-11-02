# This function creates a side by side boxplot of leachate and extract per week
# per N type

# Sarah Gao
# November 1, 2022
# hellosarahgao@gmail.com

library("dplyr")
library("ggplot2")

plot_n_data <- function(n_data_long, y_var, facet_labels) {
  # Set n_type for titles
  if (str_detect(y_var, "nh3")) {
    n_type <- "NH3"
  } else if (str_detect(y_var, "no2")) {
    n_type <- "NO2-NO3"
  }

  # Set facets as leachate and total N
  facet_1 <- sym(paste0("leach_", y_var, "_median"))
  facet_2 <- sym(paste0("ext_", y_var, "_median"))
  levels <- c(facet_1, facet_2)

  wk_initial_plot <- n_data_long %>%
    filter(drying_treatment == "initial") %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (get(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(samp_type,
                        levels = levels),
               labeller = as_labeller(facet_labels))  +
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
    labs(title = paste(n_type, "in Initial Soils Before Drying"),
         y = paste0("mg ", n_type, " / g of Fresh Soil"))
  # Save plot
  ggsave(wk_initial_plot,
         filename = paste0("output/2022/n_plots/", y_var, "_initial.png"),
         width = 10, height = 8, units = "in")

  wk_one_plot <- n_data_long %>%
    filter(drying_treatment == "one_wk") %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (get(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(samp_type,
                        levels = levels),
               labeller = as_labeller(facet_labels))  +
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
    labs(title = paste(n_type,
                       "in Soils Rewet After One Week of Drying"),
         y = paste0("mg ", n_type, " / g of Fresh Soil"))
  # Save plot
  ggsave(wk_one_plot,
         filename = paste0("output/2022/n_plots/", y_var, "_wk_one.png"),
         width = 10, height = 8, units = "in")

  wk_two_plot <- n_data_long %>%
    filter(drying_treatment == "two_wk") %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (get(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(samp_type,
                        levels = levels),
               labeller = as_labeller(facet_labels))  +
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
    labs(title = paste(n_type,
                       "in Soils Rewet After Two Weeks of Drying"),
         y = paste0("mg ", n_type, " / g of Fresh Soil"))
  # Save plot
  ggsave(wk_two_plot,
         filename = paste0("output/2022/n_plots/", y_var, "_wk_two.png"),
         width = 10, height = 8, units = "in")

  wk_four_plot <- n_data_long %>%
    filter(drying_treatment == "four_wk") %>%
    ggplot(aes(x = factor(cc_treatment, levels = c("no_cc", "w_cc")),
               y = (get(y_var)),
               fill = cc_treatment,
               color = cc_treatment)) +
    geom_boxplot() +
    facet_grid(~ factor(samp_type,
                        levels = levels),
               labeller = as_labeller(facet_labels))  +
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
    labs(title = paste(n_type,
                       "in Soils Rewet After Four Weeks of Drying"),
         y = paste0("mg ", n_type, " / g of Fresh Soil"))
  # Save plot
  ggsave(wk_four_plot,
         filename = paste0("output/2022/n_plots/", y_var, "_wk_four.png"),
         width = 10, height = 8, units = "in")

# Combine plots into a list to return
  all_plots <- list(wk_initial_plot, wk_one_plot, wk_two_plot,
                    wk_four_plot)

  return(all_plots)
}
