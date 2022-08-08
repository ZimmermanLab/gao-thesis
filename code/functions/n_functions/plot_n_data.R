# This function plots the cleaned N data from the SmartChem.

# Sarah Gao
# July 7th, 2022
# hellosarahgao@gmail.com

library("ggplot2")
library("dplyr")

plot_n_data <- function(input_n_stats, n_species, poster_option) {

  # Set plot themes
  theme_update(plot.title = element_text(face = "bold",
                                         size = 12,
                                         hjust = 0.5,
                                         margin = margin(10, 0, 10, 0),
                                         lineheight = 1.2),
               axis.title.x = element_text(size = 8,
                                           face = "bold",
                                           vjust = -3),
               axis.text.x = element_text(size = 6,
                                          color = "#808080",
                                          angle=60, hjust=1) ,
               axis.title.y = element_text(size = 8,
                                           face = "bold",
                                           vjust = 3),
               axis.text.y = element_text(size = 6,
                                          color = "#808080"),
               # These margins are for standalone figures
               # plot.margin = margin(20, 30, 40, 30),
               # These margins are for poster assets
               plot.margin = margin(0, 0, 10, 5),
               legend.position = "right",
               legend.title = element_text(size = 6,
                                           face = "bold",
                                           color = "#808080",
                                           margin = margin(r = 10, unit = "pt")),
               legend.text = element_text(size = 6,
                                          color = "#808080",
                                          margin = margin(r = 0, unit = "pt")),
               legend.background = element_rect(color = "#E7E7E7"),
               legend.margin = margin(5, 5, 5, 5),
               legend.box.spacing = unit(20, "pt"),
               legend.key.size = unit(8, "pt")
  )

  # Create a list of drying_treatent labels and sample_type labels
  drying_labels <- c("One Week", "Two Weeks", "Four Weeks", "Always Dry")
  type_labels <- c("Extracts", "Leachates")
  names(drying_labels) <- c("one_wk", "two_wk", "four_wk", "all_dry")
  names(type_labels) <- c("extract", "leachate")

  # Create a plot comparing n_species levels across drying_treatment in both
  # extracts and simulated leachate
  n_plot_grid <- input_n_stats %>%
    filter(pre_post_wet != "no_soil", sample_no != "NA") %>%
    group_by(pre_post_wet, cc_treatment, drying_treatment, sample_type) %>%
    summarize("mean_all" = mean(
      get(paste0("mean_", n_species))),
      # This shows the mean across all technical *and* experimental replicates
      "mean_sd" = sd(get(paste0("mean_", n_species)))) %>%
    ggplot(aes(x = pre_post_wet,
               y = mean_all,
               fill = cc_treatment)) +
    geom_col(position = position_dodge()) +
    facet_grid(sample_type ~ factor(drying_treatment, levels = c(
      "one_wk", "two_wk", "four_wk", "all_dry")),
      labeller = labeller(sample_type = type_labels,
                          drying_treatment = drying_labels)) +
    geom_errorbar(aes(ymax = mean_all + mean_sd,
                      ymin = mean_all - mean_sd),
                  size = 0.25,
                  width = 0.2,
                  position = position_dodge(0.9)) +
    scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                     labels = c("All Dry", "Constant Water", "Pre Wetting",
                                "Post Wetting")) +
      coord_cartesian(ylim=c(0, 8)) +
    labs(title = paste(toupper(n_species), "in Samples With and Without Cover Crop\n",
                       " Residue in Soil Extracts"),
         x = "Water Treatments", y = paste(n_species, " (ppm)",
         fill = "Cover Crop Treatment") +
    scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                        labels = c("Without cover crop", "With cover crop")))
  ggsave(n_plot_grid,
         filename = paste0("output/2022/n_plots/", n_species, "_plot.png"))

  # These additions are for creating poster assets
  if (poster_option == "y") {
    n_plot_grid + scale_fill_manual(values = c("no_cc" = "#00C2FF",
                                               "w_cc" = "#3EA50D")) +
      theme(legend.position = "none")
    ggsave(n_plot_grid, filename = paste0(
      "output/2022/n_plots/", n_species, "_plot_772.png",
      width = 772*2, height = 590*2, units = "px", dpi = 300))
  }
  return(n_plot_grid)
}

