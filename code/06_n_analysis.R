# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

library("ggplot2")

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
n_data_clean <- clean_n_data(
  "data/raw_data/SmartChem_N_extractions/20210212_Gao_1_export.csv")

# Source and run function to analyze stats for samples
source("code/functions/n_functions/n_calculate_sample_stats.R")
n_sample_stats <- n_calculate_sample_stats(n_data_clean)

# Source and run function that compiles all the bad jar assignment
# csvs from Spring 2021
source("code/functions/compile_sample_assignments.R")
all_treatments <- compile_sample_assignments()

# Source  and run function that organizes and aggregates n results to
# master list of treatments
source("code/functions/map_results.R")
all_treatments_n <- add_results(all_treatments, n_sample_stats)

# Calculate means of each drying x cc treatment group across replicates
n_results_means <- all_treatments_n %>%
  group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
  summarise(mean_nh3 = mean(mean_nh3), mean_no2 = mean(mean_no2),
            mean_no2_no3 = mean(mean_no2_no3))

# Set plot themes
theme_update(plot.title = element_text(face = "bold",
                                       size = 12,
                                       hjust = 0.5,
                                       margin = margin(10, 0, 10, 0),
                                       lineheight = 1.2),
             axis.title.x = element_text(size = 10,
                                         face = "bold",
                                         vjust = -3),
             axis.text.x = element_text(size = 8,
                                        color = "#808080"),
             axis.title.y = element_text(size = 10,
                                         face = "bold",
                                         vjust = 3),
             axis.text.y = element_text(size = 8,
                                        color = "#808080"),
             plot.margin = margin(20, 30, 40, 30),
             legend.position = "right",
             legend.title = element_text(size = 6,
                                         face = "bold",
                                         color = "#808080",
                                         margin = margin(r = 10, unit = "pt")),
             legend.text = element_text(size = 6,
                                        color = "#808080",
                                        margin = margin(r = 10, unit = "pt")),
             legend.background = element_rect(color = "#E7E7E7"),
             legend.margin = margin(5, 5, 5, 5),
             legend.box.spacing = unit(20, "pt"),
             legend.key.size = unit(8, "pt")
)

# Create a plot comparing NO2-NO3 levels between groups with and
# without cover crops at 4 weeks
no2_no3_compare_cc_plot <- n_results_means %>%
  filter(drying_treatment == "four_wk") %>%
  ggplot(aes(x = pre_post_wet, y = mean_no2_no3, fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2-NO3 in Samples With and Without Cover Crop Residue",
       x = "Water Treatments", y = "NO2-NO3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(labels = c("Without cover crop", "With cover crop"))

ggsave(no2_no3_compare_cc_plot,
         filename = "output/2021/n_plots/no2_no3_v_cc.png")

# Create a plot comparing NH3 levels between groups with and
# without cover crops at 4 weeks
nh3_compare_cc_plot <- n_results_means %>%
  filter(drying_treatment == "four_wk") %>%
  ggplot(aes(x = pre_post_wet, y = mean_nh3, fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NH3 in Samples With and Without Cover Crop Residue",
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(labels = c("Without cover crop", "With cover crop"))

ggsave(nh3_compare_cc_plot,
       filename = "output/2021/n_plots/nh3_v_cc.png")

# Create a plot comparing NO2 levels between groups with and
# without cover crops at 4 weeks
no2_compare_cc_plot <- n_results_means %>%
  filter(drying_treatment == "four_wk") %>%
  ggplot(aes(x = pre_post_wet, y = mean_no2, fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = "NO2 in Samples With and Without Cover Crop Residue",
       x = "Water Treatments", y = "NO2 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(labels = c("Without cover crop", "With cover crop"))

ggsave(no2_compare_cc_plot,
       filename = "output/2021/n_plots/no2_v_cc.png")
