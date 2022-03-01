# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

library("ggplot2")
library("tidyverse")

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")
n_data_clean <- clean_n_data(
  "data/raw_data/SmartChem_N_extractions/20210212_Gao_1_export.csv")

# Source and run function that compiles all the bad jar assignment
# csvs from Spring 2021
source("code/functions/compile_sample_assignments.R")
all_treatments <- compile_sample_assignments()

# Summarize data (i.e. calculate means and SDs) and map to all_treatments
n_data_stats <- n_data_clean %>%
  mutate(no3 = no2_no3 - no2) %>%
  group_by(sample_no) %>%
  mutate(no3 = no2_no3 - no2) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2 = mean(no2),
            sd_no2 = sd(no2),
            mean_no3 = mean(no3),
            sd_no3 = sd(no3),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3)) %>%
  left_join(all_treatments)

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
                                        color = "#808080"),
             axis.title.y = element_text(size = 8,
                                         face = "bold",
                                         vjust = 3),
             axis.text.y = element_text(size = 6,
                                        color = "#808080"),
             plot.margin = margin(20, 30, 40, 30),
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

# Create a plot comparing NH3 levels between groups with and
# without cover crops at 4 weeks
nh3_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_nh3 = mean(mean_nh3), # Note that "mean_mean" here denotes
            # the mean across all technical *and* experimental replicates
            sd_mean_nh3 = sd(mean_nh3)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))

ggsave(nh3_compare_cc_plot,
         filename = "output/2021/n_plots/nh3_plot.png")

# Run statistical analysis on NH3, excluding cw and no_soil jars
nh3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_nh3 ~ pre_post_wet * cc_treatment) %>%
  anova()

# Create a plot comparing NO2 levels between groups with and
# without cover crops at 4 weeks
no2_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_no2 = mean(mean_no2),
            sd_mean_no2 = sd(mean_no2)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no2,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no2 + sd_mean_no2,
                    ymin = mean_mean_no2 - sd_mean_no2),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = paste("NO2- in Samples With and Without Cover Crop\n",
                     " Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "NO2 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))

ggsave(no2_compare_cc_plot,
       filename = "output/2021/n_plots/no2_plot.png")

# Run statistical analysis on NO2, excluding cw and no_soil jars
no2_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no2 ~ pre_post_wet * cc_treatment) %>%
  anova()

# Create a plot comparing NO3 levels between groups with and
# without cover crops at 4 weeks
no3_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
  group_by(pre_post_wet, cc_treatment) %>%
  summarize(mean_mean_no3 = mean(mean_no3),
            sd_mean_no3 = sd(mean_no3)) %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_no3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymax = mean_mean_no3 + sd_mean_no3,
                    ymin = mean_mean_no3 - sd_mean_no3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("cw", "pre", "post"),
                   labels = c(
                     "Constant Water", "Pre Wetting", "Post Wetting")) +
  labs(title = paste("NO3 in Samples With and Without Cover Crop\n",
                     " Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "NO3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))

ggsave(no3_compare_cc_plot,
       filename = "output/2021/n_plots/no3_plot.png")

# Run statistical analysis on NO3, excluding cw and no_soil jars
no3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no3 ~ pre_post_wet * cc_treatment) %>%
  anova()
