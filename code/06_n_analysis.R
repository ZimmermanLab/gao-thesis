# This script organizes and analyzes results from the SmartChem measurements
# of nitrite, nitrate, and ammonia.

# Sarah Gao
# December 2, 2021
# hellosarahgao@gmail.com

library("ggplot2")
library("tidyverse")

# Source and run function to clean N data
source("code/functions/n_functions/n_clean_n_data.R")

# Get file list
file_list <- paste0("data/raw_data/SmartChem_N_extractions/2022_samples/",
                    list.files(
                      "data/raw_data/SmartChem_N_extractions/2022_samples/",
                        pattern = "*.Csv"))

# Read all csv files in the folder and create a compiled dataframe
n_data_clean <- clean_n_data(file_list) %>%
  relocate(sample_no, rep_no)

# Find means and SDs per sample
n_all_stats <- n_data_clean %>%
  group_by(sample_no, sample_type) %>%
  summarize(mean_nh3 = mean(nh3),
            sd_nh3 = sd(nh3),
            mean_no2_no3 = mean(no2_no3),
            sd_no2_no3 = sd(no2_no3),
            run_time = run_time)

# Pull samples that need to be rerun based on weirdo replicates or because
# they overshot 20 mg/L thresholds (set to 19 here)
# Set a dilution flag to "yes" if they overshot and need to be diluted
n_reruns <- n_all_stats %>%
  filter(sd_nh3 > 1 | sd_no2_no3 > 1) %>%
  rbind(n_all_stats %>%
          filter(mean_nh3 > 19 | mean_no2_no3 > 19)) %>%
  mutate(dilute = (case_when((mean_nh3 > 19 | mean_no2_no3 > 19) ~ "yes"))) %>%
  group_by(sample_no, sample_type, dilute) %>%
  summarize()

# Save this list out
write_csv(n_reruns, paste0("output/2022/", Sys.Date(), "_n_reruns.csv"))

# Get list of jar assignments from 2022
all_treatments <- readr::read_csv("output/2022/jar_assignments/master_list.csv")

# Map data to assignments and find mean + SDs of all samples
source("code/functions/n_functions/run_n_stats.R")
n_data_stats <- run_n_stats(n_data_clean, all_treatments)


source("code/functions/n_functions/plot_n_data.R")
nh3_plot <- plot_n_data(n_data_stats, "nh3", "n")

#############


# Create a plot comparing NH3 levels in extracts between groups with and
# without cover crops across all times
nh3_extracts_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         sample_type == "extract") %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(~drying_treatment) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  # coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")

# Create a plot comparing NH3 levels in leachates between groups with and
# without cover crops across all times
nh3_leach_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(sample_type ~ factor(drying_treatment, levels = c(
    "one_wk", "two_wk", "four_wk", "all_dry"))) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Leachates"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))


# Run statistical analysis on extracts for NH3, excluding cw and no_soil jars
nh3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         pre_post_wet != "cw",
         sample_type == "extract") %>%
  lm(data = ., mean_nh3 ~ pre_post_wet * cc_treatment) %>%
  anova()

# Create a plot comparing NO2 levels between groups with and
# without cover crops at 4 weeks
no2_compare_cc_plot <- n_data_stats %>%
  filter(pre_post_wet != "no soil") %>%
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
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")


# Create a plot comparing NH3 loss as a ratio of NH3 concentration in
# leachate to total extractable N
nh3_ratio_plot <- n_data_stats %>%
  filter(pre_post_wet != "no_soil",
         sample_type == "extract") %>%
  group_by(pre_post_wet, cc_treatment, drying_treatment) %>%
  #IDK HOW TO DO THIS
  summarize(ratio = case_when())
  ggplot(aes(x = pre_post_wet,
             y = mean_mean_nh3,
             fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  facet_grid(~drying_treatment) +
  geom_errorbar(aes(ymax = mean_mean_nh3 + sd_mean_nh3,
                    ymin = mean_mean_nh3 - sd_mean_nh3),
                size = 0.25,
                width = 0.2,
                position = position_dodge(0.9)) +
  scale_x_discrete(limits = c("all_dry", "cw", "pre", "post"),
                   labels = c("All Dry", "Constant Water", "Pre Wetting",
                              "Post Wetting")) +
  # coord_cartesian(ylim=c(0, 8)) +
  labs(title = paste("NH3 in Samples With and Without Cover Crop\n",
                     " Residue in Soil Extracts"),
       x = "Water Treatments", y = "NH3 (ppm)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(breaks = c("w_cc", "no_cc"),
                      labels = c("Without cover crop", "With cover crop"))
##########

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
                      labels = c("Without cover crop", "With cover crop")) +
  # These additions are for creating poster assets
  scale_fill_manual(values = c("no_cc" = "#00C2FF",
                               "w_cc" = "#3EA50D")) +
  theme(legend.position = "none")

# Run statistical analysis on NO3, excluding cw and no_soil jars
no3_stats <- n_data_stats %>%
  filter(pre_post_wet != "no_soil") %>%
  filter(pre_post_wet != "cw") %>%
  lm(data = ., mean_no3 ~ pre_post_wet * cc_treatment) %>%
  anova()
