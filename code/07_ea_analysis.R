# This script takes in EA data and evaluates the accuracy
# and precision of the standards.

# Sarah Gao
# July 28, 2021
# hellosarahgao@gmail.com

library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("scales")
library("readxl")

############

# SRMs

# First, we will examine the SRM data to see if there was any drift / trend
# over the run and to determine if we need any correcting factors.

# Read in and clean up EA data
source("code/functions/ea_functions/clean_ea_data.R")
ea_results_clean <- clean_ea_data(
  "data/raw_data/EA_CN/20210727/210727_Run.csv")

# Calculate means and RSDs for both nitrogen and carbon
# for all SRM samples
source("code/functions/ea_functions/ea_calculate_srm_stats.R")
srm_stats <- calculate_srm_stats(ea_results_clean)

############

# SRM PLOTS

# Plot the SRM values
srm_n_plot <- ea_results_clean %>%
  filter(sample_no == "SRM") %>%
  ggplot(aes(x = pos, y = n_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Nitrogen Content",
       x = "Position", y = "N percentage")

srm_c_plot <- ea_results_clean %>%
  filter(sample_no == "SRM") %>%
  ggplot(aes(x = pos, y = c_per)) +
  geom_point() +
  labs(title = "Standard Reference Material Carbon Content",
       x = "Position", y = "C percentage")

ggsave(filename = "output/ea_plots/srm_n_plot.png", srm_n_plot)
ggsave(filename = "output/ea_plots/srm_c_plot.png", srm_c_plot)

############

# SAMPLES

# Calculate means and RSDs for each sample
source("code/functions/ea_functions/ea_calculate_sample_stats.R")
sample_stats <- calculate_sample_stats(ea_results_clean)

# Calculate the mean and median RSD across all samples
sample_rsd <- sample_stats %>%
  filter(is.na(rsd_c) == FALSE) %>%
  select(rsd_c, rsd_n)

# Calculate the mean and median rsds for both C and N across all samples
# Not sure why I put this in here or why it's necessary but nice to have maybe?
median_sample_rsd_c <- median(sample_rsd$rsd_c)
mean_sample_rsd_c <- mean(sample_rsd$rsd_c)

median_sample_rsd_n <- median(sample_rsd$rsd_n)
mean_sample_rsd_n <- mean(sample_rsd$rsd_n)

# Remove RSDs and clean up sample names
samples_only <- sample_stats %>%
  select(sample_no, mean_n, mean_c)
samples_only$sample_no <- str_sub(samples_only$sample_no, -3)

# Map samples and results to master list of treatments
source("code/functions/compile_sample_assignments.R")
all_treatments <- compile_sample_assignments()

source("code/functions/map_results.R")
all_treatments_c_n <- add_results(all_treatments, samples_only)

# Calculate means of each drying x cc treatment group across replicates
c_n_results_means <- all_treatments_c_n %>%
  group_by(drying_treatment, cc_treatment, pre_post_wet) %>%
  summarise(mean_n = mean(mean_n), mean_c = mean(mean_c))

# Create a plot comparing N levels between groups with and
# without cover crops at 4 weeks
n_compare_cc_plot <- c_n_results_means %>%
  filter(drying_treatment == "four_wk") %>%
  ggplot(aes(x = pre_post_wet, y = mean_n, fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "post"),
                   labels = c(
                     "Constant Water", "Post Wetting")) +
  labs(title = paste("Total Nitrogen in Samples With and Without Cover Crop",
                     "Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "Total Nitrogen (%)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(labels = c("Without cover crop", "With cover crop"))

ggsave(n_compare_cc_plot,
       filename = "output/2021/ea_plots/n_v_cc.png")

# Create a plot comparing C levels between groups with and
# without cover crops at 4 weeks
c_compare_cc_plot <- c_n_results_means %>%
  filter(drying_treatment == "four_wk") %>%
  ggplot(aes(x = pre_post_wet, y = mean_c, fill = cc_treatment)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous() +
  scale_x_discrete(limits = c("cw", "post"),
                   labels = c(
                     "Constant Water", "Post Wetting")) +
  labs(title = paste("Total Carbon in Samples With and Without Cover Crop",
                     "Residue After 4 Weeks of Drying"),
       x = "Water Treatments", y = "Total Carbon (%)",
       fill = "Cover Crop Treatment") +
  scale_fill_discrete(labels = c("Without cover crop", "With cover crop"))

ggsave(c_compare_cc_plot,
       filename = "output/2021/ea_plots/c_v_cc_.png")

#########

# Compare C:N ratios
samples_only %>%
  left_join(all_treatments) %>%
  filter(drying_treatment == "four_wk") %>%
  filter(pre_post_wet == "post") %>%
  group_by(cc_treatment) %>%
  summarise(mean_mean_c = mean(mean_c),
            mean_mean_n = mean(mean_n)) %>%
  ggplot(aes(x = cc_treatment,
             y = mean_mean_c / mean_mean_n)) +
  geom_col(position = position_dodge())
