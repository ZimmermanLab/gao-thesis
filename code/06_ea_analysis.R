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
